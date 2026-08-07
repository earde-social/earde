(* Only allow local (same-origin) redirects from attacker-controlled values —
   the Referer header and form-carried return paths. Browsers send Referer as
   an absolute URL (scheme://host/path), so a same-origin absolute http(s) URL
   is reduced to its path+query; anything else must already be a local path
   starting with '/'. The result never carries a scheme or authority:
   protocol-relative (//...), backslash, control-character, foreign-host,
   userinfo-bearing and malformed values all collapse to [default] (itself a
   trusted local path), and fragments are dropped. Same-origin means the URL's
   host and effective port match this request's Host header; the scheme only
   has to be http(s) — behind a TLS-terminating proxy the app cannot see the
   outer scheme, and only path+query survives extraction anyway. *)
let safe_local_redirect ?(default = "/") request target =
  let has_forbidden_byte s =
    String.exists (fun c -> c < ' ' || c = '\x7f' || c = '\\') s
  in
  let drop_fragment s =
    match String.index_opt s '#' with
    | Some i -> String.sub s 0 i
    | None -> s
  in
  let local_path s =
    if String.length s >= 2 && String.sub s 0 2 = "//" then None
    else if String.length s > 0 && s.[0] = '/' then Some s
    else None
  in
  let same_origin_path s =
    let uri = Uri.of_string s in
    match (Uri.scheme uri, Uri.host uri, Dream.header request "Host") with
    | Some scheme, Some url_host, Some host_header
      when Uri.userinfo uri = None -> (
        let scheme = String.lowercase_ascii scheme in
        if not (String.equal scheme "http" || String.equal scheme "https") then
          None
        else
          let url_host = String.lowercase_ascii url_host in
          let url_port =
            match Uri.port uri with
            | Some p -> p
            | None -> if String.equal scheme "https" then 443 else 80
          in
          let host_header = String.lowercase_ascii (String.trim host_header) in
          let header_host, header_port =
            match String.rindex_opt host_header ':' with
            | Some i -> (
                let suffix =
                  String.sub host_header (i + 1)
                    (String.length host_header - i - 1)
                in
                match int_of_string_opt suffix with
                | Some p -> (String.sub host_header 0 i, Some p)
                | None -> (host_header, None))
            | None -> (host_header, None)
          in
          let port_matches =
            match header_port with
            | Some p -> p = url_port
            (* A portless Host header implies a default port; both http and
               https defaults count as ours because the proxy owns the outer
               scheme. *)
            | None -> url_port = 80 || url_port = 443
          in
          if String.equal header_host url_host && port_matches then
            let path = match Uri.path uri with "" -> "/" | p -> p in
            let with_query =
              match Uri.verbatim_query uri with
              | Some q when not (String.equal q "") -> path ^ "?" ^ q
              | _ -> path
            in
            (* A same-origin URL can still carry a protocol-relative path
               (https://host//evil) — re-check through the local-path rules. *)
            local_path with_query
          else None)
    | _ -> None
  in
  if has_forbidden_byte target then default
  else
    let target = drop_fragment target in
    match local_path target with
    | Some p -> p
    | None -> (
        match same_origin_path target with Some p -> p | None -> default)

(* Database failures must never reach a response body: the strings produced
   by Caqti_error.show may contain internal driver details, connection
   metadata, and SQL text (including constraint and relation names). Log the
   detail server-side and render this stable generic message instead. *)
let generic_db_error = "A database error occurred. Please try again later."

let db_error_message err =
  Logs.err (fun m -> m "database error: %s" err);
  generic_db_error

(* Scan body text for @username tokens without external library deps.
   Only ASCII-alphanumeric + underscore is valid; deduped via sort_uniq to avoid
   sending the same user multiple notifications from repeated mentions. *)
let extract_mentions text =
  let len = String.length text in
  let mentions = ref [] in
  let i = ref 0 in
  while !i < len do
    if text.[!i] = '@' then begin
      let start = !i + 1 in
      let j = ref start in
      while !j < len && (let c = text.[!j] in
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') || c = '_') do
        incr j
      done;
      if !j > start then
        mentions := String.sub text start (!j - start) :: !mentions;
      i := !j
    end else
      incr i
  done;
  List.sort_uniq String.compare !mentions

(* === IMAGE UPLOADS ===

   The pipeline's policy — which payloads are accepted and the exact argument
   vector ImageMagick runs with — lives in the pure [Image_upload] module.
   What is left here is the IO: the rate-limit check, writing the temporary
   file, running the process off the event loop, and moving the result into
   public storage.

   Callers MUST perform authentication, ban and resource-authorization checks
   before calling this. Nothing below re-derives who may upload; it only
   refuses work that is already authorized but abusive or malformed. *)

(* ImageMagick 7 installs `magick` and (usually) a `convert` compatibility
   alias; ImageMagick 6 installs `convert` only. Resolved once per process
   rather than per request, and deliberately NOT configurable from the
   environment — the binary that decodes hostile bytes is not something a
   request or a stray env var should be able to redirect. *)
let imagemagick_binary =
  lazy
    (let exists name =
       Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote name)) = 0
     in
     if exists "magick" then "magick" else "convert")

(* Bounds how many conversions may run at once. Without this, the move to a
   non-blocking process would replace "one upload stalls everyone" with
   "N concurrent uploads fork N ImageMagick processes", which is a worse
   failure mode on a single small instance. Requests beyond the bound wait
   for a slot rather than being refused. *)
let image_workers = 2

let image_worker_pool =
  lazy (Lwt_pool.create image_workers (fun () -> Lwt.return_unit))

(* Wall-clock ceiling on one conversion, independent of ImageMagick's own
   `-limit time`: that limit governs decode work, and cannot end a process
   wedged on IO. Comfortably above the limit so the in-process one is what
   normally fires. *)
let image_convert_timeout_seconds = 30.0

let run_image_convert argv =
  Lwt_pool.use (Lazy.force image_worker_pool) (fun () ->
      Lwt.catch
        (fun () ->
          let process = Lwt_process.open_process_none ("", argv) in
          (* Lwt.protected keeps the losing branch of the pick from
             cancelling the status promise the terminate path still needs. *)
          let status = Lwt.protected process#status in
          let%lwt outcome =
            Lwt.pick
              [ (let%lwt s = status in
                 Lwt.return (`Exited s));
                (let%lwt () = Lwt_unix.sleep image_convert_timeout_seconds in
                 Lwt.return `Timeout) ]
          in
          match outcome with
          | `Exited (Unix.WEXITED 0) -> Lwt.return true
          | `Exited _ -> Lwt.return false
          | `Timeout ->
              process#terminate;
              let%lwt _ = process#close in
              Lwt.return false)
        (fun _ -> Lwt.return false))

(* Returns Ok None when no file was submitted (the field is present but
   empty on every one of these forms). Every failure path removes both
   temporary files, so a refused or crashed conversion leaves nothing
   behind and nothing ever reaches static/uploads. *)
let process_image_upload ~db ~ip ~purpose image_bytes =
  if image_bytes = "" then Lwt.return (Ok None)
  else if String.length image_bytes > Image_upload.max_bytes then
    Lwt.return (Error Image_upload.too_large_message)
  else
    match Image_upload.detect_format image_bytes with
    | None ->
        (* Refused on the payload's own leading bytes, before a temporary
           file exists and before any decoder is invoked. *)
        Lwt.return (Error Image_upload.rejected_message)
    | Some format -> (
        match%lwt Db.Rate_limit.check_upload db ip with
        | Ok `Blocked -> Lwt.return (Error Image_upload.rate_limited_message)
        | Ok `Allowed | Error _ ->
            (* Same fail-open-on-storage-error posture as the request-path
               limiter: a database problem must not make uploads impossible. *)
            let ts = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
            let rand = Random.int 999999 in
            let base = Printf.sprintf "earde_%Ld_%06d" ts rand in
            let tmp_path = Filename.concat (Filename.get_temp_dir_name ()) (base ^ ".tmp") in
            let webp_path = Filename.concat (Filename.get_temp_dir_name ()) (base ^ ".webp") in
            let dest_name = base ^ ".webp" in
            let dest_path = "static/uploads/" ^ dest_name in
            let url_path = "/static/uploads/" ^ dest_name in
            let cleanup () =
              (try Sys.remove tmp_path with _ -> ());
              (try Sys.remove webp_path with _ -> ())
            in
            Lwt.catch
              (fun () ->
                let oc = open_out_bin tmp_path in
                output_string oc image_bytes;
                close_out oc;
                let argv =
                  Image_upload.convert_argv
                    ~binary:(Lazy.force imagemagick_binary)
                    ~format ~purpose ~input:tmp_path ~output:webp_path
                in
                let%lwt converted = run_image_convert argv in
                (try Sys.remove tmp_path with _ -> ());
                if not converted then begin
                  cleanup ();
                  Lwt.return (Error Image_upload.rejected_message)
                end
                else
                  (* Rename rather than shelling out to mv. Falls back to a
                     copy when /tmp and static/uploads are on different
                     filesystems, which Sys.rename cannot cross. *)
                  match
                    (try
                       Sys.rename webp_path dest_path;
                       `Ok
                     with _ -> (
                       try
                         let ic = open_in_bin webp_path in
                         let len = in_channel_length ic in
                         let data = really_input_string ic len in
                         close_in ic;
                         let oc = open_out_bin dest_path in
                         output_string oc data;
                         close_out oc;
                         (try Sys.remove webp_path with _ -> ());
                         `Ok
                       with _ -> `Failed))
                  with
                  | `Ok -> Lwt.return (Ok (Some url_path))
                  | `Failed ->
                      cleanup ();
                      Lwt.return (Error "Failed to store the processed image."))
              (fun _ ->
                cleanup ();
                (* The exception text is not reflected: it can name host
                   paths and errno detail the uploader has no business
                   seeing. *)
                Lwt.return (Error Image_upload.rejected_message)))

(* === RATE LIMITING === *)

(* DB-backed rate limit trades a synchronous Hashtbl lookup for a round-trip to
   Postgres; the ~1ms I/O penalty is the price of crash resilience and shared
   state across replicas — unavoidable once we move beyond a single process. *)
module Rate_limit = struct
  (* Opportunistic expiry cleanup, piggybacked on rate-limited requests at a
     bounded cadence: at most one batch per [cleanup_every_seconds] per
     process, off the response path (Lwt.async, same pattern as the PostHog
     deletion attempts). The retention rule lives in Db.Rate_limit
     (rows strictly older than 2x the enforcement window); a cleanup failure
     only logs a bounded, IP-free error and never affects the limiter's
     Allowed/Blocked decision. A racing double-fire between the read and the
     write of [last_cleanup] just runs a second idempotent batch. *)
  let cleanup_every_seconds = 600.0

  let last_cleanup = ref 0.0

  let maybe_cleanup request =
    let now = Unix.gettimeofday () in
    if now -. !last_cleanup >= cleanup_every_seconds then begin
      last_cleanup := now;
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              match%lwt
                Dream.sql request (fun db -> Db.Rate_limit.cleanup_expired db)
              with
              | Ok _deleted -> Lwt.return_unit
              | Error e ->
                  Dream.log "rate-limit cleanup failed: %s" e;
                  Lwt.return_unit)
            (fun exn ->
              Dream.log "rate-limit cleanup skipped: %s"
                (Printexc.to_string exn);
              Lwt.return_unit))
    end

  let middleware inner_handler request =
    let ip = Dream.client request in
    (* Path only: the rate-limit table must never persist query values (reset
       tokens, OAuth state/code, search terms), and /login?x=y must share
       /login's bucket rather than minting a fresh one per query string. *)
    let endpoint = Request_target_redaction.path_only (Dream.target request) in
    maybe_cleanup request;
    match%lwt Dream.sql request (fun db -> Db.Rate_limit.check db ip endpoint) with
    | Ok `Blocked ->
        let user = Dream.session_field request "username" in
        (* The blocked page's return link reuses the path-only endpoint: echoing
           the full target would leak query secrets (OAuth code/state, reset
           tokens) into the rendered HTML. *)
        Dream.html (Pages.msg_page ~auth:true ?user ~title:"Too Many Attempts"
          ~message:"Too many attempts. Please try again later."
          ~alert_type:"error" ~return_url:endpoint request)
    | Ok `Allowed -> inner_handler request
    | Error _ -> inner_handler request
end

(* === SHARED HELPERS === *)

let get_current_user_votes db request =
  match Dream.session_field request "user_id" with
  | Some uid_str ->
      (match%lwt Db.get_user_post_votes db (int_of_string uid_str) with
      | Ok v -> Lwt.return v
      | Error _ -> Lwt.return [])
  | None -> Lwt.return []

let get_current_user_comment_votes db request =
  match Dream.session_field request "user_id" with
  | Some uid_str ->
      (match%lwt Db.get_user_comment_votes db (int_of_string uid_str) with
      | Ok v -> Lwt.return v
      | Error _ -> Lwt.return [])
  | None -> Lwt.return []

(* === PRIVATE-COMMUNITY READ GATE (Slice C) ===
   Privacy is a server-side permission and is decided HERE, in the handler (the security
   boundary) — never trusted from the client. Public communities are readable by everyone; a
   private community is readable only by a global admin, a community moderator, or a member.
   The pure Db.can_read_community encodes the decision; this wrapper gathers the booleans from
   real DB/session checks. Fails CLOSED: a membership/mod DB error denies access, unless the
   viewer is a global admin (whose authority does not depend on a per-community row). *)
let can_view_community db ~user_id ~is_admin (community : Db.community) =
  match community.Db.visibility with
  | Db.Community_public -> Lwt.return true
  | Db.Community_private ->
      if is_admin then Lwt.return true
      else if user_id <= 0 then Lwt.return false
      else begin
        let%lwt is_member =
          match%lwt Db.is_member db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
        let%lwt is_mod =
          match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
        Lwt.return (Db.can_read_community community.Db.visibility ~is_member ~is_mod ~is_admin)
      end

(* SEO/discovery (Slice D), NOT access control: a community-content page renders for an
   authorized viewer but must carry <meta robots noindex> when the community is private
   (always effectively non-indexable) or public-but-indexable=false ("unlisted-ish"). Mirrors
   the DB-level public-discovery filter so noindex and feed/search exclusion stay in lockstep. *)
let community_noindex (community : Db.community) =
  not (Db.effective_indexable_community community.Db.visibility ~community_indexable:community.Db.indexable)

(* Slice G: noindex for a CHILD surface (a forum section page or a channel archive page).
   effective_indexable_child encodes the dominance order: a private community kills it outright,
   otherwise BOTH the community and the child must be indexable. A non-indexable child still
   RENDERS (this is SEO only, not access control) — the handler never gates on it. *)
let child_noindex (community : Db.community) ~child_indexable =
  not (Db.effective_indexable_child community.Db.visibility
         ~community_indexable:community.Db.indexable ~child_indexable)

(* Slice G: a thread inherits noindex from the forum section it lives in. A post in a
   non-indexable section is noindex even inside a public/indexable community; community-level
   rules (private / community indexable=false) still dominate via child_noindex. A post with no
   section (root/legacy/uncategorized — section_slug = None) falls back to community noindex.
   Fails SAFE: if a post claims a section we cannot resolve, prefer noindex over leaking it. *)
let thread_noindex db (community : Db.community) (post : Db.post) =
  match post.Db.section_slug with
  | None -> Lwt.return (community_noindex community)
  | Some slug ->
      match%lwt Db.get_section_by_slug db slug community.id with
      | Ok (Some section) ->
          Lwt.return (child_noindex community ~child_indexable:section.Db.indexable)
      | _ -> Lwt.return true

(* Single 404 used for BOTH a missing community AND a denied private read, so a hidden private
   community is byte-for-byte indistinguishable from one that never existed (no enumeration).
   Always returns to "/" — never links back into the (possibly private) community. *)
let community_not_found ?user request =
  Dream.respond ~status:`Not_Found
    (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist."
       ~alert_type:"error" ~return_url:"/" request)

(* === AUTHENTICATION === *)

(* Public signup is gated so it can be reopened deliberately after the bot incident.
   Default CLOSED: only an explicit truthy EARDE_SIGNUPS_ENABLED opens it. *)
let signups_enabled () =
  match Sys.getenv_opt "EARDE_SIGNUPS_ENABLED" with
  | Some v ->
      (match String.lowercase_ascii (String.trim v) with
       | "1" | "true" | "yes" | "on" -> true
       | _ -> false)
  | None -> false

(* Route-safe ASCII syntax for NEW usernames. A username is a path segment
   (/u/:username), a form value, and a rendered identity on a public,
   crawlable profile; before this rule the only check was a length bound, so
   quotes and angle brackets could be registered and later rendered. Escaping
   at every sink is the actual XSS defence and stays in place — this is the
   second layer, and it also keeps handles unambiguous in URLs and @mentions
   (extract_mentions already recognises exactly this alphabet).

   Deliberately NOT applied to existing accounts: login, lookup and rendering
   never call it, so no current user is locked out or renamed. Pure, so it is
   unit-tested without a database. *)
let is_valid_new_username name =
  String.length name > 0
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_' || c = '-')
       name

let signups_closed_page request =
  let user = Dream.session_field request "username" in
  Pages.msg_page ~auth:true ?user ~title:"Signups are closed"
    ~message:"Earde is in private alpha and public signups are temporarily closed. Check back soon."
    ~alert_type:"info" ~return_url:"/" request

(* Shown after a POST that produced (or would have produced) a pending signup. The honeypot
   path renders the SAME page so a bot can't tell from the response that it was caught. *)
let check_your_email_page request =
  let user = Dream.session_field request "username" in
  Pages.msg_page ~auth:true ?user ~title:"Check your email"
    ~message:"If everything looks good, we've sent a confirmation link to your email address. Click it within 24 hours to finish creating your account."
    ~alert_type:"info" ~return_url:"/login" request

(* EARDE_TURNSTILE_REQUIRED is set but the Turnstile keys are missing/empty.
   Rather than serve a normal signup form that would create accounts with no bot
   protection, fail closed. Copy is generic so it doesn't reveal the misconfig. *)
let turnstile_unavailable_page request =
  let user = Dream.session_field request "username" in
  Pages.msg_page ~auth:true ?user ~title:"Signup temporarily unavailable"
    ~message:"Signups are temporarily unavailable. Please try again later."
    ~alert_type:"info" ~return_url:"/" request

let signup_page request =
  if not (signups_enabled ()) then Dream.html (signups_closed_page request)
  else
    let user = Dream.session_field request "username" in
    match Turnstile.status () with
    | Turnstile.Misconfigured -> Dream.html (turnstile_unavailable_page request)
    | Turnstile.Configured site_key ->
        Dream.html (Pages.signup_form ?user ~turnstile_site_key:site_key request)
    | Turnstile.Disabled -> Dream.html (Pages.signup_form ?user request)

(* Resolve the Turnstile outcome for a POST. Honeypot is handled by the caller
   FIRST (a caught bot never reaches here, so we spend no siteverify call on it).
   On `Failed the site key is returned so the form re-renders with a fresh widget. *)
let verify_turnstile form_data =
  match Turnstile.status () with
  | Turnstile.Misconfigured -> Lwt.return `Unavailable
  | Turnstile.Disabled -> Lwt.return (`Passed None)
  | Turnstile.Configured site_key ->
      let token =
        String.trim (List.assoc_opt "cf-turnstile-response" form_data |> Option.value ~default:"")
      in
      if token = "" then Lwt.return (`Failed site_key)
      else
        let%lwt ok = Turnstile.verify ~response:token in
        if ok then Lwt.return (`Passed (Some site_key)) else Lwt.return (`Failed site_key)

let signup_handler request =
  (* Gate first: closed signup creates no pending row, no user, and sends no email. *)
  if not (signups_enabled ()) then Dream.html (signups_closed_page request)
  else
  match%lwt Dream.form request with
  | `Ok form_data ->
      let username = String.trim (List.assoc_opt "username" form_data |> Option.value ~default:"") in
      let email    = String.trim (List.assoc_opt "email"    form_data |> Option.value ~default:"") in
      let password = List.assoc_opt "password" form_data |> Option.value ~default:"" in
      (* Honeypot: a hidden field no human ever fills. If populated, silently drop —
         create nothing, send nothing — but show the normal "check your email" page. *)
      let honeypot = String.trim (List.assoc_opt "website" form_data |> Option.value ~default:"") in

      if honeypot <> "" then Dream.html (check_your_email_page request)
      else
      (* Turnstile gate: must pass BEFORE any precheck, argon2, pending row, or
         email. Fails closed — a missing/invalid token or a Cloudflare timeout
         creates nothing and sends nothing. *)
      (match%lwt verify_turnstile form_data with
      | `Unavailable -> Dream.html (turnstile_unavailable_page request)
      | `Failed site_key ->
          let user = Dream.session_field request "username" in
          Dream.html (Pages.signup_form ?user ~turnstile_site_key:site_key
                        ~error:"Human verification failed. Please try again." request)
      | `Passed turnstile_site_key ->

      (* Validate before hashing — argon2 is expensive, reject obvious bad input early. *)
      if username = "" || email = "" || password = "" then
        Dream.html (Pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Username, email, and password are all required." ~alert_type:"error" ~return_url:"/signup" request)
      else if String.length username < 3 || String.length username > 30 then
        Dream.html (Pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Username must be between 3 and 30 characters." ~alert_type:"error" ~return_url:"/signup" request)
      else if not (is_valid_new_username username) then
        Dream.html (Pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Username can only contain letters, numbers, underscores and hyphens." ~alert_type:"error" ~return_url:"/signup" request)
      else if not (String.contains email '@') then
        Dream.html (Pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Please enter a valid email address." ~alert_type:"error" ~return_url:"/signup" request)
      else if String.length password < 8 then
        Dream.html (Pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Password must be at least 8 characters long." ~alert_type:"error" ~return_url:"/signup" request)
      else

      (* Pre-checks before the expensive argon2 hash (mirrors the old user_exists pattern,
         avoiding raw constraint errors): reject a name/email already owned by a real user,
         or a username already held by a DIFFERENT live pending signup. *)
      let%lwt precheck = Dream.sql request (fun db ->
        match%lwt Db.user_exists db username email with
        | Error e -> Lwt.return (Error e)
        | Ok true -> Lwt.return (Ok `User_taken)
        | Ok false ->
            (match%lwt Db.pending_signup_username_elsewhere db username email with
             | Error e -> Lwt.return (Error e)
             | Ok true -> Lwt.return (Ok `Username_pending)
             | Ok false -> Lwt.return (Ok `Available))
      ) in
      (match precheck with
      | Error err ->
          Dream.html (Pages.msg_page ~auth:true ~title:"Registration Failed" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/signup" request)
      | Ok `User_taken ->
          let user = Dream.session_field request "username" in
          Dream.html (Pages.signup_form ?user ?turnstile_site_key ~error:"This username or email is already taken." request)
      | Ok `Username_pending ->
          let user = Dream.session_field request "username" in
          Dream.html (Pages.signup_form ?user ?turnstile_site_key ~error:"That username is already taken or pending confirmation." request)
      | Ok `Available ->

      (match%lwt Auth.hash_password password with
      | Ok password_hash ->

          let token = Dream.to_base64url (Dream.random 32) in
          let token_hash = Db.pending_signup_hash_token token in
          let ip = Some (Dream.client request) in
          let user_agent = Dream.header request "User-Agent" in

          (* DB connection is returned to the pool before the Brevo HTTP call —
             holding a pool slot for a third-party round-trip would starve concurrent
             signups under load. No users row and no session are created here: only a
             confirmed (link-clicked) pending becomes a real user. *)
          let%lwt create_result = Dream.sql request (fun db ->
            let%lwt r = Db.pending_signup_upsert db ~username ~email ~password_hash ~token_hash ~ip ~user_agent in
            (* Best-effort secondary cleanup; correctness does not depend on it. *)
            let%lwt _ = Db.pending_signup_sweep_expired db in
            Lwt.return r
          ) in
          (match create_result with
          | Ok () ->
              let%lwt () = Email.send_pending_signup_confirmation_email ~to_email:email ~token in
              Dream.html (check_your_email_page request)
          | Error err ->
              Dream.html (Pages.msg_page ~auth:true ~title:"Registration Failed" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/signup" request))
      | Error err -> Dream.html (Pages.msg_page ~auth:true ~title:"Security Error" ~message:("Security error: " ^ err) ~alert_type:"error" ~return_url:"/signup" request))))

  | _ -> Dream.html (Pages.msg_page ~auth:true ~title:"Form Error" ~message:"Your form submission failed. The CSRF token was invalid or your session expired. Please try again." ~alert_type:"error" ~return_url:"/signup" request)

let verify_email_handler request =
  match Dream.query request "token" with
  | None -> Dream.html (Pages.msg_page ~auth:true ~title:"Verification Error" ~message:"The verification token is missing from the URL." ~alert_type:"error" ~return_url:"/signup" request)
  | Some token ->
      Dream.sql request (fun db ->
        match%lwt Db.verify_email db token with
        | Ok (Some username) ->
            Dream.html (Pages.msg_page ~auth:true ~title:"Email Verified!" ~message:(Printf.sprintf "Your account u/%s is now verified. You can log in." username) ~alert_type:"success" ~return_url:"/login" request)
        | Ok None ->
            Dream.html (Pages.msg_page ~auth:true ~title:"Verification Failed" ~message:"This link is invalid or your email has already been verified." ~alert_type:"error" ~return_url:"/signup" request)
        | Error err -> Dream.html (Pages.msg_page ~auth:true ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
      )

(* Pending-signup confirmation: hashing the URL token and matching it is what creates the
   real users row (in one DB transaction). Per product decision we do NOT auto-login — the
   user is sent to /login. Replay (already-consumed) and expired tokens both fail as `Invalid;
   a username/email taken since signup fails as `Conflict. Login/session logic is untouched. *)
(* === ANALYTICS CONSENT (spec §9) === *)

(* Session read that also behaves in tests where no session middleware is
   installed: no middleware simply means no authenticated session. *)
let session_user_id_opt request =
  match Dream.session_field request "user_id" with
  | exception _ -> None
  | value -> value

(* Analytics-only person sync on consent grant. Every failure mode (bad id,
   missing pool, DB error) is swallowed and logged without email/tokens —
   the consent cookie must be set regardless. *)
let consent_grant_person_sync request =
  match session_user_id_opt request with
  | None -> Lwt.return_unit
  | Some uid_str ->
      Lwt.catch
        (fun () ->
          match int_of_string_opt uid_str with
          | None -> Lwt.return_unit
          | Some user_id -> (
              let%lwt props =
                Dream.sql request (fun db ->
                    Db.get_user_analytics_props db user_id)
              in
              match props with
              | Ok (Some (username, _email, signup_date, is_admin)) ->
                  (* Email is deliberately excluded from the closed person
                     properties — it never reaches PostHog. *)
                  Analytics.sync_person_after_consent_grant
                    ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                    { Analytics.username; signup_date; is_admin };
                  Lwt.return_unit
              | Ok None -> Lwt.return_unit
              | Error e ->
                  Dream.log "analytics consent sync lookup failed (user %d): %s"
                    user_id e;
                  Lwt.return_unit))
        (fun exn ->
          Dream.log "analytics consent sync skipped: %s"
            (Printexc.to_string exn);
          Lwt.return_unit)

(* No Dream CSRF and no session required (a first-time landing visitor has
   neither); protection is the §9 JSON-only + Origin/Sec-Fetch-Site check in
   Analytics.validate_consent_request. All responses are controlled JSON/204 —
   never a rendered HTML error page. *)
let analytics_consent_handler request =
  let%lwt body = Dream.body request in
  match
    Analytics.validate_consent_request
      ~content_type:(Dream.header request "Content-Type")
      ~origin:(Dream.header request "Origin")
      ~sec_fetch_site:(Dream.header request "Sec-Fetch-Site")
      ~body
  with
  | Error (`Forbidden reason) ->
      Dream.json ~status:`Forbidden (Printf.sprintf {|{"error":%S}|} reason)
  | Error (`Bad_request reason) ->
      Dream.json ~status:`Bad_Request (Printf.sprintf {|{"error":%S}|} reason)
  | Ok state ->
      (* Person sync runs only on granted, only for an authenticated session,
         and can never fail the response. The just-granted value exists only
         in the outgoing Set-Cookie, so this transition uses the dedicated
         sync function, not capture_if_consented (§3.1). *)
      let%lwt () =
        match state with
        | `Granted -> consent_grant_person_sync request
        | `Denied -> Lwt.return_unit
      in
      let value = match state with `Granted -> "granted" | `Denied -> "denied" in
      let response = Dream.response ~status:`No_Content "" in
      (* Plaintext (encrypt:false) and no HttpOnly: the §9 contract requires
         document.cookie readability (the prerendered landing can determine
         consent only client-side). Secure follows the public origin scheme.
         ~prefix:None is load-bearing: without it Dream infers __Host- for a
         Secure + Path=/ cookie, breaking the exact cross-repo cookie name. *)
      Dream.set_cookie ~prefix:None ~encrypt:false
        ~max_age:Analytics.consent_cookie_max_age ~path:(Some "/")
        ~secure:(Analytics.consent_cookie_secure ())
        ~http_only:false ~same_site:(Some `Lax) response request
        Analytics.consent_cookie_name value;
      Lwt.return response

(* Same exact path, non-POST methods: controlled JSON 405, never landing or
   error HTML (deployment must route the path to Dream before any static
   fallback — spec §10.4). *)
let analytics_consent_method_not_allowed _request =
  Dream.json ~status:`Method_Not_Allowed
    ~headers:[ ("Allow", "POST") ]
    {|{"error":"method not allowed"}|}

(* Step-6 domain events: a success path inside a Dream.sql block RECORDS its
   emission; the recorded thunk runs only after the block returns and its
   pooled connection is released, so the fire-and-forget analytics HTTP never
   overlaps a checked-out DB connection. Nothing recorded ⇒ nothing emitted,
   so validation/authorization/DB failures stay silent by construction, and a
   thunk can only call the closed consent-gated Analytics entry points. *)
let with_analytics_after_sql make_response =
  let pending = ref (fun () -> ()) in
  let%lwt response = make_response (fun thunk -> pending := thunk) in
  !pending ();
  Lwt.return response

(* Centralized private-safe analytics shaping (§13). Fully private
   communities keep numeric ids, counts, and the community:<id> group key,
   but no human-readable identifier (slug, name, channel/section slug) ever
   enters an analytics payload. Every handler and the $groupidentify mapping
   below go through this ONE helper — no per-handler visibility checks. The
   visibility comes from the authoritative community record the handler
   already loaded; no analytics-only DB query exists. *)
let analytics_public_string (community : Db.community) value =
  if Db.community_is_private community.Db.visibility then None else Some value

(* The closed $groupidentify record from an authoritative Db.community row.
   The shared community record carries no created_at column, so that optional
   group property is omitted rather than approximated. Private communities:
   slug and name are None (§13), id and closed visibility remain. *)
let community_group_of (community : Db.community) : Analytics.community_group =
  {
    Analytics.community_id = community.Db.id;
    community_slug = analytics_public_string community community.Db.slug;
    community_name = analytics_public_string community community.Db.name;
    community_visibility =
      Db.community_visibility_to_string community.Db.visibility;
    created_at = None;
  }

(* Immediate async attempt for a freshly enqueued §13 group-cleanup job —
   the exact analog of attempt_posthog_deletion_job: claim, one bounded HTTP
   scrub, mark. Every DB touch is its own short call; failures only leave
   the durable job pending. Logs carry the job id and bounded error classes
   only — never a community name or slug. *)
let attempt_posthog_group_cleanup_job request ~job_id =
  Lwt.catch
    (fun () ->
      let%lwt claimed =
        Dream.sql request (fun db -> Db.claim_posthog_group_cleanup_job db job_id)
      in
      match claimed with
      | Ok (Some group_key) ->
          let%lwt (_ : [ `Completed | `Left_pending of string ]) =
            Posthog_deletion.process_claimed_group_job
              ~mark_completed:(fun () ->
                Dream.sql request (fun db ->
                    Db.complete_posthog_group_cleanup_job db job_id))
              ~mark_failed:(fun err ->
                Dream.sql request (fun db ->
                    Db.fail_posthog_group_cleanup_job db job_id err))
              ~group_key
          in
          Lwt.return_unit
      | Ok None | Error _ -> Lwt.return_unit)
    (fun exn ->
      Dream.log "posthog group cleanup immediate attempt error: %s"
        (Printexc.to_string exn);
      Lwt.return_unit)

let confirm_email_handler request =
  match Dream.query request "token" with
  | None ->
      Dream.html (Pages.msg_page ~auth:true ~title:"Confirmation Error" ~message:"The confirmation token is missing from the URL." ~alert_type:"error" ~return_url:"/signup" request)
  | Some token ->
      let token_hash = Db.pending_signup_hash_token token in
      let%lwt result =
        Dream.sql request (fun db -> Db.pending_signup_confirm db token_hash)
      in
      (match result with
        | Ok (`Confirmed (user_id, username, _email, created_at, is_admin)) ->
            (* The confirmation transaction returned the closed person
               properties with the insert, so the capture needs no lookup. A
               confirmation link opened without granted consent emits
               nothing. Email never enters the analytics payload. *)
            Analytics.capture_if_consented request
              ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
              (Analytics.Account_signed_up
                 {
                   user_id;
                   person =
                     { Analytics.username; signup_date = created_at; is_admin };
                 });
            Dream.html (Pages.msg_page ~auth:true ~title:"Email Confirmed!" ~message:(Printf.sprintf "Your account u/%s is now active. You can log in." username) ~alert_type:"success" ~return_url:"/login" request)
        | Ok `Invalid ->
            Dream.html (Pages.msg_page ~auth:true ~title:"Confirmation Failed" ~message:"This confirmation link is invalid or has expired. Please sign up again." ~alert_type:"error" ~return_url:"/signup" request)
        | Ok `Conflict ->
            Dream.html (Pages.msg_page ~auth:true ~title:"Already Registered" ~message:"An account with this username or email already exists. Please log in." ~alert_type:"error" ~return_url:"/login" request)
        | Error err ->
            Dream.html (Pages.msg_page ~auth:true ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request))

let login_page request =
  let user = Dream.session_field request "username" in
  Dream.html (Pages.login_form ?user request)

let login_handler request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let identifier = List.assoc "identifier" form_data in
      let password = List.assoc "password" form_data in

      (* Credential check uses constant-message pattern: every failure path
         returns the same string to prevent username enumeration. Ban check
         happens only after password is verified to avoid leaking existence. *)
      let%lwt lookup =
        Dream.sql request (fun db -> Db.get_user_for_login db identifier)
      in
      (match lookup with
        | Ok (Some ((id, user, _email, created_at), (hash, is_admin, is_banned))) ->
            (* Argon2 verification runs after the lookup's connection is back
               in the pool — CPU-bound work must not hold a connection open
               (same rule as reset_password_handler). *)
            (match%lwt Auth.verify_password ~password ~hash with
            | Ok true ->
                if is_banned then
                  Dream.html (Pages.msg_page ~auth:true ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/login" request)
                else
                  (* The browser may present a session that already carries
                     another user's identity (or a pre-auth session an attacker
                     could have fixated). Invalidate it so the new login starts
                     from a fresh, empty session with a rotated session id,
                     then write every canonical auth field from the newly
                     authenticated row — is_admin unconditionally, so a prior
                     admin session can never leak privileges into this one. *)
                  let%lwt () = Dream.invalidate_session request in
                  let%lwt () = Dream.set_session_field request "user_id" (string_of_int id) in
                  let%lwt () = Dream.set_session_field request "username" user in
                  let%lwt () = Dream.set_session_field request "is_admin" (if is_admin then "true" else "false") in
                  (* Exactly once per fully successful login (credentials
                     verified, not banned); the incoming request still carries
                     the consent cookie the gate reads. *)
                  Analytics.capture_if_consented request
                    ~distinct_id:(Analytics.distinct_id_of_user_id id)
                    (Analytics.Account_logged_in
                       {
                         user_id = id;
                         person =
                           { Analytics.username = user; signup_date = created_at; is_admin };
                       });
                  Dream.redirect request "/"
              | _ -> Dream.html (Pages.msg_page ~auth:true ~title:"Login Failed" ~message:"Invalid username or password." ~alert_type:"error" ~return_url:"/login" request))
        | Ok None -> Dream.html (Pages.msg_page ~auth:true ~title:"Login Failed" ~message:"Invalid username or password." ~alert_type:"error" ~return_url:"/login" request)
        | Error err -> Dream.html (Pages.msg_page ~auth:true ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/login" request))
  | _ -> Dream.html (Pages.msg_page ~auth:true ~title:"Form Error" ~message:"There was a problem with your form submission. Your session may have expired." ~alert_type:"error" ~return_url:"/login" request)

let logout_handler request =
  let%lwt () = Dream.invalidate_session request in
  Dream.redirect request "/"

let forgot_password_page request = Dream.html (Pages.forgot_password_page request)

(* Never confirm or deny email existence — identical response hides whether the
   address is registered, preventing account enumeration via the reset flow. *)
let forgot_password_handler request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let email = String.trim (List.assoc_opt "email" form_data |> Option.value ~default:"") in
      if email = "" then
        Dream.html (Pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Email address is required." ~alert_type:"error" ~return_url:"/forgot-password" request)
      else begin
        let token = Dream.to_base64url (Dream.random 32) in
        (* DB connection released before Brevo call — same pattern as signup. *)
        let%lwt result = Dream.sql request (fun db ->
          Db.password_reset_create_token db email token
        ) in
        (match result with
        | Ok true ->
            let%lwt () = Email.send_password_reset_email ~to_email:email ~token in
            Dream.html (Pages.msg_page ~auth:true ~title:"Check your email" ~message:"If an account with that email exists, a reset link has been sent. Check your inbox (and spam folder)." ~alert_type:"info" ~return_url:"/login" request)
        | Ok false ->
            Dream.html (Pages.msg_page ~auth:true ~title:"Check your email" ~message:"If an account with that email exists, a reset link has been sent. Check your inbox (and spam folder)." ~alert_type:"info" ~return_url:"/login" request)
        | Error err ->
            Dream.log "forgot_password DB error: %s" err;
            Dream.html (Pages.msg_page ~auth:true ~title:"Check your email" ~message:"If an account with that email exists, a reset link has been sent. Check your inbox (and spam folder)." ~alert_type:"info" ~return_url:"/login" request))
      end
  | _ -> Dream.html (Pages.msg_page ~auth:true ~title:"Form Error" ~message:"Your form submission failed. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)

let reset_password_page_handler request =
  match Dream.query request "token" with
  | None ->
      Dream.html (Pages.msg_page ~auth:true ~title:"Invalid Link" ~message:"This password reset link is missing a token. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
  | Some token ->
      (match%lwt Dream.sql request (fun db -> Db.password_reset_validate_token db token) with
      | Ok (Some _) -> Dream.html (Pages.reset_password_page ~token request)
      | Ok None ->
          Dream.html (Pages.msg_page ~auth:true ~title:"Link Expired" ~message:"This password reset link is invalid or has expired. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
      | Error _ ->
          Dream.html (Pages.msg_page ~auth:true ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request))

(* Argon2 hashing runs before the DB transaction — CPU-bound work must not hold
   a connection open. The token DELETE and password UPDATE are atomic: a hash or
   DB error rolls back the DELETE so the user keeps the reset link. *)
let reset_password_handler request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let token    = List.assoc_opt "token"            form_data |> Option.value ~default:"" in
      let password = List.assoc_opt "password"         form_data |> Option.value ~default:"" in
      let confirm  = List.assoc_opt "confirm_password" form_data |> Option.value ~default:"" in
      if token = "" then
        Dream.html (Pages.msg_page ~auth:true ~title:"Invalid Request" ~message:"Token is missing. Please use the link from your email." ~alert_type:"error" ~return_url:"/forgot-password" request)
      else if password <> confirm then
        Dream.html (Pages.reset_password_page ~token ~error:"Passwords do not match." request)
      else if String.length password < 8 then
        Dream.html (Pages.reset_password_page ~token ~error:"Password must be at least 8 characters." request)
      else
        (match%lwt Auth.hash_password password with
        | Error err ->
            Dream.log "reset_password hash error: %s" err;
            Dream.html (Pages.msg_page ~auth:true ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)
        | Ok new_hash ->
            Dream.sql request (fun db ->
              match%lwt Db.password_reset_atomically db token new_hash with
              | Ok false ->
                  Dream.html (Pages.msg_page ~auth:true ~title:"Link Expired" ~message:"This reset link is invalid or has expired. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
              | Ok true ->
                  Dream.html (Pages.msg_page ~auth:true ~title:"Password Updated" ~message:"Your password has been updated. You can now log in with your new password." ~alert_type:"success" ~return_url:"/login" request)
              | Error err ->
                  Dream.log "reset_password error: %s" err;
                  Dream.html (Pages.msg_page ~auth:true ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)))
  | _ -> Dream.html (Pages.msg_page ~auth:true ~title:"Form Error" ~message:"Your form submission failed. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)

(* === CORE FEED ===

   /feed is the only global feed surface; / and /all are redirects to it (see
   bin/main.ml). The pre-/feed home handler and its warm-chrome renderer were
   removed with the rest of the legacy chrome. *)

(* /feed — the global Feed surface (shell-language, outside any one community). Reuses the same
   feed queries as home/all: "following" → personalized feed from joined communities, "all" →
   every public persistent post. No new query, no migration, no realtime. Default scope is
   following for logged-in users; guests are forced to "all" (no personalized feed without a
   session) and never see the toggle. *)
let feed_handler request =
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in
  let is_logged_in = user_id > 0 in

  let page = match Dream.query request "page" with
    | Some p_str -> (try int_of_string p_str with _ -> 1)
    | None -> 1
  in
  let sort_mode = match Dream.query request "sort" with
    | Some "new"    -> Db.Newest
    | Some "top"    -> Db.Top
    | Some "active" -> Db.Active
    | _             -> Db.Hot
  in
  let sort_str = match sort_mode with Db.Newest -> "new" | Db.Top -> "top" | Db.Hot -> "hot" | Db.Active -> "active" in
  (* Guests can't have a personalized feed, so any scope coerces to "all" for them. *)
  let scope = match Dream.query request "scope", is_logged_in with
    | Some "all", _ -> "all"
    | _, false      -> "all"
    | _             -> "following"
  in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  Dream.sql request (fun db ->
    let%lwt posts =
      if scope = "following" then Db.get_personalized_feed db user_id sort_mode limit offset
      else Db.get_all_posts db sort_mode limit offset
    in
    let%lwt user_votes =
      if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok []
    in
    let%lwt user_communities =
      if user_id > 0 then Db.get_user_communities db user_id else Lwt.return_ok []
    in
    let%lwt admin_usernames_res = Db.get_admin_usernames db in
    let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in

    match posts, user_votes, user_communities with
    | Ok p, Ok v, Ok rail ->
        (* Origin-side Shared Threads provenance for exactly the page of
           canonical posts the feed query already selected: ONE bounded
           batch read (never per-post), grouped here into a
           post_id -> destinations map. Purely additive enrichment — the
           feed's selection, ordering, and pagination above are untouched —
           and a failure degrades to no indicators, never a failed page
           (the chat-provenance pattern). *)
        let%lwt shared_destinations =
          match%lwt
            Shared_thread_reading.public_destinations_for_posts db
              ~post_ids:(List.map (fun (post : Db.post) -> post.id) p)
          with
          | Ok rows ->
              let grouped =
                List.fold_left
                  (fun acc (post_id, dest) ->
                    let existing =
                      Option.value ~default:[] (List.assoc_opt post_id acc) in
                    (post_id, existing @ [ dest ])
                    :: List.remove_assoc post_id acc)
                  [] rows
              in
              Lwt.return grouped
          | Error _ -> Lwt.return []
        in
        Dream.html (Pages.feed_page ?user ~scope ~sort_mode:sort_str ~is_logged_in
                      ~admin_usernames ~rail_communities:rail ~user_votes:v ~current_page:page
                      ~shared_destinations p request)
    | Error e, _, _ | _, Error e, _ | _, _, Error e ->
        Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
  )

let search_handler request =
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in

  let active_tab = match Dream.query request "t" with Some t -> t | None -> "posts" in
  (* Trim the query; whitespace-only is treated as empty. An empty query renders the search
     page's local prompt state (no redirect) so /search is a real, bookmarkable surface. No
     minimum length is imposed — a one-character query like /search?q=a still runs. *)
  let search_term = match Dream.query request "q" with Some q -> String.trim q | None -> "" in

  if search_term = "" then
    (* The empty-query prompt state needs no search data, but members still get
       their joined communities for the launch rail (same degrade-to-empty rule
       as notifications). Anonymous prompts stay DB-free. *)
    if user_id > 0 then
      Dream.sql request (fun db ->
        let%lwt rail_communities =
          match%lwt Db.get_user_communities db user_id with
          | Ok cs -> Lwt.return cs
          | Error _ -> Lwt.return []
        in
        Dream.html (Pages.search_results_page ?user ~admin_usernames:[] ~rail_communities [] 1 active_tab "" [] [] [] [] request))
    else
      Dream.html (Pages.search_results_page ?user ~admin_usernames:[] [] 1 active_tab "" [] [] [] [] request)
  else begin
      let page = match Dream.query request "page" with Some p_str -> (try int_of_string p_str with _ -> 1) | None -> 1 in
      let limit = 20 in
      let offset = (max 1 page - 1) * limit in

      Dream.sql request (fun db ->
        let%lwt communities_res = Db.search_communities db search_term limit offset in
        let%lwt users_res = Db.search_users db search_term limit offset in
        let%lwt posts_res = Db.search_posts db search_term limit offset in
        let%lwt comments_res = Db.search_comments db search_term limit offset in
        let%lwt user_votes = if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok [] in
        let%lwt admin_usernames_res = Db.get_admin_usernames db in
        let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
        (* Joined communities feed the launch rail only; a failure degrades to
           an empty rail rather than failing the search. *)
        let%lwt rail_communities =
          if user_id > 0 then
            match%lwt Db.get_user_communities db user_id with
            | Ok cs -> Lwt.return cs
            | Error _ -> Lwt.return []
          else Lwt.return []
        in

        match communities_res, users_res, posts_res, comments_res, user_votes with
        | Ok communities, Ok users, Ok posts, Ok comments, Ok votes ->
            (* Chat-provenance is only shown on the Threads tab, so fetch it only there — one
               bounded query over this page's post ids (no N+1). A failure degrades to no
               markers rather than failing the whole search. *)
            let%lwt chat_sources =
              if active_tab = "communities" || active_tab = "people" || active_tab = "comments"
              then Lwt.return []
              else match%lwt Db.get_thread_sources_for_posts db (List.map (fun (p : Db.post) -> p.id) posts) with
                | Ok l -> Lwt.return l
                | Error _ -> Lwt.return []
            in
            Dream.html (Pages.search_results_page ?user ~admin_usernames ~chat_sources ~rail_communities votes page active_tab search_term communities users posts comments request)
        | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:"Database error during search. Please try again." ~alert_type:"error" ~return_url:"/" request)
      )
    end

(* === COMMUNITY === *)

let new_community_page request =
  (* Legacy generic creation is global-admin only; everyone else lands on the
     onboarding explainer. Admins keep the pre-existing flow untouched. *)
  match
    Project_onboarding.legacy_creation_get_decision
      ~is_admin:(Dream.session_field request "is_admin" = Some "true")
  with
  | Project_onboarding.Redirect_to_bring | Project_onboarding.Forbid ->
      Dream.redirect request "/bring"
  | Project_onboarding.Show_form ->
  match Dream.session_field request "user_id" with
  | None ->
      Dream.redirect request "/login"
  | Some uid_str ->
      let user = Dream.session_field request "username" in
      (* Rail data only AFTER both gates above: denied or redirected viewers
         never reach this query. Best-effort — a failure degrades to an
         empty rail rather than blocking the admin utility. *)
      let%lwt rail_communities =
        match int_of_string_opt uid_str with
        | None -> Lwt.return []
        | Some uid ->
            Dream.sql request (fun db ->
                match%lwt Db.get_user_communities db uid with
                | Ok communities -> Lwt.return communities
                | Error _ -> Lwt.return [])
      in
      Dream.html (Pages.new_community_form ?user ~rail_communities request)

let create_community_handler request =
  (* Server-side admin gate before any form parsing, so a forged form from a
     non-admin session (or no session) is rejected outright. *)
  match
    Project_onboarding.legacy_creation_post_decision
      ~is_admin:(Dream.session_field request "is_admin" = Some "true")
  with
  | Project_onboarding.Forbid | Project_onboarding.Redirect_to_bring ->
      Dream.respond ~status:`Forbidden
        "Forbidden: community creation is restricted to Earde administrators."
  | Project_onboarding.Show_form ->
  match Dream.session_field request "user_id" with
  | None ->
      Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let name = String.trim (List.assoc_opt "name" form_data |> Option.value ~default:"") in
          let slug = String.trim (List.assoc_opt "slug" form_data |> Option.value ~default:"") in

          let description_str = List.assoc_opt "description" form_data |> Option.value ~default:"" in
          let description =
            if description_str = "" then None else Some description_str
          in

          (* Post-pivot: every community is a structured shell. The form's
             community_type field is ignored — no community is ever "simple". *)
          let sections_enabled = true in

          let section_count = List.assoc_opt "section_count" form_data |> Option.value ~default:"0" |> int_of_string in

          (* Validate before hitting DB — slug uniqueness error is more helpful than a generic 500. *)
          if name = "" || slug = "" then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Community name and URL slug are required." ~alert_type:"error" ~return_url:"/new-community" request)
          else

          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            match%lwt Db.create_community db name slug description sections_enabled with
            | Ok () ->
                (* Divine Right: creator becomes first top_mod automatically.
                   Round-trip via get_community_by_slug is necessary — INSERT does not
                   return the new id, and changing create_community's return type would
                   cascade through the mli and all other callers.
                   add_top_moderator (not add_moderator) so the creator can see the
                   Manage Moderators link immediately — default role is 'mod'. *)
                (match%lwt Db.get_community_by_slug db slug with
                 | Ok (Some community) ->
                     (* Shell invariant: every community must have a default General forum
                        section and a default general chat channel. These two are created
                        FIRST and their results are CHECKED — a community is only considered
                        successfully created if both exist, so the invariant is real rather
                        than aspirational. (top-mod/join/custom-sections below stay
                        best-effort, matching prior behaviour.) *)
                     let setup_error () =
                       Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Could not finish setting up the community. Please try again." ~alert_type:"error" ~return_url:"/new-community" request)
                     in
                     (match%lwt Db.create_section db community.id "General" (Some "General discussion") 0 "new" false with
                      | Error _ -> setup_error ()
                      | Ok () ->
                          (match%lwt Db.create_channel db community.id "general" (Some "General chat") 0 with
                           | Error _ -> setup_error ()
                           | Ok _slug ->
                               let%lwt _ = Db.add_top_moderator db user_id community.id in
                               let%lwt _ = Db.join_community db user_id community.id in
                               (* Insert any custom sections from the form, in addition to General. *)
                               let%lwt () = Lwt_list.iteri_s (fun i pos ->
                                 let idx = i + 1 in
                                 let sname = String.trim (List.assoc_opt ("section_name_" ^ string_of_int idx) form_data |> Option.value ~default:"") in
                                 if sname = "" then Lwt.return_unit
                                 else begin
                                   let sdesc_str = List.assoc_opt ("section_desc_" ^ string_of_int idx) form_data |> Option.value ~default:"" in
                                   let sdesc = if sdesc_str = "" then None else Some sdesc_str in
                                   let ssort = List.assoc_opt ("section_sort_" ^ string_of_int idx) form_data |> Option.value ~default:"new" in
                                   let%lwt _ = Db.create_section db community.id sname sdesc pos ssort false in
                                   Lwt.return_unit
                                 end
                               ) (List.init section_count (fun i -> i + 1)) in
                               (* Insert any optional extra live-chat channels, in addition to
                                  the default #general. Best-effort like custom sections (errors
                                  ignored, no transaction). channel_count is clamped to a small
                                  max so a hand-crafted form can't force a huge insert loop;
                                  Db.create_channel slugifies + dedupes, so blank/duplicate names
                                  can't violate the UNIQUE (community_id, slug) constraint. The
                                  default general channel sits at position 0, so extras start at 1. *)
                               let channel_count =
                                 List.assoc_opt "channel_count" form_data
                                 |> Option.value ~default:"0"
                                 |> (fun s -> match int_of_string_opt s with Some n -> n | None -> 0)
                                 |> max 0 |> min 20
                               in
                               let%lwt () = Lwt_list.iteri_s (fun i _ ->
                                 let idx = i + 1 in
                                 let cname = String.trim (List.assoc_opt ("channel_name_" ^ string_of_int idx) form_data |> Option.value ~default:"") in
                                 (* skip blanks and an explicit "general" so we don't shadow the
                                    default #general with a "general-2". Any odd input that still
                                    slugifies to general is harmless — create_channel just dedupes. *)
                                 if cname = "" || String.lowercase_ascii cname = "general" then Lwt.return_unit
                                 else begin
                                   let%lwt _ = Db.create_channel db community.id cname None idx in
                                   Lwt.return_unit
                                 end
                               ) (List.init channel_count (fun i -> i + 1)) in
                               (* §5.3: creation emits only $groupidentify (no
                                  community_created event), attributed to the
                                  creator's authenticated identity, from the
                                  round-tripped authoritative record. *)
                               record (fun () ->
                                   Analytics.identify_community_if_consented request
                                     ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                     (community_group_of community));
                               Dream.redirect request ("/c/" ^ slug)))
                 | _ -> Dream.redirect request ("/c/" ^ slug))
            | Error _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Could not create community. The URL slug may already be taken." ~alert_type:"error" ~return_url:"/new-community" request)
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Your form submission was invalid. Please try again." ~alert_type:"error" ~return_url:"/new-community" request)

(* === Connected projects on the community page ===
   The accepted project-home relations of a community, rendered inside the community page
   the route already serves. Deliberately read only AFTER the route's own community lookup
   and can_view_community decision have completed, so this section can never become a side
   channel that reveals a private, draft, or otherwise unviewable community — it only adds
   detail to a page the viewer was already entitled to see. It broadens access to nothing:
   no GitHub authorization is consulted, and anonymous visitors to a public community see
   exactly what any other viewer of that page sees. *)

(* The read model and the page module are deliberately independent — neither depends on the
   other — so this route is the one place the two vocabularies meet. *)
let connected_project_page_model project =
  let module R = Community_connected_projects_read_model in
  let repositories =
    List.map
      (fun r : Community_connected_projects_pages.repository ->
        { full_name = R.repository_full_name r;
          html_url = R.repository_html_url r;
          is_primary = R.repository_is_primary r;
          is_archived = R.repository_is_archived r })
      (R.project_repositories project)
  in
  let verification : Community_connected_projects_pages.verification =
    match R.project_verification project with
    | R.Verified -> Verified
    | R.Stale -> Stale
    | R.Revoked -> Revoked
  in
  ({ name = R.project_name project;
     slug = R.project_slug project;
     kind = R.project_kind project;
     namespace_login = R.project_namespace_login project;
     verification;
     website_url = R.project_website_url project;
     repositories }
    : Community_connected_projects_pages.project)

(* One generic, non-cacheable 500: no Caqti/PostgreSQL detail, error constructor, or durable
   value reaches the page. Durable corruption is never rendered away as a quietly incomplete
   community page. *)
let connected_projects_error_page ?user request =
  Dream.respond ~status:`Internal_Server_Error
    ~headers:[ ("Cache-Control", "no-store"); ("Pragma", "no-cache") ]
    (Pages.msg_page ?user ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* A slug or community that no longer resolves reuses the route's existing generic
   unavailable response, byte-for-byte — a community that vanished between the route's own
   lookup and this read must not become distinguishable from one that never existed.

   The continuation receives the page models, not a rendered fragment: three surfaces now
   read the same publicly visible set and need it differently — the community page renders
   the full block, the community home shows only how many there are, and the Network page
   renders the full block again. Rendering at the call site keeps that one read, and one
   visibility rule, shared. *)
let with_connected_projects db ?user request ~community_slug k =
  let module R = Community_connected_projects_read_model in
  match%lwt R.load_for_community db ~community_slug with
  | Ok projects -> k (List.map connected_project_page_model projects)
  | Error (R.Invalid_community_slug | R.Community_unavailable) ->
      community_not_found ?user request
  | Error (R.Inconsistent_data | R.Storage_error) ->
      connected_projects_error_page ?user request

(* The community↔community counterpart of [with_connected_projects], on the same discipline:
   read only AFTER the route's own community lookup and can_view_community decision, reuse the
   route's generic unavailable response for a slug that stopped resolving, and never render
   durable corruption away as a quietly incomplete page.

   Public visibility is entirely the read model's: it applies the connection-eligibility
   predicate to the viewed community and to every counterpart, so this helper has no rule of
   its own to keep in sync. An ineligible community simply comes back with an empty list and
   the fragment collapses to "". *)
let with_connected_communities db ?user request ~community_slug k =
  let module R = Community_connected_communities_read_model in
  match%lwt R.load_for_community db ~community_slug with
  | Ok communities ->
      k
        (List.map
           (fun c : Community_connected_communities_pages.connected_community ->
             { name = R.community_name c; slug = R.community_slug c })
           communities)
  | Error (R.Invalid_community_slug | R.Community_unavailable) ->
      community_not_found ?user request
  | Error (R.Inconsistent_data | R.Storage_error) ->
      connected_projects_error_page ?user request

(* The private settings counterpart of [with_connected_projects]: the same read model, the
   same generic failure responses, but rendered through the removal-pages management
   fragment so each accepted project carries a removal form.

   [authorized] is the settings surface's own top-mod/admin decision, and it gates the read
   itself — an ordinary moderator's settings page never issues this query and never receives
   a fragment, so the panel cannot exist for them. Rendering a form still authorizes nothing:
   Project_home_removal_store reauthorizes every removal POST against the three durable
   sources.

   [removal_allowed] is this surface's own reading of the community it already
   loaded: an unpublished network setup draft structurally requires its
   provisioned home, so the section renders identities without controls. It
   suppresses a form, never a fact, and the store stays authoritative — a forged
   POST against a protected draft is refused there, not here. *)
let with_settings_connected_projects db ?user request ~community_slug ~authorized
    ~removal_allowed k =
  let module R = Community_connected_projects_read_model in
  if not authorized then k ""
  else
    match%lwt R.load_for_community db ~community_slug with
    | Ok projects ->
        let page_model project =
          let verification : Project_home_removal_pages.verification =
            match R.project_verification project with
            | R.Verified -> Verified
            | R.Stale -> Stale
            | R.Revoked -> Revoked
          in
          ({ name = R.project_name project;
             slug = R.project_slug project;
             namespace_login = R.project_namespace_login project;
             verification }
            : Project_home_removal_pages.connected_project)
        in
        k
          (Project_home_removal_pages.community_side_management_section ~request
             ~removal_allowed ~community_slug
             ~projects:(List.map page_model projects) ())
    | Error (R.Invalid_community_slug | R.Community_unavailable) ->
        community_not_found ?user request
    | Error (R.Inconsistent_data | R.Storage_error) ->
        (* Durable corruption is never rendered away as a quietly incomplete settings
           page. *)
        connected_projects_error_page ?user request

let community_page_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let sort_str_opt = Dream.query request "sort" in
  let page = match Dream.query request "page" with Some p -> (try int_of_string p with _ -> 1) | None -> 1 in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  let shared_sidebar_data db (community : Db.community) =
    let%lwt mods_res = Db.get_community_mods_with_roles db community.Db.id in
    let%lwt admin_usernames_res = Db.get_admin_usernames db in
    let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
    let%lwt banned_res = Db.community_get_banned_users db community.Db.id in
    let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: Db.user) -> u.username) bs | _ -> [] in
    let%lwt user_communities_res = if user_id > 0 then Db.get_user_communities db user_id else Lwt.return_ok [] in
    let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
    let%lwt moderated_communities_res = if user_id > 0 then Db.get_moderated_communities db user_id else Lwt.return_ok [] in
    let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
    let%lwt is_mem = if user_id > 0 then Db.is_member db user_id community.Db.id else Lwt.return_ok false in
    Lwt.return (mods_res, admin_usernames, banned_usernames, user_communities, moderated_communities, is_mem)
  in

  Dream.sql request (fun db ->
    let%lwt _ = Db.demote_inactive_mods db in
    match%lwt Db.get_community_by_slug db slug with
    | Ok (Some community) when community.sections_enabled ->
        let%lwt authorized = can_view_community db ~user_id ~is_admin community in
        if not authorized then community_not_found ?user request
        else
        (* Structured community: /c/:slug always shows the sections overview.
           Section feeds are at /c/:slug/s/:section_slug. *)
        let%lwt sections_res = Db.get_sections_with_stats db community.id in
        let%lwt orphaned_res = Db.get_orphaned_count_and_activity db community.id in
        (* One clean call each for the channels and recent-discussions blocks. Both degrade to
           an empty list on error so the overview still renders — neither is load-bearing. *)
        let%lwt channels_res = Db.get_channels_by_community db community.id in
        let channels = match channels_res with
          | Ok cs -> List.filter (fun (c : Db.channel) -> not c.is_archived) cs
          | Error _ -> []
        in
        let%lwt recent_posts_res = Db.get_posts_by_community db community.id Db.Newest 5 0 in
        let recent_posts = match recent_posts_res with Ok ps -> ps | Error _ -> [] in
        let%lwt (mods_res, _admin_usernames, _banned_usernames, user_communities, _moderated_communities, is_mem) =
          shared_sidebar_data db community
        in
        (match sections_res, is_mem with
         | Ok section_stats, Ok m ->
             let orphaned = match orphaned_res with Ok o -> o | Error _ -> (0, None) in
             let mods = match mods_res with Ok ms -> ms | _ -> [] in
             let mod_usernames = List.map (fun (e: Db.moderator_entry) -> e.username) mods in
             let is_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id) mods in
             let is_top_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id && e.role = "top_mod") mods in
             (* Order: lookup → authorization → existing page data → connected projects →
                connected communities → render. Never before the authorization decision
                above. The home shows only how many of each are publicly visible and
                links to /c/:slug/network for the lists themselves, so it takes the
                counts of exactly the sets that page renders — one read model, one
                visibility rule, two presentations. *)
             with_connected_projects db ?user request ~community_slug:slug (fun projects ->
             with_connected_communities db ?user request ~community_slug:slug (fun communities ->
               Dream.html (Pages.community_overview_page ?user ~noindex:(community_noindex community) ~connected_projects_count:(List.length projects) ~connected_communities_count:(List.length communities) ~is_member:m ~is_current_user_mod:is_mod ~is_current_user_top_mod:is_top_mod ~mod_usernames ~orphaned ~rail_communities:user_communities ~channels ~recent_posts community section_stats request)))
         | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:"Failed to load community sections." ~alert_type:"error" ~return_url:"/" request))
    | Ok (Some community) ->
        let%lwt authorized = can_view_community db ~user_id ~is_admin community in
        if not authorized then community_not_found ?user request
        else
        (* Simple community feed *)
        let sort_mode = match sort_str_opt with
          | Some "new" -> Db.Newest | Some "top" -> Db.Top | Some "hot" -> Db.Hot | _ -> Db.Hot
        in
        let sort_str = match sort_mode with Db.Newest -> "new" | Db.Top -> "top" | Db.Hot -> "hot" | Db.Active -> "active" in
        let%lwt posts = Db.get_posts_by_community db community.id sort_mode limit offset in
        let%lwt user_votes = if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok [] in
        let%lwt (mods_res, admin_usernames, banned_usernames, user_communities, moderated_communities, is_mem) =
          shared_sidebar_data db community
        in
        (match posts, user_votes, is_mem with
         | Ok p, Ok v, Ok m ->
             let mods = match mods_res with Ok ms -> ms | _ -> [] in
             let mod_usernames = List.map (fun (e: Db.moderator_entry) -> e.username) mods in
             let is_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id) mods in
             let is_top_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id && e.role = "top_mod") mods in
             (* Same order as the structured branch: both connected-* reads follow the
                authorization decision and the existing feed load. The flat community
                page keeps both full blocks in its side stack — the home reorganization
                is the structured overview's. *)
             with_connected_projects db ?user request ~community_slug:slug (fun projects ->
             let connected_projects =
               Community_connected_projects_pages.connected_projects_section ~projects
             in
             with_connected_communities db ?user request ~community_slug:slug (fun communities ->
             let connected_communities =
               Community_connected_communities_pages.connected_communities_section
                 ~communities
             in
               Dream.html (Pages.community_page ?user ~noindex:(community_noindex community) ~connected_projects ~connected_communities ~is_member:m ~is_current_user_mod:is_mod ~is_current_user_top_mod:is_top_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities v page sort_str community p request)))
         | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:"Failed to load community data." ~alert_type:"error" ~return_url:"/" request))
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* === Public Network page: GET /c/:slug/network ===
   The community's external network — the complete connected-projects and connected-
   communities lists that used to occupy the community home's main column. The home now
   carries only a compact entry point with the two counts, so this is where the lists
   themselves live.

   Same access discipline as the sibling public community routes and deliberately no new
   one: the community is resolved, can_view_community decides, and only then is anything
   else read. Both lists come from the same two helpers the community page uses, so the
   eligibility and visibility rules are the read models' single copy — this route restates
   none of them and can reveal nothing /c/:slug would not.

   The Connect-a-community link is gated by the same top-mod-or-admin reading the community
   sidebar already applies to that destination. It authorizes nothing: the connections
   surface re-decides every request in SQL. *)
let community_network_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with
    | Some id -> (try int_of_string id with _ -> 0) | None -> 0 in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  Dream.sql request (fun db ->
    match%lwt Db.get_community_by_slug db slug with
    | Ok (Some community) ->
        let%lwt authorized = can_view_community db ~user_id ~is_admin community in
        if not authorized then community_not_found ?user request
        else
        (* Launch-chrome data, loaded only after the authorization decision above:
           sections/channels feed the shared community sidebar and the viewer's joined
           communities the global rail. Each degrades to an empty list rather than
           blocking the page — none of it is load-bearing. *)
        let%lwt sections =
          if community.sections_enabled then
            (match%lwt Db.get_sections_by_community db community.id with
             | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
          else Lwt.return []
        in
        let%lwt channels =
          match%lwt Db.get_channels_by_community db community.id with
          | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
        in
        let%lwt rail_communities =
          if user_id > 0 then
            (match%lwt Db.get_user_communities db user_id with
             | Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
          else Lwt.return []
        in
        let%lwt mods_res = Db.get_community_mods_with_roles db community.id in
        let mods = match mods_res with Ok ms -> ms | Error _ -> [] in
        let is_mod =
          user_id > 0 && List.exists (fun (e : Db.moderator_entry) -> e.user_id = user_id) mods in
        let is_top_mod =
          user_id > 0
          && List.exists
               (fun (e : Db.moderator_entry) -> e.user_id = user_id && e.role = "top_mod")
               mods
        in
        let sidebar =
          Pages.launch_knowledge_sidebar ~community ~channels ~sections
            ~can_manage:(is_mod || is_admin) ()
        in
        with_connected_projects db ?user request ~community_slug:slug (fun projects ->
        with_connected_communities db ?user request ~community_slug:slug (fun communities ->
          Dream.html
            (Community_network_pages.community_network_page ?user
               ~noindex:(community_noindex community) ~rail_communities ~community ~sidebar
               ~projects_section:
                 (Community_connected_projects_pages.connected_projects_section ~projects)
               ~communities_section:
                 (Community_connected_communities_pages.connected_communities_section
                    ~communities)
               ~can_connect:(is_top_mod || is_admin) request)))
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* Section feed at /c/:slug/s/:section_slug — pretty URL replaces the old ?section=id param. *)
let community_section_handler request =
  let community_slug = Dream.param request "slug" in
  let section_slug = Dream.param request "section_slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let sort_str_opt = Dream.query request "sort" in
  let page = match Dream.query request "page" with Some p -> (try int_of_string p with _ -> 1) | None -> 1 in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  Dream.sql request (fun db ->
    let%lwt _ = Db.demote_inactive_mods db in
    match%lwt Db.get_community_by_slug db community_slug with
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
    | Ok (Some community) ->
        let%lwt authorized = can_view_community db ~user_id ~is_admin community in
        if not authorized then community_not_found ?user request
        else
        let render_section_feed (section : Db.community_section) sort_mode sort_str fetch_posts =
          let%lwt posts = fetch_posts () in
          let%lwt user_votes = if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok [] in
          let%lwt mods_res = Db.get_community_mods_with_roles db community.id in
          let%lwt admin_usernames_res = Db.get_admin_usernames db in
          let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
          let%lwt banned_res = Db.community_get_banned_users db community.id in
          let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: Db.user) -> u.username) bs | _ -> [] in
          let%lwt user_communities_res = if user_id > 0 then Db.get_user_communities db user_id else Lwt.return_ok [] in
          let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
          let%lwt moderated_communities_res = if user_id > 0 then Db.get_moderated_communities db user_id else Lwt.return_ok [] in
          let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
          let%lwt is_mem = if user_id > 0 then Db.is_member db user_id community.id else Lwt.return_ok false in
          (* One stats query drives BOTH the col-2 sidebar and the right-rail Threads/Last-activity
             rows — same ordering as get_sections_by_community, so the sidebar is unchanged, and no
             fabricated numbers. Folded into the success match below so a DB error surfaces as the
             standard error page, never an empty sidebar. *)
          let%lwt sections_res = Db.get_sections_with_stats db community.id in
          (* Channels feed the shell's new Channels nav group; a load error degrades to an
             empty group rather than failing the whole section page. *)
          let%lwt channels_res = Db.get_channels_by_community db community.id in
          let channels = match channels_res with Ok cs -> cs | Error _ -> [] in
          (match posts, user_votes, is_mem, sections_res with
           | Ok p, Ok v, Ok _m, Ok section_stats ->
               let mods = match mods_res with Ok ms -> ms | _ -> [] in
               let mod_usernames = List.map (fun (e: Db.moderator_entry) -> e.username) mods in
               let is_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id) mods in
               let _ = sort_mode in
               let _ = moderated_communities in  (* unused by the shell page; kept loaded above for parity *)
               let sections = List.map (fun ((s : Db.community_section), _, _) -> s) section_stats in
               (* Real Threads/Last-activity for the rail: find this section in the stats. The virtual
                  Uncategorized feed isn't a community_sections row, so fall back to the orphaned-posts
                  aggregate. Any miss → None → the rail just omits that row (never faked). *)
               let%lwt (thread_count, last_activity) =
                 match List.find_opt (fun ((s : Db.community_section), _, _) -> s.section_id = section.Db.section_id) section_stats with
                 | Some (_, cnt, act) -> Lwt.return (Some cnt, act)
                 | None when section.Db.slug = "uncategorized" ->
                     (match%lwt Db.get_orphaned_count_and_activity db community.id with
                      | Ok (cnt, act) -> Lwt.return (Some cnt, act)
                      | Error _ -> Lwt.return (None, None))
                 | None -> Lwt.return (None, None)
               in
               (* rail shows the communities the user belongs to; the shell wraps layout itself. *)
               Dream.html (Pages.community_section_shell_page ?user ~noindex:(child_noindex community ~child_indexable:section.Db.indexable) ?thread_count ?last_activity ~is_current_user_mod:is_mod ~mod_usernames ~admin_usernames ~banned_usernames ~rail_communities:user_communities ~channels ~sections ~section ~user_votes:v ~current_page:page ~sort_mode:sort_str ~community ~posts:p request)
           | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:"Failed to load section." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request))
        in
        if section_slug = "uncategorized" then begin
          (* Virtual section: shows posts orphaned by deleted sections. *)
          let sort_mode =
            match sort_str_opt with
            | Some "top" -> Db.Top | Some "hot" -> Db.Hot | Some "active" -> Db.Active
            | _ -> Db.Newest
          in
          let sort_str = match sort_mode with Db.Newest -> "new" | Db.Top -> "top" | Db.Hot -> "hot" | Db.Active -> "active" in
          let%lwt orphaned_count_res = Db.get_orphaned_count_and_activity db community.id in
          let orphaned_count = match orphaned_count_res with Ok (c, _) -> c | Error _ -> 0 in
          if orphaned_count = 0 then
            Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"No uncategorized posts in this community." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
          else
            let virtual_section = {
              Db.section_id = -1; community_id = community.id;
              name = "Uncategorized"; slug = "uncategorized";
              description = Some "Posts from deleted sections";
              position = 9999; default_sort = "new"; is_introduction_section = false;
              indexable = true;
            } in
            render_section_feed virtual_section sort_mode sort_str
              (fun () -> Db.get_orphaned_posts db community.id sort_mode limit offset)
        end else begin
          match%lwt Db.get_section_by_slug db section_slug community.id with
          | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This section does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
          | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
          | Ok (Some section) ->
              let sort_mode =
                let from_query = match sort_str_opt with
                  | Some "new" -> Some Db.Newest | Some "top" -> Some Db.Top
                  | Some "active" -> Some Db.Active | Some "hot" -> Some Db.Hot | _ -> None
                in
                match from_query with
                | Some sm -> sm
                | None -> (match section.Db.default_sort with
                    | "new" -> Db.Newest | "top" -> Db.Top | "active" -> Db.Active | _ -> Db.Hot)
              in
              let sort_str = match sort_mode with Db.Newest -> "new" | Db.Top -> "top" | Db.Hot -> "hot" | Db.Active -> "active" in
              render_section_feed section sort_mode sort_str
                (fun () -> Db.get_posts_by_section db community.id section.Db.section_id sort_mode limit offset)
        end
  )

(* GET /c/:slug/ch/:channel_slug — minimal SSR chat channel inside the shell. Mirrors
   community_section_handler: resolve community → channel, then load the sidebar data
   (channels + sections), the recent messages (author-resolved for render), membership,
   and the rail. No realtime here — see Pages.community_channel_shell_page. *)
let community_channel_handler request =
  let community_slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> (try int_of_string id with _ -> 0) | None -> 0 in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  Dream.sql request (fun db ->
    match%lwt Db.get_community_by_slug db community_slug with
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
    | Ok (Some community) ->
        let%lwt authorized = can_view_community db ~user_id ~is_admin community in
        if not authorized then community_not_found ?user request
        else
        match%lwt Db.get_channel_by_slug db channel_slug community.id with
        | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
        | Ok (Some channel) ->
            let%lwt channels = match%lwt Db.get_channels_by_community db community.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [] in
            let%lwt sections = match%lwt Db.get_sections_by_community db community.id with Ok ss -> Lwt.return ss | Error _ -> Lwt.return [] in
            (* Reverse navigation (?source_thread=<post_id>): instead of the last-50 tail,
               SSR a bounded window anchored on the promoted thread's earliest surviving
               source message. Promotion only ever selects from a ±10-message window
               around the seed, so 25-before + 60-after always covers the full span plus
               tail context — deliberately NOT general chat-history pagination. Any
               failure (bad id, unknown thread, thread from another channel, no surviving
               sources) falls back silently to the normal page. *)
            let%lwt source_focus, messages =
              let normal () =
                let%lwt ms = match%lwt Db.get_recent_messages_with_authors db channel.id 50 with Ok ms -> Lwt.return ms | Error _ -> Lwt.return [] in
                Lwt.return (None, ms) in
              match Pages.Start_thread.parse_source_thread (Dream.query request "source_thread") with
              | None -> normal ()
              | Some post_id ->
                  (match%lwt Db.get_source_span_for_thread db post_id with
                   | Ok (Some (source_channel_id, post_title, (first :: _ as ids))) when source_channel_id = channel.id ->
                       let latest = List.fold_left (fun _ id -> id) first ids in
                       let%lwt before = match%lwt Db.get_messages_before_id_with_authors db channel.id first 25 with Ok l -> Lwt.return l | Error _ -> Lwt.return [] in
                       let%lwt after = match%lwt Db.get_messages_after_id_with_authors db channel.id (Int64.sub first 1L) 60 with Ok l -> Lwt.return l | Error _ -> Lwt.return [] in
                       (* Trim the tail to ~25 rows past the last source message so the
                          window stays tight even when the span sits deep in history. *)
                       let rec trim_after kept = function
                         | [] -> List.rev kept
                         | ((m : Db.chat_message), _) as row :: rest ->
                             if m.id <= latest then trim_after (row :: kept) rest
                             else
                               let rec take n acc = function
                                 | [] -> List.rev acc
                                 | _ when n <= 0 -> List.rev acc
                                 | r :: rs -> take (n - 1) (r :: acc) rs in
                               List.rev_append kept (row :: take 24 [] rest) in
                       Lwt.return (Some (post_id, post_title, ids), before @ trim_after [] after)
                   | _ -> normal ()) in
            let%lwt is_member = if user_id > 0 then (match%lwt Db.is_member db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false) else Lwt.return false in
            let%lwt rail_communities = if user_id > 0 then (match%lwt Db.get_user_communities db user_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) else Lwt.return [] in
            (* All thread-source links for the channel -> the renderer marks seeds
               ("Thread ->") and context references ("Referenced in ->") per message in
               one query. can_start mirrors the composer's gate (a member); the
               start-thread form re-checks every permission server-side. *)
            let%lwt thread_links = match%lwt Db.get_thread_links_for_channel db channel.id with Ok l -> Lwt.return l | Error _ -> Lwt.return [] in
            let can_start = is_member in
            let realtime_topic = Printf.sprintf "chan:%d" channel.id in
            let realtime_token =
              match user, user_id > 0 with
              | Some username, true ->
                  Realtime_token.create_for_topic
                    ~user_id
                    ~username
                    ~topic:realtime_topic
                    ~shared_cursors:
                      (Features.shared_cursors_enabled
                         ~community_slug:community.slug)
              | _ -> None
            in
            Dream.html (Pages.community_channel_shell_page ?user ?realtime_token ~noindex:(child_noindex community ~child_indexable:channel.Db.indexable) ~is_member ~can_start ~thread_links ?source_focus ~rail_communities ~channels ~sections ~channel ~messages ~community request)
  )

let int64_param_default name default request =
  match Dream.query request name with
  | None -> default
  | Some raw ->
      try Int64.of_string raw with _ -> default

let json_string s =
  `String s

let json_int i =
  `Int i

let json_int64 i =
  `Intlit (Int64.to_string i)

let chat_message_json
    ~(channel_id : int)
    ~(community_id : int)
    ?(thread_id : int option)
    ((message : Db.chat_message), (author : string option)) =
  let username =
    match author with
    | Some username -> username
    | None -> "[deleted]"
  in
  (* thread_id/deleted mirror the SSR "Start thread" gate so a catch-up message renders the
     same affordance: the client hides Start thread when the message is deleted or already
     seeds a thread (thread_id present). Live new_msg events omit these — a brand-new message
     is never deleted or promoted — so their absence reads correctly as "still startable". *)
  `Assoc
    [ ("v", `Int 1)
    ; ("type", `String "chat_message_created")
    ; ("id", json_int64 message.id)
    ; ("channel_id", json_int channel_id)
    ; ("community_id", json_int community_id)
    ; ("user_id",
        (match message.user_id with
         | Some user_id -> json_int user_id
         | None -> `Null))
    ; ("username", json_string username)
    ; ("content",
        (match message.deleted_at with
         | Some _ -> `String "[message deleted]"
         | None -> json_string message.content))
      (* Minute precision everywhere a chat row is serialized, matching the SSR
         renderer, so live, catch-up and composer-response rows display alike. *)
    ; ("created_at", json_string (Pages.Start_thread.minute_of_ts message.created_at))
    ; ("deleted", `Bool (message.deleted_at <> None))
    ; ("thread_id",
        (match thread_id with
         | Some pid -> json_int pid
         | None -> `Null))
    ]

(* ---- Chat composer JSON contract -----------------------------------------
   The chat composer submits over fetch with "Accept: application/json"; the
   no-JS fallback stays an ordinary form POST and keeps its redirect/HTML
   responses. These helpers are pure and exposed for tests so the negotiation
   rule, the validation boundary and the error shape cannot silently drift. *)
module Chat_api = struct
  (* A submission opts into JSON by sending an Accept value that mentions
     application/json; browser navigation Accept headers never do. *)
  let wants_json (accept : string option) =
    match accept with
    | None -> false
    | Some value ->
        let value = String.lowercase_ascii value in
        let needle = "application/json" in
        let nlen = String.length needle in
        let vlen = String.length value in
        let rec scan i =
          i + nlen <= vlen
          && (String.sub value i nlen = needle || scan (i + 1))
        in
        scan 0

  let max_content_length = 4000

  (* One validation for both response modes: trimmed content or a closed error
     variant, so the JSON and HTML paths always agree on what is sendable. *)
  let validate_content (raw : string) =
    let content = String.trim raw in
    if content = "" then Error `Empty
    else if String.length content > max_content_length then Error `Too_long
    else Ok content

  (* Client-safe error body: a stable code plus display copy only — never an
     exception string, SQL error or anything else internal. *)
  let error_json ~code ~message =
    Yojson.Safe.to_string
      (`Assoc [ ("error", `String code); ("message", `String message) ])

  let internal_error_json =
    error_json ~code:"internal" ~message:"Something went wrong. Please try again."
end

let channel_messages_json_handler request =
  let slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let after_id = int64_param_default "after_id" 0L request in
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> (try int_of_string id with _ -> 0)
    | None -> 0
  in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in

  Dream.sql request (fun db ->
  match%lwt Db.get_community_by_slug db slug with
  | Error e ->
      Logs.err (fun m -> m "channel_messages_json: community lookup failed: %s" e);
      Dream.respond ~status:`Internal_Server_Error "Internal server error"

  (* Privacy: authorize BEFORE resolving the channel, and use the same not-found
     response as community_channel_handler for both a missing community AND a denied
     private read. A distinguishable 403 (or a channel 404 reached pre-auth) would let
     an unauthorized user enumerate private communities and their channel slugs. *)
  | Ok None -> community_not_found ?user request

  | Ok (Some community) ->
      let%lwt can_view = can_view_community db ~user_id ~is_admin community in
      if not can_view then community_not_found ?user request
      else (
        match%lwt Db.get_channel_by_slug db channel_slug community.id with
        | Error e ->
            Logs.err (fun m -> m "channel_messages_json: channel lookup failed: %s" e);
            Dream.respond ~status:`Internal_Server_Error "Internal server error"

        | Ok None ->
            Dream.respond ~status:`Not_Found "Channel not found"

        | Ok (Some channel) ->
            match%lwt Db.get_messages_after_id_with_authors db channel.id after_id 100 with
            | Error e ->
                Logs.err (fun m -> m "channel_messages_json: messages lookup failed: %s" e);
                Dream.respond ~status:`Internal_Server_Error "Internal server error"

            | Ok messages ->
                (* Seed-thread lookup so catch-up marks already-promoted messages, matching SSR.
                   Best-effort: on error we fall back to [] (no thread_id), i.e. Start thread may
                   briefly reappear on a promoted message until reload — never a wrong link. *)
                let%lwt thread_links =
                  match%lwt Db.get_thread_links_for_channel db channel.id with
                  | Ok l -> Lwt.return l
                  | Error _ -> Lwt.return []
                in
                let seed_thread_id (mid : int64) : int option =
                  List.find_map
                    (fun (m_id, pid, _title, is_seed) ->
                      if is_seed && m_id = mid then Some pid else None)
                    thread_links
                in
                let json =
                  `Assoc
                    [ ("messages",
                        `List
                          (List.map
                             (fun ((m : Db.chat_message), _ as row) ->
                                chat_message_json
                                  ~channel_id:channel.id
                                  ~community_id:community.id
                                  ?thread_id:(seed_thread_id m.id)
                                  row)
                             messages))
                    ]
                in
                Dream.json (Yojson.Safe.to_string json)))

(* GET /c/:slug/ch/:channel_slug/realtime-token — fresh websocket token for the chat page's
   JS, so a socket can reconnect after the initial token's expiry without a page reload.
   Authentication is required BEFORE any lookup: every slug gets the same 401 for an anonymous
   caller, so nothing is enumerable from this endpoint. Authenticated callers then follow
   channel_messages_json_handler's exact privacy order (community → can_view_community →
   channel), reusing community_not_found for denied private reads. The token itself is minted
   by the same Realtime_token.create_for_topic used at page render — no second signing path —
   and only ever travels in this response body, never in a URL or a log line. *)
let realtime_token_handler request =
  let slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> (try int_of_string id with _ -> 0)
    | None -> 0
  in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  match user, user_id > 0 with
  | None, _ | _, false ->
      Dream.json ~status:`Unauthorized {|{"error":"unauthorized"}|}
  | Some username, true ->
      Dream.sql request (fun db ->
        match%lwt Db.get_community_by_slug db slug with
        | Error e ->
            Logs.err (fun m -> m "realtime_token: community lookup failed: %s" e);
            Dream.respond ~status:`Internal_Server_Error "Internal server error"
        | Ok None -> community_not_found ?user request
        | Ok (Some community) ->
            let%lwt can_view = can_view_community db ~user_id ~is_admin community in
            if not can_view then community_not_found ?user request
            else (
              match%lwt Db.get_channel_by_slug db channel_slug community.id with
              | Error e ->
                  Logs.err (fun m -> m "realtime_token: channel lookup failed: %s" e);
                  Dream.respond ~status:`Internal_Server_Error "Internal server error"
              | Ok None -> Dream.respond ~status:`Not_Found "Channel not found"
              | Ok (Some channel) ->
                  let topic = Printf.sprintf "chan:%d" channel.id in
                  (* Capability recomputed from the community on every refresh —
                     never copied from the old token or any client input. *)
                  let shared_cursors =
                    Features.shared_cursors_enabled ~community_slug:community.slug
                  in
                  match Realtime_token.create_for_topic ~user_id ~username ~topic ~shared_cursors with
                  | None ->
                      (* Signing secret not configured: realtime is off for this
                         deployment; the client stops proactive refresh cleanly. *)
                      Dream.json ~status:`Service_Unavailable
                        {|{"error":"realtime unavailable"}|}
                  | Some token ->
                      Dream.json
                        (Yojson.Safe.to_string
                           (`Assoc
                             [ ("token", `String token)
                             ; ("expires_in", `Int Realtime_token.default_ttl_seconds)
                             ]))))

(* POST /messages — send a chat message. Two response modes over one endpoint:
   an ordinary form POST (no-JS fallback) keeps the historical redirect/HTML
   contract, while the chat page's fetch submission (Accept: application/json)
   receives JSON — a canonical new_msg-shaped row on success, a safe
   code+message body on failure — so the page never navigates. Validation,
   authorization and persistence are identical for both modes: CSRF
   auto-validated by Dream.form, hidden community_slug + channel_slug (not a
   raw id) re-resolved and re-validated server-side, safety gates mirroring
   create_post_handler (global ban → membership → local ban), Postgres write
   first, gateway publish best-effort after. *)
let send_message_handler request =
  let respond_json = Chat_api.wants_json (Dream.header request "Accept") in
  let json_error status ~code ~message =
    Dream.json ~status (Chat_api.error_json ~code ~message)
  in
  match Dream.session_field request "user_id" with
  | None ->
      if respond_json then
        json_error `Unauthorized ~code:"unauthorized"
          ~message:"Your session has ended. Reload the page and log in."
      else Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = try int_of_string uid_str with _ -> 0 in
      let uname = Dream.session_field request "username" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_slug = List.assoc_opt "community_slug" form_data |> Option.value ~default:"" in
          let channel_slug = List.assoc_opt "channel_slug" form_data |> Option.value ~default:"" in
          let raw_content = List.assoc_opt "content" form_data |> Option.value ~default:"" in
          let back_url = Printf.sprintf "/c/%s/ch/%s" community_slug channel_slug in
          let internal_error e =
            Logs.err (fun m -> m "send_message: %s" e);
            if respond_json then
              Dream.json ~status:`Internal_Server_Error Chat_api.internal_error_json
            else
              Dream.html (Pages.msg_page ?user:uname ~title:"Error" ~message:generic_db_error ~alert_type:"error" ~return_url:"/" request)
          in
          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db community_slug with
            | Ok (Some community) ->
                (match%lwt Db.get_channel_by_slug db channel_slug community.id with
                 | Ok (Some channel) ->
                     let%lwt is_gb = match%lwt Db.is_globally_banned db user_id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                     if is_gb then
                       (if respond_json then
                          json_error `Forbidden ~code:"forbidden"
                            ~message:"Your account has been permanently banned from Earde."
                        else
                          Dream.respond ~status:`Forbidden (Pages.msg_page ?user:uname ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request))
                     else begin
                       match%lwt Db.is_member db user_id community.id with
                       | Ok true ->
                           (match%lwt Db.community_is_banned db user_id community.id with
                            | Ok true ->
                                if respond_json then
                                  json_error `Forbidden ~code:"forbidden"
                                    ~message:"You are banned from this community."
                                else
                                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user:uname ~title:"Banned from Community" ~message:"You are banned from this community." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                            | _ ->
                                (match Chat_api.validate_content raw_content with
                                 | Error `Empty ->
                                     if respond_json then
                                       json_error `Bad_Request ~code:"empty" ~message:"Message is empty."
                                     else Dream.redirect request (safe_local_redirect request back_url)
                                 | Error `Too_long ->
                                     if respond_json then
                                       json_error `Bad_Request ~code:"too_long"
                                         ~message:"Messages cannot exceed 4000 characters."
                                     else
                                       Dream.html (Pages.msg_page ?user:uname ~title:"Message too long" ~message:"Messages cannot exceed 4000 characters." ~alert_type:"error" ~return_url:back_url request)
                                 | Ok content ->
                                  (match%lwt Db.send_message db channel.id user_id content with
                                   | Ok message ->
                                       (* INSERT ... RETURNING hands back the canonical
                                          persisted row (Postgres id and created_at) in
                                          the insert round-trip, so both the publish and
                                          the JSON success body come straight from what
                                          was stored — no read-back, no synthesized
                                          fields. Username joins from the already
                                          authenticated session. *)
                                       let username = Option.value uname ~default:"[unknown]" in
                                       Lwt.async (fun () ->
                                           Realtime.publish_chat_message
                                             ~channel_id:channel.id
                                             ~community_id:community.id
                                             ~message_id:message.Db.id
                                             ~user_id
                                             ~username
                                             ~content
                                             ~created_at:(Pages.Start_thread.minute_of_ts message.Db.created_at));
                                       (* One capture point ahead of the
                                          respond_json split, so JSON and
                                          redirect modes each emit exactly
                                          once, never twice. *)
                                       record (fun () ->
                                           Analytics.capture_if_consented request
                                             ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                             (Analytics.Chat_message_sent
                                                {
                                                  user_id;
                                                  community_id = community.Db.id;
                                                  community_slug =
                                                    analytics_public_string
                                                      community community.Db.slug;
                                                  channel_id = channel.Db.id;
                                                  channel_slug =
                                                    analytics_public_string
                                                      community channel.Db.slug;
                                                  message_id = message.Db.id;
                                                  content_length = String.length content;
                                                  response_mode =
                                                    (if respond_json then Analytics.Response_json
                                                     else Analytics.Response_redirect);
                                                }));
                                       if respond_json then
                                         Dream.json
                                           (Yojson.Safe.to_string
                                              (chat_message_json
                                                 ~channel_id:channel.id
                                                 ~community_id:community.id
                                                 (message, Some username)))
                                       else Dream.redirect request (safe_local_redirect request back_url)
                                   | Error e ->
                                       if respond_json then internal_error e
                                       else Dream.html (Pages.msg_page ?user:uname ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:back_url request))))
                       | Ok false ->
                           if respond_json then
                             json_error `Forbidden ~code:"not_member" ~message:"Join this community to chat."
                           else
                             Dream.respond ~status:`Forbidden (Pages.msg_page ?user:uname ~title:"Not a Member" ~message:"You must join this community to chat." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                       | Error e -> internal_error e
                     end
                 | Ok None ->
                     if respond_json then
                       json_error `Not_Found ~code:"not_found" ~message:"This channel does not exist."
                     else Dream.respond ~status:`Not_Found (Pages.msg_page ?user:uname ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                 | Error e -> internal_error e)
            | Ok None ->
                if respond_json then
                  json_error `Not_Found ~code:"not_found" ~message:"This community does not exist."
                else Dream.respond ~status:`Not_Found (Pages.msg_page ?user:uname ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> internal_error e))
      | _ ->
          (* Dream.form failure: missing/stale CSRF or a non-form body. The fetch
             path surfaces it as retry-after-reload guidance — a chat tab older
             than the CSRF token lifetime lands here. *)
          if respond_json then
            json_error `Bad_Request ~code:"stale_form"
              ~message:"This page is out of date. Reload and try again."
          else
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:uname ~title:"Form Error" ~message:"There was a problem with your submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

(* ---- Start thread from chat -------------------------------------------------
   Crystallize a chat conversation into a durable forum thread. A seed message plus
   nearby context messages become a normal post; real provenance lives in
   thread_source_messages. UI says "Start thread", never "Promote". *)

(* Nearby-message window (10 before + 10 after the seed) and the cap on how many
   source messages a thread may carry (seed included), per the approved product rules. *)
let start_thread_window = 10
let start_thread_max_total = 10

(* Default section: the community's 'general' section (guaranteed present by the
   default-structure invariant) else the first in query order; 0 when sectionless. *)
let default_thread_section_id sections =
  match List.find_opt (fun (s : Db.community_section) -> s.slug = "general") sections with
  | Some s -> s.section_id
  | None -> (match sections with (s : Db.community_section) :: _ -> s.section_id | [] -> 0)

(* Permission gate (decision: any logged-in, non-banned community member; mods/admins
   too). Mirrors send_message's order: global ban -> local ban -> membership -> mod/admin. *)
type start_perm = Start_allowed | Start_not_member | Start_banned | Start_error of string

let check_start_permission db ~user_id ~is_admin ~community_id =
  let%lwt is_gb = match%lwt Db.is_globally_banned db user_id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
  if is_gb then Lwt.return Start_banned
  else begin
    match%lwt Db.community_is_banned db user_id community_id with
    | Ok true -> Lwt.return Start_banned
    | Error e -> Lwt.return (Start_error e)
    | Ok false ->
        (match%lwt Db.is_member db user_id community_id with
         | Ok true -> Lwt.return Start_allowed
         | Error e -> Lwt.return (Start_error e)
         | Ok false ->
             if is_admin then Lwt.return Start_allowed
             else
               (match%lwt Db.is_moderator db user_id community_id with
                | Ok true -> Lwt.return Start_allowed
                | Ok false -> Lwt.return Start_not_member
                | Error e -> Lwt.return (Start_error e)))
  end

(* GET /c/:slug/ch/:channel_slug/messages/:message_id/start-thread — render the form.
   The seed is fetched with its author by reading the channel window inclusively
   (after id-1 returns the seed first), so we never need a separate user lookup. *)
let start_thread_form_handler request =
  let community_slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let message_id_str = Dream.param request "message_id" in
  let channel_url = Printf.sprintf "/c/%s/ch/%s" community_slug channel_slug in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = try int_of_string uid_str with _ -> 0 in
      let user = Dream.session_field request "username" in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (match Int64.of_string_opt message_id_str with
       | None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Invalid message reference." ~alert_type:"error" ~return_url:channel_url request)
       | Some message_id ->
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db community_slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
             | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
             | Ok (Some community) ->
                 (* Slice C: a private community is hidden — a non-authorized viewer gets the
                    same 404 as a missing community, BEFORE any membership-specific 403 below.
                    This does not broaden who may create a thread (check_start_permission still
                    runs for authorized viewers). *)
                 let%lwt authorized = can_view_community db ~user_id ~is_admin community in
                 if not authorized then community_not_found ?user request
                 else
                 (match%lwt Db.get_channel_by_slug db channel_slug community.id with
                  | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                  | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                  | Ok (Some channel) ->
                      (match%lwt Db.get_message_by_id db message_id with
                       | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                       | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That message does not exist." ~alert_type:"error" ~return_url:channel_url request)
                       | Ok (Some (seed : Db.chat_message)) ->
                           if seed.channel_id <> channel.id || seed.deleted_at <> None then
                             Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That message is not available to start a thread from." ~alert_type:"error" ~return_url:channel_url request)
                           else
                             (match%lwt check_start_permission db ~user_id ~is_admin ~community_id:community.id with
                              | Start_banned -> Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Not Allowed" ~message:"You cannot start threads in this community." ~alert_type:"error" ~return_url:channel_url request)
                              | Start_not_member -> Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Join to start a thread" ~message:"You must be a member of this community to start a thread." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                              | Start_error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                              | Start_allowed ->
                                  (match%lwt Db.get_seed_thread_for_message db message_id with
                                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                                   | Ok (Some (post_id, title, cslug)) ->
                                       Dream.html (Pages.msg_page ?user ~title:"Thread already started" ~message:"This message has already been made into a thread." ~alert_type:"info" ~return_url:(Components.canonical_thread_path cslug post_id title) request)
                                   | Ok None ->
                                       let%lwt before = (match%lwt Db.get_messages_before_id_with_authors db channel.id message_id start_thread_window with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                       let%lwt seed_and_after = (match%lwt Db.get_messages_after_id_with_authors db channel.id (Int64.sub message_id 1L) (start_thread_window + 1) with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                       let%lwt sections = (match%lwt Db.get_sections_by_community db community.id with Ok ss -> Lwt.return ss | Error _ -> Lwt.return []) in
                                       (* Exclude soft-deleted from context; the seed is guaranteed non-deleted above. *)
                                       let candidates = List.filter (fun ((m : Db.chat_message), _) -> m.deleted_at = None) (before @ seed_and_after) in
                                       let default_title = Pages.Start_thread.derive_title seed.content in
                                       (* The introduction starts EMPTY — no generated transcript. The selected
                                          messages render on the thread from their persisted relations; the
                                          textarea carries only text the curator deliberately writes. *)
                                       let def_section_id = default_thread_section_id sections in
                                       (* Launch-chrome data (pass 16A), loaded only after every gate
                                          above passed — a hidden private community, a missing/foreign
                                          seed, a banned viewer and a non-member never touch the
                                          viewer's memberships or the channel list. Each degrades to an
                                          empty list on error rather than blocking the form. can_manage
                                          mirrors the report form's gate (admin || moderator) and only
                                          picks the sidebar Settings visibility; the settings handler
                                          re-checks. *)
                                       let%lwt channels = (match%lwt Db.get_channels_by_community db community.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                       let%lwt rail_communities = (match%lwt Db.get_user_communities db user_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                       let%lwt can_manage =
                                         if is_admin then Lwt.return true
                                         else (match%lwt Db.is_moderator db user_id community.id with
                                           | Ok b -> Lwt.return b
                                           | _ -> Lwt.return false)
                                       in
                                       Dream.html (Pages.start_thread_form ?user ~rail_communities ~channels ~can_manage ~community ~channel ~seed_id:message_id ~candidates ~sections ~default_section_id:def_section_id ~default_title ~default_body:"" request)))))))

(* POST same path — validate everything server-side (never trust the client), force the
   seed into the source set, then create the thread + provenance atomically. *)
let start_thread_create_handler request =
  let community_slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let message_id_str = Dream.param request "message_id" in
  let channel_url = Printf.sprintf "/c/%s/ch/%s" community_slug channel_slug in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = try int_of_string uid_str with _ -> 0 in
      let user = Dream.session_field request "username" in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (match Int64.of_string_opt message_id_str with
            | None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Invalid message reference." ~alert_type:"error" ~return_url:channel_url request)
            | Some message_id ->
                with_analytics_after_sql (fun record ->
                Dream.sql request (fun db ->
                  match%lwt Db.get_community_by_slug db community_slug with
                  | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
                  | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                  | Ok (Some community) ->
                      (* Slice C: private community hidden — non-authorized viewer gets 404, not
                         the membership 403 below. check_start_permission still gates creation. *)
                      let%lwt authorized = can_view_community db ~user_id ~is_admin community in
                      if not authorized then community_not_found ?user request
                      else
                      (match%lwt Db.get_channel_by_slug db channel_slug community.id with
                       | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                       | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                       | Ok (Some channel) ->
                           (match%lwt Db.get_message_by_id db message_id with
                            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That message does not exist." ~alert_type:"error" ~return_url:channel_url request)
                            | Ok (Some (seed : Db.chat_message)) ->
                                if seed.channel_id <> channel.id || seed.deleted_at <> None then
                                  Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That message is not available to start a thread from." ~alert_type:"error" ~return_url:channel_url request)
                                else
                                  (match%lwt check_start_permission db ~user_id ~is_admin ~community_id:community.id with
                                   | Start_banned -> Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Not Allowed" ~message:"You cannot start threads in this community." ~alert_type:"error" ~return_url:channel_url request)
                                   | Start_not_member -> Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Join to start a thread" ~message:"You must be a member of this community to start a thread." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                                   | Start_error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                                   | Start_allowed ->
                                       (match%lwt Db.get_seed_thread_for_message db message_id with
                                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                                        | Ok (Some (pid, ttl, cslug)) ->
                                            Dream.html (Pages.msg_page ?user ~title:"Thread already started" ~message:"This message has already been made into a thread." ~alert_type:"info" ~return_url:(Components.canonical_thread_path cslug pid ttl) request)
                                        | Ok None ->
                                            let%lwt before = (match%lwt Db.get_messages_before_id_with_authors db channel.id message_id start_thread_window with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                            let%lwt seed_and_after = (match%lwt Db.get_messages_after_id_with_authors db channel.id (Int64.sub message_id 1L) (start_thread_window + 1) with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                            let%lwt sections = (match%lwt Db.get_sections_by_community db community.id with Ok ss -> Lwt.return ss | Error _ -> Lwt.return []) in
                                            let candidates = List.filter (fun ((m : Db.chat_message), _) -> m.deleted_at = None) (before @ seed_and_after) in
                                            let valid = List.map (fun ((m : Db.chat_message), _) -> m.id) candidates in
                                            let def_section_id = default_thread_section_id sections in
                                            let title = String.trim (Option.value (List.assoc_opt "title" form_data) ~default:"") in
                                            let content_raw = String.trim (Option.value (List.assoc_opt "content" form_data) ~default:"") in
                                            let content = if content_raw = "" then None else Some content_raw in
                                            let section_id_str = Option.value (List.assoc_opt "section_id" form_data) ~default:"" in
                                            let selected = Pages.Start_thread.parse_selected_ids form_data in
                                            let context = Pages.Start_thread.normalize_selection ~seed:message_id ~max_total:start_thread_max_total ~valid selected in
                                            let rerender ?(error="") () =
                                              (* Launch-chrome data (pass 16A), loaded only when a validation
                                                 state actually re-renders the form — the success path keeps
                                                 its exact query set. Same post-gate position and degradation
                                                 as the GET handler. *)
                                              let%lwt channels = (match%lwt Db.get_channels_by_community db community.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                              let%lwt rail_communities = (match%lwt Db.get_user_communities db user_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                              let%lwt can_manage =
                                                if is_admin then Lwt.return true
                                                else (match%lwt Db.is_moderator db user_id community.id with
                                                  | Ok b -> Lwt.return b
                                                  | _ -> Lwt.return false)
                                              in
                                              Dream.html (Pages.start_thread_form ?user ~error ~rail_communities ~channels ~can_manage ~community ~channel ~seed_id:message_id ~candidates ~sections ~default_section_id:def_section_id ~default_title:title ~default_body:content_raw request)
                                            in
                                            if title = "" then rerender ~error:"Please enter a title for the thread." ()
                                            else if String.length title > 300 then rerender ~error:"Title cannot exceed 300 characters." ()
                                            else begin
                                              let%lwt section_result =
                                                if not community.sections_enabled then Lwt.return (Ok None)
                                                else begin
                                                  let sid = try int_of_string section_id_str with _ -> 0 in
                                                  if sid = 0 then Lwt.return (Error "section_required")
                                                  else match%lwt Db.get_section_by_id db sid community.id with
                                                    | Ok (Some _) -> Lwt.return (Ok (Some sid))
                                                    | Ok None -> Lwt.return (Error "section_invalid")
                                                    | Error e -> Lwt.return (Error e)
                                                end
                                              in
                                              match section_result with
                                              | Error "section_required" -> rerender ~error:"Please select a section." ()
                                              | Error "section_invalid" -> rerender ~error:"The selected section is not valid for this community." ()
                                              | Error e -> rerender ~error:(db_error_message e) ()
                                              | Ok section_id ->
                                                  (match%lwt Db.start_thread_from_chat db ~title ~content ~section_id ~community_id:community.id ~user_id ~channel_id:channel.id ~seed_message_id:message_id ~context_message_ids:context with
                                                   | Ok post_id ->
                                                       let%lwt _ = Db.increment_local_post_count db user_id community.id in
                                                       (* Participant count from the candidate rows the selection was
                                                          already validated against — distinct non-tombstoned author ids
                                                          across the promoted messages; no extra query, no guessing. *)
                                                       let promoted_ids = message_id :: context in
                                                       let participant_count =
                                                         candidates
                                                         |> List.filter (fun ((m : Db.chat_message), _) -> List.mem m.id promoted_ids)
                                                         |> List.filter_map (fun ((m : Db.chat_message), _) -> m.user_id)
                                                         |> List.sort_uniq compare
                                                         |> List.length
                                                       in
                                                       record (fun () ->
                                                           Analytics.capture_if_consented request
                                                             ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                                             (Analytics.Conversation_promoted
                                                                {
                                                                  user_id;
                                                                  community_id = community.Db.id;
                                                                  community_slug =
                                                                    analytics_public_string
                                                                      community community.Db.slug;
                                                                  channel_id = channel.Db.id;
                                                                  channel_slug =
                                                                    analytics_public_string
                                                                      community channel.Db.slug;
                                                                  section_id;
                                                                  post_id;
                                                                  message_id;
                                                                  promoted_message_count = 1 + List.length context;
                                                                  promoted_participant_count = Some participant_count;
                                                                }));
                                                       Dream.redirect request ("/p/" ^ string_of_int post_id)
                                                   | Error _ ->
                                                       (* A racing double-submit trips the seed unique index. Re-check and
                                                          link the existing thread; otherwise a generic error (no raw DB text). *)
                                                       (match%lwt Db.get_seed_thread_for_message db message_id with
                                                        | Ok (Some (pid, ttl, cslug)) ->
                                                            Dream.html (Pages.msg_page ?user ~title:"Thread already started" ~message:"This message has already been made into a thread." ~alert_type:"info" ~return_url:(Components.canonical_thread_path cslug pid ttl) request)
                                                        | _ -> rerender ~error:"Could not start the thread. Please try again." ()))
                                            end)))))))
       | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"There was a problem with your submission. Please try again." ~alert_type:"error" ~return_url:channel_url request))

let join_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid ->
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let redirect_url = List.assoc_opt "redirect_to" form_data |> Option.value ~default:"/" in

          if community_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else

          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Slice C: no self-serve join for private communities — they are hidden and
               members are added by a mod/admin (later slice), not via this open endpoint.
               Resolve the community SERVER-SIDE (the form's community_id is untrusted) and deny
               private with the same 404 as a missing community. Public join is unchanged. *)
            match%lwt Db.get_community_by_id db community_id with
            | Ok (Some community) when community.Db.visibility = Db.Community_private ->
                community_not_found ?user:(Dream.session_field request "username") request
            | Ok None -> community_not_found ?user:(Dream.session_field request "username") request
            | Error _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to join community. Please try again." ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (match%lwt Db.join_community db (int_of_string uid) community_id with
                 | Ok () ->
                     let user_id = int_of_string uid in
                     let distinct_id = Analytics.distinct_id_of_user_id user_id in
                     record (fun () ->
                         Analytics.capture_if_consented request ~distinct_id
                           (Analytics.Community_joined
                              {
                                user_id;
                                community_id = community.Db.id;
                                community_slug =
                                  analytics_public_string community
                                    community.Db.slug;
                                community_visibility =
                                  Db.community_visibility_to_string
                                    community.Db.visibility;
                              });
                         (* Full authoritative record in scope after a
                            successful join ⇒ also refresh the community group
                            profile (§5.3). *)
                         Analytics.identify_community_if_consented request
                           ~distinct_id (community_group_of community));
                     Dream.redirect request (safe_local_redirect request redirect_url)
                 | Error _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to join community. Please try again." ~alert_type:"error" ~return_url:"/" request))
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

let leave_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let redirect_to = match List.assoc_opt "redirect_to" form_data with Some r -> r | None -> "/" in
          if community_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else
          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            match%lwt Db.leave_community db user_id community_id with
            | Ok deleted ->
                (* Only when a membership row was actually deleted — a
                   non-member "leave" keeps the same redirect but is a no-op,
                   not an event. Only the form's community id is in scope; the
                   event still carries the group key through $groups (built
                   from the id), and no lookup is added just for analytics. *)
                if deleted then
                  record (fun () ->
                      Analytics.capture_if_consented request
                        ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                        (Analytics.Community_left { user_id; community_id }));
                Dream.redirect request (safe_local_redirect request redirect_to)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

let community_settings_handler request =
  let slug = Dream.param request "slug" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      (* Read admin flag outside sql block — session is per-request, no DB cost. *)
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      Dream.sql request (fun db ->
        match%lwt Db.get_community_by_slug db slug with
        | Ok (Some community) ->
            (* Admins bypass the mod check — they have global authority over settings.
               is_moderator is still consulted for non-admins to keep the ACL simple. *)
            let%lwt is_authorized =
              if is_admin then Lwt.return true
              else (match%lwt Db.is_moderator db user_id community.id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"You must be a moderator to access this page." ~alert_type:"error" ~return_url:"/" request)
            else
              (match%lwt Db.get_community_moderators db community.id with
              | Ok mods ->
                  (match%lwt Db.community_get_banned_users db community.id with
                  | Ok banned_users ->
                      let%lwt sections =
                        if community.sections_enabled then
                          (match%lwt Db.get_sections_by_community db community.id with
                           | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
                        else Lwt.return []
                      in
                      (* Live chat channels (incl. archived) so the hub can list + manage them. *)
                      let%lwt channels =
                        match%lwt Db.get_channels_by_community db community.id with
                        | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
                      in
                      (* Read-only role lookup: display-gates the Moderation tools (downvote toggle)
                         to top_mod/admin so it stays hidden from regular mods exactly as it was on
                         the old public home. Authorization is still enforced by
                         toggle_downvotes_handler — this changes no settings-view permission. *)
                      let%lwt is_top_mod =
                        match%lwt Db.get_moderator_role db user_id community.id with
                        | Ok (Some "top_mod") -> Lwt.return true
                        | _ -> Lwt.return false
                      in
                      (* Cheap COUNT for the settings hub "Reports (N open)" affordance; a
                         failure degrades to 0 rather than blocking the whole settings page. *)
                      let%lwt open_reports_count =
                        match%lwt Db.count_open_reports db community.id with
                        | Ok n -> Lwt.return n | Error _ -> Lwt.return 0
                      in
                      (* Slice F: the community_members allow-list for the member-management card.
                         A lookup failure degrades to an empty list rather than blocking settings. *)
                      let%lwt members =
                        match%lwt Db.get_community_members db community.id with
                        | Ok m -> Lwt.return m | Error _ -> Lwt.return []
                      in
                      (* Global-rail parity: the viewer's joined communities, in the same
                         stable order the feed/overview/channel/section/thread handlers
                         load. Queried only after the settings authorization above
                         succeeded, so a denied or anonymous request never touches
                         membership data; a failure degrades to the no-data rail rather
                         than blocking settings. Ordering, dedup, and the active marker
                         stay owned by the shared launch doc builder. *)
                      let%lwt rail_communities =
                        match%lwt Db.get_user_communities db user_id with
                        | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
                      in
                      (* Connected-project management: loaded only after the settings
                         authorization above succeeded, and only for the same top-mod/admin
                         surface that already gates the project-home request queue. There is
                         no second accepted-project query — this is the existing public
                         read model, rendered with removal controls. *)
                      with_settings_connected_projects db ?user request
                        ~community_slug:community.slug
                        ~authorized:(is_top_mod || is_admin)
                        ~removal_allowed:
                          (not
                             (community.is_network_community
                             && community.onboarding_state = Db.Community_draft))
                        (fun connected_projects ->
                      Dream.html (Pages.community_settings_page ?user ~connected_projects ~rail_communities ~is_admin ~is_top_mod ~open_reports_count ~community ~mods ~banned_users ~members ~sections ~channels request))
                  | Error e -> Dream.html (db_error_message e))
              | Error e -> Dream.html (db_error_message e))
        | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
      )

let add_section_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let description = (match String.trim (get "description") with "" -> None | d -> Some d) in
          let default_sort = (match get "default_sort" with "new" | "top" | "active" as s -> s | _ -> "hot") in
          let position = (try int_of_string (get "position") with _ -> 1) in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Validation Error" ~message:"Section name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (match%lwt Db.create_section db community.id name description position default_sort false with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let update_section_handler request =
  let slug = Dream.param request "slug" in
  let section_id_str = Dream.param request "section_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let section_id = try int_of_string section_id_str with _ -> 0 in
      if section_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid section ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let description = (match String.trim (get "description") with "" -> None | d -> Some d) in
          let default_sort = (match get "default_sort" with "new" | "top" | "active" as s -> s | _ -> "hot") in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Validation Error" ~message:"Section name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Validate section belongs to this community before updating *)
                  (match%lwt Db.get_section_by_id db section_id community.id with
                   | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Section not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok (Some _) ->
                       (match%lwt Db.update_section db section_id name description default_sort with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request))))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let delete_section_handler request =
  let slug = Dream.param request "slug" in
  let section_id_str = Dream.param request "section_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let section_id = try int_of_string section_id_str with _ -> 0 in
      if section_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid section ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (match%lwt Db.delete_section db section_id community.id with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

(* === Live chat channel management ===
   Same security template as the section handlers above: login → form →
   community lookup → (is_admin || is_moderator) gate → ownership-validate →
   act → redirect to the settings hub. Channels are never hard-deleted; archive
   is the soft, reversible off-switch. Slug is auto-derived on create and never
   mutated on update, so existing /c/:slug/ch/:channel_slug links keep resolving
   and chat messages (keyed by channel_id) are unaffected. *)

let add_channel_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let topic = (match String.trim (get "topic") with "" -> None | t -> Some t) in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Validation Error" ~message:"Channel name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* New channel goes after the existing ones; create_channel slugifies + dedupes. *)
                  let%lwt position = match%lwt Db.get_channels_by_community db community.id with
                    | Ok cs -> Lwt.return (List.length cs) | Error _ -> Lwt.return 0 in
                  (match%lwt Db.create_channel db community.id name topic position with
                   | Ok _slug -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let update_channel_handler request =
  let slug = Dream.param request "slug" in
  let channel_id_str = Dream.param request "channel_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let channel_id = try int_of_string channel_id_str with _ -> 0 in
      if channel_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid channel ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let topic = (match String.trim (get "topic") with "" -> None | t -> Some t) in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Validation Error" ~message:"Channel name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Validate the channel belongs to this community before updating. Slug is left
                     untouched — only display name + topic change. *)
                  (match%lwt Db.get_channel_by_id db channel_id community.id with
                   | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok (Some _) ->
                       (match%lwt Db.update_channel db channel_id community.id name topic with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request))))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let archive_channel_handler request =
  let slug = Dream.param request "slug" in
  let channel_id_str = Dream.param request "channel_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let channel_id = try int_of_string channel_id_str with _ -> 0 in
      if channel_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid channel ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Load all channels once: validates ownership AND lets us guard the two
                     invariants — the default `general` channel and the last active channel
                     must never be archived (a community always keeps somewhere to chat). *)
                  (match%lwt Db.get_channels_by_community db community.id with
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok channels ->
                       (match List.find_opt (fun (c : Db.channel) -> c.id = channel_id) channels with
                        | None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                        | Some channel ->
                            let active_count = List.length (List.filter (fun (c : Db.channel) -> not c.is_archived) channels) in
                            if channel.slug = "general" then
                              Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Not Allowed" ~message:"The default #general channel cannot be archived." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                            else if (not channel.is_archived) && active_count <= 1 then
                              Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Not Allowed" ~message:"You cannot archive the last active channel — a community needs at least one." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                            else
                              (match%lwt Db.set_channel_archived db channel_id community.id true with
                               | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                               | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let unarchive_channel_handler request =
  let slug = Dream.param request "slug" in
  let channel_id_str = Dream.param request "channel_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let channel_id = try int_of_string channel_id_str with _ -> 0 in
      if channel_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid channel ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Validate ownership before flipping the flag. Unarchive is always safe. *)
                  (match%lwt Db.get_channel_by_id db channel_id community.id with
                   | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok (Some _) ->
                       (match%lwt Db.set_channel_archived db channel_id community.id false with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request))))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let modlog_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> (try int_of_string id with _ -> 0) | None -> 0 in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  Dream.sql request (fun db ->
    match%lwt Db.get_community_by_slug db slug with
    | Ok (Some community) ->
        let%lwt authorized = can_view_community db ~user_id ~is_admin community in
        if not authorized then community_not_found ?user request
        else
        (* Settings access mirrors community_settings_handler's gate (admin || moderator); it only
           picks the back-link target, so a failed lookup safely degrades to "Back to community". *)
        let%lwt can_access_settings =
          if is_admin then Lwt.return true
          else (match%lwt Db.is_moderator db user_id community.id with
            | Ok b -> Lwt.return b
            | _ -> Lwt.return false)
        in
        (* Launch-chrome data (pass 14A), loaded only after the private-community
           authorization decision above: sections/channels feed the shared community
           sidebar, the viewer's joined communities the global rail. Each degrades to
           an empty list on error rather than blocking the log; anonymous viewers
           never touch membership data. *)
        let%lwt sections =
          if community.sections_enabled then
            (match%lwt Db.get_sections_by_community db community.id with
             | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
          else Lwt.return []
        in
        let%lwt channels =
          match%lwt Db.get_channels_by_community db community.id with
          | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
        in
        let%lwt rail_communities =
          if user_id > 0 then
            (match%lwt Db.get_user_communities db user_id with
             | Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
          else Lwt.return []
        in
        (match%lwt Db.get_modlog db community.id with
         | Ok actions -> Dream.html (Pages.mod_log_page ?user ~noindex:(community_noindex community) ~rail_communities ~can_access_settings ~channels ~sections ~community actions request)
         | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* === NETWORK-COMMUNITY LEGACY MUTATION GUARDS ===

   A provisioned network community is configured only through the canonical
   setup flow: GET /c/:slug/setup reviews the draft and the future
   POST /c/:slug/publish commits identity and exposure together, atomically.
   None of the legacy settings mutations below may stand in for it, and a
   forged direct POST must fail closed *before* any write rather than
   becoming a database CHECK violation rendered back as "Database error: …"
   (the scoped communities_network_* constraints would otherwise echo a
   constraint name to the client).

   These guards are deliberately narrow: they test durable columns on the
   authoritative record the handler has already loaded and authorized, they
   never write, and a legacy (non-network) community reaches exactly the code
   it always did. *)

let is_network_setup_draft (community : Db.community) =
  community.is_network_community
  && community.onboarding_state = Db.Community_draft

(* The canonical community-identity policy, applied to a *published* network
   community's proposed description before the legacy detail write. The
   policy is not restated here: the community's own stored name and slug ride
   along so the frozen parser decides all three together, exactly as the
   provisioning store persisted them. [Error] means fail closed — the write
   never happens — and a canonical [Ok] value is what gets stored, so the
   scoped database CHECK is a backstop rather than the enforcement point. *)
let canonical_network_description (community : Db.community) ~raw_description =
  match
    Project_home_provisioning_form.of_fields
      [ ("community_name", community.name);
        ("community_slug", community.slug);
        ("community_description", raw_description)
      ]
  with
  | Ok identity ->
      Ok (Project_home_provisioning_form.community_description identity)
  | Error _ -> Error ()

let update_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.multipart request with
      | `Ok form_data ->
          let get_field name =
            match List.assoc_opt name form_data with
            | Some ((_, v) :: _) -> v
            | _ -> ""
          in
          let community_id = try int_of_string (get_field "community_id") with _ -> 0 in
          if community_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else
          (* Empty string → None: lets mods clear a field without sending NULL hacks. *)
          let str_opt s = let t = String.trim s in if t = "" then None else Some t in
          let description = str_opt (get_field "description") in
          let rules       = str_opt (get_field "rules") in
          (* If no new file uploaded, fall back to the existing URL submitted via hidden input. *)
          let avatar_bytes = get_field "avatar_url" in
          let banner_bytes = get_field "banner_url" in
          let existing_avatar = str_opt (get_field "existing_avatar_url") in
          let existing_banner = str_opt (get_field "existing_banner_url") in
          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Re-verify authority on every mutation — same TOCTOU guard as add_mod. *)
            let%lwt authorized =
              if is_admin then Lwt.return true
              else (match%lwt Db.is_moderator db user_id community_id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Access Denied" ~message:"You must be a moderator to perform this action." ~alert_type:"error" ~return_url:"/" request)
            else
            (* The community record is loaded up front because every redirect
               and return URL below must be built from the authoritative
               database slug — the submitted community_slug field is
               attacker-controlled and reached the Location header. *)
            match%lwt Db.get_community_by_id db community_id with
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok loaded ->
            let settings_url =
              match loaded with
              | Some c -> "/c/" ^ c.slug ^ "/settings"
              | None -> "/"
            in
            (* Image processing runs only AFTER the moderator check. It used
               to run before it, so any authenticated user could force two
               full ImageMagick conversions and leave two files in
               static/uploads for any community id, then be told "Access
               Denied" — the work and the storage happened regardless. *)
            let%lwt avatar_result =
              process_image_upload ~db ~ip:(Dream.client request)
                ~purpose:Image_upload.Community_avatar avatar_bytes
            in
            let%lwt banner_result =
              process_image_upload ~db ~ip:(Dream.client request)
                ~purpose:Image_upload.Community_banner banner_bytes
            in
            match avatar_result, banner_result with
            | Error e, _ | _, Error e ->
                Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Image Error" ~message:e ~alert_type:"error" ~return_url:settings_url request)
            | Ok new_avatar, Ok new_banner ->
              (* new_avatar/new_banner are None when no file was submitted; fall back to existing. *)
              let avatar_url = if new_avatar <> None then new_avatar else existing_avatar in
              let banner_url = if new_banner <> None then new_banner else existing_banner in
              (* Network-community guard, before any write. The description is
                 canonical community identity, so a setup draft is refused
                 outright (its identity belongs to /c/:slug/setup) and a
                 published network community's description must satisfy the
                 frozen canonical policy rather than reach the scoped database
                 CHECK. A legacy community keeps exactly its previous
                 behaviour, including the untouched no-op path when the id
                 matches nothing. *)
              (match loaded with
               | Some target when is_network_setup_draft target ->
                   (* The generic community 404: nothing about the draft's
                      lifecycle, identity, or authorization is disclosed, and
                      nothing is written. *)
                   community_not_found ?user:(Dream.session_field request "username") request
               | loaded ->
                 let network_description =
                   match loaded with
                   | Some target when target.is_network_community ->
                       canonical_network_description target
                         ~raw_description:(get_field "description")
                   | _ -> Ok description
                 in
                 match network_description with
                 | Error () ->
                     (* Fail closed: no write, and no constraint name, SQL, or
                        submitted value in the response. *)
                     Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your submission. Please try again." ~alert_type:"error" ~return_url:settings_url request)
                 | Ok description ->
              (match%lwt Db.update_community_details db community_id description rules avatar_url banner_url with
              | Ok (Some community) ->
                  (* UPDATE ... RETURNING supplied the authoritative updated
                     record — refresh the group profile (§5.3), attributed to
                     the acting moderator/admin. *)
                  record (fun () ->
                      Analytics.identify_community_if_consented request
                        ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                        (community_group_of community));
                  Dream.redirect request settings_url
              | Ok None ->
                  (* No community matched the id: previously a silent no-op
                     UPDATE with the same redirect; keep the response, emit
                     nothing. *)
                  Dream.redirect request settings_url
              | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:settings_url request)))
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

(* There are deliberately no add_mod_handler / remove_mod_handler. The
   /add-mod and /remove-mod routes they served were unreferenced legacy
   endpoints — no form, link, script or test emitted them — with strictly
   weaker authorization than the surface that replaced them: they admitted
   ANY moderator of the community, so an ordinary mod could appoint further
   moderators and unseat the Top Mod. Moderator management now lives only on
   /c/:slug/manage-mods/{add,promote,remove}, which requires top_mod (or a
   durable global admin) and refuses to remove a top_mod target. *)

let ban_community_user_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let target_username = String.trim (List.assoc_opt "target_username" form_data |> Option.value ~default:"") in
          let reason = String.trim (List.assoc_opt "reason" form_data |> Option.value ~default:"") in
          if community_id = 0 || target_username = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form data." ~alert_type:"error" ~return_url:"/" request)
          else
          Dream.sql request (fun db ->
            (* TOCTOU guard: re-verify authorization at mutation time, not just at render.
               Separate is_community_mod from is_admin so we can log admin overrides distinctly. *)
            let%lwt is_community_mod = match%lwt Db.is_moderator db user_id community_id with
              | Ok b -> Lwt.return b | _ -> Lwt.return false
            in
            let is_authorized = is_admin || is_community_mod in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Access Denied" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:"/" request)
            else
              (match%lwt Db.get_user_by_username db target_username with
              | Ok (Some target_user) ->
                  (* Admin immunity: local mods cannot ban global admins. *)
                  let%lwt target_is_admin = match%lwt Db.is_user_admin db target_user.id with
                    | Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                  if target_is_admin && not is_admin then
                    Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Action Denied" ~message:"You cannot ban a Global Administrator." ~alert_type:"error" ~return_url:"/" request)
                  else begin
                    (* Fetch community slug before the ban so we can redirect to /c/slug after. *)
                    let%lwt community_res = Db.get_community_by_id db community_id in
                    let%lwt _ = Db.community_ban_user db target_user.id community_id in
                    (* Admin acting without mod role logged distinctly to prevent spoofing the mod log. *)
                    let is_admin_override = is_admin && not is_community_mod in
                    let action_type = if is_admin_override then "admin_ban_user" else "ban_user" in
                    let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                    let%lwt _ = Db.log_mod_action db community_id user_id action_type (Some target_user.id) logged_reason in
                    (* Notify banned user — no post_id since a ban is not tied to a single post. *)
                    let ban_msg = "You have been banned from a community. Reason: " ^ reason in
                    let%lwt _ = Db.create_notif db target_user.id None "mod_action" ban_msg in
                    let target = match community_res with Ok (Some c) -> "/c/" ^ c.slug ^ "/settings?panel=bans" | _ -> "/" in
                    Dream.redirect request target
                  end
              | Ok None -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"User Not Found" ~message:("No user was found with the username u/" ^ target_username ^ ".") ~alert_type:"error" ~return_url:"/" request)
              | Error e -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request))
          )
      | _ -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

let unban_community_user_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let target_user_id = try int_of_string (List.assoc_opt "target_user_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if community_id = 0 || target_user_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form data." ~alert_type:"error" ~return_url:"/" request)
          else
          Dream.sql request (fun db ->
            (* Admins bypass mod-check for unban, symmetric with ban_community_user_handler. *)
            let%lwt is_authorized =
              if is_admin then Lwt.return true
              else (match%lwt Db.is_moderator db user_id community_id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Access Denied" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:"/" request)
            else
              (* The authoritative record is loaded BEFORE mutating: the
                 Location header must come from the database slug, never the
                 submitted community_slug field (redirect/header injection),
                 and a failed lookup or unban must surface as an error rather
                 than a success-shaped redirect. *)
              match%lwt Db.get_community_by_id db community_id with
              | Error e ->
                  Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
              | Ok None ->
                  Dream.respond ~status:`Not_Found (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
              | Ok (Some community) ->
                  let settings_url = "/c/" ^ community.Db.slug ^ "/settings?panel=bans" in
                  (match%lwt Db.community_unban_user db target_user_id community_id with
                   | Error e ->
                       Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:settings_url request)
                   | Ok () -> Dream.redirect request settings_url)
          )
      | _ -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

(* === POST === *)

let new_post_page request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let community_slug_opt = Dream.query request "community" in
      let section_slug_opt = Dream.query request "section" in

      (* Joined communities feed the launch rail only; a failure degrades to
         an empty rail rather than blocking the composer. Called only after
         the viewer is authorized for the requested state. *)
      let load_rail db =
        match%lwt Db.get_user_communities db user_id with
        | Ok cs -> Lwt.return cs
        | Error _ -> Lwt.return []
      in

      match community_slug_opt with
      | Some slug ->
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok (Some community) ->
                (* Privacy gate: a private community must be indistinguishable
                   from a missing one for outsiders — the same rule as the
                   overview/section/thread/report surfaces. Previously this
                   route answered a non-member's ?community=<private-slug>
                   with the join gate, confirming existence and leaking the
                   community name in the title. *)
                let%lwt authorized = can_view_community db ~user_id ~is_admin community in
                if not authorized then community_not_found ?user request
                else
                (match%lwt Db.is_member db user_id community.id with
                | Ok true ->
                    let%lwt sections =
                      if community.sections_enabled then
                        (match%lwt Db.get_sections_by_community db community.id with
                         | Ok secs -> Lwt.return secs
                         | Error _ -> Lwt.return [])
                      else Lwt.return []
                    in
                    (* Resolve ?section=slug to section_id for pre-selection in the form *)
                    let%lwt preselected_section_id_opt = match section_slug_opt with
                      | None -> Lwt.return None
                      | Some sec_slug ->
                          (match%lwt Db.get_section_by_slug db sec_slug community.id with
                           | Ok (Some s) -> Lwt.return (Some s.Db.section_id)
                           | _ -> Lwt.return None)
                    in
                    let%lwt rail_communities = load_rail db in
                    (* Optional shared-thread destinations (slice 4): the
                       eligible connected communities for THIS server-resolved
                       origin. Best-effort like the rail — a read failure
                       renders the plain composer rather than blocking post
                       creation; the list grants nothing (POST /posts
                       re-resolves the slug and the placement store
                       revalidates under its own locks). *)
                    let%lwt share_candidates =
                      match%lwt
                        Shared_thread_placement_read_model.connected_destinations
                          db ~origin_community_id:community.id
                      with
                      | Ok cs ->
                          Lwt.return
                            (List.map
                               (fun c ->
                                 ( Shared_thread_placement_read_model.candidate_slug c,
                                   Shared_thread_placement_read_model.candidate_name c ))
                               cs)
                      | Error _ -> Lwt.return []
                    in
                    Dream.html (Pages.new_post_form ?user ?preselected_section_id:preselected_section_id_opt ~rail_communities ~share_candidates sections community request)
                | Ok false ->
                    let%lwt rail_communities = load_rail db in
                    Dream.html (Pages.join_to_post_page ?user ~rail_communities community request)
                | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request))

            | Ok None -> community_not_found ?user request
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          )
      | None ->
          Dream.sql request (fun db ->
            match%lwt Db.get_all_communities db with
            | Ok communities ->
                (* The chooser must never list a community the viewer cannot
                   see: get_all_communities returns every row, and the legacy
                   page exposed private community names and slugs to any
                   logged-in user. Filter with the same per-community
                   authorization the content surfaces use. *)
                let%lwt visible =
                  Lwt_list.filter_s
                    (fun c -> can_view_community db ~user_id ~is_admin c)
                    communities
                in
                let%lwt rail_communities = load_rail db in
                Dream.html (Pages.choose_community_page ?user ~request ~rail_communities visible)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          )

let create_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id_str ->
      let user_id = int_of_string user_id_str in
      let username = Option.value (Dream.session_field request "username") ~default:"Someone" in

      (* multipart/form-data: required for file upload; replaces form which only handles
         application/x-www-form-urlencoded. Dream.multipart returns
         (field_name * (filename_opt * content) list) list — extract the first part value. *)
      match%lwt Dream.multipart request with
      | `Ok form_data ->
          let get_field name =
            match List.assoc_opt name form_data with
            | Some ((_, v) :: _) -> v
            | _ -> ""
          in
          let title = String.trim (get_field "title") in
          let community_id_str = get_field "community_id" in
          let url = match get_field "url" with "" -> None | u -> Some u in
          let content = match get_field "content" with "" -> None | c -> Some c in
          let section_id_str = get_field "section_id" in
          (* File bytes: empty string when no file is selected (browser sends empty part). *)
          let image_bytes = get_field "image" in
          (* Optional shared-thread fields (slice 4). A blank select is the
             normal share-free path; a selected slug is only re-resolved
             server-side AFTER the post exists, because the canonical post
             must never depend on any destination condition. The note alone
             is judged now — deterministic user-input validation through the
             one domain canonicalizer — so a hopeless note fails before a
             post exists. A note without a destination is ignored. *)
          let share_destination =
            match String.trim (get_field "share_destination") with
            | "" -> None
            | slug -> Some slug in
          let share_note_result =
            match share_destination with
            | None -> Ok None
            | Some _ ->
                Shared_thread_placements.canonical_request_note
                  (Some (get_field "share_note")) in

          if title = "" then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Post title cannot be empty." ~alert_type:"error" ~return_url:"/" request)
          else if String.length title > 300 then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Post title cannot exceed 300 characters." ~alert_type:"error" ~return_url:"/" request)
          else if image_bytes <> "" && String.length image_bytes > 5 * 1024 * 1024 then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Image exceeds the 5 MB limit." ~alert_type:"error" ~return_url:"/" request)
          else if Result.is_error share_note_result then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"The private request note is too long or contains characters that cannot be stored. Notes can hold up to 2,000 characters." ~alert_type:"error" ~return_url:"/" request)
          else

          let community_id = try int_of_string community_id_str with _ -> 0 in
          if community_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community selection." ~alert_type:"error" ~return_url:"/" request)
          else

          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Global ban gate: checked first — a globally banned user's session may
               still be active if they were banned after logging in. *)
            let%lwt is_gb =
              match%lwt Db.is_globally_banned db user_id with
              | Ok b -> Lwt.return b | Error _ -> Lwt.return false
            in
            if is_gb then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request)
            else
              match%lwt Db.is_member db user_id community_id with
              | Ok true ->
                  (* Local ban check: evaluated only for members. *)
                  (match%lwt Db.community_is_banned db user_id community_id with
                  | Ok true ->
                      Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Banned from Community" ~message:"You are banned from posting in this community." ~alert_type:"error" ~return_url:"/" request)
                  | _ ->
                  (* Image processing runs here, after the global-ban,
                     membership and community-ban gates. It used to run before
                     all three, so a banned user or a non-member could force a
                     full ImageMagick conversion and leave a file in
                     static/uploads for any community id and still be refused
                     the post. *)
                  (match%lwt process_image_upload ~db ~ip:(Dream.client request)
                               ~purpose:Image_upload.Post_image image_bytes with
                  | Error img_err ->
                      Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Image Error" ~message:img_err ~alert_type:"error" ~return_url:"/" request)
                  | Ok image_url ->
                      (* Server-side section validation: section_id must belong to this community.
                         Prevents posting to a section from a different community via crafted form. *)
                      (* Loaded once: section validation here and, on the
                         sharing path, the canonical redirect target after
                         creation. The Error/Ok None outcomes keep their
                         exact pre-slice-4 behavior. *)
                      let%lwt community_record = Db.get_community_by_id db community_id in
                      let%lwt section_result =
                        match community_record with
                        | Error e -> Lwt.return (Error e)
                        | Ok None -> Lwt.return (Ok None)
                        | Ok (Some comm) ->
                            if not comm.sections_enabled then
                              Lwt.return (Ok None)
                            else begin
                              let sid = try int_of_string section_id_str with _ -> 0 in
                              if sid = 0 then
                                Lwt.return (Error "section_required")
                              else
                                match%lwt Db.get_section_by_id db sid community_id with
                                | Ok (Some _) -> Lwt.return (Ok (Some sid))
                                | Ok None    -> Lwt.return (Error "section_invalid")
                                | Error e    -> Lwt.return (Error e)
                            end
                      in
                      (match section_result with
                      | Error "section_required" ->
                          Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Please select a section for your post." ~alert_type:"error" ~return_url:"/" request)
                      | Error "section_invalid" ->
                          Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Invalid Section" ~message:"The selected section does not belong to this community." ~alert_type:"error" ~return_url:"/" request)
                      | Error e ->
                          Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                      | Ok section_id ->
                          (match%lwt Db.create_post db title url content image_url section_id community_id user_id with
                          | Ok new_post_id ->
                              let%lwt _ = Db.increment_local_post_count db user_id community_id in
                              (* Fan-out @mention notifications — best-effort, skips self-mentions. *)
                              let text = title ^ " " ^ (Option.value ~default:"" content) in
                              (* Closed derived flags only — never the title,
                                 body, or URL themselves. *)
                              (* Named once: the sharing branch below must
                                 compose with this capture, because record
                                 holds a single pending slot and a second
                                 record call would silently replace it. *)
                              let capture_creation () =
                                Analytics.capture_if_consented request
                                  ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                  (Analytics.Forum_thread_created
                                     {
                                       user_id;
                                       community_id;
                                       section_id;
                                       post_id = new_post_id;
                                       content_length =
                                         String.length (Option.value ~default:"" content);
                                       has_link = url <> None;
                                       has_mention = extract_mentions text <> [];
                                     }) in
                              record capture_creation;
                              let%lwt () = Lwt_list.iter_s (fun uname ->
                                match%lwt Db.get_user_by_username db uname with
                                | Ok (Some mentioned) when mentioned.id <> user_id ->
                                    let msg = username ^ " mentioned you in a post." in
                                    let%lwt _ = Db.create_notif db mentioned.id (Some new_post_id) "mention" msg in
                                    Lwt.return_unit
                                | _ -> Lwt.return_unit
                              ) (extract_mentions text) in
                              (match share_destination with
                              | None ->
                              (* Redirect to the new post rather than "/" so the author
                                 immediately sees their submission with its canonical URL. *)
                                  Dream.redirect request ("/p/" ^ string_of_int new_post_id)
                              | Some destination_slug ->
                                  (* The canonical post is committed and every normal side
                                     effect above has already run; nothing below may undo
                                     any of it. The slug is re-resolved server-side and the
                                     placement store revalidates connection, eligibility,
                                     tombstone state and uniqueness under its own locks —
                                     its transaction stays atomic (placement + audit +
                                     notifications, or nothing). Every failure — tampered
                                     or vanished destination, lost connection, eligibility
                                     drift, store error — collapses into the one fixed
                                     partial-success notice: which condition failed never
                                     surfaces, and no raw error crosses. *)
                                  let share_note =
                                    match share_note_result with Ok n -> n | Error _ -> None in
                                  let%lwt requested =
                                    match%lwt
                                      Shared_thread_placement_read_model.resolve_destination
                                        db ~slug:destination_slug
                                    with
                                    | Ok (Some destination_community_id) -> (
                                        match%lwt
                                          Shared_thread_placement_store.request db
                                            ~actor_user_id:user_id ~post_id:new_post_id
                                            ~destination_community_id
                                            ~request_note:share_note
                                        with
                                        | Ok _ ->
                                            (* Convention: captured only for the committed
                                               request; a failed attempt produces nothing.
                                               Origin id + post id only — never the
                                               destination or the private note. Composed
                                               with the creation capture: record holds one
                                               slot, and the normal creation event must
                                               keep firing unchanged. *)
                                            record (fun () ->
                                                capture_creation ();
                                                Analytics.capture_if_consented request
                                                  ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                                  (Analytics.Shared_thread_request_submitted
                                                     { user_id; community_id; post_id = new_post_id }));
                                            Lwt.return true
                                        | Error _ -> Lwt.return false)
                                    | Ok None | Error _ -> Lwt.return false
                                  in
                                  let notice = if requested then "requested" else "failed" in
                                  (* PRG onto the canonical origin thread — never a composer
                                     re-render, which would invite a duplicate submission.
                                     The path comes from the server-loaded community record;
                                     the pathological missing-record case falls back to the
                                     legacy /p/:id redirect (the notice is lost, the thread
                                     is not). *)
                                  (match community_record with
                                  | Ok (Some comm) ->
                                      Dream.redirect request
                                        (Components.canonical_thread_path comm.Db.slug new_post_id title
                                         ^ "?shared=" ^ notice)
                                  | _ -> Dream.redirect request ("/p/" ^ string_of_int new_post_id)))
                          | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)))))
              | Ok false ->
                  Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not a Member" ~message:"You must join this community before you can post in it." ~alert_type:"error" ~return_url:"/" request)
              | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          ))
      | _ -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

let view_post_handler request =
  let user_sess = Dream.session_field request "username" in
  let user_id_opt = Dream.session_field request "user_id" in
  (* Guard against /p/notanumber — Dream's router only enforces :id is non-empty. *)
  let post_id_opt = try Some (int_of_string (Dream.param request "id")) with _ -> None in
  match post_id_opt with
  | None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"Invalid post ID." ~alert_type:"error" ~return_url:"/" request)
  | Some post_id ->

  Dream.sql request (fun db ->
    match%lwt Db.get_post_by_id db post_id with
    | Ok (Some post) ->
        (* Slice C: gate BEFORE the canonical redirect — a 301 to /c/:slug/t/:id-:title would
           otherwise leak a private community's slug + thread title in the Location header to a
           non-authorized viewer. Resolve visibility from the post's community_id (never client
           input); deny with the SAME 404 as a missing community. Fail closed if the community
           can't be resolved. *)
        let viewer_id = match user_id_opt with Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
        let is_admin = Dream.session_field request "is_admin" = Some "true" in
        let%lwt gate_ok =
          match%lwt Db.get_community_by_id db post.community_id with
          | Ok (Some community) -> can_view_community db ~user_id:viewer_id ~is_admin community
          | _ -> Lwt.return false
        in
        if not gate_ok then community_not_found ?user:user_sess request
        else if post.community_slug <> "" then
          (* /p/:id is legacy: 301 to the canonical thread URL so we converge on one URL model.
             post_id stays authoritative; the canonical route renders the shell. *)
          Dream.redirect ~status:`Moved_Permanently request
            (Components.canonical_thread_path post.community_slug post.id post.title)
        else begin
        (* Safe fallback for the pathological unmappable post (no community slug): render the
           legacy warm-card page rather than 500. In practice community_slug is always set. *)
        let%lwt comments_result = Db.get_comments db post.id in

        let%lwt is_member_result =
          match user_id_opt with
          | Some uid -> Db.is_member db (int_of_string uid) post.community_id
          | None -> Lwt.return (Ok false)
        in

        let%lwt user_post_votes = get_current_user_votes db request in
        let%lwt user_comment_votes = get_current_user_comment_votes db request in
        let%lwt is_mod_res =
          match user_id_opt with
          | Some uid -> Db.is_moderator db (int_of_string uid) post.community_id
          | None -> Lwt.return_ok false
        in
        let%lwt mods_res = Db.get_community_moderators db post.community_id in

        let%lwt admin_usernames_res = Db.get_admin_usernames db in
        let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
        let%lwt banned_res = Db.community_get_banned_users db post.community_id in
        let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: Db.user) -> u.username) bs | _ -> [] in
        let%lwt community_res = Db.get_community_by_slug db post.community_slug in
        let%lwt user_communities_res = match user_id_opt with
          | Some uid -> Db.get_user_communities db (int_of_string uid)
          | None -> Lwt.return_ok []
        in
        let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
        let%lwt moderated_communities_res = match user_id_opt with
          | Some uid -> Db.get_moderated_communities db (int_of_string uid)
          | None -> Lwt.return_ok []
        in
        let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
        (* Fallback community: if the record is somehow missing, construct a minimal one
           from post fields so the page can still render without a 500. *)
        let community_for_page : Db.community = match community_res with
          | Ok (Some a) -> a
          | _ -> { id = post.community_id; slug = post.community_slug; name = post.community_slug;
                   description = None; rules = None; avatar_url = None; banner_url = None; allow_downvotes = true; sections_enabled = false; visibility = Db.Community_public; indexable = true;
                   is_network_community = false; onboarding_state = Db.Community_published; discoverable = true }
        in
        let%lwt noindex = thread_noindex db community_for_page post in
        (match comments_result, is_member_result with
        | Ok comments, Ok is_member ->
            let is_mod = match is_mod_res with Ok b -> b | _ -> false in
            let mod_usernames = match mods_res with Ok ms -> List.map (fun (u: Db.user) -> u.username) ms | _ -> [] in
            Dream.html (Pages.post_page ?user:user_sess ~noindex ~is_member ~is_current_user_mod:is_mod ~mod_usernames ~admin_usernames ~banned_usernames ~community:community_for_page ~user_communities ~moderated_communities user_post_votes user_comment_votes post comments request)
        | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load post data. Please try again later." ~alert_type:"error" ~return_url:"/" request))
        end

    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This post does not exist or has been deleted." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* GET /c/:slug/t/:thread — canonical thread view inside the shell. ":thread" is "post_id-post_slug";
   post_id is the leading integer and is authoritative for lookup. A wrong community slug or a
   wrong/missing descriptive slug 301s to the canonical URL. Mirrors community_section_handler's
   sidebar/data load, then renders Pages.thread_shell_page. *)
let view_thread_handler request =
  let user_sess = Dream.session_field request "username" in
  let user_id_opt = Dream.session_field request "user_id" in
  let community_slug = Dream.param request "slug" in
  let thread_param = Dream.param request "thread" in
  let post_id_opt =
    let s = match String.index_opt thread_param '-' with Some i -> String.sub thread_param 0 i | None -> thread_param in
    try Some (int_of_string s) with _ -> None
  in
  match post_id_opt with
  | None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"Invalid thread URL." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
  | Some post_id ->
  Dream.sql request (fun db ->
    match%lwt Db.get_post_by_id db post_id with
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This thread does not exist or has been deleted." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
    | Ok (Some post) ->
        let viewer_id = match user_id_opt with Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
        let is_admin = Dream.session_field request "is_admin" = Some "true" in
        (* Shared Threads: a route slug that is NOT the post's own community
           may be an accepted destination context. One bounded read-model
           point query answers the placement facts (accepted, bound to this
           slug, origin currently public); the viewer's access to the
           destination is then the community's one EXISTING
           can_view_community rule over the loaded record — no third read
           rule. Every failure — no placement, an inactive placement, a
           private origin, an inaccessible private destination, a read
           error — takes the same fall-through into the pre-existing path
           below, whose observables (canonical 301 for viewers who may read
           the origin thread, one generic 404 otherwise) are identical for
           all of them, so no placement state can be inferred. *)
        let%lwt destination_context =
          if community_slug = post.community_slug then Lwt.return None
          else
            match%lwt
              Shared_thread_reading.resolve_destination_context db
                ~post_id:post.id ~destination_slug:community_slug
            with
            | Ok (Some ctx) -> (
                match%lwt
                  Db.get_community_by_id db
                    ctx.Shared_thread_reading.destination_community_id
                with
                | Ok (Some destination) ->
                    let%lwt viewable =
                      can_view_community db ~user_id:viewer_id ~is_admin destination
                    in
                    Lwt.return (if viewable then Some (ctx, destination) else None)
                | _ -> Lwt.return None)
            | Ok None | Error _ -> Lwt.return None
        in
        (match destination_context with
        | Some (ctx, destination) ->
            (* One destination URL per thread: a wrong/missing descriptive
               slug 301s within the destination context, mirroring the
               canonical redirect. Server-built path — never a stored URL. *)
            let destination_path =
              Components.canonical_thread_path destination.Db.slug post.id post.title in
            let current_path = "/c/" ^ community_slug ^ "/t/" ^ thread_param in
            if current_path <> destination_path then
              Dream.redirect ~status:`Moved_Permanently request destination_path
            else begin
              let%lwt comments_result = Db.get_comments db post.id in
              (* Membership here is the DESTINATION's (it feeds the join
                 CTA), but every canonical-content moderation input — the
                 mod flag, the moderator badges, the ban list — stays
                 ORIGIN-scoped: destination standing grants no canonical
                 controls, so a destination top mod reads as a plain
                 viewer. *)
              let%lwt is_member_result = match user_id_opt with
                | Some uid -> Db.is_member db (int_of_string uid) destination.Db.id
                | None -> Lwt.return (Ok false) in
              let%lwt user_post_votes = get_current_user_votes db request in
              let%lwt user_comment_votes = get_current_user_comment_votes db request in
              let%lwt is_mod_res = match user_id_opt with
                | Some uid -> Db.is_moderator db (int_of_string uid) post.community_id
                | None -> Lwt.return_ok false in
              let%lwt mods_res = Db.get_community_moderators db post.community_id in
              let%lwt admin_usernames_res = Db.get_admin_usernames db in
              let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
              let%lwt banned_res = Db.community_get_banned_users db post.community_id in
              let banned_usernames = match banned_res with Ok bs -> List.map (fun (u : Db.user) -> u.username) bs | _ -> [] in
              let%lwt rail_communities = match user_id_opt with
                | Some uid -> (match%lwt Db.get_user_communities db (int_of_string uid) with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
                | None -> Lwt.return [] in
              (* The destination shell's own navigation data. *)
              let%lwt channels = match%lwt Db.get_channels_by_community db destination.Db.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [] in
              let%lwt sections = match%lwt Db.get_sections_with_stats db destination.Db.id with
                | Ok stats -> Lwt.return (List.map (fun ((s : Db.community_section), _, _) -> s) stats)
                | Error _ -> Lwt.return [] in
              (* Promoted-conversation provenance, viewer-scoped exactly as
                 on the origin page. The source community IS the post's own
                 (promotion never crosses communities), which this context
                 guarantees is public — so the same rule admits it here. *)
              let%lwt thread_source =
                match%lwt Db.get_thread_source db post.id with
                | Error _ | Ok (None, []) -> Lwt.return None
                | Ok (channel_opt, msgs) ->
                    (match channel_opt with
                     | None -> Lwt.return (Some (Pages.Ts_visible (None, msgs)))
                     | Some (cslug, cname, src_community_id) ->
                         if src_community_id = post.community_id then
                           Lwt.return (Some (Pages.Ts_visible (Some (cslug, cname), msgs)))
                         else
                           (match%lwt Db.get_community_by_id db src_community_id with
                            | Ok (Some src_community) ->
                                let%lwt src_ok = can_view_community db ~user_id:viewer_id ~is_admin src_community in
                                Lwt.return (Some (if src_ok then Pages.Ts_visible (Some (cslug, cname), msgs) else Pages.Ts_private))
                            | _ -> Lwt.return (Some Pages.Ts_private))) in
              (* The one comment-participation capability — the same SQL the
                 POST enforces. A failed probe hides the composer, never
                 errors the page. *)
              let%lwt can_comment =
                if viewer_id <= 0 then Lwt.return false
                else
                  match%lwt Shared_thread_reading.viewer_may_comment db ~user_id:viewer_id ~post_id:post.id with
                  | Ok can -> Lwt.return can
                  | Error _ -> Lwt.return false
              in
              let shared_context : Pages.shared_thread_page_context =
                { stc_origin_name = ctx.Shared_thread_reading.origin_community_name;
                  stc_section = ctx.Shared_thread_reading.destination_section } in
              match comments_result, is_member_result with
              | Ok comments, Ok is_member ->
                  let is_mod = match is_mod_res with Ok b -> b | _ -> false in
                  let mod_usernames = match mods_res with Ok ms -> List.map (fun (u : Db.user) -> u.username) ms | _ -> [] in
                  (* noindex always: the canonical <link> points at the
                     immutable origin URL and this page must never compete
                     with it in search engines (it stays followable — no
                     nofollow). No Share entry point here: the first MVP
                     permits requests only from the origin page. *)
                  Dream.html (Pages.thread_shell_page ?user:user_sess ~noindex:true ~can_share:false ~can_comment
                    ~shared_context ~is_member ~is_current_user_mod:is_mod
                    ~mod_usernames ~admin_usernames ~banned_usernames ~rail_communities ~channels ~sections
                    ~community:destination ?thread_source ~user_post_votes ~user_comment_votes ~post ~comments request)
              | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load thread data. Please try again later." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
            end
        | None ->
        (* Slice C: gate BEFORE the canonical 301 below — redirecting leaks the private
           community's slug + thread title in the Location header. Resolve visibility from the
           post's community_id and deny with the SAME 404 as a missing thread. Fail closed. *)
        let%lwt gate_ok =
          match%lwt Db.get_community_by_id db post.community_id with
          | Ok (Some community) -> can_view_community db ~user_id:viewer_id ~is_admin community
          | _ -> Lwt.return false
        in
        if not gate_ok then
          Dream.respond ~status:`Not_Found (Pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This thread does not exist or has been deleted." ~alert_type:"error" ~return_url:"/" request)
        else
        let canonical = Components.canonical_thread_path post.community_slug post.id post.title in
        let current_path = "/c/" ^ community_slug ^ "/t/" ^ thread_param in
        if current_path <> canonical then
          (* Wrong community slug, or wrong/missing descriptive slug → 301 to canonical. *)
          Dream.redirect ~status:`Moved_Permanently request canonical
        else begin
          let%lwt comments_result = Db.get_comments db post.id in
          let%lwt is_member_result = match user_id_opt with
            | Some uid -> Db.is_member db (int_of_string uid) post.community_id
            | None -> Lwt.return (Ok false) in
          let%lwt user_post_votes = get_current_user_votes db request in
          let%lwt user_comment_votes = get_current_user_comment_votes db request in
          let%lwt is_mod_res = match user_id_opt with
            | Some uid -> Db.is_moderator db (int_of_string uid) post.community_id
            | None -> Lwt.return_ok false in
          let%lwt mods_res = Db.get_community_moderators db post.community_id in
          let%lwt admin_usernames_res = Db.get_admin_usernames db in
          let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
          let%lwt banned_res = Db.community_get_banned_users db post.community_id in
          let banned_usernames = match banned_res with Ok bs -> List.map (fun (u : Db.user) -> u.username) bs | _ -> [] in
          let%lwt community_res = Db.get_community_by_slug db post.community_slug in
          let%lwt rail_communities = match user_id_opt with
            | Some uid -> (match%lwt Db.get_user_communities db (int_of_string uid) with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
            | None -> Lwt.return [] in
          let%lwt channels = match%lwt Db.get_channels_by_community db post.community_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [] in
          let%lwt sections = match%lwt Db.get_sections_with_stats db post.community_id with
            | Ok stats -> Lwt.return (List.map (fun ((s : Db.community_section), _, _) -> s) stats)
            | Error _ -> Lwt.return [] in
          (* Promoted-conversation provenance, viewer-scoped. The DB returns raw rows; the
             VIEWER-visibility decision happens here: the source conversation is shown iff
             this viewer may read the source channel's community (can_view_community — the
             same predicate that gates the channel page itself), otherwise only a neutral
             Ts_private notice renders. Indexability plays no role: it is SEO-only and the
             thread's own noindex already follows the forum rules (thread_noindex above).
             In practice the source community IS the thread's community (promotion never
             crosses communities), and this viewer already passed that gate — the
             cross-community branch is defensive and fails closed to Ts_private. *)
          let%lwt thread_source =
            match%lwt Db.get_thread_source db post.id with
            | Error _ | Ok (None, []) -> Lwt.return None
            | Ok (channel_opt, msgs) ->
                (match channel_opt with
                 | None -> Lwt.return (Some (Pages.Ts_visible (None, msgs)))
                 | Some (cslug, cname, src_community_id) ->
                     if src_community_id = post.community_id then
                       Lwt.return (Some (Pages.Ts_visible (Some (cslug, cname), msgs)))
                     else
                       (match%lwt Db.get_community_by_id db src_community_id with
                        | Ok (Some src_community) ->
                            let%lwt src_ok = can_view_community db ~user_id:viewer_id ~is_admin src_community in
                            Lwt.return (Some (if src_ok then Pages.Ts_visible (Some (cslug, cname), msgs) else Pages.Ts_private))
                        | _ -> Lwt.return (Some Pages.Ts_private))) in
          (* Fallback community kept for parity with view_post_handler; in practice the record exists. *)
          let community_for_page : Db.community = match community_res with
            | Ok (Some a) -> a
            | _ -> { id = post.community_id; slug = post.community_slug; name = post.community_slug;
                     description = None; rules = None; avatar_url = None; banner_url = None; allow_downvotes = true; sections_enabled = false; visibility = Db.Community_public; indexable = true;
                     is_network_community = false; onboarding_state = Db.Community_published; discoverable = true } in
          let%lwt noindex = thread_noindex db community_for_page post in
          (* Share entry point: decided in the shared-threads read model's SQL
             (author while member and unbanned, origin top_mod, or durable
             admin — false for a tombstoned post). Render gate only: the share
             route fully reauthorizes on GET, and a mere login never shows the
             action. Anonymous viewers skip the query; a failed probe hides
             the link rather than becoming an error path. *)
          let%lwt can_share =
            if viewer_id <= 0 then Lwt.return false
            else
              match%lwt
                Shared_thread_placement_read_model.viewer_may_share db
                  ~user_id:viewer_id ~session_global_admin:is_admin
                  ~post_id:post.id
              with
              | Ok can -> Lwt.return can
              | Error _ -> Lwt.return false
          in
          (* Composer gate: the one SQL participation capability POST
             /comments enforces (origin membership, or membership in a
             currently readable accepted destination — minus tombstone and
             every ban). A failed probe hides the composer, never errors. *)
          let%lwt can_comment =
            if viewer_id <= 0 then Lwt.return false
            else
              match%lwt Shared_thread_reading.viewer_may_comment db ~user_id:viewer_id ~post_id:post.id with
              | Ok can -> Lwt.return can
              | Error _ -> Lwt.return false
          in
          (* Closed creation-notice vocabulary (slice 4): only the two values
             the composer's own redirect writes render anything; every other
             ?shared= value is ignored. Resolved here, on the ORIGIN
             rendering only — the destination-context branch above never
             reads the parameter, so no destination page can display a
             creation outcome. *)
          let creation_notice = match Dream.query request "shared" with
            | Some "requested" -> Some Pages.Creation_share_requested
            | Some "failed" -> Some Pages.Creation_share_failed
            | _ -> None in
          (* Origin-side provenance for THIS canonical rendering only (the
             destination-context branch above never computes it): the post's
             currently publicly renderable accepted destinations, from the
             same batch read the global feed uses — one bounded query, and a
             failure degrades to no indicator, never an error page. *)
          let%lwt shared_with =
            match%lwt
              Shared_thread_reading.public_destinations_for_posts db
                ~post_ids:[ post.id ]
            with
            | Ok rows -> Lwt.return (List.map snd rows)
            | Error _ -> Lwt.return []
          in
          match comments_result, is_member_result with
          | Ok comments, Ok is_member ->
              let is_mod = match is_mod_res with Ok b -> b | _ -> false in
              let mod_usernames = match mods_res with Ok ms -> List.map (fun (u : Db.user) -> u.username) ms | _ -> [] in
              Dream.html (Pages.thread_shell_page ?user:user_sess ~noindex ~can_share ~can_comment ?creation_notice ~shared_with ~is_member ~is_current_user_mod:is_mod
                ~mod_usernames ~admin_usernames ~banned_usernames ~rail_communities ~channels ~sections
                ~community:community_for_page ?thread_source ~user_post_votes ~user_comment_votes ~post ~comments request)
          | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load thread data. Please try again later." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
        end)
  )

let delete_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in

      match%lwt Dream.form request with
      | `Ok form_data ->
          let post_id = try int_of_string (List.assoc_opt "post_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if post_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid post reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Dream.sql request (fun db ->
            (* Fetch post upfront — needed for both mod-check and admin immunity. *)
            let%lwt post_opt =
              match%lwt Db.get_post_by_id db post_id with
              | Ok p -> Lwt.return p | _ -> Lwt.return None
            in
            let%lwt is_mod =
              if is_admin then Lwt.return false
              else match post_opt with
                | Some post ->
                    (match%lwt Db.is_moderator db user_id post.community_id with
                    | Ok b -> Lwt.return b
                    | _ -> Lwt.return false)
                | None -> Lwt.return false
            in
            (* Admin immunity: mods cannot delete content authored by global admins. *)
            let%lwt blocked_by_immunity =
              if is_mod then match post_opt with
                | Some post ->
                    (match%lwt Db.is_user_admin db post.user_id with
                    | Ok true -> Lwt.return true | _ -> Lwt.return false)
                | None -> Lwt.return false
              else Lwt.return false
            in
            if blocked_by_immunity then
              Dream.respond ~status:`Forbidden "⛔ You cannot moderate an Admin."
            else
            (* Delete image from disk before nulling image_url in DB — prevents
               orphaned files that would still be served by the static file handler. *)
            let () =
              match post_opt with
              | Some post
                when is_admin || is_mod || post.user_id = user_id ->
                  (match post.image_url with
                  | Some image_url ->
                      (* Basename extraction avoids /static/uploads/... prefix mismatch
                         between the URL path stored in DB and the local filesystem. *)
                      let filename = Filename.basename image_url in
                      let physical_path = Filename.concat "static/uploads" filename in
                      Dream.log "Attempting to delete physical file: %s" physical_path;
                      (try Sys.remove physical_path
                       with Sys_error e -> Dream.log "Failed to delete file: %s" e)
                  | None -> ())
              | _ -> ()
            in
            let%lwt db_action =
              if is_admin || is_mod then Db.admin_delete_post db ~label:"[removed by admin]" post_id
              else Db.soft_delete_post db post_id user_id
            in
            match db_action with
            | Ok () ->
                let target = safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request target
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

(* Mod removal is a separate endpoint from /delete-post so that:
   (a) a reason is always required and stored, (b) the action is always
   attributed to a community moderator (not an admin shortcut), keeping
   mod_actions as a faithful community-level audit trail. *)
let mod_delete_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let slug = Dream.param request "slug" in
      let post_id = try int_of_string (Dream.param request "id") with _ -> 0 in
      if post_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Bad Request" ~message:"Invalid post ID." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
      else
      match%lwt Dream.form request with
      | `Ok form_data ->
          let reason = String.trim (List.assoc_opt "reason" form_data |> Option.value ~default:"") in
          if reason = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"A reason is required for moderation actions." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Error err ->
                Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
            | Ok None ->
                Dream.respond ~status:`Not_Found (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* Always query is_moderator even for admins — we need the distinction
                   to write the correct action_type in the audit log (admin_delete_post
                   vs delete_post), preventing admin spoofing via the community mod log. *)
                let is_admin = Dream.session_field request "is_admin" = Some "true" in
                let%lwt is_community_mod_res = Db.is_moderator db user_id community.id in
                let is_community_mod = match is_community_mod_res with Ok true -> true | _ -> false in
                if not (is_admin || is_community_mod) then
                    Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Forbidden" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                else
                    (* Ownership gate: load the post and prove it belongs to the route
                       community BEFORE any side effect (disk, DB, modlog, notification).
                       A missing post and a post from another community get the same
                       neutral 404 — the response must not reveal that the numeric id
                       exists elsewhere, and admins on this community-scoped route obey
                       the same route-to-target relationship. *)
                    let not_found_here () =
                      Dream.respond ~status:`Not_Found (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"This post does not exist in this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    in
                    (match%lwt Db.get_post_by_id db post_id with
                    | Error err ->
                        Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    | Ok None -> not_found_here ()
                    | Ok (Some post) when post.community_id <> community.id -> not_found_here ()
                    | Ok (Some post) ->
                        (* The mutation re-proves the scope: id AND community_id, with
                           RETURNING as the match evidence. Ok false means the post
                           vanished or moved since the read above — still a neutral 404,
                           still zero side effects. *)
                        (match%lwt Db.mod_delete_post db ~community_id:community.id post_id with
                        | Error err ->
                            Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                        | Ok false -> not_found_here ()
                        | Ok true ->
                            (* Disk cleanup only after the scoped mutation confirmed the
                               target matched this community. *)
                            let () = match post.image_url with
                              | Some image_url ->
                                  let filename = Filename.basename image_url in
                                  let physical_path = Filename.concat "static/uploads" filename in
                                  Dream.log "Attempting to delete physical file: %s" physical_path;
                                  (try Sys.remove physical_path
                                   with Sys_error e -> Dream.log "Failed to delete file: %s" e)
                              | None -> ()
                            in
                            (* Admin acting without mod role: flag action_type and prefix reason
                               so the public mod_actions log explicitly shows "Admin Intervention". *)
                            let is_admin_override = is_admin && not is_community_mod in
                            let action_type = if is_admin_override then "admin_delete_post" else "delete_post" in
                            let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                            let%lwt _ = Db.log_mod_action db community.id user_id action_type (Some post_id) logged_reason in
                            (* Notify the author from the row validated above — no re-query
                               of the tombstoned row. *)
                            let%lwt _ =
                              let msg = "Your post was removed by a moderator. Reason: " ^ reason in
                              Db.create_notif db post.user_id (Some post_id) "mod_action" msg
                            in
                            Dream.redirect request ("/c/" ^ slug)))
          )
      | _ ->
          Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)

(* === COMMENT === *)

(* Mirrors mod_delete_post_handler exactly. slug + comment_id come from URL params;
   we must query the community to resolve community.id for the mod_actions log. *)
let mod_delete_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let slug = Dream.param request "slug" in
      let comment_id = try int_of_string (Dream.param request "id") with _ -> 0 in
      if comment_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Bad Request" ~message:"Invalid comment ID." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
      else
      match%lwt Dream.form request with
      | `Ok form_data ->
          let reason = String.trim (List.assoc_opt "reason" form_data |> Option.value ~default:"") in
          if reason = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"A reason is required for moderation actions." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Error err ->
                Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
            | Ok None ->
                Dream.respond ~status:`Not_Found (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* Always query is_moderator even for admins — we need the distinction
                   to write the correct action_type in the audit log (admin_delete_comment
                   vs delete_comment), preventing admin spoofing via the community mod log. *)
                let is_admin = Dream.session_field request "is_admin" = Some "true" in
                let%lwt is_community_mod_res = Db.is_moderator db user_id community.id in
                let is_community_mod = match is_community_mod_res with Ok true -> true | _ -> false in
                if not (is_admin || is_community_mod) then
                    Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Forbidden" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                else
                    (* Ownership gate: resolve comment -> post -> community BEFORE the
                       mutation. A missing comment and a comment under another
                       community's post get the same neutral 404 — the response must
                       not reveal that the numeric id exists elsewhere.
                       get_comment_post_id collapses "no row" and DB failure into one
                       Error; both are safe to treat as not-found because nothing has
                       been mutated yet. *)
                    let not_found_here () =
                      Dream.respond ~status:`Not_Found (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"This comment does not exist in this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    in
                    let%lwt in_this_community =
                      match%lwt Db.get_comment_post_id db comment_id with
                      | Error _ -> Lwt.return false
                      | Ok pid ->
                          (match%lwt Db.get_post_by_id db pid with
                          | Ok (Some post) -> Lwt.return (post.community_id = community.id)
                          | _ -> Lwt.return false)
                    in
                    if not in_this_community then not_found_here ()
                    else
                    (* Author read while the row is intact (the tombstone keeps user_id,
                       but the notification must never depend on that detail). *)
                    let%lwt author_res = Db.get_comment_owner db comment_id in
                    (* The mutation re-proves comment -> post -> community atomically;
                       RETURNING c.post_id is both the match evidence and the redirect
                       target. Ok None means the comment vanished since the check above —
                       still a neutral 404, still zero side effects. *)
                    (match%lwt Db.mod_delete_comment db ~community_id:community.id comment_id with
                    | Error err ->
                        Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    | Ok None -> not_found_here ()
                    | Ok (Some post_id) ->
                        (* Admin acting without mod role: flag action_type and prefix reason
                           so the public mod_actions log explicitly shows "Admin Intervention". *)
                        let is_admin_override = is_admin && not is_community_mod in
                        let action_type = if is_admin_override then "admin_delete_comment" else "delete_comment" in
                        let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                        let%lwt _ = Db.log_mod_action db community.id user_id action_type (Some comment_id) logged_reason in
                        let%lwt _ = match author_res with
                          | Ok author_id ->
                              let msg = "Your comment was removed by a moderator. Reason: " ^ reason in
                              Db.create_notif db author_id (Some post_id) "mod_action" msg
                          | Error _ -> Lwt.return (Ok ())
                        in
                        Dream.redirect request ("/p/" ^ string_of_int post_id))
          )
      | _ ->
          Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)

(* === REPORTS === *)

(* Slice B: report creation for posts & comments (chat messages out of scope). The form
   is no-JS SSR; the POST handler — not the render-time link gate — is the security
   boundary. Both handlers gate on can_view_community first (an outsider to a private
   community gets the canonical community_not_found 404 before any ban check or target
   resolution), then re-resolve the target from the trusted :slug + hidden type/id,
   verify it belongs to this community, re-run the global/community ban gates (active
   sessions outlive a ban, mirroring create_comment_handler), and disallow self-reports. *)

(* Shared post/comment target resolution. Returns the target's author id and the canonical
   return URL, or None when the target is missing / hard-deleted / not in this community.
   chat_message is rejected before this is ever called, but is handled for exhaustiveness. *)
let resolve_report_target db community_id (target_type : Db.report_target) target_id =
  match target_type with
  | Db.Report_post ->
      (match%lwt Db.get_post_by_id db target_id with
       | Ok (Some post) when post.community_id = community_id ->
           Lwt.return (Ok (Some (post.user_id, post.title,
             Components.canonical_thread_path post.community_slug post.id post.title)))
       | Ok _ -> Lwt.return (Ok None)
       | Error e -> Lwt.return (Error e))
  | Db.Report_comment ->
      (match%lwt Db.get_comment_report_target db target_id with
       | Ok (Some crt) when crt.crt_community_id = community_id ->
           Lwt.return (Ok (Some (crt.crt_author_user_id, crt.crt_content,
             Components.canonical_thread_path crt.crt_community_slug crt.crt_post_id crt.crt_post_title)))
       | Ok _ -> Lwt.return (Ok None)
       | Error e -> Lwt.return (Error e))
  | Db.Report_chat_message -> Lwt.return (Ok None)

let report_form_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let target_type_opt = match Dream.query request "type" with
        | Some s -> Db.report_target_of_string s | None -> None in
      let target_id = match Dream.query request "id" with
        | Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
      let bad () =
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Invalid Report"
          ~message:"That report link is not valid." ~alert_type:"error" ~return_url:("/c/" ^ slug) request) in
      (match target_type_opt with
       | Some ((Db.Report_post | Db.Report_comment) as target_type) when target_id > 0 ->
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db slug with
             | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
             | Ok None -> community_not_found ?user request
             | Ok (Some community) ->
                 (* Private-community read gate BEFORE any ban check or target
                    resolution: an outsider must get the same 404 as a missing
                    community — a ban page or target-dependent response here
                    would confirm the community (or target) exists. *)
                 let is_admin = Dream.session_field request "is_admin" = Some "true" in
                 let%lwt authorized = can_view_community db ~user_id ~is_admin community in
                 if not authorized then community_not_found ?user request
                 else
                 let%lwt is_gb = match%lwt Db.is_globally_banned db user_id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                 if is_gb then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request)
                 else
                 let%lwt is_cb = match%lwt Db.community_is_banned db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                 if is_cb then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Banned from Community" ~message:"You are banned from this community." ~alert_type:"error" ~return_url:("/c/" ^ community.slug) request)
                 else
                 (match%lwt resolve_report_target db community.id target_type target_id with
                  | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                  | Ok None -> bad ()
                  | Ok (Some (author_id, target_title, return_url)) ->
                      if author_id = user_id then
                        Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Cannot Report"
                          ~message:"You cannot report your own content. You can delete it instead."
                          ~alert_type:"error" ~return_url request)
                      else
                        (* Launch-chrome data (pass 14D), loaded only after every
                           gate above passed — a banned viewer, a foreign or
                           deleted target and a self-report never touch sections,
                           channels or the viewer's membership. Each degrades to
                           an empty list on error rather than blocking the form.
                           can_manage mirrors modlog's can_access_settings gate
                           (admin || moderator) and only picks the sidebar
                           Settings visibility; the settings handler re-checks. *)
                        let%lwt can_manage =
                          if is_admin then Lwt.return true
                          else (match%lwt Db.is_moderator db user_id community.id with
                            | Ok b -> Lwt.return b
                            | _ -> Lwt.return false)
                        in
                        let%lwt sections =
                          if community.sections_enabled then
                            (match%lwt Db.get_sections_by_community db community.id with
                             | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
                          else Lwt.return []
                        in
                        let%lwt channels =
                          match%lwt Db.get_channels_by_community db community.id with
                          | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
                        in
                        let%lwt rail_communities =
                          match%lwt Db.get_user_communities db user_id with
                          | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
                        in
                        Dream.html (Pages.report_form_page ?user ~rail_communities ~channels ~sections
                          ~can_manage ~community ~target_type ~target_id ~target_title ~return_url request)))
       | _ -> bad ())

let create_report_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get n = Option.value ~default:"" (List.assoc_opt n form_data) in
          let target_type_opt = Db.report_target_of_string (get "target_type") in
          let reason_opt = Db.report_reason_of_string (get "reason") in
          let target_id = try int_of_string (get "target_id") with _ -> 0 in
          (* Cap details server-side; the form's maxlength is advisory only. *)
          let details = match String.trim (get "details") with
            | "" -> None
            | d -> Some (if String.length d > 1000 then String.sub d 0 1000 else d) in
          (match target_type_opt, reason_opt with
           | Some ((Db.Report_post | Db.Report_comment) as target_type), Some reason when target_id > 0 ->
               Dream.sql request (fun db ->
                 match%lwt Db.get_community_by_slug db slug with
                 | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                 | Ok None -> community_not_found ?user request
                 | Ok (Some community) ->
                     (* Same private-community read gate as the GET form, re-proved
                        from server-side state — the POST must not trust that the
                        viewer ever loaded the form. Denial happens before target
                        resolution so valid and invalid private target ids are
                        indistinguishable and no report row is ever inserted. *)
                     let is_admin = Dream.session_field request "is_admin" = Some "true" in
                     let%lwt authorized = can_view_community db ~user_id ~is_admin community in
                     if not authorized then community_not_found ?user request
                     else
                     let%lwt is_gb = match%lwt Db.is_globally_banned db user_id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                     if is_gb then
                       Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request)
                     else
                     let%lwt is_cb = match%lwt Db.community_is_banned db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                     if is_cb then
                       Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Banned from Community" ~message:"You are banned from this community." ~alert_type:"error" ~return_url:("/c/" ^ community.slug) request)
                     else
                     (* Re-resolve from the trusted slug — never trust a client-supplied community_id. *)
                     (match%lwt resolve_report_target db community.id target_type target_id with
                      | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ community.slug) request)
                      | Ok None ->
                          Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That content is no longer available." ~alert_type:"error" ~return_url:("/c/" ^ community.slug) request)
                      | Ok (Some (author_id, _title, return_url)) ->
                          if author_id = user_id then
                            Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Cannot Report"
                              ~message:"You cannot report your own content. You can delete it instead."
                              ~alert_type:"error" ~return_url request)
                          else
                            (match%lwt Db.create_report db ~community_id:community.id ~reporter_user_id:user_id
                                       ~target_type ~target_id:(Int64.of_int target_id)
                                       ~target_author_user_id:(Some author_id) ~reason ~details with
                             | Ok (`Created _) ->
                                 Dream.html (Pages.msg_page ?user ~title:"Report submitted"
                                   ~message:"Thanks — a moderator will review this." ~alert_type:"success" ~return_url request)
                             | Ok `Duplicate ->
                                 Dream.html (Pages.msg_page ?user ~title:"Already reported"
                                   ~message:"You've already reported this item. A moderator will review it." ~alert_type:"info" ~return_url request)
                             | Error e ->
                                 Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url request))))
           | _ ->
               Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Invalid Report" ~message:"That report could not be processed." ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
      | _ ->
          Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)

(* Read-only community mod queue. PRIVATE: gated to M/TM/A with the exact
   community_settings_handler idiom (admin bypass, else is_moderator) — never the public
   modlog_handler shape, since this exposes reporter identities. No mutation. *)
let reports_queue_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (* Default open; an unknown ?status= falls back to open (least surprising). *)
      let status = match Dream.query request "status" with
        | Some s -> (match Db.report_status_of_string s with Some st -> st | None -> Db.Report_open)
        | None -> Db.Report_open in
      Dream.sql request (fun db ->
        match%lwt Db.get_community_by_slug db slug with
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
        | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
        | Ok (Some community) ->
            let%lwt is_authorized =
              if is_admin then Lwt.return true
              else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false)
            in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"You must be a moderator to view reports." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
            else
              (* Launch-chrome data (pass 14B), loaded only after the M/TM/A
                 authorization above — a denied request never touches sections,
                 channels or the viewer's membership. Each degrades to an empty
                 list on error rather than blocking the queue. Same plumbing as
                 the sibling converted management routes. *)
              let%lwt sections =
                if community.sections_enabled then
                  (match%lwt Db.get_sections_by_community db community.id with
                   | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
                else Lwt.return []
              in
              let%lwt channels =
                match%lwt Db.get_channels_by_community db community.id with
                | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
              in
              let%lwt rail_communities =
                match%lwt Db.get_user_communities db user_id with
                | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
              in
              (* Read-only role lookup for the shared settings shell's nav:
                 display-gates the top-mod/admin entries (Network group,
                 Manage moderators) exactly like the settings hub. Every
                 linked route still reauthorizes — this changes no
                 permission. *)
              let%lwt is_top_mod =
                match%lwt Db.get_moderator_role db user_id community.id with
                | Ok (Some "top_mod") -> Lwt.return true
                | _ -> Lwt.return false
              in
              (match%lwt Db.get_reports_by_community db community.id ~status with
               | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
               | Ok reports ->
                   (* Bounded per-row context+preview lookup (read-only MVP): reuse
                      resolve_report_target so deleted / foreign / chat targets degrade to no
                      preview. Capped so a flooded queue can't fan out into an unbounded N+1. *)
                   let preview_cap = 100 in
                   let rec take n = function
                     | [] -> [] | _ when n <= 0 -> [] | x :: xs -> x :: take (n - 1) xs in
                   let%lwt previews =
                     Lwt_list.filter_map_s (fun (r : Db.report_row) ->
                       match%lwt resolve_report_target db community.id r.target_type (Int64.to_int r.target_id) with
                       | Ok (Some (_author, preview, url)) -> Lwt.return (Some (r.id, (url, preview)))
                       | _ -> Lwt.return None)
                       (take preview_cap reports)
                   in
                   Dream.html (Pages.reports_queue_page ?user ~rail_communities ~is_admin ~is_top_mod ~channels ~sections ~community ~status ~reports ~previews request)))

(* Slice E: resolve an open report (dismiss / mark action-taken) and write a modlog entry.
   Shared by dismiss_report_handler and action_report_handler. Both gate exactly like the
   read-only queue (M/TM/A), resolve the community from the trusted :slug (never a form field),
   verify the report belongs to THIS community, and only mutate while status=open — re-resolving
   an already-closed report is a friendly redirect, not a double-log or a 500.

   The modlog target_id is the report_id (always INTEGER), never report.target_id, which may be
   a chat-message BIGINT that mod_actions.target_id (INTEGER) cannot hold. *)
let resolve_report_action request ~new_status ~action_kind ~action_type ~default_reason =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let report_id = try int_of_string (Dream.param request "report_id") with _ -> 0 in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let reports_url = "/c/" ^ slug ^ "/reports" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* Cap the optional note so a hostile/huge paste can't bloat the row or the modlog. *)
           let note =
             match List.assoc_opt "resolution_note" form_data with
             | Some s ->
                 let t = String.trim s in
                 if t = "" then None
                 else Some (if String.length t > 1000 then String.sub t 0 1000 else t)
             | None -> None
           in
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db slug with
             | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
             | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
             | Ok (Some community) ->
                 let%lwt is_authorized =
                   if is_admin then Lwt.return true
                   else (match%lwt Db.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false)
                 in
                 if not is_authorized then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"You must be a moderator to resolve reports." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                 else
                   (match%lwt Db.get_report_by_id db report_id with
                    | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:reports_url request)
                    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That report does not exist." ~alert_type:"error" ~return_url:reports_url request)
                    | Ok (Some report) ->
                        (* Bind to the slug's community: a report_id from another community must not
                           be mutable under this community's mod authority. *)
                        if report.community_id <> community.id then
                          Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"That report does not belong to this community." ~alert_type:"error" ~return_url:reports_url request)
                        else if report.status <> Db.Report_open then
                          (* Already resolved (possibly by another mod): no mutation, bounce to the
                             tab it now lives in. *)
                          Dream.redirect request (reports_url ^ "?status=" ^ Db.report_status_to_string report.status)
                        else
                          (match%lwt Db.resolve_report db report_id ~resolver_user_id:user_id ~status:new_status ~action_kind ~note with
                           | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:reports_url request)
                           | Ok () ->
                               let reason = match note with Some n -> n | None -> default_reason report_id in
                               let%lwt _ = Db.log_mod_action db community.id user_id action_type (Some report_id) reason in
                               Dream.redirect request (reports_url ^ "?status=" ^ Db.report_status_to_string new_status))))
       | _ ->
           Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:reports_url request))

(* Dismiss: report had no actionable merit. No content action, so action_kind stays None. *)
let dismiss_report_handler request =
  resolve_report_action request
    ~new_status:Db.Report_dismissed
    ~action_kind:None
    ~action_type:"dismiss_report"
    ~default_reason:(fun id -> Printf.sprintf "Dismissed report #%d" id)

(* Mark action taken: the mod acted on this report. This slice does NOT remove content or ban
   anyone, so the recorded action_kind is Report_other_action — never removed_content/banned_author,
   which would claim an action that did not happen. Content removal is a later slice. *)
let action_report_handler request =
  resolve_report_action request
    ~new_status:Db.Report_action_taken
    ~action_kind:(Some Db.Report_other_action)
    ~action_type:"resolve_report"
    ~default_reason:(fun id -> Printf.sprintf "Marked report #%d as action taken" id)

(* Push notifications are fan-out on write: one notification per comment, sent to
   either post owner or parent comment owner. Best-effort — failure is silently
   ignored so a notification DB error never blocks the comment submission. *)
let create_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let username = Option.value (Dream.session_field request "username") ~default:"Someone" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let content = String.trim (List.assoc_opt "content" form_data |> Option.value ~default:"") in
          let post_id_str = List.assoc_opt "post_id" form_data |> Option.value ~default:"" in
          let parent_id_opt = match List.assoc_opt "parent_id" form_data with
            | Some p when p <> "" -> (try Some (int_of_string p) with _ -> None)
            | _ -> None
          in

          if content = "" then
            Dream.html (Pages.msg_page ~user:username ~title:"Validation Error" ~message:"Comment cannot be empty." ~alert_type:"error" ~return_url:"/" request)
          else if String.length content > 10000 then
            Dream.html (Pages.msg_page ~user:username ~title:"Validation Error" ~message:"Comment cannot exceed 10,000 characters." ~alert_type:"error" ~return_url:"/" request)
          else

          let post_id = try int_of_string post_id_str with _ -> 0 in
          if post_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ~user:username ~title:"Form Error" ~message:"Invalid post reference." ~alert_type:"error" ~return_url:"/" request)
          else

          with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Global ban gate: same reasoning as create_post_handler — active sessions
               survive a ban until the next login, so we must check on every write. *)
            let%lwt is_gb =
              match%lwt Db.is_globally_banned db user_id with
              | Ok b -> Lwt.return b | Error _ -> Lwt.return false
            in
            if is_gb then
              Dream.respond ~status:`Forbidden (Pages.msg_page ~user:username ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request)
            else
            (* Lookup post to get community_id for the ban check — avoids adding a hidden
               form field that a client could forge to bypass their own community ban. *)
            match%lwt Db.get_post_by_id db post_id with
            | Ok (Some post) ->
                let is_tombstone = match post.content with
                  | Some "[deleted]" | Some "[removed by admin]" | Some "[removed by moderator]" -> true
                  | _ -> false
                in
                if is_tombstone then
                  Dream.respond ~status:`Forbidden "⛔ You cannot comment on a deleted post."
                else
                (match%lwt Db.community_is_banned db user_id post.community_id with
                | Ok true ->
                    Dream.respond ~status:`Forbidden (Pages.msg_page ~user:username ~title:"Banned from Community" ~message:"You are banned from commenting in this community." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                | _ ->
                    (* Shared Threads: the server-side participation rule.
                       One SQL capability (the same one that gates the
                       composer) requires a CURRENT path onto the canonical
                       discussion — origin membership, or membership in an
                       accepted destination whose placement is currently
                       readable, with no ban there. This closes the old
                       gap where a manual POST needed no membership at all:
                       no hidden field, route, or composer sighting grants
                       anything — every qualifying community is derived
                       from the post and its placements. The specific
                       global-ban / tombstone / origin-ban responses above
                       keep their exact observable behavior; this gate only
                       adds the membership requirement after them. Fails
                       closed on a storage error. *)
                    (match%lwt Shared_thread_reading.viewer_may_comment db ~user_id ~post_id with
                    | Error Shared_thread_reading.Storage_error ->
                        Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ~user:username ~title:"Error" ~message:"Something went wrong on our side. Please try again." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                    | Ok false ->
                        Dream.respond ~status:`Forbidden (Pages.msg_page ~user:username ~title:"Membership required" ~message:"Only current members of a community this thread belongs to can comment." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                    | Ok true ->
                    (match%lwt Db.create_comment db content post_id user_id parent_id_opt with
                    | Ok `Invalid_parent ->
                        (* The submitted parent does not exist, or belongs to a
                           different post — and therefore possibly a different
                           community, possibly a private one. The store refused
                           inside the INSERT, so there is no comment row, no
                           notification, no karma change, no comment counter
                           and no activity bump to undo. The response is a
                           generic client error: it names no post, comment or
                           community, so it cannot be used to probe which
                           parent ids exist or where they live. *)
                        Dream.respond ~status:`Bad_Request (Pages.msg_page ~user:username ~title:"Form Error" ~message:"Invalid reply reference." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                    | Ok (`Created comment_id) ->
                        (* comment_id is the real inserted id from the step-3
                           INSERT ... RETURNING. Length/mention flags only —
                           never the comment text. *)
                        record (fun () ->
                            Analytics.capture_if_consented request
                              ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                              (Analytics.Forum_comment_created
                                 {
                                   user_id;
                                   community_id = post.community_id;
                                   post_id;
                                   comment_id;
                                   parent_comment_id = parent_id_opt;
                                   content_length = String.length content;
                                   has_mention = extract_mentions content <> [];
                                 }));
                        let%lwt _ = Db.increment_local_comment_count db user_id post.community_id in
                        (* Bump last_activity_at so the post rises in "active" sorted feeds. *)
                        let%lwt _ = Db.touch_post_last_activity db post_id in
                        let%lwt target_user = match parent_id_opt with
                          | Some cid -> Db.get_comment_owner db cid
                          | None -> Db.get_post_owner db post_id
                        in
                        let%lwt _ = match target_user with
                          | Ok target_id when target_id <> user_id ->
                              let msg = if parent_id_opt = None then username ^ " replied to your post." else username ^ " replied to your comment." in
                              Db.create_notif db target_id (Some post_id) "comment_reply" msg
                          | _ -> Lwt.return (Ok ())
                        in
                        (* Fan-out @mention notifications for comment body — best-effort, skips self. *)
                        let%lwt () = Lwt_list.iter_s (fun uname ->
                          match%lwt Db.get_user_by_username db uname with
                          | Ok (Some mentioned) when mentioned.id <> user_id ->
                              let msg = username ^ " mentioned you in a comment." in
                              let%lwt _ = Db.create_notif db mentioned.id (Some post_id) "mention" msg in
                              Lwt.return_unit
                          | _ -> Lwt.return_unit
                        ) (extract_mentions content) in
                        (* Redirect: preserve the DESTINATION context when a
                           destination-context composer posted this comment
                           AND that context is still readable by this
                           viewer. The closed context_community field only
                           names a community — the server re-resolves the
                           placement and re-authorizes through the
                           destination's existing access rule, and the
                           Location is rebuilt from the SERVER-loaded
                           community record and canonical post, never from
                           the submitted value. Anything stale, forged, or
                           unreadable falls back to the existing canonical
                           /p/:id redirect. *)
                        let%lwt redirect_target =
                          match List.assoc_opt "context_community" form_data with
                          | Some slug when slug <> "" && slug <> post.community_slug -> (
                              match%lwt Shared_thread_reading.resolve_destination_context db ~post_id ~destination_slug:slug with
                              | Ok (Some ctx) -> (
                                  match%lwt Db.get_community_by_id db ctx.Shared_thread_reading.destination_community_id with
                                  | Ok (Some destination) ->
                                      let is_admin = Dream.session_field request "is_admin" = Some "true" in
                                      let%lwt viewable = can_view_community db ~user_id ~is_admin destination in
                                      if viewable then
                                        Lwt.return (Components.canonical_thread_path destination.Db.slug post_id post.title)
                                      else Lwt.return ("/p/" ^ string_of_int post_id)
                                  | _ -> Lwt.return ("/p/" ^ string_of_int post_id))
                              | _ -> Lwt.return ("/p/" ^ string_of_int post_id))
                          | _ -> Lwt.return ("/p/" ^ string_of_int post_id)
                        in
                        Dream.redirect request redirect_target
                    | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request))))
            | Ok None -> Dream.html (Pages.msg_page ~user:username ~title:"Post Not Found" ~message:"The post you tried to comment on could not be found." ~alert_type:"error" ~return_url:"/" request)
            | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          ))
      | _ -> Dream.html (Pages.msg_page ~user:username ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

(* Authorization for the general /delete-comment endpoint. Pure and deliberately
   blind to any community id: the old handler trusted a hidden community_id form
   field for its moderator check, which let a moderator of community A delete a
   comment in community B by pairing A's id with B's comment id. Community
   moderation now lives exclusively on /c/:slug/comments/:id/mod_delete, which
   requires a reason and writes the public modlog — so this endpoint is
   author-only for non-admins, and the decision needs nothing but the session
   role and the server-resolved comment owner. *)
module Comment_delete = struct
  type decision = Admin_delete | Author_delete | Forbidden

  let decide ~is_admin ~requester_id ~owner_id =
    if is_admin then Admin_delete
    else if requester_id = owner_id then Author_delete
    else Forbidden
end

let delete_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let comment_id = try int_of_string (List.assoc_opt "comment_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if comment_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid comment reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Dream.sql request (fun db ->
            (* Resolve the target server-side: comment -> owner and parent post.
               A missing comment and a comment whose parent post is gone get the
               same neutral 404 with no mutation. *)
            let not_found () =
              Dream.respond ~status:`Not_Found (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"This comment does not exist." ~alert_type:"error" ~return_url:"/" request)
            in
            let%lwt target =
              match%lwt Db.get_comment_owner db comment_id with
              | Error _ -> Lwt.return None
              | Ok owner_id ->
                  (match%lwt Db.get_comment_post_id db comment_id with
                  | Error _ -> Lwt.return None
                  | Ok pid ->
                      (match%lwt Db.get_post_by_id db pid with
                      | Ok (Some _) -> Lwt.return (Some (owner_id, pid))
                      | _ -> Lwt.return None))
            in
            match target with
            | None -> not_found ()
            | Some (owner_id, post_id) ->
                let redirect_target = "/p/" ^ string_of_int post_id in
                (match Comment_delete.decide ~is_admin ~requester_id:user_id ~owner_id with
                | Comment_delete.Forbidden ->
                    (* Moderators included: community removal must go through the
                       mod_delete flow (required reason, public modlog). *)
                    Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Forbidden" ~message:"You can only delete your own comments. Community moderation goes through the Mod Remove flow." ~alert_type:"error" ~return_url:redirect_target request)
                | Comment_delete.Admin_delete ->
                    (match%lwt Db.admin_delete_comment db ~label:"[removed by admin]" comment_id with
                    | Ok () -> Dream.redirect request redirect_target
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:redirect_target request))
                | Comment_delete.Author_delete ->
                    (* The SQL is also ownership-scoped (id AND user_id), so even a
                       race with an ownership change cannot delete someone else's
                       comment. *)
                    (match%lwt Db.soft_delete_comment db comment_id user_id with
                    | Ok () -> Dream.redirect request redirect_target
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:redirect_target request)))
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

(* === VOTING === *)

(* direction=0 removes the vote; +1/-1 upserts. The DB uses ON CONFLICT DO UPDATE,
   making this idempotent — double-clicks and network retries are safe. *)
let vote_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let post_id = try int_of_string (List.assoc_opt "post_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let direction = try int_of_string (List.assoc_opt "direction" form_data |> Option.value ~default:"") with _ -> 99 in
          (* Clamp direction: only -1, 0, +1 are valid — reject crafted submissions silently. *)
          if post_id = 0 || not (direction = -1 || direction = 0 || direction = 1) then
            Dream.respond ~status:`Bad_Request "Invalid vote parameters."
          else
          Dream.sql request (fun db ->
            (* Guard downvote at the handler boundary — community may have disabled them. *)
            let%lwt downvotes_ok =
              if direction = -1 then Db.get_allows_downvotes_for_post db post_id
              else Lwt.return (Ok true)
            in
            match downvotes_ok with
            | Error _ -> Dream.respond ~status:`Internal_Server_Error "DB Error: could not check community settings."
            | Ok false -> Dream.respond ~status:`Forbidden "Downvotes are disabled in this community."
            | Ok true ->
            let%lwt db_action =
              if direction = 0 then Db.remove_post_vote db user_id post_id
              else Db.vote_post db user_id post_id direction
            in

            match db_action with
            | Ok () ->
                let referer = safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."

(* Same idempotent upsert semantics as vote_handler; kept separate to avoid a
   polymorphic action field that would couple post and comment vote paths. *)
let vote_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let comment_id = try int_of_string (List.assoc_opt "comment_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let direction = try int_of_string (List.assoc_opt "direction" form_data |> Option.value ~default:"") with _ -> 99 in
          (* Same direction guard as vote_handler. *)
          if comment_id = 0 || not (direction = -1 || direction = 0 || direction = 1) then
            Dream.respond ~status:`Bad_Request "Invalid vote parameters."
          else
          Dream.sql request (fun db ->
            let%lwt downvotes_ok =
              if direction = -1 then Db.get_allows_downvotes_for_comment db comment_id
              else Lwt.return (Ok true)
            in
            match downvotes_ok with
            | Error _ -> Dream.respond ~status:`Internal_Server_Error "DB Error: could not check community settings."
            | Ok false -> Dream.respond ~status:`Forbidden "Downvotes are disabled in this community."
            | Ok true ->
            let%lwt db_action =
              if direction = 0 then Db.remove_comment_vote db user_id comment_id
              else Db.vote_comment db user_id comment_id direction
            in

            match db_action with
            | Ok () ->
                let referer = safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."

(* Top Mod only — flips allow_downvotes; guards via get_moderator_role to prevent
   plain mods or non-members from toggling a community-wide setting. *)
let toggle_downvotes_handler request =
  let slug = Dream.param request "slug" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let new_val = List.assoc_opt "allow_downvotes" form_data = Some "true" in
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found "Community not found."
            | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
            | Ok (Some community) ->
                let%lwt role_res = Db.get_moderator_role db user_id community.id in
                let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                if not (is_top_mod || is_admin) then
                  Dream.respond ~status:`Forbidden "Only the Top Moderator can change this setting."
                else
                  match%lwt Db.toggle_community_downvotes db community.id new_val with
                  | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=moderation")
                  | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."

(* Slice E: visibility + indexability writes from /c/:slug/settings.
   Authorization is STRICTER than the settings page itself: visibility and discovery control who
   can read a private community, so only Top Mods and global admins (TM/A) may change them — a
   regular mod hitting these POSTs directly is rejected `Forbidden, mirroring toggle_downvotes
   (top_mod || is_admin). These never change read gates or noindex behavior; they only flip the
   Slice B columns the gate/resolver already consult. *)

(* Extracted so the settings flow's lifecycle decision is unit-testable without
   Dream/DB plumbing: the handler consults exactly this, on the authoritative
   record it just loaded, before any write. The rule itself lives in
   Network_communities — this only maps the record's fields onto it and picks
   the user-facing rejection copy. *)
let visibility_update_rejection (community : Db.community) ~requested_visibility =
  if
    Network_communities.visibility_change_allowed
      ~is_network_community:community.is_network_community
      ~onboarding_state:community.onboarding_state
      ~requested_visibility
  then None
  else
    Some
      "Published network communities must remain public. Private channels and \
       sections may still be used."

let update_community_visibility_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* Closed-variant parse: anything other than public/private is a validation error,
              not a 500. _of_string returns None for off-enum input. *)
           (match Db.community_visibility_of_string (Option.value ~default:"" (List.assoc_opt "visibility" form_data)) with
            | None ->
                Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Invalid setting" ~message:"Visibility must be either public or private." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
            | Some visibility ->
                with_analytics_after_sql (fun record ->
                Dream.sql request (fun db ->
                  match%lwt Db.get_community_by_slug db slug with
                  | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
                  | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
                  | Ok (Some community) when is_network_setup_draft community ->
                      (* A setup draft's visibility is not an independent
                         switch: publication sets visibility, indexability,
                         discoverability, and onboarding state together. This
                         route must never publish one, and it never reveals
                         that the community exists in that state — the same
                         generic 404 a missing community gets, before any
                         authorization branch or write. *)
                      Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
                  | Ok (Some community) ->
                      let%lwt role_res = Db.get_moderator_role db user_id community.id in
                      let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                      if not (is_top_mod || is_admin) then
                        Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change visibility." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                      else
                        match visibility_update_rejection community ~requested_visibility:visibility with
                        | Some message ->
                            (* Server-side lifecycle gate: a forged POST must not
                               reach the update. `Conflict, not a redirect — the
                               transition is refused, never silently dropped. *)
                            Dream.respond ~status:`Conflict (Pages.msg_page ?user ~title:"Visibility unavailable" ~message ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                        | None ->
                        match%lwt Db.update_community_visibility_and_enqueue_group_cleanup db community.id visibility with
                        | Ok (Some updated, cleanup_job) ->
                            (* Visibility is a closed group property: refresh
                               the group profile from the UPDATE ... RETURNING
                               record so PostHog never holds a stale value
                               after a successful change (§5.3). On a
                               ->private transition the SAME committed
                               transaction also enqueued the durable §13
                               scrub of the previously sent name/slug; its
                               immediate attempt runs async off the response
                               path and — like §3.3 person deletion — is a
                               privacy duty, not collection, so it is not
                               consent-gated. A PostHog failure only leaves
                               the job pending; the visibility change itself
                               is already committed. *)
                            record (fun () ->
                                Analytics.identify_community_if_consented request
                                  ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                  (community_group_of updated);
                                match cleanup_job with
                                | Some job_id ->
                                    Lwt.async (fun () ->
                                        attempt_posthog_group_cleanup_job
                                          request ~job_id)
                                | None -> ());
                            Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=visibility")
                        | Ok (None, _) ->
                            (* Community vanished between fetch and update: the
                               old code's silent no-op — same redirect, no
                               emission. *)
                            Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=visibility")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err))))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

let update_community_indexability_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* Hidden input carries the explicit next state ("true"/"false"); a missing/garbage
              value is treated as false (non-indexable) — fail toward LESS exposure, never a 500. *)
           let indexable = List.assoc_opt "indexable" form_data = Some "true" in
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
             | Ok (Some community) when community.is_network_community ->
                 (* Indexing on a network community is never independent: a
                    setup draft must stay non-indexable, and a published one
                    is either indexable *and* discoverable or neither. This
                    route can only move [indexable], so on a network community
                    it can only produce a state
                    Network_communities.lifecycle_state_valid rejects. It
                    therefore refuses both lifecycle states outright, before
                    any authorization branch or write, with the same generic
                    404 a missing community gets — publication owns this
                    pair. *)
                 Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Ok (Some community) ->
                 let%lwt role_res = Db.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change discovery settings." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   match%lwt Db.update_community_indexable db community.id indexable with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=visibility")
                   | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

(* Slice H: per-channel / per-section indexability toggles from /c/:slug/settings. Same TM/A gate
   as the Slice E community visibility/indexability controls — a regular mod hitting these POSTs
   directly is rejected `Forbidden. These flip ONLY the Slice-B child indexable columns the Slice-G
   resolver already consults for child noindex + discovery/provenance exclusion; they change no read
   gate and create no privacy. Ownership is validated (get_*_by_id) AND the UPDATE is community-scoped,
   so a forged cross-community id can neither resolve nor write. The hidden `indexable` field carries
   the explicit next state; a missing/garbage value fails toward false (non-indexable / less exposure),
   mirroring update_community_indexability_handler. *)
let update_channel_indexability_handler request =
  let slug = Dream.param request "slug" in
  let channel_id_str = Dream.param request "channel_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let channel_id = try int_of_string channel_id_str with _ -> 0 in
      if channel_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid channel ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      (match%lwt Dream.form request with
       | `Ok form_data ->
           let indexable = List.assoc_opt "indexable" form_data = Some "true" in
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
             | Ok (Some community) ->
                 let%lwt role_res = Db.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change discovery settings." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   (match%lwt Db.get_channel_by_id db channel_id community.id with
                    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
                    | Ok (Some _) ->
                        match%lwt Db.update_channel_indexable db channel_id community.id indexable with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

let update_section_indexability_handler request =
  let slug = Dream.param request "slug" in
  let section_id_str = Dream.param request "section_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      let section_id = try int_of_string section_id_str with _ -> 0 in
      if section_id = 0 then
        Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid section ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      (match%lwt Dream.form request with
       | `Ok form_data ->
           let indexable = List.assoc_opt "indexable" form_data = Some "true" in
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
             | Ok (Some community) ->
                 let%lwt role_res = Db.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change discovery settings." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   (match%lwt Db.get_section_by_id db section_id community.id with
                    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Section not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
                    | Ok (Some _) ->
                        match%lwt Db.update_section_indexable db section_id community.id indexable with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

(* Slice F: minimal member-management (the allow-list for private communities). Same TM/A gate as
   the Slice E visibility/indexability controls — membership controls who can read a private
   community, so a regular mod must not add/remove members. The community is always resolved from
   the slug; the form's user_id (remove) only selects WHICH row, and the DELETE is scoped to this
   community server-side, so a forged community_id is impossible. These touch ONLY community_members
   — moderator/admin rows are never affected, so removing a member who is also a mod leaves their
   role-based read access intact (correct per the product spec). Add/remove are idempotent at the
   DB layer (ON CONFLICT DO NOTHING / DELETE-no-row), so duplicates and missing rows never 500. *)
let add_member_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           let target_username = String.trim (Option.value ~default:"" (List.assoc_opt "username" form_data)) in
           Dream.sql request (fun db ->
             match%lwt Db.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
             | Ok (Some community) ->
                 let%lwt role_res = Db.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can manage members." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else if target_username = "" then
                   Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Validation Error" ~message:"Enter a username to add." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   (* Add only EXISTING users — never create. Unknown username is a friendly 404
                      message, not a 500. *)
                   (match%lwt Db.get_user_by_username db target_username with
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
                    | Ok None ->
                        Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"User Not Found" ~message:(Printf.sprintf "No user named \"%s\" exists." target_username) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                    | Ok (Some target) ->
                        (* Idempotent: re-adding an existing member is a no-op (ON CONFLICT DO NOTHING). *)
                        match%lwt Db.join_community db target.id community.id with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=members")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

let remove_member_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* The member to remove is identified by user_id from the server-rendered member list.
              An absent/garbage id is a friendly validation error, never a 500. *)
           (match int_of_string_opt (String.trim (Option.value ~default:"" (List.assoc_opt "target_user_id" form_data))) with
            | None ->
                Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Validation Error" ~message:"Invalid member selection." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
            | Some target_user_id ->
                Dream.sql request (fun db ->
                  match%lwt Db.get_community_by_slug db slug with
                  | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
                  | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)
                  | Ok (Some community) ->
                      let%lwt role_res = Db.get_moderator_role db user_id community.id in
                      let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                      if not (is_top_mod || is_admin) then
                        Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can manage members." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                      else
                        (* Idempotent: removing a non-member deletes zero rows (no error). Only the
                           community_members row is touched — moderator/admin rows are untouched.
                           No community_left analytics here: this is a moderator acting on ANOTHER
                           user's membership, not that user leaving. *)
                        match%lwt Db.leave_community db target_user_id community.id with
                        | Ok _deleted -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=members")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

(* === USER === *)

let view_profile_handler request =
  let username_param = Dream.param request "username" in
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let viewer_id = match Dream.session_field request "user_id" with Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
  let active_tab = Option.value ~default:"posts" (Dream.query request "tab") in

  Dream.sql request (fun db ->
    let%lwt user_votes = get_current_user_votes db request in
    (* Joined communities feed the launch rail only (viewer's own memberships,
       same source/order as every other launch surface); a failure degrades to
       an empty rail rather than blocking the profile. *)
    let%lwt rail_communities =
      if viewer_id > 0 then
        (match%lwt Db.get_user_communities db viewer_id with
         | Ok cs -> Lwt.return cs
         | Error _ -> Lwt.return [])
      else Lwt.return []
    in
    (* Slice C + D profile leak-filter. A profile aggregates a user's activity across communities
       and is itself a PUBLIC discovery surface, so it must not surface activity that is either
       (a) PRIVATE and unreadable by the *viewer* (Slice C), or (b) public-but-non-indexable, i.e.
       "unlisted" (Slice D). [blocked_post_ids] classifies a page of post ids in one bounded query
       (visibility + indexable), drops public-non-indexable outright, and for the rare private ones
       checks the viewer's membership/mod per distinct community. Private communities the viewer CAN
       read stay visible (Slice C — no broadening). Fails CLOSED: a classification error blocks the
       whole page. Public + indexable activity is unaffected. *)
    let blocked_post_ids post_ids =
      match post_ids with
      | [] -> Lwt.return []
      | _ ->
          match%lwt Db.get_post_communities db post_ids with
          | Error _ -> Lwt.return post_ids
          | Ok rows ->
              (* (b) public + non-indexable → never surfaced as public discovery. *)
              let unindexed_public =
                List.filter_map
                  (fun (pid, _cid, vis, indexable, _sec) ->
                     if vis = "public" && not indexable then Some pid else None)
                  rows in
              (* (c) Slice G: in a non-indexable forum section → never surfaced as public discovery,
                 regardless of community-level flags. *)
              let section_excluded =
                List.filter_map
                  (fun (pid, _cid, _vis, _ix, sec_excluded) -> if sec_excluded then Some pid else None)
                  rows in
              (* (a) private → surfaced only to viewers who can read the community. *)
              let private_rows = List.filter (fun (_pid, _cid, vis, _ix, _sec) -> vis = "private") rows in
              let distinct_cids =
                List.sort_uniq compare (List.map (fun (_pid, cid, _vis, _ix, _sec) -> cid) private_rows) in
              let%lwt readable_cids =
                Lwt_list.filter_s (fun cid ->
                  if is_admin then Lwt.return true
                  else if viewer_id <= 0 then Lwt.return false
                  else
                    let%lwt m = match%lwt Db.is_member db viewer_id cid with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
                    if m then Lwt.return true
                    else (match%lwt Db.is_moderator db viewer_id cid with Ok b -> Lwt.return b | Error _ -> Lwt.return false))
                  distinct_cids
              in
              let blocked_private =
                List.filter_map
                  (fun (pid, cid, _vis, _ix, _sec) -> if List.mem cid readable_cids then None else Some pid)
                  private_rows in
              Lwt.return (unindexed_public @ section_excluded @ blocked_private)
    in
    (* Shared profile-surfaceable rule for community badges/stats (each carries a community record
       or slug → a discovery link): show only public+indexable communities, plus private ones the
       viewer is authorized to read (Slice C). Public-but-non-indexable communities are hidden
       (Slice D). Fails closed. *)
    let community_surfaceable (c : Db.community) =
      match c.Db.visibility with
      | Db.Community_public -> Lwt.return c.Db.indexable
      | Db.Community_private -> can_view_community db ~user_id:viewer_id ~is_admin c
    in
    let stat_is_readable (s : Db.community_user_stat) =
      match%lwt Db.get_community_by_slug db s.community_slug with
      | Ok (Some community) -> community_surfaceable community
      | _ -> Lwt.return false
    in
    match%lwt Db.get_user_public db username_param with
    | Ok (Some (uid, _, joined_at, bio, avatar_url)) ->

        (match%lwt Db.get_user_karma db uid with
        | Ok karma ->
            let%lwt admin_usernames_res = Db.get_admin_usernames db in
            let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
            let%lwt moderated_communities_res = Db.get_moderated_communities db uid in
            let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
            (* Slice C + D: the "Mod of /c/x" badges render on every tab and are discovery links.
               They would otherwise leak that this user moderates a PRIVATE community (Slice C) or
               an unlisted public-non-indexable one (Slice D). Surface only public+indexable
               communities, plus private ones the viewer can read. *)
            let%lwt moderated_communities =
              Lwt_list.filter_s community_surfaceable moderated_communities in
            let%lwt is_gb_res = Db.is_globally_banned db uid in
            let is_globally_banned = match is_gb_res with Ok b -> b | Error _ -> false in

            (* Fetch only the data the active tab needs — avoids double DB round-trips. *)
            if active_tab = "comments" then
              (match%lwt Db.get_comments_by_user db uid with
              | Ok user_comments ->
                  let post_ids = List.map (fun (_, _, _, pid, _, _) -> pid) user_comments in
                  let%lwt blocked = blocked_post_ids post_ids in
                  let user_comments = List.filter (fun (_, _, _, pid, _, _) -> not (List.mem pid blocked)) user_comments in
                  Dream.html (Pages.user_profile_page ?user:current_user ~is_admin ~is_globally_banned ~profile_id:uid ~admin_usernames ~moderated_communities ~active_tab ~rail_communities user_votes username_param joined_at bio avatar_url karma [] user_comments [] request)
              | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request))
            else if active_tab = "communities" then
              (match%lwt Db.get_user_community_stats db uid with
              | Ok community_stats ->
                  let%lwt community_stats = Lwt_list.filter_s stat_is_readable community_stats in
                  Dream.html (Pages.user_profile_page ?user:current_user ~is_admin ~is_globally_banned ~profile_id:uid ~admin_usernames ~moderated_communities ~active_tab ~rail_communities user_votes username_param joined_at bio avatar_url karma [] [] community_stats request)
              | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request))
            else
              (match%lwt Db.get_posts_by_user db uid with
              | Ok posts ->
                  let post_ids = List.map (fun (p : Db.post) -> p.id) posts in
                  let%lwt blocked = blocked_post_ids post_ids in
                  let posts = List.filter (fun (p : Db.post) -> not (List.mem p.id blocked)) posts in
                  Dream.html (Pages.user_profile_page ?user:current_user ~is_admin ~is_globally_banned ~profile_id:uid ~admin_usernames ~moderated_communities ~active_tab ~rail_communities user_votes username_param joined_at bio avatar_url karma posts [] [] request)
              | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request))

        | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request))

    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:current_user ~title:"Not Found" ~message:"This user does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

let settings_page_handler request =
  match Dream.session_field request "username" with
  | None -> Dream.redirect request "/login"
  | Some username ->
      Dream.sql request (fun db ->
        match%lwt Db.get_user_public db username with
        | Ok (Some (_, _, _, bio, avatar_url)) ->
            (* Joined communities feed the launch rail only; a failure (or a
               missing/garbled user_id session field) degrades to an empty
               rail rather than blocking the settings page. *)
            let%lwt rail_communities =
              match
                Option.bind (Dream.session_field request "user_id")
                  int_of_string_opt
              with
              | Some uid -> (
                  match%lwt Db.get_user_communities db uid with
                  | Ok cs -> Lwt.return cs
                  | Error _ -> Lwt.return [])
              | None -> Lwt.return []
            in
            Dream.html (Pages.settings_page ~user:username ~rail_communities bio avatar_url request)
        | _ -> Dream.redirect request "/login"
      )

let update_profile_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.multipart request with
      | `Ok form_data ->
          let get_field name =
            match List.assoc_opt name form_data with
            | Some ((_, v) :: _) -> v
            | _ -> ""
          in
          let bio = match get_field "bio" with "" -> None | b -> Some b in
          let avatar_bytes = get_field "avatar_url" in
          (* The browser supplies bytes for a NEW avatar and nothing else.
             The form used to also round-trip the stored URL in a hidden
             existing_avatar_url field, which this handler wrote back
             verbatim — so a caller could set their own users.avatar_url to
             ANY string, including another user's /static/uploads/ file, and
             then have delete_account_handler unlink it. The submitted value
             is now ignored entirely (the field is gone from the form) and
             the fallback is re-read from the caller's own row, which is the
             only avatar they could legitimately keep. *)
          Dream.sql request (fun db ->
            match%lwt process_image_upload ~db ~ip:(Dream.client request)
                        ~purpose:Image_upload.Profile_avatar avatar_bytes with
            | Error e ->
                Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Image Error" ~message:e ~alert_type:"error" ~return_url:"/settings" request)
            | Ok new_avatar ->
            (* A failed read must not silently clear the stored avatar: the
               profile write is refused instead. *)
            let%lwt stored_avatar =
              match new_avatar with
              | Some _ -> Lwt.return (Ok new_avatar)
              | None -> Db.get_user_avatar_url db user_id
            in
            match stored_avatar with
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/settings" request)
            | Ok avatar_url ->
            match%lwt Db.update_user_profile db bio avatar_url user_id with
            | Ok () -> Dream.redirect request "/settings"
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/settings" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/settings" request)

(* Re-authenticate with old password before rotating the secret — prevents session
   hijack from silently changing credentials via a stolen cookie. *)
let change_password_handler request =
  match Dream.session_field request "user_id", Dream.session_field request "username" with
  | Some uid_str, Some username ->
      let user_id = int_of_string uid_str in
      (match%lwt Dream.form request with
      | `Ok form_data ->
          let old_password = List.assoc "old_password" form_data in
          let new_password = List.assoc "new_password" form_data in
          let confirm_password = List.assoc "confirm_password" form_data in

          if new_password <> confirm_password then
            Dream.html (Pages.msg_page ~user:username ~title:"Password Mismatch" ~message:"The new passwords you entered do not match. Please go back and try again." ~alert_type:"error" ~return_url:"/settings" request)
          else if String.length new_password < 8 then
            Dream.html (Pages.msg_page ~user:username ~title:"Password Too Short" ~message:"Your new password must be at least 8 characters long." ~alert_type:"error" ~return_url:"/settings" request)
          else
            Dream.sql request (fun db ->
              match%lwt Db.get_user_for_login db username with
              | Ok (Some (_, (hash, _, _))) ->
                  (match%lwt Auth.verify_password ~password:old_password ~hash with
                  | Ok true ->
                      (match%lwt Auth.hash_password new_password with
                      | Ok new_hash ->
                          (match%lwt Db.update_password db user_id new_hash with
                          | Ok () -> Dream.redirect request "/settings"
                          | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/settings" request))
                      | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:("Hashing error: " ^ err) ~alert_type:"error" ~return_url:"/settings" request))
                  | _ -> Dream.html (Pages.msg_page ~user:username ~title:"Wrong Password" ~message:"The current password you entered is incorrect. Please go back and try again." ~alert_type:"error" ~return_url:"/settings" request))
              | _ -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:"User not found in the database." ~alert_type:"error" ~return_url:"/settings" request)
            )
      | _ -> Dream.html (Pages.msg_page ~user:username ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/settings" request))
  | _ -> Dream.redirect request "/login"

(* GDPR Art. 20 (data portability): JSON chosen over CSV for machine-readability;
   Content-Disposition triggers browser download rather than inline render. *)
let export_data_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let username = Option.value (Dream.session_field request "username") ~default:"unknown" in

      Dream.sql request (fun db ->
        let%lwt profile_res = Db.get_user_public db username in
        let%lwt posts_res = Db.get_posts_by_user db user_id in
        let%lwt comments_res = Db.get_comments_by_user db user_id in

        match profile_res, posts_res, comments_res with
        | Ok (Some (_, _, joined_at, bio, avatar)), Ok posts, Ok comments ->

            let profile_json = `Assoc [
              ("username", `String username);
              ("joined_at", `String joined_at);
              ("bio", match bio with Some b -> `String b | None -> `Null);
              ("avatar_url", match avatar with Some a -> `String a | None -> `Null);
            ] in

            let posts_json = `List (List.map (fun (p: Db.post) ->
              `Assoc [
                ("id", `Int p.id);
                ("title", `String p.title);
                ("content", match p.content with Some c -> `String c | None -> `Null);
                ("url", match p.url with Some u -> `String u | None -> `Null);
                ("community_slug", `String p.community_slug);
                ("created_at", `String p.created_at);
                ("score", `Int p.score);
              ]
            ) posts) in

            let comments_json = `List (List.map (fun (id, content, created_at, post_id, post_title, score) ->
              `Assoc [
                ("id", `Int id);
                ("post_id", `Int post_id);
                ("post_title", `String post_title);
                ("content", `String content);
                ("created_at", `String created_at);
                ("score", `Int score);
              ]
            ) comments) in

            let export_json = `Assoc [
              ("profile", profile_json);
              ("posts", posts_json);
              ("comments", comments_json);
            ] in

            let json_str = Yojson.Safe.pretty_to_string export_json in
            Dream.respond
              ~headers:[
                ("Content-Type", "application/json");
                (* The filename is derived from the numeric user id, never the
                   username: a header value must stay within a conservative
                   ASCII alphabet (no quotes, control bytes, or separators). *)
                ("Content-Disposition", Printf.sprintf "attachment; filename=\"earde_export_user_%d.json\"" user_id)
              ]
              json_str

        | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to generate export data. Please try again." ~alert_type:"error" ~return_url:"/settings" request)
      )

(* Immediate §3.3 attempt for a just-committed deletion job. The atomic claim
   (status + lease in one statement) means a concurrently running maintenance
   retry can never process the same job at the same time; every DB step is its
   own short Dream.sql call, so no connection is held across the PostHog HTTP
   attempt. All failures are swallowed — the job stays durably pending. *)
let attempt_posthog_deletion_job request ~job_id =
  Lwt.catch
    (fun () ->
      let%lwt claimed =
        Dream.sql request (fun db -> Db.claim_posthog_deletion_job db job_id)
      in
      match claimed with
      | Ok (Some distinct_id) ->
          let%lwt (_ : [ `Completed | `Left_pending of string ]) =
            Posthog_deletion.process_claimed_job
              ~mark_completed:(fun () ->
                Dream.sql request (fun db ->
                    Db.complete_posthog_deletion_job db job_id))
              ~mark_failed:(fun err ->
                Dream.sql request (fun db ->
                    Db.fail_posthog_deletion_job db job_id err))
              ~distinct_id
          in
          Lwt.return_unit
      | Ok None | Error _ -> Lwt.return_unit)
    (fun exn ->
      Dream.log "posthog deletion immediate attempt error: %s"
        (Printexc.to_string exn);
      Lwt.return_unit)

(* GDPR Art. 17 (right to erasure): anonymize rather than hard-delete to preserve
   thread coherence; posts remain as [deleted] rather than leaving orphaned replies. *)
let delete_account_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok _ ->
          (* The stored avatar reference must be read BEFORE the anonymize
             rewrite NULLs it. A read failure only skips file cleanup — it
             must never block the deletion itself. *)
          let%lwt avatar_url =
            Lwt.catch
              (fun () ->
                let%lwt res =
                  Dream.sql request (fun db ->
                      Db.get_user_avatar_url db user_id)
                in
                match res with
                | Ok v -> Lwt.return v
                | Error _ -> Lwt.return None)
              (fun _ -> Lwt.return None)
          in
          (* §3.3 atomic local deletion: anonymization and the durable
             deletion job commit together (or roll back together) — no crash
             window with an anonymized user and no job. The transaction never
             performs HTTP. *)
          let%lwt result =
            Dream.sql request (fun db ->
                Db.anonymize_user_and_enqueue_posthog_deletion db user_id)
          in
          (match result with
            | Ok (job_id, _distinct_id) ->
                (* One async cleanup chain, off the response path:
                   1. the locally stored avatar file, only after the commit
                      (Avatar_uploads validates the path shape; anything not
                      a pipeline upload is untouched, a missing file is
                      success, and a real failure logs a fixed, path-free
                      line and never unwinds the committed deletion);
                   2. the consent-gated PERSONLESS account_deleted metric
                      (constant system distinct id, person processing off — so
                      ingestion timing can never associate it with, or
                      recreate, the person being deleted);
                   3. then — regardless of the metric's outcome — the
                      immediate durable deletion attempt for the real
                      user:<id> job. PostHog being down or unconfigured only
                      leaves the committed job pending. *)
                Lwt.async (fun () ->
                    (match
                       Avatar_uploads.cleanup_deleted_account_avatar avatar_url
                     with
                    | `Removed | `Absent | `Not_local -> ()
                    | `Failed ->
                        Dream.log
                          "avatar cleanup failed for a deleted account; file \
                           retained under static/uploads");
                    let%lwt () =
                      Lwt.catch
                        (fun () ->
                          Analytics.capture_account_deleted_sequenced request)
                        (fun _ -> Lwt.return_unit)
                    in
                    attempt_posthog_deletion_job request ~job_id);
                let%lwt () = Dream.invalidate_session request in
                Dream.redirect request "/"
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/settings" request))
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/settings" request)

(* === NOTIFICATIONS === *)

let notifications_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      (* The session claim only enables the durable users.is_admin check
         inside the capability columns, never replaces it. *)
      let session_admin = Dream.session_field request "is_admin" = Some "true" in
      Dream.sql request (fun db ->
        let%lwt notifs = Db.get_notifications db ~session_admin user_id in
        let%lwt _ = Db.mark_notifs_read db user_id in
        (* Joined communities feed the launch rail only; a failure degrades to
           an empty rail rather than blocking the notification list. *)
        let%lwt rail_communities_res = Db.get_user_communities db user_id in
        let rail_communities = match rail_communities_res with Ok cs -> cs | Error _ -> [] in
        match notifs with
        | Ok n -> Dream.html (Pages.notifications_page ?user ~rail_communities n request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
      )

(* GET /api/unread-notifs is gone with the client-side badge it existed to
   feed. It could only answer "0" when the count query failed, which the
   browser could not distinguish from a real zero. The count is now resolved
   server-side, once per request, by Notification_badge.middleware. *)

(* === LEGAL / PRIVACY === *)

let privacy_page_handler request =
  let user = Dream.session_field request "username" in
  Dream.html (Pages.privacy_page ?user request)

(* === ADMIN ===

   PostHog is the authoritative KPI/analytics product; the old in-app KPI
   dashboard (GET /earde-hq-dashboard) was removed with its renderer and its
   two exclusive aggregate queries. Nothing replaced it and no route redirects
   to PostHog — /admin stays the operational admin surface. *)

let ban_user_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" -> (
      (* The ban form carries only Dream's CSRF field; parsing the body is
         what actually validates the session-bound token, and it must happen
         before any mutation. Path ids never grant authority on their own. *)
      match%lwt Dream.form request with
      | `Ok _ ->
      let user_id_to_ban = try int_of_string (Dream.param request "id") with _ -> 0 in
      if user_id_to_ban = 0 then Dream.respond ~status:`Bad_Request "Invalid user ID." else
      Dream.sql request (fun db ->
        match%lwt Db.ban_user db user_id_to_ban with
        | Ok () ->
            (* Notify banned user — best-effort; no post to link to. *)
            let%lwt _ = Db.create_notif db user_id_to_ban None "mod_action" "You have been globally banned by an administrator." in
            (* Redirect back to the profile page rather than "/" so the admin
               immediately sees the updated 🚫 badge and the Unban button. *)
            let target = safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
            Dream.redirect request target
        | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/admin" request)
      )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request))
  | _ -> Dream.html (Pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

let unban_user_global_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" -> (
      (* Same contract as ban_user_handler: the unban form has no application
         fields, but Dream.form must still run — it is the CSRF validation. *)
      match%lwt Dream.form request with
      | `Ok _ ->
      let user_id_to_unban = try int_of_string (Dream.param request "id") with _ -> 0 in
      if user_id_to_unban = 0 then Dream.respond ~status:`Bad_Request "Invalid user ID." else
      Dream.sql request (fun db ->
        match%lwt Db.unban_user_global db user_id_to_unban with
        | Ok () ->
            let target = safe_local_redirect ~default:"/admin" request (match Dream.header request "Referer" with Some r -> r | None -> "/admin") in
            Dream.redirect request target
        | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(db_error_message err) ~alert_type:"error" ~return_url:"/admin" request)
      )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/admin" request))
  | _ -> Dream.html (Pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

let admin_dashboard_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" ->
      let user = Dream.session_field request "username" in
      (* Safe config status only — the Turnstile site-key payload in [Configured] is
         discarded here so no secret/key reaches the page; Email.is_configured returns a
         bool, never the API key. *)
      let signups_enabled = signups_enabled () in
      let turnstile = match Turnstile.status () with
        | Turnstile.Configured _ -> `Configured
        | Turnstile.Disabled     -> `Disabled
        | Turnstile.Misconfigured -> `Misconfigured
      in
      let brevo_configured = Email.is_configured () in
      Dream.sql request (fun db ->
        let%lwt banned_res  = Db.get_globally_banned_users db in
        let%lwt recent_res  = Db.Admin.list_recent_users db ~limit:50 in
        let%lwt pending_res = Db.Admin.list_recent_pending db ~limit:50 in
        (* Joined communities feed the shared launch rail only; loaded here —
           after the admin gate — so denied requests never touch membership
           data, and a failure degrades to an empty rail rather than blocking
           the dashboard. *)
        let%lwt rail_res =
          match Option.bind (Dream.session_field request "user_id") int_of_string_opt with
          | Some uid -> Db.get_user_communities db uid
          | None -> Lwt.return (Ok [])
        in
        let rail_communities = match rail_res with Ok cs -> cs | Error _ -> [] in
        match banned_res, recent_res, pending_res with
        | Ok banned_users, Ok recent_users, Ok pending ->
            Dream.html (Pages.admin_dashboard_page ?user ~rail_communities ~signups_enabled ~turnstile
              ~brevo_configured ~recent_users ~pending ~banned_users request)
        | (Error e, _, _) | (_, Error e, _) | (_, _, Error e) ->
            Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
      )
  | _ -> Dream.respond ~status:`Forbidden (Pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

(* Admin-only: GC stats expose heap pressure without a profiler attachment.
   Cheaper than pprof; useful for spotting minor-GC spikes on staging. *)
let debug_state_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" ->
      let gc = Gc.stat () in
      let sess_field k =
        match Dream.session_field request k with Some s -> `String s | None -> `Null
      in
      let gc_json = `Assoc [
        ("minor_words",       `Float gc.Gc.minor_words);
        ("promoted_words",    `Float gc.Gc.promoted_words);
        ("major_words",       `Float gc.Gc.major_words);
        ("minor_collections", `Int   gc.Gc.minor_collections);
        ("major_collections", `Int   gc.Gc.major_collections);
        ("compactions",       `Int   gc.Gc.compactions);
        ("heap_words",        `Int   gc.Gc.heap_words);
        ("live_words",        `Int   gc.Gc.live_words);
        ("free_words",        `Int   gc.Gc.free_words);
      ] in
      let session_json = `Assoc [
        ("user_id",  sess_field "user_id");
        ("username", sess_field "username");
        ("is_admin", sess_field "is_admin");
      ] in
      let body = Yojson.Safe.pretty_to_string (`Assoc [
        ("gc",      gc_json);
        ("session", session_json);
      ]) in
      Dream.respond ~headers:[("Content-Type", "application/json")] body
  | _ ->
      Dream.respond ~status:`Forbidden
        ~headers:[("Content-Type", "application/json")]
        {|{"error":"forbidden"}|}

(* === MANAGE MODS === *)

let manage_mods_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      Dream.sql request (fun db ->
        match%lwt Db.get_community_by_slug db slug with
        | Ok (Some community) ->
            let%lwt role_res = Db.get_moderator_role db user_id community.id in
            let current_user_role = match role_res with Ok r -> r | _ -> None in
            let is_authorized = is_admin || current_user_role = Some "top_mod" in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and Admins can manage moderators." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
            else
              (* Launch-chrome data (pass 14C), loaded only after the TM/A
                 authorization above — a denied request never touches sections,
                 channels or the viewer's membership. Each degrades to an empty
                 list on error rather than blocking the roster. Same plumbing as
                 the sibling converted management routes. *)
              let%lwt sections =
                if community.sections_enabled then
                  (match%lwt Db.get_sections_by_community db community.id with
                   | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
                else Lwt.return []
              in
              let%lwt channels =
                match%lwt Db.get_channels_by_community db community.id with
                | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
              in
              let%lwt rail_communities =
                match%lwt Db.get_user_communities db user_id with
                | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
              in
              (match%lwt Db.get_community_mods_with_roles db community.id with
               | Ok mods ->
                   Dream.html (Pages.manage_mods_page ?user ~rail_communities ~is_admin ~current_user_role ~channels ~sections ~community ~mods request)
               | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
        | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
      )

let manage_mods_add_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let target_username = String.trim (List.assoc_opt "username" form_data |> Option.value ~default:"") in
          if target_username = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Username is required." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok (Some community) ->
                let%lwt role_res = Db.get_moderator_role db user_id community.id in
                let current_user_role = match role_res with Ok r -> r | _ -> None in
                let is_authorized = is_admin || current_user_role = Some "top_mod" in
                if not is_authorized then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and Admins can add moderators." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                else
                  (match%lwt Db.get_user_by_username db target_username with
                   | Ok (Some target_user) ->
                       let%lwt _ = Db.add_moderator db target_user.id community.id in
                       Dream.redirect request ("/c/" ^ slug ^ "/manage-mods")
                   | Ok None -> Dream.html (Pages.msg_page ?user ~title:"User Not Found" ~message:("No user found: u/" ^ target_username) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                   | Error e -> Dream.html (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request))
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)

let manage_mods_promote_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let target_user_id = try int_of_string (List.assoc_opt "target_user_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if target_user_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid user reference." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok (Some community) ->
                let%lwt role_res = Db.get_moderator_role db user_id community.id in
                let current_user_role = match role_res with Ok r -> r | _ -> None in
                let is_authorized = is_admin || current_user_role = Some "top_mod" in
                if not is_authorized then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and Admins can promote moderators." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                else
                  (match%lwt Db.promote_to_top_mod db target_user_id community.id with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/manage-mods")
                   | Error (Db.Promotion_refused msg) ->
                       (* Fixed domain refusals (not a moderator, already Top
                          Mod, seat cap) stay user-visible verbatim. *)
                       Dream.html (Pages.msg_page ?user ~title:"Promotion Failed" ~message:msg ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                   | Error (Db.Promotion_storage_error e) ->
                       Dream.html (Pages.msg_page ?user ~title:"Promotion Failed" ~message:(db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request))
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)

let manage_mods_remove_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let target_user_id = try int_of_string (List.assoc_opt "target_user_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if target_user_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid user reference." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
          else
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok (Some community) ->
                let%lwt role_res = Db.get_moderator_role db user_id community.id in
                let current_user_role = match role_res with Ok r -> r | _ -> None in
                let is_authorized = is_admin || current_user_role = Some "top_mod" in
                if not is_authorized then
                  Dream.respond ~status:`Forbidden (Pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and Admins can remove moderators." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                else
                  (* Re-fetch target role server-side: prevents a top_mod from removing
                     another top_mod by manipulating the form — TOCTOU guard. *)
                  (match%lwt Db.get_moderator_role db target_user_id community.id with
                   | Ok (Some "top_mod") when not is_admin ->
                       Dream.html (Pages.msg_page ?user ~title:"Action Denied" ~message:"Top Mods cannot remove other Top Mods. Only an admin can do this." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                   | Ok None ->
                       Dream.html (Pages.msg_page ?user ~title:"Not a Moderator" ~message:"That user is not a moderator of this community." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                   | Ok _ ->
                       (match%lwt Db.get_community_mods_with_roles db community.id with
                        | Ok mods when List.length mods > 1 ->
                            let%lwt _ = Db.remove_moderator db target_user_id community.id in
                            Dream.redirect request ("/c/" ^ slug ^ "/manage-mods")
                        | Ok _ ->
                            Dream.html (Pages.msg_page ?user ~title:"Cannot Remove" ~message:"You cannot remove the last moderator of a community." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)
                        | Error e -> Dream.html (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request))
                   | Error e -> Dream.html (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request))
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:(db_error_message e) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)

(* === MIDDLEWARE === *)

(* Shared eligibility gate for both the presence touch and page-view logging.
   Pure. Skips the admin dashboard (reads page_view data — recursive
   self-counting), /api/unread-notifs (a retired path — the route is gone with
   the client-side badge, but the exclusion stays so anything still hitting the
   old URL cannot become tracked traffic), static assets, and bot user agents.
   Both middlewares must use this same decision so presence keeps the exact
   pre-extraction touch semantics. *)
let is_tracked_request ~path ~user_agent =
  (* Static asset filter: log only meaningful page navigations, not asset fetches. *)
  let is_static =
    let has_prefix p = String.length path >= String.length p && String.sub path 0 (String.length p) = p in
    let has_suffix s =
      let pl = String.length path and sl = String.length s in
      pl >= sl && String.sub path (pl - sl) sl = s
    in
    has_prefix "/static/" || has_prefix "/css/" || has_prefix "/js/"
    || has_suffix ".js" || has_suffix ".css" || has_suffix ".ico"
    || has_suffix ".png" || has_suffix ".jpg" || has_suffix ".svg"
    || has_suffix ".woff" || has_suffix ".woff2" || has_suffix ".ttf"
  in

  (* Bot filter: skip synthetic traffic that inflates page-view counts. *)
  let is_bot =
    match user_agent with
    | None -> false
    | Some ua ->
        let lc = String.lowercase_ascii ua in
        let contains needle =
          let hl = String.length lc and nl = String.length needle in
          if nl = 0 || hl < nl then false
          else
            let rec loop i =
              if i > hl - nl then false
              else if String.sub lc i nl = needle then true
              else loop (i + 1)
            in
            loop 0
        in
        contains "bot" || contains "crawler" || contains "spider" || contains "scraper"
  in

  let is_admin_route =
    (* Retired path: the KPI dashboard route is gone (PostHog is authoritative),
       but the exclusion stays so page-view logging and the last_active_at touch
       behave exactly as before for anything still hitting the old URL — its
       requests now 404 and must not become tracked traffic. Prefix match covers
       query-string variants too. *)
    let admin_prefix = "/earde-hq-dashboard" in
    let plen = String.length path and alen = String.length admin_prefix in
    (plen >= alen && String.sub path 0 alen = admin_prefix
     && (plen = alen || path.[alen] = '?' || path.[alen] = '/'))
  in
  not (is_admin_route || path = "/api/unread-notifs" || is_static || is_bot)

(* Presence, not analytics: sole writer of users.last_active_at, which moderator
   auto-demotion (Db.demote_inactive_mods) reads. Kept separate from
   analytics_middleware so replacing the page-view system cannot break it, but
   gated on the same is_tracked_request decision so the touch fires exactly
   where the old in-analytics touch did (never on polling/asset/bot requests).
   Best-effort — the touch result is ignored, so a failed update never fails
   the user's request. *)
let presence_middleware inner_handler request =
  let%lwt () =
    match Dream.session_field request "user_id" with
    | Some uid_str
      when is_tracked_request ~path:(Dream.target request)
             ~user_agent:(Dream.header request "User-Agent") ->
        Dream.sql request (fun db ->
          let%lwt _ = Db.touch_user_active db (int_of_string uid_str) in
          Lwt.return_unit)
    | _ -> Lwt.return_unit
  in
  inner_handler request

let analytics_middleware inner_handler request =
  let path = Dream.target request in
  let%lwt _ =
    if not (is_tracked_request ~path
              ~user_agent:(Dream.header request "User-Agent"))
    then Lwt.return_unit
    else begin
      (* Sanitize referer: keep only host to avoid leaking tokens in paths/query strings. *)
      let referer =
        match Dream.header request "Referer" with
        | None -> None
        | Some raw ->
            let uri = Uri.of_string raw in
            (match Uri.host uri with
             | None -> None
             | Some h -> Some h)
      in
      (* Daily-rotating session hash: IP + UA + date → MD5 hex. Rotating daily means
         the hash never accumulates cross-day fingerprinting risk (GDPR Art. 5(1)(e)).
         MD5 is intentionally chosen — cryptographic strength is not needed here,
         just collision-resistance within a single day's window. *)
      let ip = Dream.client request in
      let ua = Option.value ~default:"" (Dream.header request "User-Agent") in
      let date =
        let t = Unix.gettimeofday () in
        let tm = Unix.gmtime t in
        Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      in
      let session_hash = Digest.to_hex (Digest.string (ip ^ ua ^ date)) in
      Dream.sql request (fun db ->
        let%lwt _ = Db.log_page_view db path referer session_hash in
        Lwt.return_unit
      )
    end
  in
  inner_handler request
