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
   auto-demotion (Moderator_store.demote_inactive_mods) reads. Kept separate from
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
          let%lwt _ = User_store.touch_user_active db (int_of_string uid_str) in
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
        let%lwt _ = Page_view_store.log_page_view db path referer session_hash in
        Lwt.return_unit
      )
    end
  in
  inner_handler request
