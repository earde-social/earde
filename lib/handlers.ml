(* Only allow local (same-origin) redirects from attacker-controlled headers.
   Protocol-relative URLs (//) redirect to external hosts despite leading slash.
   Used wherever we follow the Referer header to send the user back. *)
let safe_local_redirect target =
  if String.length target >= 2 && String.sub target 0 2 = "//" then "/"
  else if String.length target > 0 && target.[0] = '/' then target
  else "/"

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

(* === RATE LIMITING === *)

(* DB-backed rate limit trades a synchronous Hashtbl lookup for a round-trip to
   Postgres; the ~1ms I/O penalty is the price of crash resilience and shared
   state across replicas — unavoidable once we move beyond a single process. *)
module Rate_limit = struct
  let middleware inner_handler request =
    let ip = Dream.client request in
    let endpoint = Dream.target request in
    match%lwt Dream.sql request (fun db -> Db.Rate_limit.check db ip endpoint) with
    | Ok `Blocked ->
        Dream.response ~status:`Too_Many_Requests
          "Too many attempts. Try again later."
        |> Lwt.return
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

(* === AUTHENTICATION === *)

let signup_page request =
  let user = Dream.session_field request "username" in
  Dream.html (Pages.signup_form ?user request)

let signup_handler request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let username = String.trim (List.assoc_opt "username" form_data |> Option.value ~default:"") in
      let email    = String.trim (List.assoc_opt "email"    form_data |> Option.value ~default:"") in
      let password = List.assoc_opt "password" form_data |> Option.value ~default:"" in

      (* Validate before hashing — argon2 is expensive, reject obvious bad input early. *)
      if username = "" || email = "" || password = "" then
        Dream.html (Pages.msg_page ~title:"Validation Error" ~message:"Username, email, and password are all required." ~alert_type:"error" ~return_url:"/signup" request)
      else if String.length username < 3 || String.length username > 30 then
        Dream.html (Pages.msg_page ~title:"Validation Error" ~message:"Username must be between 3 and 30 characters." ~alert_type:"error" ~return_url:"/signup" request)
      else if not (String.contains email '@') then
        Dream.html (Pages.msg_page ~title:"Validation Error" ~message:"Please enter a valid email address." ~alert_type:"error" ~return_url:"/signup" request)
      else if String.length password < 8 then
        Dream.html (Pages.msg_page ~title:"Validation Error" ~message:"Password must be at least 8 characters long." ~alert_type:"error" ~return_url:"/signup" request)
      else

      (match%lwt Auth.hash_password password with
      | Ok password_hash ->

          let token = Dream.to_base64url (Dream.random 32) in

          (* DB connection is returned to the pool before the Brevo HTTP call —
             holding a pool slot for a third-party round-trip would starve concurrent
             signups under load. *)
          let%lwt create_result = Dream.sql request (fun db ->
            Db.create_user db username email password_hash token
          ) in
          (match create_result with
          | Ok () ->
              let%lwt () = Email.send_verification_email ~to_email:email ~token in
              let%lwt () = Dream.set_session_field request "username" username in
              Dream.redirect request "/"
          | Error err ->
              Dream.html (Pages.msg_page ~title:"Registration Failed" ~message:("Registration failed: " ^ err) ~alert_type:"error" ~return_url:"/signup" request))
      | Error err -> Dream.html (Pages.msg_page ~title:"Security Error" ~message:("Security error: " ^ err) ~alert_type:"error" ~return_url:"/signup" request))

  | _ -> Dream.html (Pages.msg_page ~title:"Form Error" ~message:"Your form submission failed. The CSRF token was invalid or your session expired. Please try again." ~alert_type:"error" ~return_url:"/signup" request)

let verify_email_handler request =
  match Dream.query request "token" with
  | None -> Dream.html (Pages.msg_page ~title:"Verification Error" ~message:"The verification token is missing from the URL." ~alert_type:"error" ~return_url:"/signup" request)
  | Some token ->
      Dream.sql request (fun db ->
        match%lwt Db.verify_email db token with
        | Ok (Some username) ->
            Dream.html (Pages.msg_page ~title:"Email Verified!" ~message:(Printf.sprintf "Your account u/%s is now verified. You can log in." username) ~alert_type:"success" ~return_url:"/login" request)
        | Ok None ->
            Dream.html (Pages.msg_page ~title:"Verification Failed" ~message:"This link is invalid or your email has already been verified." ~alert_type:"error" ~return_url:"/signup" request)
        | Error err -> Dream.html (Pages.msg_page ~title:"Error" ~message:("A database error occurred: " ^ err) ~alert_type:"error" ~return_url:"/" request)
      )

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
      Dream.sql request (fun db ->
        match%lwt Db.get_user_for_login db identifier with
        | Ok (Some (id, user, _, hash, is_admin, is_banned)) ->
            (match%lwt Auth.verify_password ~password ~hash with
            | Ok true ->
                if is_banned then
                  Dream.html (Pages.msg_page ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/login" request)
                else
                  let%lwt () = Dream.set_session_field request "user_id" (string_of_int id) in
                  let%lwt () = Dream.set_session_field request "username" user in
                  let%lwt () = if is_admin then Dream.set_session_field request "is_admin" "true" else Lwt.return () in
                  Dream.redirect request "/"
              | _ -> Dream.html (Pages.msg_page ~title:"Login Failed" ~message:"Invalid username or password." ~alert_type:"error" ~return_url:"/login" request))
        | Ok None -> Dream.html (Pages.msg_page ~title:"Login Failed" ~message:"Invalid username or password." ~alert_type:"error" ~return_url:"/login" request)
        | Error err -> Dream.html (Pages.msg_page ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/login" request)
      )
  | _ -> Dream.html (Pages.msg_page ~title:"Form Error" ~message:"There was a problem with your form submission. Your session may have expired." ~alert_type:"error" ~return_url:"/login" request)

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
        Dream.html (Pages.msg_page ~title:"Validation Error" ~message:"Email address is required." ~alert_type:"error" ~return_url:"/forgot-password" request)
      else begin
        let token = Dream.to_base64url (Dream.random 32) in
        (* DB connection released before Brevo call — same pattern as signup. *)
        let%lwt result = Dream.sql request (fun db ->
          Db.password_reset_create_token db email token
        ) in
        (match result with
        | Ok true ->
            let%lwt () = Email.send_password_reset_email ~to_email:email ~token in
            Dream.html (Pages.msg_page ~title:"Check your email" ~message:"If an account with that email exists, a reset link has been sent. Check your inbox (and spam folder)." ~alert_type:"info" ~return_url:"/login" request)
        | Ok false ->
            Dream.html (Pages.msg_page ~title:"Check your email" ~message:"If an account with that email exists, a reset link has been sent. Check your inbox (and spam folder)." ~alert_type:"info" ~return_url:"/login" request)
        | Error err ->
            Dream.log "forgot_password DB error: %s" err;
            Dream.html (Pages.msg_page ~title:"Check your email" ~message:"If an account with that email exists, a reset link has been sent. Check your inbox (and spam folder)." ~alert_type:"info" ~return_url:"/login" request))
      end
  | _ -> Dream.html (Pages.msg_page ~title:"Form Error" ~message:"Your form submission failed. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)

let reset_password_page_handler request =
  match Dream.query request "token" with
  | None ->
      Dream.html (Pages.msg_page ~title:"Invalid Link" ~message:"This password reset link is missing a token. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
  | Some token ->
      (match%lwt Dream.sql request (fun db -> Db.password_reset_validate_token db token) with
      | Ok (Some _) -> Dream.html (Pages.reset_password_page ~token request)
      | Ok None ->
          Dream.html (Pages.msg_page ~title:"Link Expired" ~message:"This password reset link is invalid or has expired. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
      | Error _ ->
          Dream.html (Pages.msg_page ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request))

(* Token is consumed atomically by DELETE+RETURNING before the password update —
   prevents replay even if the hash step fails; user must re-request a fresh link. *)
let reset_password_handler request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let token    = List.assoc_opt "token"            form_data |> Option.value ~default:"" in
      let password = List.assoc_opt "password"         form_data |> Option.value ~default:"" in
      let confirm  = List.assoc_opt "confirm_password" form_data |> Option.value ~default:"" in
      if token = "" then
        Dream.html (Pages.msg_page ~title:"Invalid Request" ~message:"Token is missing. Please use the link from your email." ~alert_type:"error" ~return_url:"/forgot-password" request)
      else if password <> confirm then
        Dream.html (Pages.reset_password_page ~token ~error:"Passwords do not match." request)
      else if String.length password < 8 then
        Dream.html (Pages.reset_password_page ~token ~error:"Password must be at least 8 characters." request)
      else
        Dream.sql request (fun db ->
          match%lwt Db.password_reset_consume_token db token with
          | Ok None ->
              Dream.html (Pages.msg_page ~title:"Link Expired" ~message:"This reset link is invalid or has expired. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
          | Ok (Some user_id) ->
              (match%lwt Auth.hash_password password with
              | Ok new_hash ->
                  (match%lwt Db.update_password db user_id new_hash with
                  | Ok () ->
                      Dream.html (Pages.msg_page ~title:"Password Updated" ~message:"Your password has been updated. You can now log in with your new password." ~alert_type:"success" ~return_url:"/login" request)
                  | Error err ->
                      Dream.log "reset_password update_password error: %s" err;
                      Dream.html (Pages.msg_page ~title:"Error" ~message:"An error occurred while updating your password. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request))
              | Error err ->
                  Dream.log "reset_password hash error: %s" err;
                  Dream.html (Pages.msg_page ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request))
          | Error err ->
              Dream.log "reset_password consume_token error: %s" err;
              Dream.html (Pages.msg_page ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request))
  | _ -> Dream.html (Pages.msg_page ~title:"Form Error" ~message:"Your form submission failed. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)

(* === CORE FEED === *)

let home_handler request =
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in
  let is_logged_in = user_id > 0 in

  let page = match Dream.query request "page" with
    | Some p_str -> (try int_of_string p_str with _ -> 1)
    | None -> 1
  in
  let sort_mode = match Dream.query request "sort" with
    | Some s when s = "new" || s = "top" -> s
    | _ -> "hot"
  in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  Dream.sql request (fun db ->
    (* Logged-in users get a personalised feed from their joined communities;
       guests fall back to the global feed so the page is never empty. *)
    let%lwt posts =
      if is_logged_in then Db.get_personalized_feed db user_id sort_mode limit offset
      else Db.get_all_posts db sort_mode limit offset
    in
    let feed_type = if is_logged_in then "home" else "all" in
    let%lwt user_votes =
      if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok []
    in
    let%lwt user_communities =
      if user_id > 0 then Db.get_user_communities db user_id else Lwt.return_ok []
    in
    let%lwt admin_usernames_res = Db.get_admin_usernames db in
    let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
    let%lwt moderated_communities_res = if user_id > 0 then Db.get_moderated_communities db user_id else Lwt.return_ok [] in
    let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in

    match posts, user_votes, user_communities with
    | Ok p, Ok v, Ok a -> Dream.html (Pages.index ?user v page sort_mode ~feed_type ~admin_usernames ~moderated_communities p a request)
    | Error e, _, _ | _, Error e, _ | _, _, Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
  )

let global_feed_handler request =
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in

  let page = match Dream.query request "page" with
    | Some p_str -> (try int_of_string p_str with _ -> 1)
    | None -> 1
  in
  let sort_mode = match Dream.query request "sort" with
    | Some s when s = "new" || s = "top" -> s
    | _ -> "hot"
  in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  Dream.sql request (fun db ->
    let%lwt posts = Db.get_all_posts db sort_mode limit offset in
    let%lwt user_votes =
      if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok []
    in
    let%lwt user_communities =
      if user_id > 0 then Db.get_user_communities db user_id else Lwt.return_ok []
    in
    let%lwt admin_usernames_res = Db.get_admin_usernames db in
    let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
    let%lwt moderated_communities_res = if user_id > 0 then Db.get_moderated_communities db user_id else Lwt.return_ok [] in
    let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in

    match posts, user_votes, user_communities with
    | Ok p, Ok v, Ok a -> Dream.html (Pages.index ?user v page sort_mode ~feed_type:"all" ~admin_usernames ~moderated_communities p a request)
    | Error e, _, _ | _, Error e, _ | _, _, Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
  )

let search_handler request =
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in

  match Dream.query request "q" with
  | None | Some "" -> Dream.redirect request "/"
  | Some search_term ->
      let page = match Dream.query request "page" with Some p_str -> (try int_of_string p_str with _ -> 1) | None -> 1 in
      let active_tab = match Dream.query request "t" with Some t -> t | None -> "posts" in
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

        match communities_res, users_res, posts_res, comments_res, user_votes with
        | Ok communities, Ok users, Ok posts, Ok comments, Ok votes ->
            Dream.html (Pages.search_results_page ?user ~admin_usernames votes page active_tab search_term communities users posts comments request)
        | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:"Database error during search. Please try again." ~alert_type:"error" ~return_url:"/" request)
      )

(* === COMMUNITY === *)

let new_community_page request =
  match Dream.session_field request "user_id" with
  | None ->
      Dream.redirect request "/login"
  | Some _ ->
      let user = Dream.session_field request "username" in
      Dream.html (Pages.new_community_form ?user request)

let create_community_handler request =
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

          (* Validate before hitting DB — slug uniqueness error is more helpful than a generic 500. *)
          if name = "" || slug = "" then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Community name and URL slug are required." ~alert_type:"error" ~return_url:"/new-community" request)
          else

          Dream.sql request (fun db ->
            match%lwt Db.create_community db name slug description with
            | Ok () ->
                (* Divine Right: creator becomes first top_mod automatically.
                   Round-trip via get_community_by_slug is necessary — INSERT does not
                   return the new id, and changing create_community's return type would
                   cascade through the mli and all other callers.
                   add_top_moderator (not add_moderator) so the creator can see the
                   Manage Moderators link immediately — default role is 'mod'. *)
                let%lwt _ = match%lwt Db.get_community_by_slug db slug with
                  | Ok (Some community) ->
                      let%lwt _ = Db.add_top_moderator db user_id community.id in
                      (* Auto-subscribe creator: join_community uses ON CONFLICT DO NOTHING,
                         so this is idempotent even on concurrent retries. Non-fatal. *)
                      Db.join_community db user_id community.id
                  | _ -> Lwt.return (Ok ())
                in
                Dream.redirect request "/"
            | Error _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Could not create community. The URL slug may already be taken." ~alert_type:"error" ~return_url:"/new-community" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Your form submission was invalid. Please try again." ~alert_type:"error" ~return_url:"/new-community" request)

let community_page_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in

  let page = match Dream.query request "page" with
    | Some p_str -> (try int_of_string p_str with _ -> 1)
    | None -> 1
  in
  let sort_mode = match Dream.query request "sort" with
    | Some s when s = "new" || s = "top" -> s
    | _ -> "hot"
  in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  Dream.sql request (fun db ->
    (* Lazy inactivity enforcement: runs on every community page load rather than a
       background job, trading occasional extra latency for zero infrastructure cost. *)
    let%lwt _ = Db.demote_inactive_mods db in
    match%lwt Db.get_community_by_slug db slug with
    | Ok (Some community) ->
        let%lwt posts = Db.get_posts_by_community db community.id sort_mode limit offset in
        let%lwt user_votes =
          if user_id > 0 then Db.get_user_post_votes db user_id else Lwt.return_ok []
        in
        let%lwt is_mem =
          if user_id > 0 then Db.is_member db user_id community.id else Lwt.return_ok false
        in
        (* get_community_mods_with_roles subsumes is_moderator: we derive both
           is_mod and is_top_mod from one query instead of two round-trips. *)
        let%lwt mods_res = Db.get_community_mods_with_roles db community.id in
        let%lwt admin_usernames_res = Db.get_admin_usernames db in
        let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
        let%lwt banned_res = Db.community_get_banned_users db community.id in
        let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: Db.user) -> u.username) bs | _ -> [] in
        let%lwt user_communities_res = if user_id > 0 then Db.get_user_communities db user_id else Lwt.return_ok [] in
        let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
        let%lwt moderated_communities_res = if user_id > 0 then Db.get_moderated_communities db user_id else Lwt.return_ok [] in
        let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
        (match posts, user_votes, is_mem with
         | Ok p, Ok v, Ok m ->
             let mods = match mods_res with Ok ms -> ms | _ -> [] in
             let mod_usernames = List.map (fun (e: Db.moderator_entry) -> e.username) mods in
             let is_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id) mods in
             let is_top_mod = user_id > 0 && List.exists (fun (e: Db.moderator_entry) -> e.user_id = user_id && e.role = "top_mod") mods in
             Dream.html (Pages.community_page ?user ~is_member:m ~is_current_user_mod:is_mod ~is_current_user_top_mod:is_top_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities v page sort_mode community p request)
         | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:"Failed to load community data. Please try again later." ~alert_type:"error" ~return_url:"/" request))
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
  )

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

          Dream.sql request (fun db ->
            match%lwt Db.join_community db (int_of_string uid) community_id with
            | Ok () -> Dream.redirect request (safe_local_redirect redirect_url)
            | Error _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to join community. Please try again." ~alert_type:"error" ~return_url:"/" request)
          )
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
          Dream.sql request (fun db ->
            match%lwt Db.leave_community db user_id community_id with
            | Ok () -> Dream.redirect request (safe_local_redirect redirect_to)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
          )
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
                      Dream.html (Pages.community_settings_page ?user ~community ~mods ~banned_users request)
                  | Error e -> Dream.html ("DB Error: " ^ e))
              | Error e -> Dream.html ("DB Error: " ^ e))
        | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
      )

let modlog_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  Dream.sql request (fun db ->
    match%lwt Db.get_community_by_slug db slug with
    | Ok (Some community) ->
        (match%lwt Db.get_modlog db community.id with
         | Ok actions -> Dream.html (Pages.mod_log_page ?user ~community actions request)
         | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
  )

let update_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let community_slug = List.assoc_opt "community_slug" form_data |> Option.value ~default:"" in
          if community_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else
          (* Empty string → None: lets mods clear a field without sending NULL hacks. *)
          let str_opt s = let t = String.trim s in if t = "" then None else Some t in
          let description = str_opt (List.assoc_opt "description" form_data |> Option.value ~default:"") in
          let rules      = str_opt (List.assoc_opt "rules"      form_data |> Option.value ~default:"") in
          let avatar_url = str_opt (List.assoc_opt "avatar_url" form_data |> Option.value ~default:"") in
          let banner_url = str_opt (List.assoc_opt "banner_url" form_data |> Option.value ~default:"") in
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
              (match%lwt Db.update_community_details db community_id description rules avatar_url banner_url with
              | Ok () -> Dream.redirect request ("/c/" ^ community_slug ^ "/settings")
              | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:("/c/" ^ community_slug ^ "/settings") request))
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

let add_mod_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let community_slug = List.assoc_opt "community_slug" form_data |> Option.value ~default:"" in
          let target_username = String.trim (List.assoc_opt "username" form_data |> Option.value ~default:"") in
          if community_id = 0 || target_username = "" then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form data." ~alert_type:"error" ~return_url:"/" request)
          else
          Dream.sql request (fun db ->
            (* Global admins bypass local mod check; local mods re-verified on
               each mutation to prevent TOCTOU between render and submission. *)
            let%lwt is_authorized =
              if is_admin then Lwt.return true
              else (match%lwt Db.is_moderator db user_id community_id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Access Denied" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:"/" request)
            else
              match%lwt Db.get_user_by_username db target_username with
              | Ok (Some target_user) ->
                  let%lwt _ = Db.add_moderator db target_user.id community_id in
                  Dream.redirect request ("/c/" ^ community_slug ^ "/settings")
              | Ok None -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"User Not Found" ~message:("No user was found with the username u/" ^ target_username ^ ".") ~alert_type:"error" ~return_url:("/c/" ^ community_slug ^ "/settings") request)
              | Error e -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("A database error occurred: " ^ e) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

let remove_mod_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let community_slug = List.assoc_opt "community_slug" form_data |> Option.value ~default:"" in
          let target_user_id = try int_of_string (List.assoc_opt "target_user_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if community_id = 0 || target_user_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form data." ~alert_type:"error" ~return_url:"/" request)
          else
          Dream.sql request (fun db ->
            (* Global admins bypass local mod check; local mods re-verified on
               each mutation to prevent TOCTOU between render and submission. *)
            let%lwt is_authorized =
              if is_admin then Lwt.return true
              else (match%lwt Db.is_moderator db user_id community_id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Access Denied" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:"/" request)
            else
              (* Re-count mods server-side: the render-time guard is advisory;
                 only this check is authoritative against the last-mod race. *)
              match%lwt Db.get_community_moderators db community_id with
              | Ok mods when List.length mods > 1 ->
                  (* Global admins have sovereign immunity: a local mod cannot demote
                     a site admin from mod status via the community settings UI. *)
                  (match%lwt Db.is_user_admin db target_user_id with
                  | Ok true ->
                      Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Action Denied" ~message:"You cannot remove a Global Administrator from moderation." ~alert_type:"error" ~return_url:("/c/" ^ community_slug ^ "/settings") request)
                  | _ ->
                      let%lwt _ = Db.remove_moderator db target_user_id community_id in
                      Dream.redirect request ("/c/" ^ community_slug ^ "/settings"))
              | Ok _ ->
                  Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Cannot Remove Moderator" ~message:"You cannot remove the last moderator of a community." ~alert_type:"error" ~return_url:("/c/" ^ community_slug ^ "/settings") request)
              | Error e -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("A database error occurred: " ^ e) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

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
                    let%lwt _ = Db.community_ban_user db target_user.id community_id in
                    (* Admin acting without mod role logged distinctly to prevent spoofing the mod log. *)
                    let is_admin_override = is_admin && not is_community_mod in
                    let action_type = if is_admin_override then "admin_ban_user" else "ban_user" in
                    let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                    let%lwt _ = Db.log_mod_action db community_id user_id action_type (Some target_user.id) logged_reason in
                    (* Notify banned user — no post_id since a ban is not tied to a single post. *)
                    let ban_msg = "You have been banned from a community. Reason: " ^ reason in
                    let%lwt _ = Db.create_notif db target_user.id None "mod_action" ban_msg in
                    let referer = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                    Dream.redirect request referer
                  end
              | Ok None -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"User Not Found" ~message:("No user was found with the username u/" ^ target_username ^ ".") ~alert_type:"error" ~return_url:"/" request)
              | Error e -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("A database error occurred: " ^ e) ~alert_type:"error" ~return_url:"/" request))
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
          let community_slug = List.assoc_opt "community_slug" form_data |> Option.value ~default:"" in
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
            else begin
              let%lwt _ = Db.community_unban_user db target_user_id community_id in
              Dream.redirect request ("/c/" ^ community_slug ^ "/settings")
            end
          )
      | _ -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

(* === POST === *)

let new_post_page request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      let community_slug_opt = Dream.query request "community" in

      match community_slug_opt with
      | Some slug ->
          Dream.sql request (fun db ->
            match%lwt Db.get_community_by_slug db slug with
            | Ok (Some community) ->
                (match%lwt Db.is_member db user_id community.id with
                | Ok true ->
                    Dream.html (Pages.new_post_form ?user community request)
                | Ok false ->
                    Dream.html (Pages.join_to_post_page ?user community request)
                | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request))

            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
          )
      | None ->
          Dream.sql request (fun db ->
            match%lwt Db.get_all_communities db with
            | Ok communities ->
                Dream.html (Pages.choose_community_page ?user communities)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
          )

let create_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id_str ->
      let user_id = int_of_string user_id_str in
      let username = Option.value (Dream.session_field request "username") ~default:"Someone" in

      match%lwt Dream.form request with
      | `Ok form_data ->
          let title = String.trim (List.assoc_opt "title" form_data |> Option.value ~default:"") in
          let community_id_str = List.assoc_opt "community_id" form_data |> Option.value ~default:"" in
          let url = match List.assoc_opt "url" form_data with Some "" | None -> None | Some u -> Some u in
          let content = match List.assoc_opt "content" form_data with Some "" | None -> None | Some c -> Some c in

          if title = "" then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Post title cannot be empty." ~alert_type:"error" ~return_url:"/" request)
          else if String.length title > 300 then
            Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Post title cannot exceed 300 characters." ~alert_type:"error" ~return_url:"/" request)
          else

          let community_id = try int_of_string community_id_str with _ -> 0 in
          if community_id = 0 then
            Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community selection." ~alert_type:"error" ~return_url:"/" request)
          else

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
                      (match%lwt Db.create_post db title url content community_id user_id with
                      | Ok new_post_id ->
                          (* Fan-out @mention notifications — best-effort, skips self-mentions. *)
                          let text = title ^ " " ^ (Option.value ~default:"" content) in
                          let%lwt () = Lwt_list.iter_s (fun uname ->
                            match%lwt Db.get_user_by_username db uname with
                            | Ok (Some mentioned) when mentioned.id <> user_id ->
                                let msg = username ^ " mentioned you in a post." in
                                let%lwt _ = Db.create_notif db mentioned.id (Some new_post_id) "mention" msg in
                                Lwt.return_unit
                            | _ -> Lwt.return_unit
                          ) (extract_mentions text) in
                          Dream.redirect request "/"
                      | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Error: " ^ err) ~alert_type:"error" ~return_url:"/" request)))
              | Ok false ->
                  Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not a Member" ~message:"You must join this community before you can post in it." ~alert_type:"error" ~return_url:"/" request)
              | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
          )
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
                   description = None; rules = None; avatar_url = None; banner_url = None }
        in
        (match comments_result, is_member_result with
        | Ok comments, Ok is_member ->
            let is_mod = match is_mod_res with Ok b -> b | _ -> false in
            let mod_usernames = match mods_res with Ok ms -> List.map (fun (u: Db.user) -> u.username) ms | _ -> [] in
            Dream.html (Pages.post_page ?user:user_sess ~is_member ~is_current_user_mod:is_mod ~mod_usernames ~admin_usernames ~banned_usernames ~community:community_for_page ~user_communities ~moderated_communities user_post_votes user_comment_votes post comments request)
        | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load post data. Please try again later." ~alert_type:"error" ~return_url:"/" request))

    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This post does not exist or has been deleted." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:user_sess ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
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
            let%lwt db_action =
              if is_admin || is_mod then Db.admin_delete_post db ~label:"[removed by admin]" post_id
              else Db.soft_delete_post db post_id user_id
            in
            match db_action with
            | Ok () ->
                let target = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request target
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
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
                Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
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
                    let%lwt delete_res = Db.mod_delete_post db post_id in
                    (match delete_res with
                    | Error err ->
                        Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    | Ok () ->
                        (* Admin acting without mod role: flag action_type and prefix reason
                           so the public mod_actions log explicitly shows "Admin Intervention". *)
                        let is_admin_override = is_admin && not is_community_mod in
                        let action_type = if is_admin_override then "admin_delete_post" else "delete_post" in
                        let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                        let%lwt _ = Db.log_mod_action db community.id user_id action_type (Some post_id) logged_reason in
                        (* Notify post author — best-effort; post is a tombstone at this point so get_post_owner still works. *)
                        let%lwt _ = match%lwt Db.get_post_owner db post_id with
                          | Ok author_id ->
                              let msg = "Your post was removed by a moderator. Reason: " ^ reason in
                              Db.create_notif db author_id (Some post_id) "mod_action" msg
                          | Error _ -> Lwt.return (Ok ())
                        in
                        Dream.redirect request ("/c/" ^ slug))
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
                Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
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
                    let%lwt delete_res = Db.mod_delete_comment db comment_id in
                    (match delete_res with
                    | Error err ->
                        Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    | Ok () ->
                        (* Admin acting without mod role: flag action_type and prefix reason
                           so the public mod_actions log explicitly shows "Admin Intervention". *)
                        let is_admin_override = is_admin && not is_community_mod in
                        let action_type = if is_admin_override then "admin_delete_comment" else "delete_comment" in
                        let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                        let%lwt _ = Db.log_mod_action db community.id user_id action_type (Some comment_id) logged_reason in
                        (* Notify comment author — fetch both owner and post_id for the notification link. *)
                        let%lwt _ = match%lwt Db.get_comment_owner db comment_id with
                          | Ok author_id ->
                              (match%lwt Db.get_comment_post_id db comment_id with
                              | Ok pid ->
                                  let msg = "Your comment was removed by a moderator. Reason: " ^ reason in
                                  Db.create_notif db author_id (Some pid) "mod_action" msg
                              | Error _ -> Lwt.return (Ok ()))
                          | Error _ -> Lwt.return (Ok ())
                        in
                        let referer = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/c/" ^ slug) in
                        Dream.redirect request referer)
          )
      | _ ->
          Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)

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
                    (match%lwt Db.create_comment db content post_id user_id parent_id_opt with
                    | Ok () ->
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
                        Dream.redirect request ("/p/" ^ string_of_int post_id)
                    | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)))
            | Ok None -> Dream.html (Pages.msg_page ~user:username ~title:"Post Not Found" ~message:"The post you tried to comment on could not be found." ~alert_type:"error" ~return_url:"/" request)
            | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.html (Pages.msg_page ~user:username ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

let delete_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let is_admin = Dream.session_field request "is_admin" = Some "true" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let comment_id = try int_of_string (List.assoc_opt "comment_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let community_id_opt = match List.assoc_opt "community_id" form_data with
            | Some s -> (try Some (int_of_string s) with _ -> None)
            | None -> None
          in
          if comment_id = 0 then Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid comment reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Dream.sql request (fun db ->
            (* community_id comes from a hidden form field (post.community_id set in post_page).
               Even if a client tampers the value, is_moderator checks the DB — a fake
               community_id returns false, so authorization is always correct server-side. *)
            let%lwt is_mod =
              if is_admin then Lwt.return false
              else
                match community_id_opt with
                | Some community_id ->
                    (match%lwt Db.is_moderator db user_id community_id with
                    | Ok b -> Lwt.return b
                    | _ -> Lwt.return false)
                | None -> Lwt.return false
            in
            (* Admin immunity: mods cannot delete comments authored by global admins. *)
            let%lwt blocked_by_immunity =
              if is_mod then
                (match%lwt Db.get_comment_owner db comment_id with
                | Ok owner_id ->
                    (match%lwt Db.is_user_admin db owner_id with
                    | Ok true -> Lwt.return true | _ -> Lwt.return false)
                | _ -> Lwt.return false)
              else Lwt.return false
            in
            if blocked_by_immunity then
              Dream.respond ~status:`Forbidden "⛔ You cannot moderate an Admin."
            else
            let%lwt db_action =
              if is_admin || is_mod then Db.admin_delete_comment db ~label:"[removed by admin]" comment_id
              else Db.soft_delete_comment db comment_id user_id
            in
            match db_action with
            | Ok () ->
                let referer = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
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
            let%lwt db_action =
              if direction = 0 then Db.remove_post_vote db user_id post_id
              else Db.vote_post db user_id post_id direction
            in

            match db_action with
            | Ok () ->
                let referer = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error ("DB Error: " ^ err)
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
            let%lwt db_action =
              if direction = 0 then Db.remove_comment_vote db user_id comment_id
              else Db.vote_comment db user_id comment_id direction
            in

            match db_action with
            | Ok () ->
                let referer = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error ("DB Error: " ^ err)
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."

(* === USER === *)

let view_profile_handler request =
  let username_param = Dream.param request "username" in
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let active_tab = Option.value ~default:"posts" (Dream.query request "tab") in

  Dream.sql request (fun db ->
    let%lwt user_votes = get_current_user_votes db request in
    match%lwt Db.get_user_public db username_param with
    | Ok (Some (uid, _, joined_at, bio, avatar_url)) ->

        (match%lwt Db.get_user_karma db uid with
        | Ok karma ->
            let%lwt admin_usernames_res = Db.get_admin_usernames db in
            let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
            let%lwt moderated_communities_res = Db.get_moderated_communities db uid in
            let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
            let%lwt is_gb_res = Db.is_globally_banned db uid in
            let is_globally_banned = match is_gb_res with Ok b -> b | Error _ -> false in

            (* Fetch only the data the active tab needs — avoids double DB round-trips. *)
            if active_tab = "comments" then
              (match%lwt Db.get_comments_by_user db uid with
              | Ok user_comments ->
                  Dream.html (Pages.user_profile_page ?user:current_user ~is_admin ~is_globally_banned ~profile_id:uid ~admin_usernames ~moderated_communities ~active_tab user_votes username_param joined_at bio avatar_url karma [] user_comments request)
              | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request))
            else
              (match%lwt Db.get_posts_by_user db uid with
              | Ok posts ->
                  Dream.html (Pages.user_profile_page ?user:current_user ~is_admin ~is_globally_banned ~profile_id:uid ~admin_usernames ~moderated_communities ~active_tab user_votes username_param joined_at bio avatar_url karma posts [] request)
              | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request))

        | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request))

    | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user:current_user ~title:"Not Found" ~message:"This user does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:current_user ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/" request)
  )

let settings_page_handler request =
  match Dream.session_field request "username" with
  | None -> Dream.redirect request "/login"
  | Some username ->
      Dream.sql request (fun db ->
        match%lwt Db.get_user_public db username with
        | Ok (Some (_, _, _, bio, avatar_url)) ->
            Dream.html (Pages.settings_page ~user:username bio avatar_url request)
        | _ -> Dream.redirect request "/login"
      )

let update_profile_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let bio = match List.assoc_opt "bio" form_data with Some "" | None -> None | Some b -> Some b in
          let avatar_url = match List.assoc_opt "avatar_url" form_data with Some "" | None -> None | Some a -> Some a in
          Dream.sql request (fun db ->
            match%lwt Db.update_user_profile db bio avatar_url user_id with
            | Ok () -> Dream.redirect request "/settings"
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/settings" request)
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
              | Ok (Some (_, _, _, hash, _, _)) ->
                  (match%lwt Auth.verify_password ~password:old_password ~hash with
                  | Ok true ->
                      (match%lwt Auth.hash_password new_password with
                      | Ok new_hash ->
                          (match%lwt Db.update_password db user_id new_hash with
                          | Ok () -> Dream.redirect request "/settings"
                          | Error err -> Dream.html (Pages.msg_page ~user:username ~title:"Error" ~message:("Database error: " ^ err) ~alert_type:"error" ~return_url:"/settings" request))
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
                ("Content-Disposition", Printf.sprintf "attachment; filename=\"%s_earde_export.json\"" username)
              ]
              json_str

        | _ -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to generate export data. Please try again." ~alert_type:"error" ~return_url:"/settings" request)
      )

(* GDPR Art. 17 (right to erasure): anonymize rather than hard-delete to preserve
   thread coherence; posts remain as [deleted] rather than leaving orphaned replies. *)
let delete_account_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Db.anonymize_user db user_id with
            | Ok () ->
                let%lwt () = Dream.invalidate_session request in
                Dream.redirect request "/"
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Error during account deletion: " ^ err) ~alert_type:"error" ~return_url:"/settings" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/settings" request)

(* === NOTIFICATIONS === *)

let notifications_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      Dream.sql request (fun db ->
        let%lwt notifs = Db.get_notifications db user_id in
        let%lwt _ = Db.mark_notifs_read db user_id in
        match notifs with
        | Ok n -> Dream.html (Pages.notifications_page ?user n request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
      )

let unread_notifs_api request =
  match Dream.session_field request "user_id" with
  | None -> Dream.respond "0"
  | Some uid_str ->
      Dream.sql request (fun db ->
        match%lwt Db.count_unread_notifs db (int_of_string uid_str) with
        | Ok c -> Dream.respond (string_of_int c)
        | Error _ -> Dream.respond "0"
      )

(* === LEGAL / PRIVACY === *)

let privacy_page_handler request =
  let user = Dream.session_field request "username" in
  Dream.html (Pages.privacy_page ?user request)

let about_page_handler request =
  let user = Dream.session_field request "username" in
  Dream.html (Pages.about_page ?user request)

(* === ADMIN === *)

(* KPI dashboard exposes aggregate user/post/engagement metrics — admin-only.
   No session check existed before; added to prevent data leakage to any visitor. *)
let hq_dashboard_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" ->
      Dream.sql request (fun db ->
        match%lwt Db.get_kpi_dashboard db with
        | Ok stats -> Dream.html (Pages.hq_dashboard_page stats)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Analytics error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
      )
  | _ -> Dream.respond ~status:`Forbidden
      (Pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

let ban_user_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" ->
      let user_id_to_ban = try int_of_string (Dream.param request "id") with _ -> 0 in
      if user_id_to_ban = 0 then Dream.respond ~status:`Bad_Request "Invalid user ID." else
      Dream.sql request (fun db ->
        match%lwt Db.ban_user db user_id_to_ban with
        | Ok () ->
            (* Notify banned user — best-effort; no post to link to. *)
            let%lwt _ = Db.create_notif db user_id_to_ban None "mod_action" "You have been globally banned by an administrator." in
            (* Redirect back to the profile page rather than "/" so the admin
               immediately sees the updated 🚫 badge and the Unban button. *)
            let target = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/") in
            Dream.redirect request target
        | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Error banning user: " ^ err) ~alert_type:"error" ~return_url:"/admin" request)
      )
  | _ -> Dream.html (Pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

let unban_user_global_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" ->
      let user_id_to_unban = try int_of_string (Dream.param request "id") with _ -> 0 in
      if user_id_to_unban = 0 then Dream.respond ~status:`Bad_Request "Invalid user ID." else
      Dream.sql request (fun db ->
        match%lwt Db.unban_user_global db user_id_to_unban with
        | Ok () ->
            let target = safe_local_redirect (match Dream.header request "Referer" with Some r -> r | None -> "/admin") in
            Dream.redirect request target
        | Error err -> Dream.html (Pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:("Error unbanning user: " ^ err) ~alert_type:"error" ~return_url:"/admin" request)
      )
  | _ -> Dream.html (Pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

let admin_dashboard_handler request =
  match Dream.session_field request "is_admin" with
  | Some "true" ->
      let user = Dream.session_field request "username" in
      Dream.sql request (fun db ->
        match%lwt Db.get_globally_banned_users db with
        | Ok banned_users -> Dream.html (Pages.admin_dashboard_page ?user ~banned_users request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
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
              (match%lwt Db.get_community_mods_with_roles db community.id with
               | Ok mods ->
                   Dream.html (Pages.manage_mods_page ?user ~is_admin ~current_user_role ~community ~mods request)
               | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
        | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
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
                   | Error e -> Dream.html (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request))
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
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
                   | Error msg -> Dream.html (Pages.msg_page ?user ~title:"Promotion Failed" ~message:msg ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request))
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
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
                        | Error e -> Dream.html (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request))
                   | Error e -> Dream.html (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request))
            | Ok None -> Dream.respond ~status:`Not_Found (Pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Pages.msg_page ?user ~title:"Error" ~message:("Database error: " ^ e) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/manage-mods") request)

(* === MIDDLEWARE === *)

(* Skip dashboard and unread-notifs paths: dashboard reads page_view data (recursive
   self-counting); unread-notifs is polled every page load and would inflate counts. *)
let analytics_middleware inner_handler request =
  let path = Dream.target request in
  let user_id_opt = Dream.session_field request "user_id" in

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
    match Dream.header request "User-Agent" with
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

  let%lwt _ =
    if path = "/earde-hq-dashboard" || path = "/api/unread-notifs" || is_static || is_bot
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
        match user_id_opt with
        | Some uid_str ->
            let%lwt _ = Db.touch_user_active db (int_of_string uid_str) in
            Lwt.return_unit
        | None -> Lwt.return_unit
      )
    end
  in
  inner_handler request
