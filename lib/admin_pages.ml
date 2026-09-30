(* Display-only heuristic for the admin dashboard: flags bot-like usernames (the
   signup incident produced runs of random handles). Deliberately conservative and
   imperfect — it only paints a UI chip, never gates an action or touches the DB. Pure,
   so it is unit-tested in test_earde.ml. A name is "random-looking" when digits dominate,
   it has no vowels at all, it has a long consonant run, or its vowel ratio is very low. *)
let looks_random_username raw =
  let s = String.lowercase_ascii raw in
  let n = String.length s in
  if n < 5 then false
  else begin
    let is_vowel c = c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' in
    let letters = ref 0 and vowels = ref 0 and digits = ref 0 in
    let cur_run = ref 0 and max_run = ref 0 in
    String.iter (fun c ->
      if c >= '0' && c <= '9' then (incr digits; cur_run := 0)
      else if c >= 'a' && c <= 'z' then begin
        incr letters;
        if is_vowel c then (incr vowels; cur_run := 0)
        else (incr cur_run; if !cur_run > !max_run then max_run := !cur_run)
      end else cur_run := 0
    ) s;
    let digit_ratio = float_of_int !digits /. float_of_int n in
    let vowel_ratio = if !letters = 0 then 0.0 else float_of_int !vowels /. float_of_int !letters in
    digit_ratio >= 0.4
    || (!letters >= 5 && !vowels = 0)
    || !max_run >= 5
    || (!letters >= 6 && vowel_ratio < 0.15)
  end

(* Global admin dashboard on the Cartographic Civic launch shell (pass 18A):
   launch_app_page under body.launch-global-admin — earde.css only, no
   legacy per-page CSS, no Tailwind, no Google Fonts. Read-only operational panels
   (status / recent users / pending signups) plus the preserved global unban
   action are spliced verbatim below a serif Administration head; the legacy
   admin-head is the only markup replaced (its KPI-dashboard link is gone —
   KPI monitoring lives in PostHog now). Authorization is enforced by
   admin_dashboard_handler (is_admin session field); this renderer assumes it.
   All config status is a safe boolean/label — no secret value (site key, API
   key, URL) is ever rendered. [rail_communities] feeds the shared launch rail
   only. *)
let admin_dashboard_page ?user ?(rail_communities = []) ~signups_enabled
    ~(turnstile : [ `Configured | `Disabled | `Misconfigured ]) ~brevo_configured
    ~(recent_users : Admin_store.admin_recent_user list) ~(pending : Admin_store.pending_signup_row list)
    ~(banned_users : User_store.user list) request =
  let csrf_token = Dream.csrf_tag request in
  let esc = Components.html_escape in
  (* "recently created" cutoff as an ISO-ish string; created_at::text sorts lexically the
     same as chronologically, so a string compare avoids any timestamp parsing. *)
  let recent_cutoff =
    let tm = Unix.gmtime (Unix.gettimeofday () -. 86400.) in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  let stat label cls value =
    Printf.sprintf
      "<div class='admin-stat'><span class='admin-stat-label'>%s</span><span class='admin-stat-val %s'>%s</span></div>"
      label cls value
  in
  let status_panel =
    let signups_row =
      if signups_enabled then stat "signups" "admin-stat-val--ok" "enabled"
      else stat "signups" "admin-stat-val--off" "closed"
    in
    let turnstile_row = match turnstile with
      | `Configured   -> stat "turnstile (bot check)" "admin-stat-val--ok"   "required &amp; configured"
      | `Disabled     -> stat "turnstile (bot check)" "admin-stat-val--off"  "disabled · dev bypass"
      | `Misconfigured -> stat "turnstile (bot check)" "admin-stat-val--bad" "misconfigured"
    in
    let brevo_row =
      if brevo_configured then stat "email (brevo)" "admin-stat-val--ok" "configured"
      else stat "email (brevo)" "admin-stat-val--warn" "not configured"
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Signup &amp; security status</h2>\
         <p class='admin-panel-desc'>Operational config flags only — no secret values are shown.</p>\
         <div class='admin-stats'>%s%s%s</div>\
       </section>"
      signups_row turnstile_row brevo_row
  in
  let recent_users_panel =
    let rows =
      if recent_users = [] then
        "<tr><td colspan='6' class='admin-empty'>No users yet.</td></tr>"
      else
        String.concat "\n" (List.map (fun (u : Admin_store.admin_recent_user) ->
          let badges =
            (if u.is_admin  then "<span class='admin-badge admin-badge--admin'>admin</span>" else "")
            ^ (if u.is_banned then "<span class='admin-badge admin-badge--banned'>banned</span>" else "")
          in
          let total = u.post_count + u.comment_count + u.message_count in
          let flags =
            let fs =
              (if total = 0 then ["<span class='admin-flag'>no activity</span>"] else [])
              @ (if looks_random_username u.username then ["<span class='admin-flag'>random name?</span>"] else [])
              @ (if u.created_at >= recent_cutoff then ["<span class='admin-flag admin-flag--quiet'>new &lt;24h</span>"] else [])
            in
            if fs = [] then "<span class='admin-cell-muted'>—</span>" else String.concat "" fs
          in
          let num c = Printf.sprintf "<td class='admin-num%s'>%d</td>" (if c = 0 then " admin-num--zero" else "") c in
          Printf.sprintf
            "<tr>\
               <td><a class='admin-user-link' href='/u/%s'>%s</a>%s</td>\
               <td class='admin-cell-muted'>%s</td>%s%s%s\
               <td>%s</td>\
             </tr>"
            (esc u.username) (esc u.username) badges
            (Components.time_ago u.created_at)
            (num u.post_count) (num u.comment_count) (num u.message_count)
            flags
        ) recent_users)
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Recent users</h2>\
         <p class='admin-panel-desc'>Latest %d accounts by signup time, with activity counts and quick suspicious-signal flags.</p>\
         <div class='admin-table-wrap'><table class='admin-table ph-no-capture'>\
           <thead><tr><th>User</th><th>Joined</th><th>Posts</th><th>Comments</th><th>Msgs</th><th>Signals</th></tr></thead>\
           <tbody>%s</tbody>\
         </table></div>\
       </section>"
      (List.length recent_users) rows
  in
  let pending_panel =
    let rows =
      if pending = [] then
        "<tr><td colspan='5' class='admin-empty'>No active pending signups.</td></tr>"
      else
        String.concat "\n" (List.map (fun (p : Admin_store.pending_signup_row) ->
          let ip = match p.ip_address with Some s when s <> "" -> esc s | _ -> "—" in
          Printf.sprintf
            "<tr>\
               <td class='admin-cell-mono'>%s</td>\
               <td class='admin-cell-muted'>%s</td>\
               <td class='admin-cell-muted'>%s</td>\
               <td class='admin-cell-muted'>%s</td>\
               <td class='admin-cell-mono'>%s</td>\
             </tr>"
            (esc p.username) (esc p.email)
            (Components.time_ago p.created_at) (Components.time_ago p.expires_at) ip
        ) pending)
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Pending signups</h2>\
         <p class='admin-panel-desc'>Unconfirmed signups still within their 24h window (latest %d). These have not become user accounts yet.</p>\
         <div class='admin-table-wrap'><table class='admin-table ph-no-capture'>\
           <thead><tr><th>Username</th><th>Email</th><th>Requested</th><th>Expires</th><th>IP</th></tr></thead>\
           <tbody>%s</tbody>\
         </table></div>\
       </section>"
      (List.length pending) rows
  in
  let banned_panel =
    let rows =
      if banned_users = [] then
        "<tr><td colspan='3' class='admin-empty'>No users are currently globally banned.</td></tr>"
      else
        String.concat "\n" (List.map (fun (u : User_store.user) ->
          Printf.sprintf
            "<tr>\
               <td><a class='admin-user-link' href='/u/%s'>%s</a></td>\
               <td class='admin-cell-muted'>%s</td>\
               <td>\
                 <form class='admin-act-form' action='/admin/unban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/%s?')\">\
                   %s\
                   <button type='submit' class='admin-btn-unban'>Unban</button>\
                 </form>\
               </td>\
             </tr>"
            (esc u.username) (esc u.username) (esc u.email) u.id
            (* JS string literal inside an HTML attribute: html_escape alone
               decodes back to a live apostrophe before JavaScript parses it. *)
            (Components.js_single_quoted_attr u.username) csrf_token
        ) banned_users)
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Globally banned users</h2>\
         <p class='admin-panel-desc'>These accounts are blocked from logging in and posting anywhere on Earde.</p>\
         <div class='admin-table-wrap'><table class='admin-table ph-no-capture'>\
           <thead><tr><th>User</th><th>Email</th><th>Action</th></tr></thead>\
           <tbody>%s</tbody>\
         </table></div>\
       </section>"
      rows
  in
  (* Serif civic head band outside the panel stack. KPI monitoring moved to
     PostHog, so the head carries no dashboard link. *)
  let page_head =
    "<div class='page__head'><div class='page__head-inner page__head-inner--list'>\
     <h1 class='page__title'>Administration</h1>\
     <p class='page__sub launch-admin-ctx'>/admin &middot; global registry &mdash; signup status, accounts, bans</p>\
     </div></div>"
  in
  let content = Printf.sprintf
    "%s<div class='scroll'><div class='container container--list'>\
     <div class='admin-wrap'>%s%s%s%s</div>\
     </div></div>"
    page_head status_panel recent_users_panel pending_panel banned_panel
  in
  Page_shell.launch_app_page ~noindex:true ?user ~request ~rail_communities
    ~page_class:"launch-global-admin" ~title:"Admin Dashboard" ~content ()
