(* HTTP layer for the GitHub onboarding entry point, kept out of the legacy
   Handlers macro-module per the feature-module guideline. *)

(* GET /bring — public informational page; no login requirement. Viewer
   identity comes from the same session fields the rest of the app trusts
   (username + is_admin, set at login and used to gate /admin), so no database
   query is needed — anonymous visitors cost nothing. The mode is re-read from
   the environment on every request via Project_onboarding.mode_from_env;
   deliberately uncached, matching that module's contract. *)
let bring_page_handler request =
  let user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let mode = Project_onboarding.mode_from_env () in
  Dream.html (Project_onboarding_pages.bring_page ?user ~request ~is_admin ~mode ())
