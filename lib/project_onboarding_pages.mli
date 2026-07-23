(** /bring — server-rendered entry point for the GitHub-anchored open-source
    onboarding. Pure: the closed mode and viewer context are arguments; no
    environment or session reads happen in this module. *)

val bring_page :
  ?user:string ->
  ?request:Dream.request ->
  is_admin:bool ->
  mode:Project_onboarding.mode ->
  unit ->
  string
(** Render the page for [mode] as seen by the viewer ([user] = session
    username, [is_admin] = global-admin status the handler resolved).
    [request] only feeds the shared layout (analytics/identity attributes);
    rendering works without it, which is what the tests use. Always rendered
    noindex — this is a transitional onboarding surface. *)
