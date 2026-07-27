(** /bring — server-rendered entry and return page for the GitHub project
    onboarding flow. Pure rendering: the handler derives the closed access
    state and one-time callback feedback; this module never reads the
    environment, the session, the query string, or the database. *)

type access =
  | Onboarding_disabled
      (** Mode [Off]: onboarding is unavailable to everyone. *)
  | Login_required
      (** The mode permits onboarding, but there is no valid authenticated
          user (a session [user_id] counts only when it parses as a positive
          integer). *)
  | Rollout_limited
      (** Mode [Admins] and the authenticated viewer is not a global
          admin. *)
  | Ready
      (** The authenticated viewer passes
          [Project_onboarding.onboarding_available]. *)

type feedback =
  | Connected  (** The OAuth callback reported [github=connected]. *)
  | Failed  (** The OAuth callback reported [github=failed]. *)

val bring_page :
  ?user:string ->
  ?request:Dream.request ->
  access:access ->
  feedback:feedback option ->
  unit ->
  string
(** The complete /bring page in the shared auth-card layout. Only [Ready]
    renders the single POST form to /integrations/github/install/start (no
    hidden fields — the start handler repeats every access and same-origin
    check); [Login_required] renders a plain /login link instead. Feedback
    renders at most one banner — the success copy never claims a community
    was created or repositories synchronized, and the failure copy is one
    generic sentence that keeps the failed stage indistinguishable. [None]
    renders no banner element at all. Feedback never changes which action
    (if any) is offered. *)
