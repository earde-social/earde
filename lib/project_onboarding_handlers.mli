(** HTTP layer for the GitHub onboarding entry point (GET /bring), kept out of
    the legacy [Handlers] macro-module. *)

val bring_page_handler : Dream.handler
(** GET /bring — public informational page, readable anonymously. Reads the
    optional session (username + is_admin, the same fields that gate /admin)
    and the closed mode via [Project_onboarding.mode_from_env]; no database
    access. *)
