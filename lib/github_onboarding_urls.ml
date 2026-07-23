(* Pure construction of the two GitHub-hosted entry URLs for onboarding. The
   scheme and host are fixed literals: deriving them from configuration would
   let a bad environment value send users — and the raw one-time state — to an
   attacker-chosen host. [Uri] performs all query encoding, so opaque values
   can never break out of their own parameter. *)

let github_url ~path ~query =
  Uri.to_string (Uri.make ~scheme:"https" ~host:"github.com" ~path ~query ())

let installation_url config ~state =
  github_url
    ~path:("/apps/" ^ Github_app_config.app_slug config ^ "/installations/new")
    ~query:[ ("state", [ Github_onboarding_crypto.state_to_string state ]) ]

let authorization_url config ~state ~code_challenge =
  github_url ~path:"/login/oauth/authorize"
    ~query:
      [
        ("client_id", [ Github_app_config.client_id config ]);
        ("redirect_uri", [ Github_app_config.callback_url config ]);
        ("state", [ Github_onboarding_crypto.state_to_string state ]);
        ( "code_challenge",
          [ Github_onboarding_pkce.challenge_to_string code_challenge ] );
        ("code_challenge_method", [ "S256" ]);
      ]
