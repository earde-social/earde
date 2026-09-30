let check_parse name expected body =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool)
        name expected
        (Earde.Turnstile.parse_siteverify body))

let check_random name expected username =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool)
        name expected
        (Earde.Admin_pages.looks_random_username username))

let suites =
  (* Turnstile siteverify response parsing. Pure, no network — fail closed on
       anything that is not an explicit {"success": true}. *)
  [
    ( "turnstile_parse",
      [
        check_parse "success true" true {|{"success": true}|};
        check_parse "success true with extras" true
          {|{"success": true, "challenge_ts": "2026-06-16T00:00:00Z", "hostname": "earde.com"}|};
        check_parse "success false" false
          {|{"success": false, "error-codes": ["invalid-input-response"]}|};
        check_parse "success missing" false {|{"hostname": "earde.com"}|};
        check_parse "success non-bool" false {|{"success": "true"}|};
        check_parse "empty object" false {|{}|};
        check_parse "non-object body" false {|"success"|};
        check_parse "malformed json" false {|not json at all|};
        check_parse "empty string" false "";
      ] )
    (* Suspicious-username heuristic — pure, display-only. Human-looking handles must
       not be flagged; bot-like ones should. No DB, no network. *);
    ( "looks_random_username",
      [
        check_random "human alice" false "alice";
        check_random "human damiano" false "damiano";
        check_random "human snake_case" false "john_doe";
        check_random "human with digits" false "kevin99";
        check_random "human long" false "mariarossi";
        check_random "short ignored" false "xkq";
        check_random "digit heavy" true "a8f3k9d2";
        check_random "no vowels" true "xkqjwzbf";
        check_random "long consonant run" true "bcdfghjk";
        check_random "mixed bot" true "tbvkxwlqz";
      ] );
  ]
