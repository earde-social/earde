module Phvf = Earde.Project_home_provisioning_form

(* === Initial community-identity form (Project_home_provisioning_form) ===
   The strict parser behind the future POST /projects/:slug/community-home:
   exact field grammar, the canonicalization the future provisioning store
   inherits, and the payload-free error contract. Pure — no DB, no request,
   no session. *)

let phvf_case = Case.quick

let phvf_expect label expected fields =
  match Phvf.of_fields fields with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Home_provisioning_fixture.phvf_err expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Home_provisioning_fixture.phvf_err expected)
        (Home_provisioning_fixture.phvf_err e)

let phvf_grammar_cases =
  [
    phvf_case "form: a valid submission keeps every canonical value exactly"
      (fun () ->
        let parsed =
          Home_provisioning_fixture.phvf_ok "valid"
            (Home_provisioning_fixture.phvf_fields ~name:"Phvf Community"
               ~slug:"phvf-community" ~description:"A durable description." ())
        in
        Alcotest.(check string)
          "name" "Phvf Community"
          (Phvf.community_name parsed);
        Alcotest.(check string)
          "slug" "phvf-community"
          (Phvf.community_slug parsed);
        Alcotest.(check (option string))
          "description" (Some "A durable description.")
          (Phvf.community_description parsed));
    phvf_case "form: field order is irrelevant" (fun () ->
        let parsed =
          Home_provisioning_fixture.phvf_ok "reordered"
            [
              ("community_description", "Body");
              ("community_slug", "phvf-order");
              ("community_name", "Phvf Order");
            ]
        in
        Alcotest.(check string) "name" "Phvf Order" (Phvf.community_name parsed);
        Alcotest.(check string) "slug" "phvf-order" (Phvf.community_slug parsed);
        Alcotest.(check (option string))
          "description" (Some "Body")
          (Phvf.community_description parsed));
    phvf_case "form: every missing field is the same structural rejection"
      (fun () ->
        List.iter
          (fun drop ->
            let fields =
              List.filter
                (fun (k, _) -> k <> drop)
                (Home_provisioning_fixture.phvf_fields ())
            in
            phvf_expect ("missing " ^ drop) Phvf.Invalid_form fields)
          [ "community_name"; "community_slug"; "community_description" ];
        phvf_expect "empty submission" Phvf.Invalid_form []);
    phvf_case "form: every duplicated field is the same structural rejection"
      (fun () ->
        List.iter
          (fun (k, v) ->
            phvf_expect ("duplicate " ^ k) Phvf.Invalid_form
              (Home_provisioning_fixture.phvf_fields () @ [ (k, v) ]))
          [
            ("community_name", "Phvf Community");
            ("community_slug", "phvf-community");
            ("community_description", "");
          ]);
    phvf_case "form: unknown, case-variant, and padded field names reject"
      (fun () ->
        List.iter
          (fun name ->
            phvf_expect ("unknown " ^ name) Phvf.Invalid_form
              (Home_provisioning_fixture.phvf_fields () @ [ (name, "x") ]))
          [
            "Community_name";
            "COMMUNITY_SLUG";
            " community_name";
            "community_name ";
            "community_name\t";
            "communityname";
            "community_visibility";
            "project_slug";
            "project_id";
            "user_id";
            "return_url";
            "publish";
            "";
          ];
        (* A recognized field spelled differently is not a substitute. *)
        phvf_expect "case-variant replacement" Phvf.Invalid_form
          [
            ("Community_name", "Phvf");
            ("community_slug", "phvf-x");
            ("community_description", "");
          ]);
    phvf_case
      "form: a dream.csrf field reaching the parser is a wiring bug, not an \
       accepted field" (fun () ->
        phvf_expect "csrf present" Phvf.Invalid_form
          (("dream.csrf", "token") :: Home_provisioning_fixture.phvf_fields ());
        phvf_expect "csrf last" Phvf.Invalid_form
          (Home_provisioning_fixture.phvf_fields ()
          @ [ ("dream.csrf", "token") ]));
    phvf_case "form: a structural failure never becomes a semantic one"
      (fun () ->
        (* Malformed name and slug plus an unknown field: the answer names
           no field at all. *)
        phvf_expect "structure wins" Phvf.Invalid_form
          [
            ("community_name", "   ");
            ("community_slug", "NOPE");
            ("community_description", "");
            ("extra", "x");
          ]);
  ]

let phvf_name_cases =
  [
    phvf_case
      "form name: outer ASCII whitespace is trimmed, inner bytes are untouched"
      (fun () ->
        let parsed =
          Home_provisioning_fixture.phvf_ok "trimmed"
            (Home_provisioning_fixture.phvf_fields ~name:"  Progetto Phvf \t\n"
               ())
        in
        Alcotest.(check string)
          "canonical name" "Progetto Phvf"
          (Phvf.community_name parsed);
        (* No lowercasing, no Unicode rewriting. *)
        let accented =
          Home_provisioning_fixture.phvf_ok "accented"
            (Home_provisioning_fixture.phvf_fields
               ~name:("Perch" ^ Home_provisioning_fixture.phvf_scalar)
               ())
        in
        Alcotest.(check string)
          "bytes preserved"
          ("Perch" ^ Home_provisioning_fixture.phvf_scalar)
          (Phvf.community_name accented));
    phvf_case "form name: blank names reject" (fun () ->
        List.iter
          (fun name ->
            phvf_expect
              ("blank " ^ String.escaped name)
              Phvf.Invalid_community_name
              (Home_provisioning_fixture.phvf_fields ~name ()))
          [ ""; " "; "\t"; "\n"; "\r"; "\x0c"; "\x0b"; "   \t\n  " ]);
    phvf_case
      "form name: the maximum is counted in scalars, not bytes, and nothing is \
       truncated" (fun () ->
        let at_max =
          Home_provisioning_fixture.phvf_repeat
            Home_provisioning_fixture.phvf_scalar 120
        in
        let parsed =
          Home_provisioning_fixture.phvf_ok "120 scalars"
            (Home_provisioning_fixture.phvf_fields ~name:at_max ())
        in
        Alcotest.(check string) "kept whole" at_max (Phvf.community_name parsed);
        Alcotest.(check int)
          "240 bytes accepted" 240
          (String.length (Phvf.community_name parsed));
        phvf_expect "121 scalars" Phvf.Invalid_community_name
          (Home_provisioning_fixture.phvf_fields
             ~name:
               (Home_provisioning_fixture.phvf_repeat
                  Home_provisioning_fixture.phvf_scalar 121)
             ());
        phvf_expect "121 ASCII" Phvf.Invalid_community_name
          (Home_provisioning_fixture.phvf_fields ~name:(String.make 121 'a') ());
        let ascii_max =
          Home_provisioning_fixture.phvf_ok "120 ASCII"
            (Home_provisioning_fixture.phvf_fields ~name:(String.make 120 'a')
               ())
        in
        Alcotest.(check int)
          "120 ASCII kept" 120
          (String.length (Phvf.community_name ascii_max)));
    phvf_case "form name: invalid UTF-8, NUL, controls, and DEL reject"
      (fun () ->
        List.iter
          (fun name ->
            phvf_expect
              ("hostile " ^ String.escaped name)
              Phvf.Invalid_community_name
              (Home_provisioning_fixture.phvf_fields ~name ()))
          [
            "Phvf\xff";
            "\xc3";
            "\xed\xa0\x80";
            "Phvf\x00Community";
            "Phvf\x01";
            "Phvf\x1f";
            "Phvf\x7f";
            "Phvf\tCommunity";
            "Phvf\nCommunity";
            "Phvf\rCommunity";
          ]);
  ]

let phvf_slug_cases =
  [
    phvf_case
      "form slug: the canonical grammar is accepted exactly as submitted"
      (fun () ->
        List.iter
          (fun slug ->
            let parsed =
              Home_provisioning_fixture.phvf_ok ("valid " ^ slug)
                (Home_provisioning_fixture.phvf_fields ~slug ())
            in
            Alcotest.(check string)
              ("byte-identical " ^ slug) slug
              (Phvf.community_slug parsed))
          [
            "a";
            "z9";
            "phvf";
            "phvf-community";
            "a-b-c-d";
            "0";
            "0-1";
            String.make 80 'a';
            "new";
          ]);
    phvf_case "form slug: nothing is trimmed, lowercased, or repaired"
      (fun () ->
        List.iter
          (fun slug ->
            phvf_expect
              ("noncanonical " ^ String.escaped slug)
              Phvf.Invalid_community_slug
              (Home_provisioning_fixture.phvf_fields ~slug ()))
          [
            " phvf";
            "phvf ";
            " phvf ";
            "\tphvf";
            "phvf\n";
            "PHVF";
            "Phvf-Community";
            "phvF";
          ]);
    phvf_case "form slug: structural violations reject" (fun () ->
        List.iter
          (fun slug ->
            phvf_expect
              ("malformed " ^ String.escaped slug)
              Phvf.Invalid_community_slug
              (Home_provisioning_fixture.phvf_fields ~slug ()))
          [
            "";
            "-";
            "-phvf";
            "phvf-";
            "phvf--community";
            "phvf_community";
            "phvf.community";
            "phvf/community";
            "phvf community";
            "phvf%20x";
            "phvf\x00";
            "phvf\x7f";
            "phvf" ^ Home_provisioning_fixture.phvf_scalar;
            "../phvf";
            "phvf?x=1";
            "phvf#a";
          ]);
    phvf_case "form slug: the length boundary is exact" (fun () ->
        let at_max = String.make 80 'a' in
        Alcotest.(check string)
          "80 accepted" at_max
          (Phvf.community_slug
             (Home_provisioning_fixture.phvf_ok "80"
                (Home_provisioning_fixture.phvf_fields ~slug:at_max ())));
        phvf_expect "81" Phvf.Invalid_community_slug
          (Home_provisioning_fixture.phvf_fields ~slug:(String.make 81 'a') ()));
  ]

let phvf_description_cases =
  [
    phvf_case "form description: blank input canonicalizes to no description"
      (fun () ->
        List.iter
          (fun description ->
            let parsed =
              Home_provisioning_fixture.phvf_ok
                ("blank " ^ String.escaped description)
                (Home_provisioning_fixture.phvf_fields ~description ())
            in
            Alcotest.(check (option string))
              ("None for " ^ String.escaped description)
              None
              (Phvf.community_description parsed))
          [ ""; " "; "\t"; "\n"; "\r\n"; "   \t \n  " ]);
    phvf_case "form description: multiline text survives with LF endings"
      (fun () ->
        let parsed =
          Home_provisioning_fixture.phvf_ok "multiline"
            (Home_provisioning_fixture.phvf_fields
               ~description:"  First line\r\nSecond\rThird\tcol  " ())
        in
        Alcotest.(check (option string))
          "normalized and trimmed" (Some "First line\nSecond\nThird\tcol")
          (Phvf.community_description parsed));
    phvf_case
      "form description: the maximum is counted in scalars and nothing is \
       truncated" (fun () ->
        let at_max =
          Home_provisioning_fixture.phvf_repeat
            Home_provisioning_fixture.phvf_scalar 2000
        in
        let parsed =
          Home_provisioning_fixture.phvf_ok "2000 scalars"
            (Home_provisioning_fixture.phvf_fields ~description:at_max ())
        in
        Alcotest.(check (option string))
          "kept whole" (Some at_max)
          (Phvf.community_description parsed);
        phvf_expect "2001 scalars" Phvf.Invalid_community_description
          (Home_provisioning_fixture.phvf_fields
             ~description:
               (Home_provisioning_fixture.phvf_repeat
                  Home_provisioning_fixture.phvf_scalar 2001)
             ());
        phvf_expect "2001 ASCII" Phvf.Invalid_community_description
          (Home_provisioning_fixture.phvf_fields
             ~description:(String.make 2001 'a') ()));
    phvf_case
      "form description: invalid UTF-8, NUL, and forbidden controls reject"
      (fun () ->
        List.iter
          (fun description ->
            phvf_expect
              ("hostile " ^ String.escaped description)
              Phvf.Invalid_community_description
              (Home_provisioning_fixture.phvf_fields ~description ()))
          [
            "Body\xff";
            "\xc3";
            "Body\x00";
            "Body\x01";
            "Body\x1f";
            "Body\x7f";
            "Bo\x0bdy";
            "Bo\x0cdy";
            "Bo\rdy\x00";
          ];
        (* A trailing VT or FF is outer whitespace: it is trimmed away, so
           the surviving text is ordinary and accepted. *)
        List.iter
          (fun description ->
            Alcotest.(check (option string))
              ("trimmed " ^ String.escaped description)
              (Some "Body")
              (Phvf.community_description
                 (Home_provisioning_fixture.phvf_ok "trimmed control"
                    (Home_provisioning_fixture.phvf_fields ~description ()))))
          [ "Body\x0b"; "Body\x0c"; "\x0bBody\x0c" ]);
    phvf_case "form description: no Markdown or HTML processing happens here"
      (fun () ->
        let raw = "**bold** <script>alert(1)</script> [x](y)" in
        let parsed =
          Home_provisioning_fixture.phvf_ok "raw"
            (Home_provisioning_fixture.phvf_fields ~description:raw ())
        in
        Alcotest.(check (option string))
          "byte-identical" (Some raw)
          (Phvf.community_description parsed));
    phvf_case "form: validation order is name, then slug, then description"
      (fun () ->
        phvf_expect "name first" Phvf.Invalid_community_name
          (Home_provisioning_fixture.phvf_fields ~name:"" ~slug:"NOPE"
             ~description:"Body\x00" ());
        phvf_expect "slug second" Phvf.Invalid_community_slug
          (Home_provisioning_fixture.phvf_fields ~name:"Phvf" ~slug:"NOPE"
             ~description:"Body\x00" ());
        phvf_expect "description last" Phvf.Invalid_community_description
          (Home_provisioning_fixture.phvf_fields ~name:"Phvf" ~slug:"phvf-x"
             ~description:"Body\x00" ()));
  ]

let phvf_suite =
  phvf_grammar_cases @ phvf_name_cases @ phvf_slug_cases
  @ phvf_description_cases

let suites =
  (* Initial community-identity form for a dedicated project home: the
       exact field grammar, the name/slug/description canonicalization the
       future provisioning store inherits, boundary and hostile-byte
       coverage, and the payload-free error contract. DB-free. *)
  [ ("project_home_provisioning_form", phvf_suite) ]
