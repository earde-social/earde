module Phvf = Earde.Project_home_provisioning_form
module Ncpf = Earde.Network_community_publication_form

(* === Network-community publication form (Network_community_publication_form) ===
   The final setup submission of a provisioned network community: the exact
   four-field grammar, the publication vocabulary, and the delegation of the
   whole identity half to the frozen provisioning-form policy. Pure — no DB,
   no request, no session. *)

let ncpf_case = Case.quick

let ncpf_expect label expected fields =
  match Ncpf.of_fields fields with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (Network_community_fixture.ncpf_err expected)
  | Error e -> Alcotest.(check string) label (Network_community_fixture.ncpf_err expected) (Network_community_fixture.ncpf_err e)

(* Every ordering of a four-element list, so field-order independence is
   proven exhaustively rather than on a hand-picked pair. *)
let rec ncpf_permutations = function
  | [] -> [ [] ]
  | items ->
      List.concat_map
        (fun x ->
          let rest = List.filter (fun y -> y != x) items in
          List.map (fun p -> x :: p) (ncpf_permutations rest))
        items

let ncpf_field_names =
  [ "community_name"; "community_slug"; "community_description";
    "publication_visibility"
  ]

(* The four canonical values one accepted submission must expose, as one
   comparable signature. *)
let ncpf_signature parsed =
  String.concat "|"
    [ Ncpf.community_name parsed;
      Ncpf.community_slug parsed;
      (match Ncpf.community_description parsed with
      | None -> "<none>"
      | Some text -> text);
      Network_community_fixture.ncpf_vis (Ncpf.publication_visibility parsed)
    ]

let ncpf_grammar_cases =
  [ ncpf_case "publication form: a valid Public submission keeps every \
               canonical value exactly" (fun () ->
        let parsed =
          Network_community_fixture.ncpf_ok "public"
            (Network_community_fixture.ncpf_fields ~name:"Ncpf Community" ~slug:"ncpf-community"
               ~description:"A durable description." ~visibility:"public" ())
        in
        Alcotest.(check string) "name" "Ncpf Community"
          (Ncpf.community_name parsed);
        Alcotest.(check string) "slug" "ncpf-community"
          (Ncpf.community_slug parsed);
        Alcotest.(check (option string)) "description"
          (Some "A durable description.") (Ncpf.community_description parsed);
        Alcotest.(check string) "visibility" "public"
          (Network_community_fixture.ncpf_vis (Ncpf.publication_visibility parsed)))
  ; ncpf_case "publication form: a valid Unlisted submission differs only in \
               the publication choice" (fun () ->
        let parsed =
          Network_community_fixture.ncpf_ok "unlisted" (Network_community_fixture.ncpf_fields ~visibility:"unlisted" ())
        in
        Alcotest.(check string) "visibility" "unlisted"
          (Network_community_fixture.ncpf_vis (Ncpf.publication_visibility parsed));
        Alcotest.(check string) "name unchanged" "Ncpf Community"
          (Ncpf.community_name parsed);
        Alcotest.(check string) "slug unchanged" "ncpf-community"
          (Ncpf.community_slug parsed);
        (* A structurally required but blank description collapses to None,
           never Some "". *)
        Alcotest.(check (option string)) "blank description" None
          (Ncpf.community_description parsed))
  ; ncpf_case "publication form: every field order yields byte-identical \
               values" (fun () ->
        let base =
          Network_community_fixture.ncpf_fields ~name:"Ncpf Order" ~slug:"ncpf-order"
            ~description:"Order body." ~visibility:"unlisted" ()
        in
        let expected = ncpf_signature (Network_community_fixture.ncpf_ok "canonical order" base) in
        let orders = ncpf_permutations base in
        Alcotest.(check int) "24 permutations" 24 (List.length orders);
        List.iter
          (fun fields ->
            let parsed = Network_community_fixture.ncpf_ok "permuted" fields in
            Alcotest.(check string) "same canonical values" expected
              (ncpf_signature parsed))
          orders)
  ; ncpf_case "publication form: every missing field is the same payload-free \
               structural rejection" (fun () ->
        List.iter
          (fun dropped ->
            let fields =
              List.filter
                (fun (key, _) -> not (String.equal key dropped))
                (Network_community_fixture.ncpf_fields ())
            in
            ncpf_expect ("missing " ^ dropped) Ncpf.Invalid_form fields)
          ncpf_field_names;
        ncpf_expect "empty field list" Ncpf.Invalid_form [])
  ; ncpf_case "publication form: every duplicated field is a structural \
               rejection, even when both copies are valid" (fun () ->
        List.iter
          (fun duplicated ->
            let extra =
              match duplicated with
              | "publication_visibility" -> (duplicated, "unlisted")
              | "community_slug" -> (duplicated, "ncpf-community")
              | _ -> (duplicated, "Ncpf Community")
            in
            ncpf_expect ("duplicate " ^ duplicated) Ncpf.Invalid_form
              (Network_community_fixture.ncpf_fields () @ [ extra ]))
          ncpf_field_names)
  ; ncpf_case "publication form: unknown, case-variant, and padded field \
               names are structural rejections" (fun () ->
        List.iter
          (fun (key, value) ->
            ncpf_expect ("unknown " ^ String.escaped key) Ncpf.Invalid_form
              (Network_community_fixture.ncpf_fields () @ [ (key, value) ]))
          [ ("community_id", "42"); ("actor_id", "7"); ("current_slug", "x");
            ("return_url", "/"); ("indexable", "true");
            ("discoverable", "true"); ("onboarding_state", "published")
          ];
        List.iter
          (fun key ->
            let fields =
              List.map
                (fun (k, v) ->
                  if String.equal k "community_name" then (key, v) else (k, v))
                (Network_community_fixture.ncpf_fields ())
            in
            ncpf_expect ("variant " ^ String.escaped key) Ncpf.Invalid_form
              fields)
          [ "Community_name"; "COMMUNITY_NAME"; " community_name";
            "community_name "; "community_name\t"; "communityname"
          ])
  ; ncpf_case "publication form: a dream.csrf field reaching the pure parser \
               is a structural rejection, never silently filtered" (fun () ->
        ncpf_expect "framework field" Ncpf.Invalid_form
          (("dream.csrf", "opaque-token") :: Network_community_fixture.ncpf_fields ());
        ncpf_expect "framework field last" Ncpf.Invalid_form
          (Network_community_fixture.ncpf_fields () @ [ ("dream.csrf", "opaque-token") ]))
  ; ncpf_case "publication form: structure is decided before semantics, so a \
               doubly invalid submission never names a field" (fun () ->
        (* Both the field set and the name are invalid; only the structural
           answer travels. *)
        ncpf_expect "unknown field plus blank name" Ncpf.Invalid_form
          (Network_community_fixture.ncpf_fields ~name:"   " ~visibility:"private" ()
          @ [ ("surprise", "1") ]);
        ncpf_expect "missing field plus bad slug" Ncpf.Invalid_form
          [ ("community_name", ""); ("community_slug", "Not A Slug") ])
  ]

let ncpf_publication_cases =
  [ ncpf_case "publication form: exactly public and unlisted are accepted"
      (fun () ->
        Alcotest.(check string) "public" "public"
          (Network_community_fixture.ncpf_vis
             (Ncpf.publication_visibility
                (Network_community_fixture.ncpf_ok "public" (Network_community_fixture.ncpf_fields ~visibility:"public" ()))));
        Alcotest.(check string) "unlisted" "unlisted"
          (Network_community_fixture.ncpf_vis
             (Ncpf.publication_visibility
                (Network_community_fixture.ncpf_ok "unlisted" (Network_community_fixture.ncpf_fields ~visibility:"unlisted" ())))))
  ; ncpf_case "publication form: private is rejected like any other unknown \
               value — a network community has no fully private published \
               shape" (fun () ->
        List.iter
          (fun value ->
            ncpf_expect ("value " ^ String.escaped value)
              Ncpf.Invalid_publication_visibility
              (Network_community_fixture.ncpf_fields ~visibility:value ()))
          [ "private"; "Private"; "PRIVATE"; "secret"; "hidden" ])
  ; ncpf_case "publication form: nothing is trimmed, case-folded, or repaired \
               on the publication value" (fun () ->
        List.iter
          (fun value ->
            ncpf_expect ("value " ^ String.escaped value)
              Ncpf.Invalid_publication_visibility
              (Network_community_fixture.ncpf_fields ~visibility:value ()))
          [ ""; " "; " public"; "public "; "\tpublic"; "public\n"; "Public";
            "PUBLIC"; "pUbLiC"; "Unlisted"; "UNLISTED"; " unlisted";
            "unlisted "; "public,unlisted"; "published"; "listed"; "0"; "1";
            "true"
          ])
  ; ncpf_case "publication form: identity is validated before the publication \
               choice, and each field maps to its own error" (fun () ->
        (* Both halves invalid: the identity error is the one that travels. *)
        ncpf_expect "bad name wins over bad visibility"
          Ncpf.Invalid_community_name
          (Network_community_fixture.ncpf_fields ~name:"" ~visibility:"private" ());
        ncpf_expect "bad slug wins over bad visibility"
          Ncpf.Invalid_community_slug
          (Network_community_fixture.ncpf_fields ~slug:"Not A Slug" ~visibility:"private" ());
        ncpf_expect "bad description wins over bad visibility"
          Ncpf.Invalid_community_description
          (Network_community_fixture.ncpf_fields ~description:"body\x01here" ~visibility:"private" ());
        (* A valid identity with a bad choice reaches the publication
           error. *)
        ncpf_expect "valid identity, bad visibility"
          Ncpf.Invalid_publication_visibility
          (Network_community_fixture.ncpf_fields ~visibility:"private" ()))
  ]

(* Cross-module parity: the identity half of this form is exactly the frozen
   provisioning policy — same verdicts, same canonical bytes, same error
   constructor — so a community created by provisioning and edited here can
   never drift into a second grammar. *)
let ncpf_identity_probes =
  [ ("plain", "Ncpf Community", "ncpf-community", "");
    ("trimmed name", "  Ncpf Community  ", "ncpf-community", "");
    ("name at the 120-scalar limit", Home_provisioning_fixture.phvf_repeat Home_provisioning_fixture.phvf_scalar 120, "ncpf-a", "");
    ("name one scalar over", Home_provisioning_fixture.phvf_repeat Home_provisioning_fixture.phvf_scalar 121, "ncpf-a", "");
    ("blank name", "   ", "ncpf-a", "");
    ("control in name", "Ncpf\x01Name", "ncpf-a", "");
    ("invalid utf-8 name", "Ncpf\xff", "ncpf-a", "");
    ("single-character slug", "Ncpf", "a", "");
    ("slug at 80 bytes", "Ncpf", String.make 80 'a', "");
    ("slug at 81 bytes", "Ncpf", String.make 81 'a', "");
    ("uppercase slug", "Ncpf", "Ncpf-Community", "");
    ("padded slug", "Ncpf", " ncpf-community", "");
    ("trailing hyphen slug", "Ncpf", "ncpf-", "");
    ("double hyphen slug", "Ncpf", "ncpf--community", "");
    ("underscore slug", "Ncpf", "ncpf_community", "");
    ("slash slug", "Ncpf", "ncpf/community", "");
    ("empty slug", "Ncpf", "", "");
    ("multiline description", "Ncpf", "ncpf-a", "one\r\ntwo\rthree\nfour");
    ("tabbed description", "Ncpf", "ncpf-a", "col\tumn");
    ("whitespace-only description", "Ncpf", "ncpf-a", "  \n\t ");
    ("description at 2000 scalars", "Ncpf", "ncpf-a",
     Home_provisioning_fixture.phvf_repeat Home_provisioning_fixture.phvf_scalar 2000);
    ("description one scalar over", "Ncpf", "ncpf-a",
     Home_provisioning_fixture.phvf_repeat Home_provisioning_fixture.phvf_scalar 2001);
    ("control in description", "Ncpf", "ncpf-a", "body\x01here");
    ("invalid utf-8 description", "Ncpf", "ncpf-a", "body\xff")
  ]

let ncpf_parity_cases =
  [ ncpf_case "publication form: every identity verdict matches \
               Project_home_provisioning_form exactly" (fun () ->
        List.iter
          (fun (label, name, slug, description) ->
            let mine =
              Ncpf.of_fields (Network_community_fixture.ncpf_fields ~name ~slug ~description ())
            in
            let frozen = Phvf.of_fields (Home_provisioning_fixture.phvf_fields ~name ~slug ~description ()) in
            match (mine, frozen) with
            | Ok a, Ok b ->
                Alcotest.(check string)
                  (label ^ ": name")
                  (Phvf.community_name b) (Ncpf.community_name a);
                Alcotest.(check string)
                  (label ^ ": slug")
                  (Phvf.community_slug b) (Ncpf.community_slug a);
                Alcotest.(check (option string))
                  (label ^ ": description")
                  (Phvf.community_description b) (Ncpf.community_description a)
            | Error Ncpf.Invalid_community_name,
              Error Phvf.Invalid_community_name
            | Error Ncpf.Invalid_community_slug,
              Error Phvf.Invalid_community_slug
            | Error Ncpf.Invalid_community_description,
              Error Phvf.Invalid_community_description ->
                ()
            | Error a, Error b ->
                Alcotest.failf "%s: error mismatch %s vs %s" label
                  (Network_community_fixture.ncpf_err a) (Home_provisioning_fixture.phvf_err b)
            | Ok _, Error b ->
                Alcotest.failf "%s: accepted here, %s there" label (Home_provisioning_fixture.phvf_err b)
            | Error a, Ok _ ->
                Alcotest.failf "%s: %s here, accepted there" label (Network_community_fixture.ncpf_err a))
          ncpf_identity_probes)
  ; ncpf_case "publication form: an accepted identity is byte-identical under \
               both publication choices" (fun () ->
        List.iter
          (fun (label, name, slug, description) ->
            match Ncpf.of_fields (Network_community_fixture.ncpf_fields ~name ~slug ~description ()) with
            | Error _ -> ()
            | Ok public ->
                let unlisted =
                  Network_community_fixture.ncpf_ok (label ^ " unlisted")
                    (Network_community_fixture.ncpf_fields ~name ~slug ~description
                       ~visibility:"unlisted" ())
                in
                Alcotest.(check string)
                  (label ^ ": name")
                  (Ncpf.community_name public) (Ncpf.community_name unlisted);
                Alcotest.(check string)
                  (label ^ ": slug")
                  (Ncpf.community_slug public) (Ncpf.community_slug unlisted);
                Alcotest.(check (option string))
                  (label ^ ": description")
                  (Ncpf.community_description public)
                  (Ncpf.community_description unlisted))
          ncpf_identity_probes)
  ; ncpf_case "publication form: accepted values are preserved byte-exactly, \
               never lowercased, normalized, or truncated" (fun () ->
        let parsed =
          Network_community_fixture.ncpf_ok "preservation"
            (Network_community_fixture.ncpf_fields ~name:"  Ncpf MiXeD Ünïcode  " ~slug:"ncpf-a1-b2"
               ~description:"  line one\r\nline two\ttabbed  " ())
        in
        Alcotest.(check string) "name outer-trimmed only" "Ncpf MiXeD Ünïcode"
          (Ncpf.community_name parsed);
        Alcotest.(check string) "slug byte-identical" "ncpf-a1-b2"
          (Ncpf.community_slug parsed);
        Alcotest.(check (option string)) "description normalized to LF"
          (Some "line one\nline two\ttabbed")
          (Ncpf.community_description parsed))
  ]

let suites =
    (* Final network-community setup submission: the exact four-field
       grammar, the publication vocabulary (public/unlisted only, nothing
       trimmed or case-folded, private rejected), and byte-for-byte parity
       of the whole identity half with the frozen provisioning-form policy.
       DB-free. *)
  [ ("network_community_publication_form_grammar", ncpf_grammar_cases)
  ; ("network_community_publication_form_publication", ncpf_publication_cases)
  ; ("network_community_publication_form_parity", ncpf_parity_cases)
  ]
