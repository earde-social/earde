module Pi = Earde.Project_identity

(* ===== Project identity: pure canonical constructor =====
   DB-free. Rejection labels name only the malformed category — never the
   fixture value — and every rejection is asserted as a bare boolean against
   a nullary constructor, so no supplied string can reach Alcotest output;
   the error type is payload-free by construction. The absence of a
   selected-ids accessor on Pi.t is a compile-time property of the mli (the
   list is constructor context only), so no runtime case asserts it. *)

let pi_case name f = Alcotest.test_case name `Quick f

let pi_create ?(kind = Pi.Organization) ?(name = "Example") ?(slug = "example")
    ?description ?website_url ?(selected = [ 1L ]) ?primary () =
  Pi.create ~kind ~name ~slug ~description ~website_url
    ~selected_snapshot_ids:selected ~primary_snapshot_id:primary

let pi_expect_ok = function
  | Ok t -> t
  | Error _ -> Alcotest.fail "expected a valid identity"

let pi_reject name expected result =
  pi_case name (fun () ->
      Alcotest.(check bool) "rejected with the expected error" true
        (match result with Error e -> e = expected | Ok _ -> false))

let pi_accept name result =
  pi_case name (fun () -> ignore (pi_expect_ok result : Pi.t))

let pi_name_ok label ~raw ~expect =
  pi_case label (fun () ->
      Alcotest.(check string) "canonical name" expect
        (Pi.name (pi_expect_ok (pi_create ~name:raw ()))))

let pi_slug_ok label ~raw ~expect =
  pi_case label (fun () ->
      Alcotest.(check string) "canonical slug" expect
        (Pi.slug (pi_expect_ok (pi_create ~slug:raw ()))))

let pi_desc_ok label ~raw ~expect =
  pi_case label (fun () ->
      Alcotest.(check (option string)) "canonical description" expect
        (Pi.description (pi_expect_ok (pi_create ~description:raw ()))))

let pi_web_ok label ~raw ~expect =
  pi_case label (fun () ->
      Alcotest.(check (option string)) "canonical website" expect
        (Pi.website_url (pi_expect_ok (pi_create ~website_url:raw ()))))

let pi_kind_cases =
  [ pi_case "every canonical string parses and serializes back" (fun () ->
        List.iter
          (fun (s, k) ->
            (match Pi.kind_of_string s with
            | Some parsed ->
                Alcotest.(check bool) "parses to the right variant" true
                  (parsed = k)
            | None -> Alcotest.fail "canonical kind string did not parse");
            Alcotest.(check string) "serializes to the database value" s
              (Pi.string_of_kind k))
          [ ("project", Pi.Project)
          ; ("organization", Pi.Organization)
          ; ("ecosystem", Pi.Ecosystem)
          ; ("foundation", Pi.Foundation)
          ; ("working_group", Pi.Working_group)
          ; ("other", Pi.Other)
          ])
  ; pi_case "every variant round-trips" (fun () ->
        List.iter
          (fun k ->
            Alcotest.(check bool) "round trip" true
              (Pi.kind_of_string (Pi.string_of_kind k) = Some k))
          Project_setup_fixture.pi_all_kinds)
  ; pi_case "capitalization, spacing, and alias variants reject" (fun () ->
        List.iter
          (fun s ->
            Alcotest.(check bool) "rejected" true (Pi.kind_of_string s = None))
          [ "Project"
          ; "PROJECT"
          ; " project"
          ; "project "
          ; "working-group"
          ; "workinggroup"
          ; "Working_group"
          ; "org"
          ; ""
          ; "unknown"
          ])
  ]

let pi_selected_cases =
  [ pi_accept "one selected repository suffices" (pi_create ~selected:[ 5L ] ())
  ; pi_accept "2000 selected repositories accepted"
      (pi_create ~selected:(List.init 2000 (fun i -> Int64.of_int (i + 1))) ())
  ; pi_reject "empty selection" Pi.No_repositories_selected
      (pi_create ~selected:[] ())
  ; pi_reject "zero id" Pi.Invalid_repository_selection
      (pi_create ~selected:[ 0L ] ())
  ; pi_reject "negative id" Pi.Invalid_repository_selection
      (pi_create ~selected:[ -3L ] ())
  ; pi_reject "duplicate id" Pi.Invalid_repository_selection
      (pi_create ~selected:[ 4L; 4L ] ())
  ; pi_reject "duplicate id among valid neighbours"
      Pi.Invalid_repository_selection
      (pi_create ~selected:[ 1L; 2L; 1L ] ())
  ; pi_reject "2001 selected repositories" Pi.Invalid_repository_selection
      (pi_create ~selected:(List.init 2001 (fun i -> Int64.of_int (i + 1))) ())
  ; pi_case "ids above the OCaml int range stay exact" (fun () ->
        let t =
          pi_expect_ok
            (pi_create ~kind:Pi.Project
               ~selected:[ Int64.max_int; 9223372036854775806L ]
               ~primary:Int64.max_int ())
        in
        Alcotest.(check (option int64)) "primary snapshot id"
          (Some Int64.max_int)
          (Pi.primary_snapshot_id t))
  ]

let pi_name_cases =
  [ pi_name_ok "basic ascii" ~raw:"Lwt" ~expect:"Lwt"
  ; pi_name_ok "internal ordinary spaces survive" ~raw:"OCaml Platform"
      ~expect:"OCaml Platform"
  ; pi_name_ok "outer ascii whitespace trims" ~raw:"  OCaml Platform \t "
      ~expect:"OCaml Platform"
  ; pi_name_ok "utf-8 name preserved without normalization"
      ~raw:"Progetto Citt\xc3\xa0 \xe2\x98\x95"
      ~expect:"Progetto Citt\xc3\xa0 \xe2\x98\x95"
  ; pi_case "exactly 120 unicode scalars accepted, counted per scalar"
      (fun () ->
        (* 120 two-byte scalars: byte counting would see 240 and reject. *)
        let name = String.concat "" (List.init 120 (fun _ -> "\xc3\xa9")) in
        Alcotest.(check string) "canonical name" name
          (Pi.name (pi_expect_ok (pi_create ~name ()))))
  ; pi_reject "121 unicode scalars reject" Pi.Invalid_name
      (pi_create ~name:(String.make 121 'a') ())
  ; pi_reject "blank name" Pi.Invalid_name (pi_create ~name:"" ())
  ; pi_reject "whitespace-only name" Pi.Invalid_name
      (pi_create ~name:" \t\r\n " ())
  ; pi_reject "malformed utf-8: stray continuation" Pi.Invalid_name
      (pi_create ~name:"a\xffb" ())
  ; pi_reject "malformed utf-8: truncated sequence" Pi.Invalid_name
      (pi_create ~name:"a\xc3" ())
  ; pi_reject "malformed utf-8: overlong encoding" Pi.Invalid_name
      (pi_create ~name:"a\xc0\xafb" ())
  ; pi_reject "malformed utf-8: utf-16 surrogate" Pi.Invalid_name
      (pi_create ~name:"a\xed\xa0\x80b" ())
  ; pi_reject "malformed utf-8: above U+10FFFF" Pi.Invalid_name
      (pi_create ~name:"a\xf4\x90\x80\x80b" ())
  ; pi_reject "embedded nul" Pi.Invalid_name (pi_create ~name:"a\x00b" ())
  ; pi_reject "internal tab" Pi.Invalid_name (pi_create ~name:"a\tb" ())
  ; pi_reject "internal newline" Pi.Invalid_name (pi_create ~name:"a\nb" ())
  ; pi_reject "internal carriage return" Pi.Invalid_name
      (pi_create ~name:"a\rb" ())
  ; pi_reject "another control byte" Pi.Invalid_name
      (pi_create ~name:"a\x01b" ())
  ; pi_reject "del byte" Pi.Invalid_name (pi_create ~name:"a\x7fb" ())
  ]

let pi_slug_cases =
  [ pi_slug_ok "plain lowercase" ~raw:"ocaml" ~expect:"ocaml"
  ; pi_slug_ok "uppercase lowercases" ~raw:"OCAML" ~expect:"ocaml"
  ; pi_slug_ok "mixed case with hyphen" ~raw:"OCaml-Platform"
      ~expect:"ocaml-platform"
  ; pi_slug_ok "digits allowed" ~raw:"project2" ~expect:"project2"
  ; pi_slug_ok "outer whitespace trims before lowercasing" ~raw:"  LWT  "
      ~expect:"lwt"
  ; pi_slug_ok "80 ascii characters accepted" ~raw:(String.make 80 'a')
      ~expect:(String.make 80 'a')
  ; pi_reject "blank slug" Pi.Invalid_slug (pi_create ~slug:"" ())
  ; pi_reject "whitespace-only slug" Pi.Invalid_slug (pi_create ~slug:"   " ())
  ; pi_reject "non-ascii slug" Pi.Invalid_slug
      (pi_create ~slug:"\xc3\xa9arde" ())
  ; pi_reject "internal space" Pi.Invalid_slug
      (pi_create ~slug:"ocaml platform" ())
  ; pi_reject "underscore" Pi.Invalid_slug
      (pi_create ~slug:"ocaml_platform" ())
  ; pi_reject "slash" Pi.Invalid_slug (pi_create ~slug:"ocaml/platform" ())
  ; pi_reject "leading hyphen" Pi.Invalid_slug (pi_create ~slug:"-ocaml" ())
  ; pi_reject "trailing hyphen" Pi.Invalid_slug (pi_create ~slug:"ocaml-" ())
  ; pi_reject "consecutive hyphens" Pi.Invalid_slug
      (pi_create ~slug:"ocaml--platform" ())
  ; pi_reject "81 characters reject" Pi.Invalid_slug
      (pi_create ~slug:(String.make 81 'a') ())
    (* The only exact static direct child of /projects/* today is
       /projects/new, so the reserved set is exactly { new }. *)
  ; pi_reject "reserved slug new" Pi.Reserved_slug (pi_create ~slug:"new" ())
  ; pi_reject "reserved slug via uppercase canonicalization" Pi.Reserved_slug
      (pi_create ~slug:"NEW" ())
  ; pi_reject "reserved slug via trim and case" Pi.Reserved_slug
      (pi_create ~slug:" New " ())
  ; pi_reject "structurally invalid never reports reserved" Pi.Invalid_slug
      (pi_create ~slug:"-new" ())
  ]

let pi_description_cases =
  [ pi_case "absent description stays absent" (fun () ->
        Alcotest.(check (option string)) "no description" None
          (Pi.description (pi_expect_ok (pi_create ()))))
  ; pi_desc_ok "empty collapses to none" ~raw:"" ~expect:None
  ; pi_desc_ok "whitespace-only collapses to none" ~raw:" \t\r\n \x0c\x0b"
      ~expect:None
  ; pi_desc_ok "outer whitespace trims" ~raw:"  hello  " ~expect:(Some "hello")
  ; pi_desc_ok "crlf normalizes to lf" ~raw:"line one\r\nline two"
      ~expect:(Some "line one\nline two")
  ; pi_desc_ok "lone cr normalizes to lf" ~raw:"line one\rline two"
      ~expect:(Some "line one\nline two")
  ; pi_desc_ok "multiline utf-8 preserved"
      ~raw:"prima riga\nseconda riga \xc3\xa8 \xe2\x9c\x93"
      ~expect:(Some "prima riga\nseconda riga \xc3\xa8 \xe2\x9c\x93")
  ; pi_desc_ok "internal tab preserved" ~raw:"colonna\tvalore"
      ~expect:(Some "colonna\tvalore")
  ; pi_case "exactly 2000 unicode scalars accepted without truncation"
      (fun () ->
        (* 2000 two-byte scalars: byte counting would see 4000 and reject;
           the accessor must return every byte, proving no silent cut. *)
        let text = String.concat "" (List.init 2000 (fun _ -> "\xc3\xa8")) in
        Alcotest.(check (option string)) "canonical description" (Some text)
          (Pi.description (pi_expect_ok (pi_create ~description:text ()))))
  ; pi_reject "2001 unicode scalars reject rather than truncate"
      Pi.Invalid_description
      (pi_create ~description:(String.make 2001 'a') ())
  ; pi_reject "malformed utf-8" Pi.Invalid_description
      (pi_create ~description:"a\xffb" ())
  ; pi_reject "embedded nul" Pi.Invalid_description
      (pi_create ~description:"a\x00b" ())
  ; pi_reject "forbidden control byte" Pi.Invalid_description
      (pi_create ~description:"a\x01b" ())
  ; pi_reject "del byte" Pi.Invalid_description
      (pi_create ~description:"a\x7fb" ())
  ]

let pi_website_cases =
  [ pi_case "absent website stays absent" (fun () ->
        Alcotest.(check (option string)) "no website" None
          (Pi.website_url (pi_expect_ok (pi_create ()))))
  ; pi_web_ok "empty collapses to none" ~raw:"" ~expect:None
  ; pi_web_ok "whitespace-only collapses to none" ~raw:"   " ~expect:None
    (* Preservation policy: the accepted value is the trimmed input
       byte-for-byte — Uri validates structure but never re-serializes. *)
  ; pi_web_ok "https" ~raw:"https://example.com"
      ~expect:(Some "https://example.com")
  ; pi_web_ok "http" ~raw:"http://example.com"
      ~expect:(Some "http://example.com")
  ; pi_web_ok "explicit port and path" ~raw:"https://example.com:8443/path"
      ~expect:(Some "https://example.com:8443/path")
  ; pi_web_ok "query string" ~raw:"https://example.com/path?q=value"
      ~expect:(Some "https://example.com/path?q=value")
  ; pi_web_ok "outer whitespace trims" ~raw:"  https://example.com  "
      ~expect:(Some "https://example.com")
  ; pi_web_ok "fragment accepted and preserved"
      ~raw:"https://example.com/docs#install"
      ~expect:(Some "https://example.com/docs#install")
  ; pi_web_ok "uppercase scheme accepted, bytes preserved"
      ~raw:"HTTPS://example.com" ~expect:(Some "HTTPS://example.com")
  ; pi_web_ok "utf-8 path text accepted"
      ~raw:"https://example.com/caff\xc3\xa8"
      ~expect:(Some "https://example.com/caff\xc3\xa8")
  ; pi_reject "relative path" Pi.Invalid_website_url
      (pi_create ~website_url:"/example" ())
  ; pi_reject "host without scheme" Pi.Invalid_website_url
      (pi_create ~website_url:"example.com" ())
  ; pi_reject "missing host" Pi.Invalid_website_url
      (pi_create ~website_url:"http:///missing-host" ())
  ; pi_reject "mailto scheme" Pi.Invalid_website_url
      (pi_create ~website_url:"mailto:someone@example.com" ())
  ; pi_reject "javascript scheme" Pi.Invalid_website_url
      (pi_create ~website_url:"javascript:alert(1)" ())
  ; pi_reject "data scheme" Pi.Invalid_website_url
      (pi_create ~website_url:"data:text/plain,x" ())
  ; pi_reject "ftp scheme" Pi.Invalid_website_url
      (pi_create ~website_url:"ftp://example.com" ())
  ; pi_reject "userinfo with password" Pi.Invalid_website_url
      (pi_create ~website_url:"https://user:pass@example.com" ())
  ; pi_reject "userinfo without password" Pi.Invalid_website_url
      (pi_create ~website_url:"https://user@example.com" ())
  ; pi_reject "internal whitespace" Pi.Invalid_website_url
      (pi_create ~website_url:"https://exa mple.com" ())
  ; pi_reject "control byte" Pi.Invalid_website_url
      (pi_create ~website_url:"https://example.com/\x01" ())
  ; pi_reject "malformed utf-8" Pi.Invalid_website_url
      (pi_create ~website_url:"https://example.com/\xff" ())
  ; pi_accept "2048 unicode scalars accepted"
      (pi_create
         ~website_url:("https://example.com/" ^ String.make 2028 'a')
         ())
  ; pi_reject "2049 unicode scalars reject" Pi.Invalid_website_url
      (pi_create
         ~website_url:("https://example.com/" ^ String.make 2029 'a')
         ())
  ]

let pi_primary_cases =
  [ pi_case "project with a selected primary succeeds" (fun () ->
        let t =
          pi_expect_ok
            (pi_create ~kind:Pi.Project ~selected:[ 1L; 2L ] ~primary:2L ())
        in
        Alcotest.(check (option int64)) "primary snapshot id" (Some 2L)
          (Pi.primary_snapshot_id t))
  ; pi_reject "project without primary" Pi.Primary_repository_required
      (pi_create ~kind:Pi.Project ())
  ; pi_case "organization without primary succeeds" (fun () ->
        Alcotest.(check (option int64)) "no primary" None
          (Pi.primary_snapshot_id
             (pi_expect_ok (pi_create ~kind:Pi.Organization ()))))
  ; pi_case "every non-project kind accepts an absent primary" (fun () ->
        List.iter
          (fun k ->
            match k with
            | Pi.Project -> ()
            | _ -> ignore (pi_expect_ok (pi_create ~kind:k ()) : Pi.t))
          Project_setup_fixture.pi_all_kinds)
  ; pi_case "every kind accepts one selected primary" (fun () ->
        List.iter
          (fun k ->
            ignore (pi_expect_ok (pi_create ~kind:k ~primary:1L ()) : Pi.t))
          Project_setup_fixture.pi_all_kinds)
  ; pi_reject "zero primary id" Pi.Invalid_primary_repository
      (pi_create ~primary:0L ())
  ; pi_reject "negative primary id" Pi.Invalid_primary_repository
      (pi_create ~primary:(-7L) ())
  ; pi_reject "positive but unselected primary" Pi.Invalid_primary_repository
      (pi_create ~selected:[ 1L; 2L ] ~primary:99L ())
  ; pi_reject "no primary inferred for a one-repository project"
      Pi.Primary_repository_required
      (pi_create ~kind:Pi.Project ~selected:[ 42L ] ())
  ]

let pi_accessor_cases =
  [ pi_case "complete identity exposes exact canonical values" (fun () ->
        let t =
          pi_expect_ok
            (Pi.create ~kind:Pi.Project ~name:"  Progetto Citt\xc3\xa0  "
               ~slug:"OCaml-Platform"
               ~description:(Some "  line one\r\nline two  ")
               ~website_url:(Some "  https://example.com/path?q=v  ")
               ~selected_snapshot_ids:[ 10L; 20L; 30L ]
               ~primary_snapshot_id:(Some 20L))
        in
        Alcotest.(check string) "kind" "project"
          (Pi.string_of_kind (Pi.kind t));
        Alcotest.(check string) "canonical name" "Progetto Citt\xc3\xa0"
          (Pi.name t);
        Alcotest.(check string) "canonical slug" "ocaml-platform" (Pi.slug t);
        Alcotest.(check (option string)) "canonical description"
          (Some "line one\nline two") (Pi.description t);
        Alcotest.(check (option string)) "canonical website"
          (Some "https://example.com/path?q=v")
          (Pi.website_url t);
        Alcotest.(check (option int64)) "primary snapshot id" (Some 20L)
          (Pi.primary_snapshot_id t))
  ]

let pi_ordering_cases =
  [ pi_reject "invalid selected context wins before invalid name"
      Pi.Invalid_repository_selection
      (pi_create ~selected:[ 4L; 4L ] ~name:"" ())
  ; pi_reject "no selected repository wins before invalid name"
      Pi.No_repositories_selected
      (pi_create ~selected:[] ~name:"" ())
  ; pi_reject "invalid name wins before invalid slug" Pi.Invalid_name
      (pi_create ~name:"" ~slug:"-bad-" ())
  ; pi_reject "invalid website wins before invalid primary"
      Pi.Invalid_website_url
      (pi_create ~website_url:"ftp://example.com" ~primary:99L ())
  ; pi_reject "identity fields report before the missing project primary"
      Pi.Invalid_slug
      (pi_create ~kind:Pi.Project ~slug:"-bad-" ())
  ; pi_reject "missing project primary reports once all fields are valid"
      Pi.Primary_repository_required
      (pi_create ~kind:Pi.Project ~name:"Valid Name" ~slug:"valid-slug"
         ~description:"fine" ~website_url:"https://example.com" ())
  ]

let pi_privacy_cases =
  [ pi_case "every rejection is a payload-free constructor" (fun () ->
        let all =
          [ Pi.Invalid_repository_selection
          ; Pi.No_repositories_selected
          ; Pi.Invalid_name
          ; Pi.Invalid_slug
          ; Pi.Reserved_slug
          ; Pi.Invalid_description
          ; Pi.Invalid_website_url
          ; Pi.Invalid_primary_repository
          ; Pi.Primary_repository_required
          ]
        in
        (* Distinctive fixture values: were any error to carry its input, the
           closed-membership check below could not hold for all of them. *)
        let rejections =
          [ pi_create ~name:"pi-fixture-name-\x01" ()
          ; pi_create ~slug:"pi_fixture_slug" ()
          ; pi_create ~slug:"new" ()
          ; pi_create ~description:"pi-fixture-description-\x00" ()
          ; pi_create ~website_url:"https://pi-fixture:secret@example.com" ()
          ; pi_create ~selected:[ 0L ] ()
          ; pi_create ~selected:[] ()
          ; pi_create ~primary:987654321L ()
          ; pi_create ~kind:Pi.Project ()
          ]
        in
        List.iter
          (fun outcome ->
            match outcome with
            | Ok _ -> Alcotest.fail "expected a rejection"
            | Error e ->
                Alcotest.(check bool) "nullary error" true (List.mem e all))
          rejections)
  ]

let suites =
    (* Project identity: pure canonical constructor for the finalization
       flow. Closed kind conversion, deterministic validation order,
       scalar-counted UTF-8 limits, byte-preserving canonicalization, and
       payload-free errors. *)
  [ ("project_identity_kind", pi_kind_cases)
  ; ("project_identity_selected", pi_selected_cases)
  ; ("project_identity_name", pi_name_cases)
  ; ("project_identity_slug", pi_slug_cases)
  ; ("project_identity_description", pi_description_cases)
  ; ("project_identity_website", pi_website_cases)
  ; ("project_identity_primary", pi_primary_cases)
  ; ("project_identity_accessors", pi_accessor_cases)
  ; ("project_identity_ordering", pi_ordering_cases)
  ; ("project_identity_privacy", pi_privacy_cases)
  ]
