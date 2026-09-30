(* Community visibility / effective-indexability / read-predicate helpers (Slice B): all pure,
   no DB. Privacy is the stronger property — private is always effectively non-indexable. *)
let check_vis_round_trip name v =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true
        (Earde.Community_types.community_visibility_of_string (Earde.Community_types.community_visibility_to_string v) = Some v))

let check_vis_none name s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true (Earde.Community_types.community_visibility_of_string s = None))

(* Onboarding-state enum (network-community lifecycle foundation): result-based
   decode — off-enum values are an explicit Error, never a silent published. *)
let check_onb_decodes name s v =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true
        (Earde.Community_types.community_onboarding_state_of_string s = Ok v))

let check_onb_serializes name v s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name s (Earde.Community_types.string_of_community_onboarding_state v))

let check_onb_error name s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true
        (match Earde.Community_types.community_onboarding_state_of_string s with
         | Error _ -> true
         | Ok _ -> false))

let check_idx_community name expected vis ~community_indexable =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (Earde.Community_types.effective_indexable_community vis ~community_indexable))

let check_idx_child name expected vis ~community_indexable ~child_indexable =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (Earde.Community_types.effective_indexable_child vis ~community_indexable ~child_indexable))

let check_can_read name expected vis ~is_member ~is_mod ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (Earde.Community_types.can_read_community vis ~is_member ~is_mod ~is_admin))

let suites =
    (* Community visibility enum: round-trips; off-enum strings rejected. *)
  [ ( "community_visibility_roundtrip"
    , [ check_vis_round_trip "public" Earde.Community_types.Community_public
      ; check_vis_round_trip "private" Earde.Community_types.Community_private
      ; check_vis_none "empty" ""
      ; check_vis_none "unknown" "secret"
      ; check_vis_none "unlisted not a value yet" "unlisted"
      ; check_vis_none "case-sensitive" "Public"
      ] )
    (* Onboarding-state enum: canonical decode + serialize; off-enum is an explicit Error. *)
  ; ( "community_onboarding_state"
    , [ check_onb_decodes "draft decodes" "draft" Earde.Community_types.Community_draft
      ; check_onb_decodes "published decodes" "published" Earde.Community_types.Community_published
      ; check_onb_serializes "draft serializes" Earde.Community_types.Community_draft "draft"
      ; check_onb_serializes "published serializes" Earde.Community_types.Community_published "published"
      ; check_onb_error "unknown is an error" "archived"
      ; check_onb_error "blank is an error" ""
      ; check_onb_error "case-sensitive: Draft rejected" "Draft"
      ; check_onb_error "case-sensitive: PUBLISHED rejected" "PUBLISHED"
      ] )
    (* Effective COMMUNITY indexability: private is always non-indexable; public follows the flag. *)
  ; ( "effective_indexable_community"
    , [ check_idx_community "public + indexable => true" true Earde.Community_types.Community_public ~community_indexable:true
      ; check_idx_community "public + non-indexable => false" false Earde.Community_types.Community_public ~community_indexable:false
      ; check_idx_community "private + indexable flag still false" false Earde.Community_types.Community_private ~community_indexable:true
      ; check_idx_community "private + non-indexable => false" false Earde.Community_types.Community_private ~community_indexable:false
      ] )
    (* Effective CHILD (channel/section) indexability: needs community AND child opt-in; private kills all. *)
  ; ( "effective_indexable_child"
    , [ check_idx_child "public, community idx, child idx => true" true
          Earde.Community_types.Community_public ~community_indexable:true ~child_indexable:true
      ; check_idx_child "public, community idx, child non-idx => false" false
          Earde.Community_types.Community_public ~community_indexable:true ~child_indexable:false
      ; check_idx_child "public, community non-idx overrides child idx => false" false
          Earde.Community_types.Community_public ~community_indexable:false ~child_indexable:true
      ; check_idx_child "private overrides everything => false" false
          Earde.Community_types.Community_private ~community_indexable:true ~child_indexable:true
      ] )
    (* Private read predicate: public readable by anyone; private only by member/mod/admin. *)
  ; ( "can_read_community"
    , [ check_can_read "public readable for logged-out/non-member" true
          Earde.Community_types.Community_public ~is_member:false ~is_mod:false ~is_admin:false
      ; check_can_read "private unreadable for logged-out/non-member" false
          Earde.Community_types.Community_private ~is_member:false ~is_mod:false ~is_admin:false
      ; check_can_read "private readable for member" true
          Earde.Community_types.Community_private ~is_member:true ~is_mod:false ~is_admin:false
      ; check_can_read "private readable for mod" true
          Earde.Community_types.Community_private ~is_member:false ~is_mod:true ~is_admin:false
      ; check_can_read "private readable for admin" true
          Earde.Community_types.Community_private ~is_member:false ~is_mod:false ~is_admin:true
      ] )
  ]
