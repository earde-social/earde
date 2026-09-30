module NC = Earde.Network_communities

(* === Network-community lifecycle: pure domain rules (no DB, no IO) ===
   Publication mode parsing is exact-match (no trim/casefold); publication
   yields a closed configuration record; drafts must not leak through
   indexing/discovery; a published network community can never go private. *)

let nc_case name f = Alcotest.test_case name `Quick f

let nc_mode_str = function NC.Public -> "Public" | NC.Unlisted -> "Unlisted"

let nc_parse_ok name expected input =
  nc_case name (fun () ->
      match NC.publication_mode_of_string input with
      | Ok m ->
          Alcotest.(check string) "parsed mode" (nc_mode_str expected)
            (nc_mode_str m)
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let nc_parse_err name input =
  nc_case name (fun () ->
      match NC.publication_mode_of_string input with
      | Ok m -> Alcotest.failf "expected Error, got Ok %s" (nc_mode_str m)
      | Error _ -> ())

(* Field-by-field configuration check; onboarding_state is always Published. *)
let nc_check_config ~visibility ~indexable ~discoverable
    (c : NC.publication_configuration) =
  Alcotest.(check string) "visibility"
    (Earde.Community_types.community_visibility_to_string visibility)
    (Earde.Community_types.community_visibility_to_string c.visibility);
  Alcotest.(check bool) "indexable" indexable c.indexable;
  Alcotest.(check bool) "discoverable" discoverable c.discoverable;
  Alcotest.(check string) "onboarding_state" "published"
    (Earde.Community_types.string_of_community_onboarding_state c.onboarding_state)

let nc_publish_err name expected ~is_network_community ~onboarding_state mode =
  nc_case name (fun () ->
      match NC.publish ~is_network_community ~onboarding_state mode with
      | Ok _ -> Alcotest.fail "expected publication to be rejected"
      | Error e ->
          Alcotest.(check string) "error"
            (NC.string_of_publication_error expected)
            (NC.string_of_publication_error e))

let nc_vis_case name expected ~is_network_community ~onboarding_state
    ~requested_visibility =
  nc_case name (fun () ->
      Alcotest.(check bool) "allowed" expected
        (NC.visibility_change_allowed ~is_network_community ~onboarding_state
           ~requested_visibility))

let nc_valid_case name expected ~is_network_community ~onboarding_state
    ~visibility ~indexable ~discoverable =
  nc_case name (fun () ->
      Alcotest.(check bool) "valid" expected
        (NC.lifecycle_state_valid ~is_network_community ~onboarding_state
           ~visibility ~indexable ~discoverable))

(* === Settings-flow lifecycle gate (Handlers.visibility_update_rejection) ===
   The exact decision function update_community_visibility_handler consults on
   the loaded record before writing. These prove the settings flow invokes the
   lifecycle rule; the exhaustive matrix lives in network_visibility_change. *)
let vis_gate_community ~is_network_community ~onboarding_state ~visibility :
    Earde.Community_types.community =
  { id = 7; slug = "gate"; name = "Gate"; description = None; rules = None
  ; avatar_url = None; banner_url = None; allow_downvotes = true
  ; sections_enabled = false; visibility; indexable = true
  ; is_network_community; onboarding_state; discoverable = true }

let vis_gate_allowed name ~is_network_community ~onboarding_state ~visibility
    ~requested_visibility =
  nc_case name (fun () ->
      let community =
        vis_gate_community ~is_network_community ~onboarding_state ~visibility in
      match
        Earde.Handlers.visibility_update_rejection community
          ~requested_visibility
      with
      | None -> ()
      | Some m -> Alcotest.failf "expected update to proceed, got %S" m)

let vis_gate_rejected name ~is_network_community ~onboarding_state ~visibility
    ~requested_visibility =
  nc_case name (fun () ->
      let community =
        vis_gate_community ~is_network_community ~onboarding_state ~visibility in
      match
        Earde.Handlers.visibility_update_rejection community
          ~requested_visibility
      with
      | Some message ->
          Alcotest.(check string) "rejection copy"
            "Published network communities must remain public. Private \
             channels and sections may still be used."
            message
      | None -> Alcotest.fail "expected rejection, update was allowed")

let suites =
    (* Publication mode strings are exact-match: no trimming, no case
       folding — off-enum values are explicit errors. *)
  [ ( "network_publication_mode"
    , [ nc_parse_ok "public parses" NC.Public "public"
      ; nc_parse_ok "unlisted parses" NC.Unlisted "unlisted"
      ; nc_parse_err "blank rejected" ""
      ; nc_parse_err "unknown rejected" "private"
      ; nc_parse_err "capitalized rejected" "Public"
      ; nc_parse_err "leading whitespace rejected" " public"
      ; nc_parse_err "trailing whitespace rejected" "unlisted "
      ; nc_case "Public serializes" (fun () ->
            Alcotest.(check string) "canonical" "public"
              (NC.string_of_publication_mode NC.Public))
      ; nc_case "Unlisted serializes" (fun () ->
            Alcotest.(check string) "canonical" "unlisted"
              (NC.string_of_publication_mode NC.Unlisted))
      ] )
    (* Every field of both publication configurations. Unlisted stays
       publicly accessible — it only opts out of indexing and discovery. *)
  ; ( "network_publication_config"
    , [ nc_case "Public: public + indexable + discoverable + published"
          (fun () ->
            nc_check_config ~visibility:Earde.Community_types.Community_public
              ~indexable:true ~discoverable:true
              (NC.configuration_for_publication NC.Public))
      ; nc_case "Unlisted: public, not indexable, not discoverable, published"
          (fun () ->
            nc_check_config ~visibility:Earde.Community_types.Community_public
              ~indexable:false ~discoverable:false
              (NC.configuration_for_publication NC.Unlisted))
      ] )
    (* Publication decision: only a network draft may publish. *)
  ; ( "network_publish_decision"
    , [ nc_publish_err "legacy draft rejected" NC.Not_a_network_community
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_draft NC.Public
      ; nc_publish_err "legacy published rejected" NC.Not_a_network_community
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published NC.Public
      ; nc_publish_err "network published rejected"
          NC.Community_already_published ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published NC.Unlisted
      ; nc_case "network draft + Public succeeds" (fun () ->
            match
              NC.publish ~is_network_community:true
                ~onboarding_state:Earde.Community_types.Community_draft NC.Public
            with
            | Error e ->
                Alcotest.failf "expected Ok, got %s"
                  (NC.string_of_publication_error e)
            | Ok c ->
                nc_check_config ~visibility:Earde.Community_types.Community_public
                  ~indexable:true ~discoverable:true c)
      ; nc_case "network draft + Unlisted succeeds" (fun () ->
            match
              NC.publish ~is_network_community:true
                ~onboarding_state:Earde.Community_types.Community_draft NC.Unlisted
            with
            | Error e ->
                Alcotest.failf "expected Ok, got %s"
                  (NC.string_of_publication_error e)
            | Ok c ->
                nc_check_config ~visibility:Earde.Community_types.Community_public
                  ~indexable:false ~discoverable:false c)
      ] )
    (* Full matrix: the only forbidden transition is published network
       community -> private. *)
  ; ( "network_visibility_change"
    , [ nc_vis_case "legacy draft -> public" true
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_draft
          ~requested_visibility:Earde.Community_types.Community_public
      ; nc_vis_case "legacy draft -> private" true
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_draft
          ~requested_visibility:Earde.Community_types.Community_private
      ; nc_vis_case "legacy published -> public" true
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published
          ~requested_visibility:Earde.Community_types.Community_public
      ; nc_vis_case "legacy published -> private" true
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published
          ~requested_visibility:Earde.Community_types.Community_private
      ; nc_vis_case "network draft -> public" true
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~requested_visibility:Earde.Community_types.Community_public
      ; nc_vis_case "network draft -> private" true
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~requested_visibility:Earde.Community_types.Community_private
      ; nc_vis_case "network published -> public" true
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~requested_visibility:Earde.Community_types.Community_public
      ; nc_vis_case "network published -> private forbidden" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~requested_visibility:Earde.Community_types.Community_private
      ] )
    (* Settings-flow gate: the decision the visibility POST handler acts on.
       None = the existing update flow continues; Some = `Conflict before any
       DB write. Legacy behavior (including existing private communities) is
       untouched; only published network communities refuse -> private. *)
  ; ( "settings_visibility_gate"
    , [ vis_gate_allowed "legacy published -> private still allowed"
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public
          ~requested_visibility:Earde.Community_types.Community_private
      ; vis_gate_allowed "legacy draft-shaped -> private still allowed"
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_public
          ~requested_visibility:Earde.Community_types.Community_private
      ; vis_gate_allowed "legacy existing private -> public unaffected"
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_private
          ~requested_visibility:Earde.Community_types.Community_public
      ; vis_gate_allowed "legacy existing private -> private unaffected"
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_private
          ~requested_visibility:Earde.Community_types.Community_private
      ; vis_gate_allowed "network draft -> private allowed"
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_private
          ~requested_visibility:Earde.Community_types.Community_private
      ; vis_gate_allowed "network published -> public allowed"
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public
          ~requested_visibility:Earde.Community_types.Community_public
      ; vis_gate_rejected "network published -> private rejected"
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public
          ~requested_visibility:Earde.Community_types.Community_private
      ] )
    (* Whole-state validity: legacy always passes; drafts must be fully
       hidden; published states are exactly Public or Unlisted shaped. *)
  ; ( "network_lifecycle_validity"
    , [ nc_valid_case "legacy public indexable accepted" true
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public ~indexable:true
          ~discoverable:false
      ; nc_valid_case "legacy private draft accepted" true
          ~is_network_community:false
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_private ~indexable:true
          ~discoverable:true
      ; nc_valid_case "canonical private draft accepted" true
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_private ~indexable:false
          ~discoverable:false
      ; nc_valid_case "public draft rejected" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_public ~indexable:false
          ~discoverable:false
      ; nc_valid_case "indexable draft rejected" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_private ~indexable:true
          ~discoverable:false
      ; nc_valid_case "discoverable draft rejected" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_draft
          ~visibility:Earde.Community_types.Community_private ~indexable:false
          ~discoverable:true
      ; nc_valid_case "canonical published Public accepted" true
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public ~indexable:true
          ~discoverable:true
      ; nc_valid_case "canonical published Unlisted accepted" true
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public ~indexable:false
          ~discoverable:false
      ; nc_valid_case "published private rejected" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_private ~indexable:true
          ~discoverable:true
      ; nc_valid_case "published indexable-only rejected" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public ~indexable:true
          ~discoverable:false
      ; nc_valid_case "published discoverable-only rejected" false
          ~is_network_community:true
          ~onboarding_state:Earde.Community_types.Community_published
          ~visibility:Earde.Community_types.Community_public ~indexable:false
          ~discoverable:true
      ] )
  ]
