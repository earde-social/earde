(* Atomic publication of one provisioned network-community setup draft,
   all inside one explicit transaction. The route is keyed by the current
   community slug while every project-home mutation store locks
   project-first, so the store first resolves its candidates through
   bounded non-locking reads and only then locks in the shared global
   order — permanent project, community, the actor's target-community
   top_mod row, the actor's durable users row, the exact accepted
   provisioned relation — always all of them, in full, even after one
   authorization source qualifies, so no sibling store can deadlock
   against this one. Identity arrives already parsed by the pure
   publication form and is persisted byte-exactly; the final lifecycle
   comes only from the pure Network_communities.publish transition over
   the locked draft state. communities_slug_key remains the final slug
   arbiter: PostgreSQL has no ON CONFLICT for UPDATE, so the one guarded
   update runs under a savepoint and a unique-key failure is classified
   structurally (rollback to the savepoint, then a bounded probe for the
   row owning the requested slug) — never by inspecting diagnostics
   text. See the .mli for the full contract. *)

open Lwt.Infix

type published_community = {
  slug : string;
  visibility : Network_community_publication_form.publication_visibility;
}

let community_slug { slug; _ } = slug
let publication_visibility { visibility; _ } = visibility

type error =
  | Invalid_user_id
  | Invalid_community_slug
  | Draft_unavailable
  | Community_slug_unavailable
  | Inconsistent_data
  | Storage_error

(* Community slugs carry no schema grammar for legacy rows, so the
   strongest predicate a route value must satisfy is the shared
   addressability shape of the sibling stores: every community lives at
   /c/:slug, hence one non-empty URL path segment — no whitespace,
   controls, DEL, or '/'. Nothing is trimmed, decoded, or repaired. *)
let canonical_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* The scoped network-community slug grammar, byte-for-byte the
   communities_network_slug_check CHECK: lowercase alphanumeric runs
   joined by single hyphens, at most 80 characters. ASCII-only by
   construction, so byte length is character length. *)
let canonical_network_slug value =
  let n = String.length value in
  let is_slug_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  n >= 1 && n <= 80
  && value.[0] <> '-'
  && value.[n - 1] <> '-'
  &&
  let rec ok i =
    i >= n
    ||
    if is_slug_char value.[i] then ok (i + 1)
    else value.[i] = '-' && value.[i + 1] <> '-' && ok (i + 1)
  in
  ok 0

(* char_length counts Unicode scalars, never bytes — the same count
   PostgreSQL applies in the scoped CHECKs. Only called after UTF-8
   validity is established, so every decode is exact. *)
let utf8_scalar_count s =
  let n = String.length s in
  let rec go i count =
    if i >= n then count
    else
      let decode = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length decode) (count + 1)
  in
  go 0 0

(* communities_network_name_check: 1..120 characters, no ASCII control
   byte and no DEL anywhere, no space at either edge. *)
let valid_network_name value =
  let n = String.length value in
  n > 0
  && String.is_valid_utf_8 value
  && (not (String.exists (fun c -> c < '\x20' || c = '\x7f') value))
  && value.[0] <> ' '
  && value.[n - 1] <> ' '
  && utf8_scalar_count value <= 120

(* communities_network_description_check: NULL, or 1..2000 characters
   where LF and horizontal tab are the only permitted control bytes, with
   no space, tab, or LF at either edge. The canonical form collapses an
   empty description to NULL, so '' is corruption. *)
let valid_network_description = function
  | None -> true
  | Some text ->
      let n = String.length text in
      let is_forbidden c =
        (c < '\x20' && c <> '\n' && c <> '\t') || c = '\x7f'
      in
      let is_edge c = c = ' ' || c = '\t' || c = '\n' in
      n > 0
      && String.is_valid_utf_8 text
      && (not (String.exists is_forbidden text))
      && (not (is_edge text.[0]))
      && (not (is_edge text.[n - 1]))
      && utf8_scalar_count text <= 2000

(* The exact closed schema vocabulary (mirrors the open_source_projects
   verification CHECK); anything else in the durable column is
   corruption, never a fourth state. All three known states publish —
   losing verification after provisioning must not strand the draft. *)
let known_verification_status = function
  | "verified" | "stale" | "revoked" -> true
  | _ -> false

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* Candidate resolution, deliberately non-locking: the community row by
   the supplied current slug, then its accepted home relation, read only
   to learn which project row to lock first. Everything read here is
   advisory until re-read under lock. collect_list rather than find_opt
   so an impossible duplicate is observable, not silently truncated. *)
let candidate_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->* Caqti_type.(t3 int bool string))
  "SELECT id, is_network_community, onboarding_state \
   FROM communities WHERE slug = $1"

let candidate_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t2 (t2 int64 int64) (t2 bool bool)))
  "SELECT id, project_id, \
          requested_by_user_id IS NULL AND reviewed_by_user_id IS NULL \
            AND request_note IS NULL, \
          reviewed_at IS NOT NULL AND removed_at IS NULL \
   FROM community_projects \
   WHERE community_id = $1 AND relation_type = 'home' \
     AND status = 'accepted'"

(* Step 1: the permanent project, locked first — the lock every sibling
   home operation takes before anything else, making the project row the
   global serialization point. Deliberately no verification filter:
   verified, stale, and revoked all publish, so the closed vocabulary is
   validated on the returned row instead. *)
let lock_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? Caqti_type.(t2 int64 string))
  "SELECT p.id, p.verification_status \
   FROM open_source_projects p \
   WHERE p.id = $1 \
   FOR UPDATE OF p"

(* Every durable column of the locked project as one text signature, so
   the pre-commit validation can prove the project row byte-unchanged.
   Captured and compared only inside the transaction; never exposed. *)
let project_sig_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT id::text || '|' || \
          COALESCE(source_onboarding_draft_id::text, '<null>') || '|' || \
          name || '|' || slug || '|' || \
          COALESCE(description, '<null>') || '|' || \
          COALESCE(website_url, '<null>') || '|' || \
          kind || '|' || forge || '|' || forge_namespace_id::text || '|' || \
          forge_namespace_login || '|' || forge_namespace_type || '|' || \
          verification_status || '|' || \
          COALESCE(created_by_user_id::text, '<null>') || '|' || \
          created_at::text || '|' || updated_at::text \
   FROM open_source_projects WHERE id = $1"

(* Step 2: the exact community, locked second under the held project
   lock, by candidate id and the supplied current slug together — a
   rename between candidate resolution and this lock reads as absence.
   Lifecycle columns come back for closed-value classification rather
   than being filtered, so "published" and "malformed" stay
   distinguishable at the call site. *)
let lock_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string)
   ->? Caqti_type.(
         t2
           (t2 (t2 int string) (t2 string (option string)))
           (t2 (t2 string string) (t3 bool bool bool))))
  "SELECT id, name, slug, description, visibility, onboarding_state, \
          is_network_community, indexable, discoverable \
   FROM communities \
   WHERE id = $1 AND slug = $2 \
   FOR UPDATE"

(* Steps 3a/3b: both durable authorization sources, always queried and
   locked in this order and always in full, so a concurrent role removal
   or admin revocation serializes through the matching row lock and every
   caller takes the same authorization-row order. First the
   target-community current top moderator ('mod' and 'legacy_mod' do not
   qualify, ordinary membership is not consulted)... *)
let lock_top_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
  "SELECT role FROM community_moderators \
   WHERE user_id = $1 AND community_id = $2 AND role = 'top_mod' \
   FOR UPDATE"

(* ...then the durable global administrator flag on the users row — the
   only durable representation of Earde administrators. A session-only
   claim never reaches this store. *)
let lock_admin_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.bool)
  "SELECT is_admin FROM users \
   WHERE id = $1 AND is_admin \
   FOR UPDATE"

(* Step 4: the exact accepted relation, locked last. The partial unique
   active-home index caps this at one row for the locked project; every
   zero-row cause (a removal that committed first, chiefly) collapses
   into one variant at the call site. Timestamps reduce to the presence
   and ordering booleans the provisioned row contract needs. *)
let lock_accepted_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 string) (t2 string (option string)))
           (t2
              (t2 (option int) (option int))
              (t2 (t2 bool bool) (t2 bool bool)))))
  "SELECT id, relation_type, status, request_note, \
          requested_by_user_id, reviewed_by_user_id, \
          reviewed_at IS NOT NULL, removed_at IS NULL, \
          COALESCE(reviewed_at >= created_at, FALSE), \
          updated_at >= created_at \
   FROM community_projects \
   WHERE project_id = $1 AND community_id = $2 \
     AND relation_type = 'home' AND status = 'accepted' \
   FOR UPDATE"

(* Every durable column of the locked relation as one text signature, for
   the byte-unchanged pre-commit proof. *)
let relation_sig_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT id::text || '|' || project_id::text || '|' || \
          community_id::text || '|' || relation_type || '|' || status \
          || '|' || COALESCE(requested_by_user_id::text, '<null>') \
          || '|' || COALESCE(reviewed_by_user_id::text, '<null>') \
          || '|' || COALESCE(request_note, '<null>') \
          || '|' || created_at::text || '|' || updated_at::text \
          || '|' || COALESCE(reviewed_at::text, '<null>') \
          || '|' || COALESCE(removed_at::text, '<null>') \
   FROM community_projects WHERE id = $1"

(* Step 5 and the pre-commit re-read share one bounded aggregate: the
   complete draft shell (membership, moderation, the canonical General
   section and active general channel in exactly the shape legacy
   creation writes and provisioning copied), the relation counts on both
   sides, and the community's content volume (posts, their comments, and
   chat messages) — fourteen counts, O(1) result rows regardless of
   community size, no N+1. Comparing the whole tuple before and after
   the update proves the publication touched nothing but the community
   row itself. *)
let draft_state_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int64)
   ->! Caqti_type.(
         t2
           (t2 (t4 int int int int) (t4 int int int int))
           (t2 (t3 int int int) (t3 int int int))))
  "SELECT \
     (SELECT COUNT(*) FROM community_members WHERE community_id = $1), \
     (SELECT COUNT(*) FROM community_moderators WHERE community_id = $1), \
     (SELECT COUNT(*) FROM community_moderators \
      WHERE community_id = $1 AND role = 'top_mod'), \
     (SELECT COUNT(*) FROM community_sections WHERE community_id = $1), \
     (SELECT COUNT(*) FROM community_sections \
      WHERE community_id = $1 AND slug = 'general' AND name = 'General' \
        AND position = 0 AND default_sort = 'new' \
        AND NOT is_introduction_section), \
     (SELECT COUNT(*) FROM channels WHERE community_id = $1), \
     (SELECT COUNT(*) FROM channels \
      WHERE community_id = $1 AND slug = 'general' AND name = 'general' \
        AND position = 0 AND NOT is_archived), \
     (SELECT COUNT(*) FROM community_projects \
      WHERE community_id = $1 AND relation_type = 'home' \
        AND status = 'accepted'), \
     (SELECT COUNT(*) FROM community_projects \
      WHERE community_id = $1 AND relation_type = 'home' \
        AND status = 'pending'), \
     (SELECT COUNT(*) FROM community_projects \
      WHERE project_id = $2 AND relation_type = 'home' \
        AND status IN ('pending', 'accepted')), \
     (SELECT COUNT(*) FROM community_projects \
      WHERE project_id = $2 AND relation_type = 'home' \
        AND status = 'pending'), \
     (SELECT COUNT(*) FROM posts WHERE community_id = $1), \
     (SELECT COUNT(*) FROM comments \
      WHERE post_id IN (SELECT id FROM posts WHERE community_id = $1)), \
     (SELECT COUNT(*) FROM chat_messages \
      WHERE channel_id IN \
        (SELECT id FROM channels WHERE community_id = $1))"

(* The slug-conflict boundary. PostgreSQL aborts the whole transaction on
   a unique violation unless a savepoint fences the failing statement, so
   the guarded update runs behind one; rolling back to it releases only
   what the update took while every earlier lock survives. *)
let savepoint_query =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit) "SAVEPOINT ncps_publication"

let rollback_savepoint_query =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
  "ROLLBACK TO SAVEPOINT ncps_publication"

(* Step 7: the one mutation. Identity and lifecycle are written together
   in a single statement guarded on the exact locked draft state, so no
   partial publication can exist even transiently. The lifecycle values
   arrive exclusively from the pure publication configuration at the call
   site; nothing is derived or suffixed here. *)
let publish_update_query =
  let open Caqti_request.Infix in
  (Caqti_type.(
     t3
       (t3 int string string)
       (t3 (option string) string string)
       (t3 bool bool string))
   ->* Caqti_type.(
         t2
           (t2 (t2 int string) (t2 string (option string)))
           (t2 (t2 string string) (t3 bool bool bool))))
  "UPDATE communities \
   SET name = $2, slug = $3, description = $4, \
       visibility = $5, onboarding_state = $6, \
       indexable = $7, discoverable = $8 \
   WHERE id = $1 AND slug = $9 \
     AND is_network_community \
     AND onboarding_state = 'draft' \
     AND visibility = 'private' \
     AND NOT indexable \
     AND NOT discoverable \
   RETURNING id, name, slug, description, visibility, onboarding_state, \
             is_network_community, indexable, discoverable"

(* The post-rollback probe: does some other row own the requested final
   slug? Read committed gives this statement a fresh snapshot, so a
   concurrent winner that just committed is visible. Bounded, and its
   result is reduced to one bit at the call site. *)
let slug_owner_probe_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->! Caqti_type.int)
  "SELECT COUNT(*) FROM communities WHERE slug = $1 AND id <> $2"

(* Step 9: slug-level uniqueness and the exact published lifecycle, as
   bounded counts — exactly one community owns the final slug, none
   remains under the old one (when it changed), and the locked row
   carries precisely the selected published shape. *)
let post_slug_state_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 string string) (t3 int bool bool))
   ->! Caqti_type.(t3 int int int))
  "SELECT \
     (SELECT COUNT(*) FROM communities WHERE slug = $1), \
     (SELECT COUNT(*) FROM communities WHERE slug = $2), \
     (SELECT COUNT(*) FROM communities \
      WHERE id = $3 AND slug = $1 \
        AND is_network_community \
        AND onboarding_state = 'published' \
        AND visibility = 'public' \
        AND indexable = $4 AND discoverable = $5)"

let publish (module C : Caqti_lwt.CONNECTION) ~actor_user_id
    ~current_community_slug ~publication =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_community_slug current_community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    (* The final identity is read once, through the form's public
       accessors only — the sole source of the requested canonical
       values. *)
    let final_name =
      Network_community_publication_form.community_name publication
    in
    let final_slug =
      Network_community_publication_form.community_slug publication
    in
    let final_description =
      Network_community_publication_form.community_description publication
    in
    let selected_visibility =
      Network_community_publication_form.publication_visibility publication
    in
    (* The form's closed publication value maps onto the frozen lifecycle
       grammar's closed mode — no second browser string is ever parsed. *)
    let mode =
      match selected_visibility with
      | Network_community_publication_form.Public -> Network_communities.Public
      | Network_community_publication_form.Unlisted ->
          Network_communities.Unlisted
    in
    (* As in the sibling stores, every Caqti error is dropped payload-free
       — error payloads can echo SQL parameters — and rollback failure
       adds nothing a caller may act on either (and never turns into an
       apparent success). *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in

    (* Step 8: the returned community must be byte-exactly the parsed
       identity in exactly the pure publication configuration — anything
       else is corruption, never carried forward. The update having
       succeeded also means the new communities_network_lifecycle_check
       accepted the tuple. *)
    let published_row_ok ~community_id ~new_visibility ~new_onboarding
        ~new_indexable ~new_discoverable
        ( ((row_id, stored_name), (stored_slug, stored_description)),
          ((visibility_raw, onboarding_raw), (network, indexable, discoverable))
        ) =
      row_id = community_id
      && String.equal stored_name final_name
      && String.equal stored_slug final_slug
      && Option.equal String.equal stored_description final_description
      && network
      && indexable = new_indexable
      && discoverable = new_discoverable
      &&
      match
        ( Db.community_visibility_of_string visibility_raw,
          Db.community_onboarding_state_of_string onboarding_raw )
      with
      | Some visibility, Ok onboarding_state ->
          visibility = new_visibility
          && onboarding_state = new_onboarding
          && Network_communities.lifecycle_state_valid
               ~is_network_community:network ~onboarding_state ~visibility
               ~indexable ~discoverable
      | _, _ -> false
    in

    (* Step 9: bounded re-reads prove the publication moved exactly one
       row and nothing else — slug ownership, the published lifecycle,
       and the untouched shell, relation, project, and content. *)
    let validate_after_update ~community_id ~project_id ~relation_id
        ~new_indexable ~new_discoverable ~before ~project_sig_before
        ~relation_sig_before =
      let expected_old_count =
        (* Same-slug publication: the "old" slug is the final slug and
           still owned by exactly this community. *)
        if String.equal final_slug current_community_slug then 1 else 0
      in
      C.find post_slug_state_query
        ((final_slug, current_community_slug),
         (community_id, new_indexable, new_discoverable))
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok (final_count, old_count, published_exact) ->
          if
            not
              (final_count = 1
              && old_count = expected_old_count
              && published_exact = 1)
          then rollback_to Inconsistent_data
          else (
            C.find draft_state_query (community_id, project_id) >>= function
            | Error _ -> rollback_to Storage_error
            | Ok after ->
                if after <> before then rollback_to Inconsistent_data
                else (
                  C.find project_sig_query project_id >>= function
                  | Error _ -> rollback_to Storage_error
                  | Ok project_sig_after ->
                      if
                        not
                          (String.equal project_sig_before project_sig_after)
                      then rollback_to Inconsistent_data
                      else (
                        C.find relation_sig_query relation_id >>= function
                        | Error _ -> rollback_to Storage_error
                        | Ok relation_sig_after ->
                            if
                              not
                                (String.equal relation_sig_before
                                   relation_sig_after)
                            then rollback_to Inconsistent_data
                            else (
                              (* The audit event rides the same
                                 transaction: inserted only after the
                                 complete post-update validation, so a
                                 slug-conflict loser, a replay, or an
                                 unavailable draft never reaches it, and
                                 any audit failure rolls the publication
                                 back whole. The community foreign key is
                                 the authoritative identity — neither the
                                 old nor the final slug is copied in. *)
                              Project_home_audit.insert
                                (module C)
                                ~action:
                                  Project_home_audit
                                  .Network_community_published
                                ~actor_user_id ~project_id ~community_id
                                ~relation_id
                              >>= function
                              | Error Project_home_audit.Inconsistent_data
                                ->
                                  rollback_to Inconsistent_data
                              | Error Project_home_audit.Storage_error ->
                                  rollback_to Storage_error
                              | Ok () -> (
                                  C.commit () >>= function
                                  | Error _ ->
                                      Lwt.return (Error Storage_error)
                                  | Ok () ->
                                      Lwt.return
                                        (Ok
                                           { slug = final_slug;
                                             visibility =
                                               selected_visibility
                                           }))))))
    in

    (* Slug-conflict classification: structural cause only — a unique
       violation on the guarded update means communities_slug_key fired
       (the only unique key this statement can touch), and the probe then
       confirms which row owns the requested slug. Constraint names,
       SQLSTATE, and diagnostics never leave; anything unclassified is a
       plain storage failure. *)
    let update_error_is_unique_violation err =
      match err with
      | (`Request_failed _ | `Response_failed _) as failure -> (
          match Caqti_error.cause failure with
          | `Unique_violation -> true
          | _ -> false)
      | _ -> false
    in

    let run_update ~community_id ~project_id ~relation_id ~new_visibility
        ~new_onboarding ~new_indexable ~new_discoverable ~before
        ~project_sig_before ~relation_sig_before =
      C.exec savepoint_query () >>= function
      | Error _ -> rollback_to Storage_error
      | Ok () -> (
          C.collect_list publish_update_query
            ( (community_id, final_name, final_slug),
              ( final_description,
                Db.community_visibility_to_string new_visibility,
                Db.string_of_community_onboarding_state new_onboarding ),
              (new_indexable, new_discoverable, current_community_slug) )
          >>= function
          | Error err ->
              if not (update_error_is_unique_violation err) then
                rollback_to Storage_error
              else (
                C.exec rollback_savepoint_query () >>= function
                | Error _ -> rollback_to Storage_error
                | Ok () -> (
                    C.find slug_owner_probe_query (final_slug, community_id)
                    >>= function
                    | Error _ -> rollback_to Storage_error
                    | Ok owners ->
                        if owners >= 1 then
                          rollback_to Community_slug_unavailable
                        else
                          (* A unique failure with no surviving owner is
                             unclassifiable (the winner rolled back). *)
                          rollback_to Storage_error))
          | Ok [] ->
              (* The guard re-states exactly the state validated under the
                 held row lock, so zero updated rows is impossible without
                 an interfering rule or trigger. *)
              rollback_to Inconsistent_data
          | Ok (_ :: _ :: _) -> rollback_to Inconsistent_data
          | Ok [ returned ] ->
              if
                not
                  (published_row_ok ~community_id ~new_visibility
                     ~new_onboarding ~new_indexable ~new_discoverable
                     returned)
              then rollback_to Inconsistent_data
              else
                validate_after_update ~community_id ~project_id ~relation_id
                  ~new_indexable ~new_discoverable ~before
                  ~project_sig_before ~relation_sig_before)
    in

    (* Step 6: the pure domain owns the resulting lifecycle. The draft
       was just validated as a network community in Community_draft, so
       any failure here is an impossible pure-domain outcome. *)
    let publish_transition ~community_id ~project_id ~relation_id
        ~onboarding_state ~before ~project_sig_before ~relation_sig_before =
      match
        Network_communities.publish ~is_network_community:true
          ~onboarding_state mode
      with
      | Error _ -> rollback_to Inconsistent_data
      | Ok
          { Network_communities.visibility = new_visibility;
            indexable = new_indexable;
            discoverable = new_discoverable;
            onboarding_state = new_onboarding
          } ->
          run_update ~community_id ~project_id ~relation_id ~new_visibility
            ~new_onboarding ~new_indexable ~new_discoverable ~before
            ~project_sig_before ~relation_sig_before
    in

    (* Step 5: the complete draft, as one bounded aggregate — at least
       one member and one current top moderator (the publisher need not
       personally be a member: a durable admin may publish), exactly the
       canonical General section and active general channel, exactly one
       accepted home relation and no pending one on either side. The full
       tuple doubles as the pre-commit unchanged snapshot. *)
    let validate_draft ~community_id ~project_id ~relation_id
        ~onboarding_state ~project_sig_before ~relation_sig_before =
      C.find draft_state_query (community_id, project_id) >>= function
      | Error _ -> rollback_to Storage_error
      | Ok before ->
          let ( ( (members, _moderators, top_mods, _sections_total),
                  ( canonical_sections,
                    _channels_total,
                    canonical_channels,
                    accepted_for_community ) ),
                ( ( pending_for_community,
                    active_for_project,
                    pending_for_project ),
                  (_posts, _comments, _messages) ) ) =
            before
          in
          if
            not
              (members >= 1 && top_mods >= 1 && canonical_sections = 1
             && canonical_channels = 1 && accepted_for_community = 1
             && pending_for_community = 0 && active_for_project = 1
             && pending_for_project = 0)
          then rollback_to Inconsistent_data
          else
            publish_transition ~community_id ~project_id ~relation_id
              ~onboarding_state ~before ~project_sig_before
              ~relation_sig_before
    in

    (* Step 4: the exact provisioned relation, locked last of all. The
       pure domain's dedicated provisioned-home constructor is the
       authority for the expected shape — accepted, no note — exactly as
       the provisioning store persisted it; no moderator-reviewed request
       is reconstructed from (or written into) the row. *)
    let lock_relation ~candidate_relation_id ~community_id ~project_id
        ~onboarding_state ~project_sig_before k =
      C.find_opt lock_accepted_relation_query (project_id, community_id)
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok None ->
          (* A removal that committed first, chiefly — one
             indistinguishable answer. *)
          rollback_to Draft_unavailable
      | Ok
          (Some
            ( ((relation_id, relation_type), (status_raw, stored_note)),
              ( (requested_by, reviewed_by),
                ((has_reviewed_at, removed_at_null), (reviewed_ge, updated_ge))
              ) )) ->
          if not (Int64.equal relation_id candidate_relation_id) then
            (* The candidate relation vanished and another took its place
               through a concurrent durable change. *)
            rollback_to Draft_unavailable
          else
            let provisioned = Project_home_relation.create_provisioned_home () in
            let provisioned_status = Project_home_relation.status provisioned in
            let row_shape_ok =
              positive relation_id
              && String.equal relation_type "home"
              && provisioned_status = Project_home_relation.Accepted
              && Project_home_relation.request_note provisioned = None
              && String.equal status_raw
                   (Project_home_relation.string_of_status provisioned_status)
              && Project_home_relation.status_of_string status_raw
                 = Some Project_home_relation.Accepted
              && requested_by = None && reviewed_by = None
              && stored_note = None && has_reviewed_at && removed_at_null
              && reviewed_ge && updated_ge
            in
            if not row_shape_ok then rollback_to Inconsistent_data
            else (
              C.find relation_sig_query relation_id >>= function
              | Error _ -> rollback_to Storage_error
              | Ok relation_sig_before ->
                  k ~relation_id ~onboarding_state ~project_sig_before
                    ~relation_sig_before)
    in

    (* Step 3: both authorization rows are locked, in this fixed order,
       before either decides anything — short-circuiting would make the
       set of rows a caller locks depend on which authority it holds, and
       two such callers could then deadlock. Which authority was present
       never leaves this step: member, non-member, 'mod', 'legacy_mod',
       downgraded top moderator, session-only admin, and deleted user all
       collapse into the same Draft_unavailable at the decision point. *)
    let authorize ~community_id k =
      C.find_opt lock_top_moderator_query (actor_user_id, community_id)
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok moderator_row ->
          if
            not
              (match moderator_row with
              | None -> true
              | Some role -> String.equal role "top_mod")
          then rollback_to Inconsistent_data
          else
            let top_mod_ok = moderator_row <> None in
            C.find_opt lock_admin_query actor_user_id >>= function
            | Error _ -> rollback_to Storage_error
            | Ok admin_row ->
                if admin_row = Some false then rollback_to Inconsistent_data
                else
                  let admin_ok = admin_row = Some true in
                  if top_mod_ok || admin_ok then k ()
                  else rollback_to Draft_unavailable
    in

    (* Step 2: the exact community, under the held project lock. Missing,
       renamed, legacy, and already published all collapse into one
       answer; only a malformed network tuple (a leaking draft, a private
       or mixed published shape) or a noncanonical identity is
       corruption. *)
    let lock_community ~candidate_community_id ~candidate_relation_id
        ~project_id ~project_sig_before k =
      C.find_opt lock_community_query
        (candidate_community_id, current_community_slug)
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok None -> rollback_to Draft_unavailable
      | Ok
          (Some
            ( ((community_id, stored_name), (stored_slug, stored_description)),
              ( (visibility_raw, onboarding_raw),
                (is_network, indexable, discoverable) ) )) -> (
          match
            ( Db.community_visibility_of_string visibility_raw,
              Db.community_onboarding_state_of_string onboarding_raw )
          with
          | None, _ | _, Error _ -> rollback_to Inconsistent_data
          | Some visibility, Ok onboarding_state ->
              if not is_network then
                (* Legacy communities are outside this lifecycle. *)
                rollback_to Draft_unavailable
              else
                let exact_draft_state =
                  onboarding_state = Db.Community_draft
                  && visibility = Db.Community_private
                  && (not indexable) && not discoverable
                in
                if not exact_draft_state then
                  if
                    Network_communities.lifecycle_state_valid
                      ~is_network_community:is_network ~onboarding_state
                      ~visibility ~indexable ~discoverable
                  then
                    (* A valid non-draft shape: already published. *)
                    rollback_to Draft_unavailable
                  else rollback_to Inconsistent_data
                else if
                  not
                    (community_id > 0
                    && community_id = candidate_community_id
                    && String.equal stored_slug current_community_slug
                    && canonical_network_slug stored_slug
                    && valid_network_name stored_name
                    && valid_network_description stored_description
                    && Network_communities.lifecycle_state_valid
                         ~is_network_community:is_network ~onboarding_state
                         ~visibility ~indexable ~discoverable)
                then rollback_to Inconsistent_data
                else
                  authorize ~community_id (fun () ->
                      lock_relation ~candidate_relation_id ~community_id
                        ~project_id ~onboarding_state ~project_sig_before k))
    in

    (* Step 1: the permanent project, locked before anything else. A
       candidate project that disappeared through a concurrent durable
       change reads as the same unavailability as everything else. *)
    let lock_project ~candidate_project_id ~candidate_community_id
        ~candidate_relation_id k =
      C.find_opt lock_project_query candidate_project_id >>= function
      | Error _ -> rollback_to Storage_error
      | Ok None -> rollback_to Draft_unavailable
      | Ok (Some (project_id, stored_verification)) ->
          if not (positive project_id && Int64.equal project_id candidate_project_id)
          then rollback_to Inconsistent_data
          else if not (known_verification_status stored_verification) then
            rollback_to Inconsistent_data
          else (
            C.find project_sig_query project_id >>= function
            | Error _ -> rollback_to Storage_error
            | Ok project_sig_before ->
                lock_community ~candidate_community_id ~candidate_relation_id
                  ~project_id ~project_sig_before k)
    in

    (* Candidate resolution, before any lock: the community row by the
       supplied slug, then its accepted home relation, only to learn the
       project to lock first. Nothing is authorized or mutated here, and
       everything is re-read under lock. *)
    let rec resolve_candidates () =
      C.collect_list candidate_community_query current_community_slug
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok [] -> rollback_to Draft_unavailable
      | Ok (_ :: _ :: _) ->
          (* communities_slug_key admits one row per slug; two is durable
             corruption. *)
          rollback_to Inconsistent_data
      | Ok [ (candidate_community_id, candidate_network, onboarding_raw) ]
        ->
          if candidate_community_id <= 0 then rollback_to Inconsistent_data
          else if not candidate_network then
            (* A legacy community is outside this lifecycle; it typically
               carries no provisioned relation at all, so it must collapse
               here, before the relation lookup could misread its absence
               as corruption. *)
            rollback_to Draft_unavailable
          else (
            match Db.community_onboarding_state_of_string onboarding_raw with
            | Error _ -> rollback_to Inconsistent_data
            | Ok Db.Community_published ->
                (* Already published — including a published community
                   whose home relation was legitimately removed later, for
                   which a zero-relation candidate would otherwise read as
                   corruption. *)
                rollback_to Draft_unavailable
            | Ok Db.Community_draft -> resolve_candidate_relation
                                         ~candidate_community_id)

    and resolve_candidate_relation ~candidate_community_id =
      C.collect_list candidate_relation_query candidate_community_id
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok [] ->
          (* A dedicated-community draft is provisioned with its accepted
             relation atomically; none is corruption. *)
          rollback_to Inconsistent_data
      | Ok (_ :: _ :: _) -> rollback_to Inconsistent_data
      | Ok
          [ ( (candidate_relation_id, candidate_project_id),
              (provenance_null, lifecycle_ok) )
          ] ->
          if
            not
              (positive candidate_relation_id
              && positive candidate_project_id
              && provenance_null && lifecycle_ok)
          then rollback_to Inconsistent_data
          else
            lock_project ~candidate_project_id ~candidate_community_id
              ~candidate_relation_id
              (fun ~relation_id ~onboarding_state ~project_sig_before
                   ~relation_sig_before ->
                validate_draft
                  ~community_id:candidate_community_id
                  ~project_id:candidate_project_id ~relation_id
                  ~onboarding_state ~project_sig_before
                  ~relation_sig_before)
    in

    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> resolve_candidates ()
