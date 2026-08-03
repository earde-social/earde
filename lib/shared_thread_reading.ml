(* The shared-thread read side of the canonical thread route and the comment
   write path: whether one canonical post may be read in a requested
   DESTINATION community context, and whether one user currently holds a
   participation path onto the canonical discussion. Read-only: this module
   owns its SQL, takes no locks, writes nothing, performs no network IO, and
   logs nothing. Errors are payload-free; Caqti/PostgreSQL details are
   dropped, never returned or logged.

   Deliberately NOT here: the viewer's access to the destination community
   itself. [resolve_destination_context] answers only the placement facts
   (accepted status, destination binding, currently-public origin); the
   caller then applies the destination community's one existing
   can_view_community rule to the community record it loads by the returned
   id — so no third divergent read-authorization rule exists. *)

open Lwt.Infix

module Stp = Shared_thread_placements

type destination_context = {
  destination_community_id : int;
  destination_section : (string * string) option;
  origin_community_name : string;
}

type error = Storage_error

(* Every community is addressed at /c/:slug, so a usable slug is one
   non-empty URL path segment — the same shape the sibling read models
   require. Anything else reads as absent, never as an error. *)
let addressable_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* At most one row can match: the partial unique index allows one ACTIVE
   placement per (post, destination), and a slug names one community. The
   origin-public condition is part of the context itself — a private
   origin's discussion must not leak through a destination rendering — while
   connection state, discoverability, and onboarding eligibility are
   deliberately absent: they gate request and acceptance, not continued
   rendering. *)
let destination_context_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string)
   ->? Caqti_type.(t2 (t2 int (option string)) (t2 (option string) string)))
  "SELECT d.id, ds.name, ds.slug, oc.name \
   FROM shared_thread_placements stp \
   JOIN communities d ON d.id = stp.destination_community_id \
   JOIN communities oc ON oc.id = stp.origin_community_id \
   LEFT JOIN community_sections ds ON ds.id = stp.destination_section_id \
   WHERE stp.post_id = $1 AND d.slug = $2 \
     AND stp.status = 'accepted' \
     AND oc.visibility = 'public'"

let resolve_destination_context (module C : Caqti_lwt.CONNECTION) ~post_id
    ~destination_slug =
  if post_id <= 0 || not (addressable_slug destination_slug) then
    Lwt.return (Ok None)
  else
    C.find_opt destination_context_query (post_id, destination_slug)
    >>= function
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some ((community_id, section_name), (section_slug, origin_name))) ->
        let destination_section =
          match (section_name, section_slug) with
          | Some name, Some slug -> Some (name, slug)
          | _ -> None
        in
        Lwt.return
          (Ok
             (Some
                { destination_community_id = community_id;
                  destination_section;
                  origin_community_name = origin_name;
                }))
    | Error _ -> Lwt.return (Error Storage_error)

(* The one comment-participation capability, spelled entirely in SQL so the
   composer gate and the POST /comments authorization cannot drift. A
   logged-in user may comment on the canonical discussion iff:
     - the canonical post exists and is not tombstoned (the label set is
       spliced from the pure domain authority at construction time);
     - they are not globally banned;
     - they are not banned in the canonical ORIGIN community (an origin ban
       blocks every path, through every placement);
     - and at least one participation path is current:
         * membership in the origin community, or
         * membership in a destination community whose placement is
           currently readable (accepted + origin currently public), with no
           ban in THAT destination — a ban there closes that one path only.
   Pending, rejected, removed, and withdrawn placements grant nothing; a
   disconnected accepted placement still grants (accepted rendering survives
   disconnection); a private destination grants through its own current
   membership. No route, form, or session value participates — every
   qualifying community is derived from the post and its placements. *)
let may_comment_query =
  let open Caqti_request.Infix in
  let tombstones =
    (* The closed label set from the one pure authority — never respelled. *)
    String.concat ", "
      (List.map (fun label -> "'" ^ label ^ "'") Stp.tombstone_labels)
  in
  (Caqti_type.(t2 int int) ->! Caqti_type.bool)
    (Printf.sprintf
       "SELECT EXISTS (\
          SELECT 1 FROM posts p \
          JOIN communities oc ON oc.id = p.community_id \
          WHERE p.id = $1 \
            AND (p.content IS NULL OR p.content NOT IN (%s)) \
            AND NOT EXISTS (SELECT 1 FROM users gu \
                            WHERE gu.id = $2 AND gu.is_banned) \
            AND NOT EXISTS (SELECT 1 FROM community_bans ob \
                            WHERE ob.user_id = $2 \
                              AND ob.community_id = p.community_id) \
            AND (EXISTS (SELECT 1 FROM community_members om \
                         WHERE om.user_id = $2 \
                           AND om.community_id = p.community_id) \
                 OR EXISTS (\
                      SELECT 1 FROM shared_thread_placements stp \
                      JOIN community_members dm \
                        ON dm.user_id = $2 \
                       AND dm.community_id = stp.destination_community_id \
                      WHERE stp.post_id = p.id \
                        AND stp.status = 'accepted' \
                        AND oc.visibility = 'public' \
                        AND NOT EXISTS (\
                              SELECT 1 FROM community_bans dbn \
                              WHERE dbn.user_id = $2 \
                                AND dbn.community_id = \
                                    stp.destination_community_id))))"
       tombstones)

let viewer_may_comment (module C : Caqti_lwt.CONNECTION) ~user_id ~post_id =
  if user_id <= 0 || post_id <= 0 then Lwt.return (Ok false)
  else
    C.find may_comment_query (post_id, user_id) >>= function
    | Ok allowed -> Lwt.return (Ok allowed)
    | Error _ -> Lwt.return (Error Storage_error)
