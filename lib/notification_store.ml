open Lwt.Infix

type notification = {
  id : int;
  user_id : int;
  post_id : int option;
  notif_type : string;
  (* NULL for the structured project-home and community-connection kinds,
     which render from the joined display fields instead of stored prose. *)
  message : string option;
  is_read : bool;
  created_at : string;
  project_name : string option;
  project_slug : string option;
  (* The notification's own community subject. For the community-connection
     kinds this is the recipient's management context, never the
     counterpart. *)
  community_name : string option;
  community_slug : string option;
  (* The other community of a connection notification, derived at read time
     relative to community_slug. NULL for every other kind. *)
  counterpart_name : string option;
  counterpart_slug : string option;
  (* The shared-thread subjects, joined at read time and NULL for every
     other kind. The two _visible booleans are the recipient's CURRENT read
     access to each side (public, or member/moderator/durable admin),
     computed in the same bounded query: the thread title is canonical
     content and the community names may since have gone private, so the
     renderer must not show a detail its recipient could no longer reach. *)
  st_post_id : int option;
  st_post_title : string option;
  st_origin_name : string option;
  st_origin_slug : string option;
  st_destination_name : string option;
  st_destination_slug : string option;
  (* Whether n.community_id is the placement's origin side — which of the
     two management surfaces this recipient's copy should point at. *)
  st_origin_context : bool option;
  st_origin_visible : bool option;
  st_destination_visible : bool option;
  (* Whether the recipient may CURRENTLY open the two authorized target
     surfaces the rendered links point at — the origin Share page and the
     destination Shared-threads management page. Read access is not enough
     for either: a link the recipient is guaranteed to 404 on must not
     render, so these mirror the target pages' own gates (see the query). *)
  st_share_capable : bool option;
  st_manage_capable : bool option;
}

let get_notifications_query =
  let open Caqti_request.Infix in
  (* Caqti arity limit: encode 13 columns as t2(t2(t2(t4, t3), t4), t2)
     nested tuples.
     post_id is nullable since ban notifications have no associated post;
     message is nullable since the structured kinds carry no prose.
     The project/community display fields ride the same bounded query
     (LEFT JOINs are NULL for legacy rows) so the notification list never
     issues per-row entity lookups.

     The last two columns are the community-connection counterpart, derived
     here rather than stored: the connection row carries the unordered pair,
     and the counterpart is whichever side is not n.community_id — the
     recipient's own management context. That makes the same stored row read
     correctly from either direction, and lets a later slug or name change
     show through without touching notification history. The join chain is
     NULL for every non-connection kind, and NULL again if the connection or
     either community has since gone: the renderer degrades to a generic
     line rather than inventing names.

     The shared-thread block rides the same statement the same way: the
     placement joins its canonical post and both communities, and the two
     *_visible booleans answer, per recipient and per side, "may this user
     currently read that community" (public, or a membership row, a
     moderator row, or a durable users.is_admin). They are computed here —
     still one bounded query, guarded to shared-thread rows only — because
     the renderer must gate the thread title, the community names, and the
     links on CURRENT access: a historical notification row must not keep
     disclosing a title or slug its recipient has since lost.

     Read access decides what may be NAMED; the last two columns decide
     what may be LINKED, because the two link targets carry stricter gates
     than reading. st_share_capable mirrors the Share page's own rule
     (Shared_thread_placement_read_model.authorize_share_query plus its
     tombstone collapse): the canonical author only while a current origin
     member and free of both ban kinds, an exact origin top_mod, or a
     durable users.is_admin behind the session claim ($2 — which only
     enables the durable check, never replaces it). st_manage_capable
     mirrors the management page's rule
     (Shared_thread_placement_management_read_model.load_community_query):
     an exact destination top_mod or that same durable-admin pair — mere
     membership, mod, and legacy_mod grant nothing. A rendered link the
     recipient is guaranteed to 404 on is a broken promise, so the
     renderer must drop to the canonical thread or to plain text when the
     matching capability is false. All eleven columns are NULL for every
     non-shared-thread kind, and NULL again if the placement chain has
     since gone (its FKs cascade), which the renderer reads as the generic
     degraded line. *)
  (Caqti_type.(t2 int bool)
   ->* Caqti_type.(
         t2
           (t2
             (t2
                (t2 (t4 int int (option int) string) (t3 (option string) bool string))
                (t4 (option string) (option string) (option string) (option string)))
             (t2 (option string) (option string)))
           (t2
              (t4 (option int) (option string) (option string) (option string))
              (t2 (t3 (option string) (option string) (option bool))
                 (t4 (option bool) (option bool) (option bool) (option bool))))))
  (Printf.sprintf
  "SELECT n.id, n.user_id, n.post_id, n.notif_type, n.message, n.is_read, n.created_at::text,
          p.name, p.slug, c.name, c.slug, cp.name, cp.slug,
          stp_post.id, stp_post.title, sto.name, sto.slug, std.name, std.slug,
          (stp.origin_community_id = n.community_id),
          CASE WHEN sto.id IS NULL THEN NULL
               ELSE (sto.visibility = 'public'
                     OR EXISTS (SELECT 1 FROM community_members stv
                                WHERE stv.user_id = n.user_id AND stv.community_id = sto.id)
                     OR EXISTS (SELECT 1 FROM community_moderators stvm
                                WHERE stvm.user_id = n.user_id AND stvm.community_id = sto.id)
                     OR EXISTS (SELECT 1 FROM users stvu
                                WHERE stvu.id = n.user_id AND stvu.is_admin)) END,
          CASE WHEN std.id IS NULL THEN NULL
               ELSE (std.visibility = 'public'
                     OR EXISTS (SELECT 1 FROM community_members stw
                                WHERE stw.user_id = n.user_id AND stw.community_id = std.id)
                     OR EXISTS (SELECT 1 FROM community_moderators stwm
                                WHERE stwm.user_id = n.user_id AND stwm.community_id = std.id)
                     OR EXISTS (SELECT 1 FROM users stwu
                                WHERE stwu.id = n.user_id AND stwu.is_admin)) END,
          CASE WHEN stp_post.id IS NULL OR sto.id IS NULL THEN NULL
               ELSE ((stp_post.content IS NULL OR stp_post.content NOT IN (%s))
                     AND ((stp_post.user_id = n.user_id
                           AND EXISTS (SELECT 1 FROM community_members sca
                                       WHERE sca.user_id = n.user_id AND sca.community_id = sto.id)
                           AND NOT EXISTS (SELECT 1 FROM community_bans scb
                                           WHERE scb.user_id = n.user_id AND scb.community_id = sto.id)
                           AND NOT EXISTS (SELECT 1 FROM users scu
                                           WHERE scu.id = n.user_id AND scu.is_banned))
                          OR EXISTS (SELECT 1 FROM community_moderators scm
                                     WHERE scm.user_id = n.user_id AND scm.community_id = sto.id
                                       AND scm.role = 'top_mod')
                          OR ($2 AND EXISTS (SELECT 1 FROM users sce
                                             WHERE sce.id = n.user_id AND sce.is_admin)))) END,
          CASE WHEN std.id IS NULL THEN NULL
               ELSE (EXISTS (SELECT 1 FROM community_moderators sdm
                             WHERE sdm.user_id = n.user_id AND sdm.community_id = std.id
                               AND sdm.role = 'top_mod')
                     OR ($2 AND EXISTS (SELECT 1 FROM users sde
                                        WHERE sde.id = n.user_id AND sde.is_admin))) END
   FROM notifications n
   LEFT JOIN open_source_projects p ON p.id = n.project_id
   LEFT JOIN communities c ON c.id = n.community_id
   LEFT JOIN community_connections cc ON cc.id = n.connection_id
   LEFT JOIN communities cp
          ON cp.id = CASE WHEN cc.requester_community_id = n.community_id
                          THEN cc.recipient_community_id
                          ELSE cc.requester_community_id END
   LEFT JOIN shared_thread_placements stp ON stp.id = n.shared_thread_placement_id
   LEFT JOIN posts stp_post ON stp_post.id = stp.post_id
   LEFT JOIN communities sto ON sto.id = stp.origin_community_id
   LEFT JOIN communities std ON std.id = stp.destination_community_id
   WHERE n.user_id = $1 ORDER BY n.created_at DESC LIMIT 50"
  (* The Share page refuses tombstoned threads for every viewer, so the
     capability must too. The labels come from the one pure authority,
     never respelled here; they are fixed quote-free bytes, safe to splice
     as SQL string literals. *)
  (String.concat ", "
     (List.map
        (fun label -> "'" ^ label ^ "'")
        Shared_thread_placements.tombstone_labels)))

let get_notifications (module C: Caqti_lwt.CONNECTION) ~session_admin user_id =
  C.collect_list get_notifications_query (user_id, session_admin) >>= function
  | Ok rows -> Lwt.return (Ok (List.map (fun (((((id, user_id, post_id, notif_type), (message, is_read, created_at)), (project_name, project_slug, community_name, community_slug)), (counterpart_name, counterpart_slug)), ((st_post_id, st_post_title, st_origin_name, st_origin_slug), ((st_destination_name, st_destination_slug, st_origin_context), (st_origin_visible, st_destination_visible, st_share_capable, st_manage_capable)))) -> {id; user_id; post_id; notif_type; message; is_read; created_at; project_name; project_slug; community_name; community_slug; counterpart_name; counterpart_slug; st_post_id; st_post_title; st_origin_name; st_origin_slug; st_destination_name; st_destination_slug; st_origin_context; st_origin_visible; st_destination_visible; st_share_capable; st_manage_capable}) rows))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let count_unread_notifs_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM notifications WHERE user_id = $1 AND is_read = FALSE"

let count_unread_notifs (module C: Caqti_lwt.CONNECTION) user_id =
  C.find count_unread_notifs_query user_id >>= function
  | Ok c -> Lwt.return (Ok c)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let mark_notifs_read_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE notifications SET is_read = TRUE WHERE user_id = $1"

let mark_notifs_read (module C: Caqti_lwt.CONNECTION) user_id =
  C.exec mark_notifs_read_query user_id >>= function
  | Ok () -> Lwt.return (Ok())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let create_notif_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t4 int (option int) string string) ->. Caqti_type.unit)
  "INSERT INTO notifications (user_id, post_id, notif_type, message) VALUES ($1, $2, $3, $4)"

let create_notif (module C: Caqti_lwt.CONNECTION) user_id post_id_opt notif_type message =
  C.exec create_notif_query (user_id, post_id_opt, notif_type, message) >>= function
  | Ok () -> Lwt.return (Ok())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

(* Notification delivery is best-effort; collapse Ok None and Error into the same
   Error path so callers can skip silently if the post/comment was deleted. *)
let get_post_owner_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "SELECT user_id FROM posts WHERE id = $1"

let get_post_owner (module C: Caqti_lwt.CONNECTION) pid =
  C.find_opt get_post_owner_query pid >>= function
  | Ok (Some id) -> Lwt.return (Ok id)
  | _ -> Lwt.return (Error "not found")

let get_comment_owner_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "SELECT user_id FROM comments WHERE id = $1"

let get_comment_owner (module C: Caqti_lwt.CONNECTION) cid =
  C.find_opt get_comment_owner_query cid >>= function
  | Ok (Some id) -> Lwt.return (Ok id)
  | _ -> Lwt.return (Error "not found")

let get_comment_post_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "SELECT post_id FROM comments WHERE id = $1"

let get_comment_post_id (module C: Caqti_lwt.CONNECTION) cid =
  C.find_opt get_comment_post_id_query cid >>= function
  | Ok (Some id) -> Lwt.return (Ok id)
  | _ -> Lwt.return (Error "not found")
