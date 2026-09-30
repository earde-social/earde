(* A vote is a durable mutation — the vote row, the target's score and the
   AUTHOR's local karma — so it carries the same ban gate as posting and
   commenting, for every direction including 0 (removal).

   Both checks read CURRENT durable state. A global ban revokes the banned
   user's sessions, but authorization must not depend on revocation being the
   only line of defence; a community ban revokes nothing at all, so before this
   gate existed a community-banned user with a live session could still move
   scores and karma in the community that banned them.

   The gate FAILS CLOSED: a storage failure while establishing ban state is
   never read as "not banned". *)
type vote_gate =
  | Vote_allowed
  | Vote_target_missing  (* no such post/comment — left to the existing vote semantics *)
  | Vote_globally_banned
  | Vote_community_banned
  | Vote_gate_error of string

(* [resolve] derives the owning community FROM THE TARGET; no caller passes a
   community id off the request. The global check runs first: it needs no
   target, and it refuses a globally banned caller without touching — or
   revealing anything about — the id they submitted. *)
let vote_ban_gate db ~user_id ~resolve =
  match%lwt Admin_store.is_globally_banned db user_id with
  | Error err -> Lwt.return (Vote_gate_error err)
  | Ok true -> Lwt.return Vote_globally_banned
  | Ok false -> (
      match%lwt resolve db with
      | Error err -> Lwt.return (Vote_gate_error err)
      | Ok None -> Lwt.return Vote_target_missing
      | Ok (Some community_id) -> (
          match%lwt Community_ban_store.is_banned db user_id community_id with
          | Error err -> Lwt.return (Vote_gate_error err)
          | Ok true -> Lwt.return Vote_community_banned
          | Ok false -> Lwt.return Vote_allowed))

(* Shared by both vote handlers: the refusals are plain text like the rest of
   the vote surface, and the storage failure goes through the generic DB-error
   boundary rather than leaking Caqti detail. *)
let vote_gate_refusal = function
  | Vote_globally_banned ->
      Some (Dream.respond ~status:`Forbidden "Your account has been permanently banned from Earde.")
  | Vote_community_banned ->
      Some (Dream.respond ~status:`Forbidden "You are banned from this community.")
  | Vote_gate_error err ->
      Some (Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err))
  | Vote_allowed | Vote_target_missing -> None

(* direction=0 removes the vote; +1/-1 upserts. The DB uses ON CONFLICT DO UPDATE,
   making this idempotent — double-clicks and network retries are safe. *)
let vote_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let post_id = try int_of_string (List.assoc_opt "post_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let direction = try int_of_string (List.assoc_opt "direction" form_data |> Option.value ~default:"") with _ -> 99 in
          (* Clamp direction: only -1, 0, +1 are valid — reject crafted submissions silently. *)
          if post_id = 0 || not (direction = -1 || direction = 0 || direction = 1) then
            Dream.respond ~status:`Bad_Request "Invalid vote parameters."
          else
          Dream.sql request (fun db ->
            (* Ban gate first: no vote mutation — add, change or remove — may
               happen until it has succeeded. The owning community comes from
               the post row, never from the submitted form. *)
            let%lwt gate =
              vote_ban_gate db ~user_id
                ~resolve:(fun db -> Post_store.get_post_community_id db post_id)
            in
            match vote_gate_refusal gate with
            | Some refusal -> refusal
            | None ->
            (* Guard downvote at the handler boundary — community may have disabled them. *)
            let%lwt downvotes_ok =
              if direction = -1 then Community_store.get_allows_downvotes_for_post db post_id
              else Lwt.return (Ok true)
            in
            match downvotes_ok with
            | Error _ -> Dream.respond ~status:`Internal_Server_Error "DB Error: could not check community settings."
            | Ok false -> Dream.respond ~status:`Forbidden "Downvotes are disabled in this community."
            | Ok true ->
            let%lwt db_action =
              if direction = 0 then Post_store.remove_post_vote db user_id post_id
              else Post_store.vote_post db user_id post_id direction
            in

            match db_action with
            | Ok () ->
                let referer = Handler_support.safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."

(* Same idempotent upsert semantics as vote_handler; kept separate to avoid a
   polymorphic action field that would couple post and comment vote paths. *)
let vote_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let comment_id = try int_of_string (List.assoc_opt "comment_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let direction = try int_of_string (List.assoc_opt "direction" form_data |> Option.value ~default:"") with _ -> 99 in
          (* Same direction guard as vote_handler. *)
          if comment_id = 0 || not (direction = -1 || direction = 0 || direction = 1) then
            Dream.respond ~status:`Bad_Request "Invalid vote parameters."
          else
          Dream.sql request (fun db ->
            (* Same gate as vote_handler, resolved through the comment's
               canonical parent post — the community that owns the discussion,
               not whatever destination community the browser was reading it
               from. *)
            let%lwt gate =
              vote_ban_gate db ~user_id
                ~resolve:(fun db -> Comment_store.get_comment_community_id db comment_id)
            in
            match vote_gate_refusal gate with
            | Some refusal -> refusal
            | None ->
            let%lwt downvotes_ok =
              if direction = -1 then Community_store.get_allows_downvotes_for_comment db comment_id
              else Lwt.return (Ok true)
            in
            match downvotes_ok with
            | Error _ -> Dream.respond ~status:`Internal_Server_Error "DB Error: could not check community settings."
            | Ok false -> Dream.respond ~status:`Forbidden "Downvotes are disabled in this community."
            | Ok true ->
            let%lwt db_action =
              if direction = 0 then Comment_store.remove_comment_vote db user_id comment_id
              else Comment_store.vote_comment db user_id comment_id direction
            in

            match db_action with
            | Ok () ->
                let referer = Handler_support.safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request referer
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."
