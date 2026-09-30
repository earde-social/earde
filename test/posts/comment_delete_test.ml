module CD = Earde.Handlers.Comment_delete

(* /delete-comment authorization matrix — pure. The decision function takes no
   community id at all: the old handler trusted a hidden community_id form field
   for its moderator check, which is exactly what allowed a moderator of
   community A to delete community B's comment. Only the session role and the
   server-resolved owner may matter, so a forged community field cannot affect
   authorization by construction. *)

let cd_str = function
  | CD.Admin_delete -> "admin_delete"
  | CD.Author_delete -> "author_delete"
  | CD.Forbidden -> "forbidden"

let check_cd name expected ~is_admin ~requester_id ~owner_id =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected
        (cd_str (CD.decide ~is_admin ~requester_id ~owner_id)))

let suites =
    (* /delete-comment matrix: author-only for non-admins; admins delete with
       the admin label; moderators are refused (the mod_delete flow is the only
       community-removal path). decide has no community parameter, so there is
       nothing a forged hidden community_id could influence. *)
  [ ( "delete_comment_authorization"
    , [ check_cd "author deletes own comment" "author_delete"
          ~is_admin:false ~requester_id:7 ~owner_id:7
      ; check_cd "non-author (incl. any moderator) refused" "forbidden"
          ~is_admin:false ~requester_id:7 ~owner_id:8
      ; check_cd "admin deletes any comment" "admin_delete"
          ~is_admin:true ~requester_id:7 ~owner_id:8
      ; check_cd "admin deleting own comment stays on the admin path" "admin_delete"
          ~is_admin:true ~requester_id:7 ~owner_id:7
      ] )
  ]
