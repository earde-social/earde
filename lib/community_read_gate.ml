(* === PRIVATE-COMMUNITY READ GATE ===
   Privacy is a server-side permission and is decided HERE, in the handler (the security
   boundary) — never trusted from the client. Public communities are readable by everyone; a
   private community is readable only by a global admin, a community moderator, or a member.
   The pure Community_types.can_read_community encodes the decision; this wrapper gathers the booleans from
   real DB/session checks. Fails CLOSED: a membership/mod DB error denies access, unless the
   viewer is a global admin (whose authority does not depend on a per-community row).

   [admin_override] must be CURRENT durable admin authority — the value of
   [current_admin_read_override] for this request — never the raw session
   claim. The label is spelled differently from the session field on purpose:
   passing [Dream.session_field request "is_admin" = Some "true"] here is the
   stale-admin bug this boundary exists to refuse. *)
let can_view_community db ~user_id ~admin_override (community : Community_types.community) =
  let is_admin = admin_override in
  match community.Community_types.visibility with
  | Community_types.Community_public -> Lwt.return true
  | Community_types.Community_private ->
      if is_admin then Lwt.return true
      else if user_id <= 0 then Lwt.return false
      else begin
        let%lwt is_member =
          match%lwt Membership_store.is_member db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
        let%lwt is_mod =
          match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false in
        Lwt.return (Community_types.can_read_community community.Community_types.visibility ~is_member ~is_mod ~is_admin)
      end

(* SEO/discovery, NOT access control: a community-content page renders for an
   authorized viewer but must carry <meta robots noindex> when the community is private
   (always effectively non-indexable) or public-but-indexable=false ("unlisted-ish"). Mirrors
   the DB-level public-discovery filter so noindex and feed/search exclusion stay in lockstep. *)
let community_noindex (community : Community_types.community) =
  not (Community_types.effective_indexable_community community.Community_types.visibility ~community_indexable:community.indexable)

(* Noindex for a CHILD surface (a forum section page or a channel archive page).
   effective_indexable_child encodes the dominance order: a private community kills it outright,
   otherwise BOTH the community and the child must be indexable. A non-indexable child still
   RENDERS (this is SEO only, not access control) — the handler never gates on it. *)
let child_noindex (community : Community_types.community) ~child_indexable =
  not (Community_types.effective_indexable_child community.Community_types.visibility
         ~community_indexable:community.indexable ~child_indexable)

(* A thread inherits noindex from the forum section it lives in. A post in a
   non-indexable section is noindex even inside a public/indexable community; community-level
   rules (private / community indexable=false) still dominate via child_noindex. A post with no
   section (root/legacy/uncategorized — section_slug = None) falls back to community noindex.
   Fails SAFE: if a post claims a section we cannot resolve, prefer noindex over leaking it. *)
let thread_noindex db (community : Community_types.community) (post : Post_types.post) =
  match post.Post_types.section_slug with
  | None -> Lwt.return (community_noindex community)
  | Some slug ->
      match%lwt Section_store.get_section_by_slug db slug community.id with
      | Ok (Some section) ->
          Lwt.return (child_noindex community ~child_indexable:section.indexable)
      | _ -> Lwt.return true

(* Single 404 used for BOTH a missing community AND a denied private read, so a hidden private
   community is byte-for-byte indistinguishable from one that never existed (no enumeration).
   Always returns to "/" — never links back into the (possibly private) community. *)
let community_not_found ?user request =
  Dream.respond ~status:`Not_Found
    (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist."
       ~alert_type:"error" ~return_url:"/" request)
