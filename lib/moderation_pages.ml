open Html.Infix

let manage_mods_page ?user ?(rail_communities = []) ~is_admin ~current_user_role
    ~(channels : Channel_store.channel list) ~(sections : Section_store.community_section list)
    ~(community : Community_types.community) ~(mods : Moderator_store.moderator_entry list) request =
  let csrf_token = Csrf_field.tag request in
  let slug = (Html.text (community.slug)) in

  (* Group mods by role for visual separation. *)
  let top_mods   = List.filter (fun m -> m.Moderator_store.role = "top_mod")   mods in
  let regular_mods = List.filter (fun m -> m.Moderator_store.role = "mod")     mods in
  let legacy_mods  = List.filter (fun m -> m.Moderator_store.role = "legacy_mod") mods in

  let can_manage = is_admin || current_user_role = Some "top_mod" in

  let render_top_mod_row (m : Moderator_store.moderator_entry) =
    (* A top_mod cannot demote another top_mod; only admins have that power.
       Prevents power consolidation by a single top_mod ousting peers. *)
    let action_btn =
      if is_admin then
        (Html.template "
          <form action='/c/%s/manage-mods/remove' method='POST' class='cm-form-inline' onsubmit=\"confirmModal(event, 'Demote this Top Mod? They will be removed from the council.')\">
            %s
            <input type='hidden' name='target_user_id' value='%s'>
            <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Remove</button>
          </form>"
  [ slug
  ; csrf_token
  ; Html.int (m.user_id) ])
      else (Html.static "<span class='cm-muted-note'>Top Mod</span>")
    in
    (Html.template "
      <div class='cm-list-row'>
        <div><a href='/u/%s' class='cm-user-link'>u/%s</a><span class='cm-badge cm-badge--top'>Top Mod</span></div>
        %s
      </div>"
  [ (Html.text (m.username))
  ; (Html.text (m.username))
  ; action_btn ])
  in

  let render_mod_row (m : Moderator_store.moderator_entry) =
    let action_btns =
      if can_manage then
        (Html.template "
          <div class='cm-row-actions'>
            <form action='/c/%s/manage-mods/promote' method='POST' class='cm-form-inline'>
              %s
              <input type='hidden' name='target_user_id' value='%s'>
              <button type='submit' class='cm-btn-sm cm-btn-sm--promote'>Promote to Top Mod</button>
            </form>
            <form action='/c/%s/manage-mods/remove' method='POST' class='cm-form-inline' onsubmit=\"confirmModal(event, 'Remove this moderator?')\">
              %s
              <input type='hidden' name='target_user_id' value='%s'>
              <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Remove</button>
            </form>
          </div>"
  [ slug
  ; csrf_token
  ; Html.int (m.user_id)
  ; slug
  ; csrf_token
  ; Html.int (m.user_id) ])
      else Html.empty
    in
    (Html.template "
      <div class='cm-list-row'>
        <a href='/u/%s' class='cm-user-link'>u/%s</a>
        %s
      </div>"
  [ (Html.text (m.username))
  ; (Html.text (m.username))
  ; action_btns ])
  in

  let render_legacy_row (m : Moderator_store.moderator_entry) =
    (Html.template "
      <div class='cm-list-row'>
        <div><a href='/u/%s' class='cm-user-link cm-user-link--muted'>u/%s</a><span class='cm-badge cm-badge--legacy'>Legacy</span></div>
        <span class='cm-muted-note'>No active powers</span>
      </div>"
  [ (Html.text (m.username))
  ; (Html.text (m.username)) ])
  in

  let top_mod_section =
    if top_mods = [] then (Html.static "<p class='cm-empty'>No Top Mods yet.</p>")
    else (Html.template "<div class='cm-list'>%s</div>"
  [ ((Html.join (Html.static "\n")) (List.map render_top_mod_row top_mods)) ])
  in
  let mod_section =
    if regular_mods = [] then (Html.static "<p class='cm-empty'>No standard moderators.</p>")
    else (Html.template "<div class='cm-list'>%s</div>"
  [ ((Html.join (Html.static "\n")) (List.map render_mod_row regular_mods)) ])
  in
  let legacy_section =
    if legacy_mods = [] then Html.empty
    else (Html.template "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Legacy moderators</h2>
        <p class='cm-panel-desc'>Demoted due to inactivity. No permissions granted.</p>
        <div class='cm-list'>%s</div>
      </section>"
  [ ((Html.join (Html.static "\n")) (List.map render_legacy_row legacy_mods)) ])
  in

  let add_mod_form =
    if can_manage then (Html.template "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Add new moderator</h2>
        <form action='/c/%s/manage-mods/add' method='POST' class='cm-inline-form'>
          %s
          <input type='text' name='username' required placeholder='Username' class='cm-input'>
          <button type='submit' class='cm-btn'>Add mod</button>
        </form>
      </section>"
  [ slug
  ; csrf_token ])
    else Html.empty
  in

  (* Panel body only — the shared settings shell below owns the header band
     and the internal settings navigation (Manage moderators active). *)
  let panel_body = (Html.template "
      %s

      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Top Mods</h2>
        <p class='cm-panel-desc'>Council seats (max 3). Only admins can remove Top Mods.</p>
        %s
      </section>

      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Moderators</h2>
        %s
      </section>

      %s"
  [ add_mod_form
  ; top_mod_section
  ; mod_section
  ; legacy_section ])
  in
  let content =
    Community_settings_shell.wrap ~slug:community.slug
      ~active:Community_settings_shell.Manage_moderators
      ~can_complete_setup:
        (Community_settings_shell.can_complete_setup ~community
           ~authorized:can_manage)
      ~network_manager:can_manage ~panel:panel_body ()
  in
  (* Cartographic launch shell (pass 14C): the same four-pane chrome as the
     sibling community routes, wrapping the moderator roster verbatim — the
     cm-* role sections, the add/promote/remove forms with their hidden
     target_user_id fields, the confirmModal onsubmit hooks (the shared
     launch behavior script carries confirmModal and the #modal-confirm-msg /
     #cancel-btn / #confirm-btn contract) and the role badges are treated as
     pinned, so only this outer document changed. The sidebar reuses the
     shared knowledge grammar with Manage moderators active; can_manage is
     true by construction here — the handler only renders this page for TM/A
     viewers, exactly the settings gate. [rail_communities] carries the
     viewer's joined communities as the handler loaded them
     (post-authorization only); ordering, dedup by slug and the single active
     marker stay owned by the shared launch doc builder. Replay privacy keeps
     the existing inner ph-no-capture guard for private communities (the
     launch shell additionally marks .shell, like the sibling routes; the
     launch doc also owns the analytics assets with the same
     community-id/visibility pair the legacy wrapper received). *)
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections
      ~settings_active:true ~can_manage:true ()
  in
  Community_shell.launch_community_page ?user ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-community-moderators"
    ~title:(Printf.sprintf "Manage Mods — /c/%s" community.slug)
    ~content:(Page_shell.private_replay_guard ~community content) ()

(* SSR report form (no JS). The handler is the security boundary: it re-resolves the
   target from the trusted slug + hidden type/id and re-runs the ban/self-report gates,
   so this page only sets up the inputs. target_type is emitted as the closed-variant
   string; the reason <select> values mirror report_reason_to_string. *)
let report_form_page ?user ?(rail_communities = []) ~(channels : Channel_store.channel list)
    ~(sections : Section_store.community_section list) ~can_manage ~(community : Community_types.community)
    ~(target_type : Report_store.report_target) ~target_id ~target_title ~return_url request =
  let csrf_token = Csrf_field.tag request in
  let kind_label = match target_type with
    | Report_store.Report_post -> "post"
    | Report_store.Report_comment -> "comment"
    | Report_store.Report_chat_message -> "message" in
  let target_type_s = Report_store.report_target_to_string target_type in
  (* Trim an over-long excerpt for the context line; full content stays untouched in the DB. *)
  let excerpt =
    let t = String.trim target_title in
    if String.length t <= 160 then t else String.sub t 0 157 ^ "\xe2\x80\xa6" in
  let excerpt_html =
    if excerpt = "" then Html.empty
    else (Html.template "<div class='create-field'><label class='create-label'>Reported %s</label><p class='create-hint ph-mask'>%s</p></div>"
  [ (Html.text kind_label)
  ; (Html.text (excerpt)) ]) in
  let reason_option value label =
    Html.template "<option value='%s'>%s</option>" [ Html.text value; Html.text label ]
  in
  let reasons = Html.join (Html.static "\n") [
    reason_option "spam" "Spam";
    reason_option "abuse" "Abuse / harassment";
    reason_option "off_topic" "Off-topic";
    reason_option "illegal" "Illegal / dangerous";
    reason_option "other" "Other";
  ] in
  let content = (Html.template "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Report this %s</h1>
            <p class='create-sub'>Flag content in <span class='accent'>/c/%s</span> for the moderators. Reports are private; a moderator will review it.</p>
        </div>

        <form action='/c/%s/reports' method='POST' class='create-form'>
            %s
            <input type='hidden' name='target_type' value='%s'>
            <input type='hidden' name='target_id' value='%s'>

            %s

            <div class='create-field'>
                <label class='create-label'>Reason <span class='req'>*</span></label>
                <select name='reason' required class='create-select'>
                    <option value=''>-- Select a reason --</option>
                    %s
                </select>
            </div>

            <div class='create-field'>
                <label class='create-label'>Details <span class='create-label-opt'>(optional)</span></label>
                <textarea name='details' maxlength='1000' class='create-textarea' style='min-height:120px;' placeholder='Add any context that will help a moderator (optional).'></textarea>
                <p class='create-hint'>Up to 1000 characters.</p>
            </div>

            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Submit report</button>
                <a href='%s' class='create-btn create-btn--secondary create-btn--block' style='text-align:center;'>Cancel</a>
            </div>
        </form>
      </div>
    </div>"
  [ (Html.text kind_label)
  ; (Html.text (community.slug))
  ; (Html.text (community.slug))
  ; csrf_token
  ; (Html.text target_type_s)
  ; Html.int (target_id)
  ; excerpt_html
  ; reasons
  ; (Html.internal_path (return_url)) ])
  in
  (* Cartographic launch shell (pass 14D): the same four-pane chrome as the
     sibling community routes, wrapping the report form verbatim — the create-*
     fields, the hidden target_type/target_id inputs, the reason <select>
     values, the details maxlength and the ph-mask excerpt class are treated
     as pinned, so only this outer document changed; it is styled by the
     route-scoped earde.css section. The sidebar reuses the shared knowledge
     grammar with NO active entry: reporting is a contextual action, not a
     permanent sidebar destination, and the moderator-only entries (Reports,
     Manage moderators, Home requests) stay hidden because their active flags
     default to false. can_manage is the handler's real admin-or-moderator
     check, so Settings shows only to authorized viewers — exactly the
     overview's render-time visibility rule; the settings handler re-checks
     authority. [rail_communities] carries the viewer's joined communities as
     the handler loaded them (post-authorization only); ordering, dedup by
     slug and the single active marker stay owned by the shared launch doc
     builder. Replay privacy keeps the inner ph-no-capture guard for private
     communities (the launch shell additionally marks .shell, like the
     sibling routes; the launch doc also owns the analytics assets with the
     same community-id/visibility pair the legacy wrapper received), and the
     legacy wrapper's unconditional noindex is preserved. *)
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections ~can_manage ()
  in
  Community_shell.launch_community_page ?user ~request ~noindex:true ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-report-form"
    ~title:("Report " ^ kind_label)
    ~content:(Page_shell.private_replay_guard ~community content) ()

(* Read-only community mod queue. [previews] is a (report_id -> (context_url, excerpt))
   assoc the handler built with a bounded per-row lookup; rows missing from it (chat,
   deleted, or beyond the preview cap) degrade to "Target unavailable or deleted". This
   page is PRIVATE — the handler gates it on M/TM/A; it adds no authority of its own. *)
let reports_queue_page ?user ?(rail_communities = []) ~is_admin ~is_top_mod
    ~(channels : Channel_store.channel list)
    ~(sections : Section_store.community_section list) ~(community : Community_types.community) ~(status : Report_store.report_status)
    ~(reports : Report_store.report_row list) ~(previews : (int * (string * string)) list) request =
  let csrf_token = Csrf_field.tag request in
  let slug = (Html.text (community.slug)) in
  let reason_label = function
    | Report_store.Report_spam -> "Spam"
    | Report_store.Report_abuse -> "Abuse / harassment"
    | Report_store.Report_off_topic -> "Off-topic"
    | Report_store.Report_illegal -> "Illegal / dangerous"
    | Report_store.Report_other -> "Other" in
  let target_label = function
    | Report_store.Report_post -> "post"
    | Report_store.Report_comment -> "comment"
    | Report_store.Report_chat_message -> "chat message" in
  let action_kind_label = function
    | Report_store.Report_removed_content -> "Content removed"
    | Report_store.Report_banned_author -> "Author banned"
    | Report_store.Report_other_action -> "Action taken" in
  (* Reuse the existing badge palette so the queue needs no new status-pill CSS. *)
  let status_badge = function
    | Report_store.Report_open -> (Html.static "<span class='cm-badge cm-badge--active'>Open</span>")
    | Report_store.Report_action_taken -> (Html.static "<span class='cm-badge cm-badge--top'>Action taken</span>")
    | Report_store.Report_dismissed -> (Html.static "<span class='cm-badge cm-badge--archived'>Dismissed</span>") in
  let tab st label =
    let cls = if st = status then "cm-nav-link cm-nav-link--active" else "cm-nav-link" in
    (Html.template "<a href='/c/%s/reports?status=%s' class='%s'>%s</a>"
  [ slug
  ; (Html.text (Report_store.report_status_to_string st))
  ; (Html.text cls)
  ; Html.text label ]) in
  let tabs = Html.join (Html.static "\n") [
    tab Report_store.Report_open "Open";
    tab Report_store.Report_action_taken "Action taken";
    tab Report_store.Report_dismissed "Dismissed";
  ] in
  let excerpt s =
    let t = String.trim s in
    if String.length t <= 140 then t else String.sub t 0 137 ^ "\xe2\x80\xa6" in
  let render_row (r : Report_store.report_row) =
    let reporter = (Html.text (r.reporter_username)) in
    let author = match r.target_author_username with
      | Some u -> (Html.template "<a href='/u/%s' class='cm-table-actor'>u/%s</a>"
  [ (Html.text (u))
  ; (Html.text (u)) ])
      | None -> (Html.static "<span class='cm-table-target'>unknown</span>") in
    let details_html = match r.details with
      | Some d when String.trim d <> "" ->
          (Html.template "<p class='cm-table-reason' style='margin:4px 0 0;'>%s</p>"
  [ (Html.text ((excerpt d))) ])
      | _ -> Html.empty in
    (* Context link + live preview come from the handler's bounded enrichment; a row not
       present (chat/deleted/over-cap) degrades rather than 500s. safe_internal_path gates
       the server-built path. *)
    let context_html = match List.assoc_opt r.id previews with
      | Some (url, preview) ->
          (Html.template "<a href='%s' class='cm-table-actor'>View %s &rarr;</a><br><span class='cm-table-target ph-mask'>%s</span>"
  [ (Html.internal_path (url))
  ; (Html.text (target_label r.target_type))
  ; (Html.text ((excerpt preview))) ])
      | None -> (Html.static "<span class='cm-table-target'>Target unavailable or deleted</span>") in
    (* Resolution controls live only on still-open reports; the handler re-checks both that
       the report is open and that the actor is M/TM/A (renderer is never the boundary). One
       form, two formaction submits, so the optional note applies to whichever action fires —
       no JS, no modal. Resolved rows show the recorded outcome instead. *)
    let actions_html =
      if r.status = Report_store.Report_open then
        (Html.template "
          <form method='POST' class='cm-report-actions'>
            %s
            <textarea name='resolution_note' class='cm-report-note' rows='2' maxlength='1000' placeholder='Optional note'></textarea>
            <div class='cm-report-btns'>
              <button type='submit' formaction='/c/%s/reports/%s/dismiss' class='cm-btn-sm'>Dismiss</button>
              <button type='submit' formaction='/c/%s/reports/%s/action' class='cm-btn-sm cm-btn-sm--ok'>Mark action taken</button>
            </div>
          </form>"
  [ csrf_token
  ; slug
  ; Html.int (r.id)
  ; slug
  ; Html.int (r.id) ])
      else
        let kind = match r.action_kind with
          | Some k -> (Html.template "<div>%s</div>"
  [ (Html.text ((action_kind_label k))) ])
          | None -> Html.empty in
        let resolved = match r.resolved_at with
          | Some t -> (Html.template "<div>Resolved %s</div>"
  [ (Html.text (Components.time_ago t)) ])
          | None -> Html.empty in
        let note = match r.resolution_note with
          | Some n when String.trim n <> "" -> (Html.template "<div class='ph-mask'>Note: %s</div>"
  [ (Html.text ((excerpt n))) ])
          | _ -> Html.empty in
        let body = kind ++ resolved ++ note in
        if body = Html.empty then (Html.static "<span class='cm-table-target'>&mdash;</span>")
        else (Html.template "<div class='cm-report-meta'>%s</div>"
  [ body ])
    in
    (Html.template "
      <tr>
        <td class='cm-table-when'>#%s<br>%s<br>%s</td>
        <td><a href='/u/%s' class='cm-table-actor'>u/%s</a></td>
        <td><span class='cm-badge'>%s</span> &middot; %s<br><span class='cm-table-target'>by %s</span>%s</td>
        <td>%s</td>
        <td>%s</td>
      </tr>"
  [ Html.int (r.id)
  ; (Html.text (Components.time_ago r.created_at))
  ; (status_badge r.status)
  ; reporter
  ; reporter
  ; (Html.text (target_label r.target_type))
  ; (Html.text ((reason_label r.reason)))
  ; author
  ; details_html
  ; context_html
  ; actions_html ])
  in
  let empty_msg = match status with
    | Report_store.Report_open -> "No open reports. Nothing needs your attention right now."
    | Report_store.Report_action_taken -> "No reports have been actioned yet."
    | Report_store.Report_dismissed -> "No reports have been dismissed yet." in
  let table_body =
    if reports = [] then
      (Html.template "<tr><td colspan='5' class='cm-table-empty'>%s</td></tr>"
  [ (Html.text empty_msg) ])
    else (Html.join (Html.static "\n")) (List.map render_row reports) in
  (* The per-row preview lookup is bounded (see handler). If more reports exist than were
     enriched, say so rather than silently showing "unavailable" for the tail. *)
  let preview_note =
    if List.length reports > List.length previews
       && List.exists (fun (r : Report_store.report_row) -> not (List.mem_assoc r.id previews)) reports
    then (Html.static "<p class='cm-panel-desc'>Context previews are shown for the most recent reports; older rows link from their target type where available.</p>")
    else Html.empty in
  (* Panel body only — the shared settings shell below owns the header band
     and the internal settings navigation (Moderation active: the queue is a
     moderation work surface reached from the Moderation panel). *)
  let panel_body = (Html.template "
        <section class='cm-panel'>
            <h2 class='cm-panel-title'>Reports queue</h2>
            <p class='cm-panel-desc'>Content flagged by members of this community, newest first. Private to moderators.</p>
            <nav class='cm-nav'>%s</nav>
            %s
            <div class='cm-table-wrap'>
                <table class='cm-table'>
                    <thead>
                        <tr>
                            <th>Report</th>
                            <th>Reporter</th>
                            <th>Reason / target</th>
                            <th>Context</th>
                            <th>Resolution</th>
                        </tr>
                    </thead>
                    <tbody>%s</tbody>
                </table>
            </div>
        </section>"
  [ tabs
  ; preview_note
  ; table_body ])
  in
  let content =
    Community_settings_shell.wrap ~slug:community.slug
      ~active:Community_settings_shell.Moderation
      ~can_complete_setup:
        (Community_settings_shell.can_complete_setup ~community
           ~authorized:(is_top_mod || is_admin))
      ~network_manager:(is_top_mod || is_admin) ~panel:panel_body ()
  in
  (* Cartographic launch shell (pass 14B): the same four-pane chrome as the
     sibling community routes, wrapping the review docket verbatim — the cm-*
     report rows, the one-form-two-formaction dismiss/action submits, the
     status tabs and the ph-mask replay classes are treated as pinned, so only
     this outer document and the additive mono /c/:slug context above changed.
     The sidebar reuses the shared knowledge grammar with Reports active;
     can_manage is true by construction here — the handler only renders this
     page for M/TM/A viewers, exactly the settings gate. [rail_communities]
     carries the viewer's joined communities as the handler loaded them
     (post-authorization only); ordering, dedup by slug and the single active
     marker stay owned by the shared launch doc builder. Replay privacy keeps
     the existing inner ph-no-capture guard for private communities (the
     launch shell additionally marks .shell, like the sibling routes). *)
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections
      ~settings_active:true ~can_manage:true ()
  in
  Community_shell.launch_community_page ?user ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-community-reports"
    ~title:(community.name ^ " — Reports")
    ~content:(Page_shell.private_replay_guard ~community content) ()

(* === MODERATION LOG === *)

let mod_log_page ?user ?(noindex=false) ?(rail_communities = [])
    ~(can_access_settings : bool) ~(channels : Channel_store.channel list)
    ~(sections : Section_store.community_section list)
    ~(community : Community_types.community) (actions : Mod_log_store.mod_action list) request =
  (* Mod log is member-visible, not mod-only. Send viewers who can reach settings back into the
     moderation panel; send everyone else back to the community home. *)
  let back_link =
    if can_access_settings then
      (Html.template "<a href='/c/%s/settings?panel=moderation' class='cm-back'>&larr; Back to settings</a>"
  [ (Html.text (community.slug)) ])
    else
      (Html.template "<a href='/c/%s' class='cm-back'>&larr; Back to community</a>"
  [ (Html.text (community.slug)) ])
  in
  let render_action (a : Mod_log_store.mod_action) =
    let target_html = match a.target_id with
      | None -> Html.empty
      | Some tid -> (Html.template "<span class='cm-table-target'> &middot; target #%s</span>"
  [ Html.int (tid) ])
    in
    (* Admin overrides are logged with an "admin_"-prefixed action type; flag them
       distinctly so a global-admin action reads differently from a community-mod one. *)
    let tag_cls =
      if String.length a.action_type >= 6 && String.sub a.action_type 0 6 = "admin_"
      then "cm-action-tag cm-action-tag--admin" else "cm-action-tag"
    in
    (Html.template "
    <tr>
        <td class='cm-table-when'>%s</td>
        <td><a href='/u/%s' class='cm-table-actor'>%s</a></td>
        <td><span class='%s'>%s</span>%s</td>
        <td class='cm-table-reason'>%s</td>
    </tr>"
  [ (Html.text (Components.time_ago a.created_at))
  ; (Html.text (a.moderator_username))
  ; (Html.text (a.moderator_username))
  ; (Html.text tag_cls)
  ; (Html.text (a.action_type))
  ; target_html
  ; (Html.text (a.reason)) ])
  in
  let table_body =
    if actions = [] then
      (Html.static "<tr><td colspan='4' class='cm-table-empty'>No moderation actions recorded yet.</td></tr>")
    else (Html.join (Html.static "\n")) (List.map render_action actions)
  in
  let content = (Html.template "
    <div class='cm-wrap'>
        <div class='cm-head'>
            <h1 class='cm-h1'>Moderation <span class='accent'>log</span></h1>
            <span class='mono launch-modlog-ctx'>/c/%s</span>
            %s
        </div>
        <section class='cm-panel'>
            <h2 class='cm-panel-title'>Action history</h2>
            <p class='cm-panel-desc'>Public record of moderator actions in this community.</p>
            <div class='cm-table-wrap'>
                <table class='cm-table'>
                    <thead>
                        <tr>
                            <th>When</th>
                            <th>Moderator</th>
                            <th>Action</th>
                            <th>Reason</th>
                        </tr>
                    </thead>
                    <tbody>%s</tbody>
                </table>
            </div>
        </section>
    </div>"
  [ (Html.text (community.slug))
  ; back_link
  ; table_body ])
  in
  (* Cartographic launch shell (pass 14A): the same four-pane chrome as the
     sibling community routes, wrapping the moderation ledger verbatim — the
     cm-* event rows, empty row and .cm-table-reason replay-mask class are
     treated as pinned (analytics.js masks .cm-table-reason by name), so only
     this outer document and the additive mono /c/:slug context above changed.
     The sidebar reuses the shared knowledge grammar with Moderation log
     active; can_manage mirrors the handler's settings gate (admin ||
     moderator), which here only decorates navigation — the route itself
     stays public exactly as modlog_handler allows. [rail_communities]
     carries the viewer's joined communities exactly as the handler loaded
     them (post-authorization only); ordering, dedup by slug, and the single
     active marker stay owned by the shared launch doc builder. Replay
     privacy keeps the existing inner ph-no-capture guard for private
     communities (the launch shell additionally marks .shell, exactly like
     the sibling routes). *)
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections
      ~moderation_log_active:true ~can_manage:can_access_settings ()
  in
  Community_shell.launch_community_page ?user ~noindex ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-community-modlog"
    ~title:(community.name ^ " — Mod Log")
    ~content:(Page_shell.private_replay_guard ~community content) ()
