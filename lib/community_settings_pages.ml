open Html.Infix

(* Only the outer document changes — the legacy
   create_page wrapper becomes the launch app chrome (Components.launch_app_page:
   earde.css + the shared mobile gate only, no legacy per-page CSS, no Tailwind).
   The inner create-* fragment — the POST /communities form contract (action,
   method, Dream CSRF, field names/ids, hidden counts, row scripts) — is byte
   preserved inside the same <div class='create-shell'> marker create_page
   emitted. The handler still gates on the global-admin session BEFORE loading
   rail data or rendering; [rail_communities] defaults to [] so pure renders
   stay possible. The mono context strip is factual (an administrator utility,
   not the onboarding flow) and precedes the create-shell marker so the feature
   fragment slice (create-shell → </main>) carries no launch chrome. *)
let new_community_form ?user ?(rail_communities = []) request =
  let csrf_token = Csrf_field.tag request in
  let content =
    Html.template
      {html|
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
          <h1 class='create-title'>Start a community</h1>
          <p class='create-sub'>Every community comes with a <strong>#general</strong> live chat channel and a <strong>General</strong> forum section. Add more below — you can always change things later.</p>
        </div>

        <form action='/communities' method='POST' class='create-form' id='new-community-form'>
            %s
            <!-- counts drive the server-side row loops; renumbered by JS on add/remove -->
            <input type='hidden' name='section_count' id='section_count' value='0'>
            <input type='hidden' name='channel_count' id='channel_count' value='0'>

            <div class='create-field'>
                <label class='create-label'>Community name <span class='req'>*</span></label>
                <input type='text' name='name' required class='create-input' placeholder='e.g., Italian Cuisine'>
            </div>

            <div class='create-field'>
                <label class='create-label'>URL slug <span class='req'>*</span></label>
                <div class='create-slug'>
                    <span class='create-slug-prefix'>/c/</span>
                    <input type='text' name='slug' required class='create-input' placeholder='italian-cuisine'>
                </div>
                <p class='create-hint'>Lowercase, no spaces. This is the community's public address.</p>
            </div>

            <div class='create-field'>
                <label class='create-label'>Description</label>
                <textarea name='description' class='create-textarea' placeholder='What is this community about?'></textarea>
            </div>

            <!-- Live chat channels (#): #general is always created; extras are optional -->
            <div class='create-group'>
                <div class='create-group-head'>
                    <div class='create-group-title'><span class='sigil'>#</span> Live chat channels</div>
                    <p class='create-group-desc'>Real-time chat rooms. Messages can later be promoted into permanent forum threads.</p>
                </div>
                <div class='create-chip'>
                    <span class='create-chip-sigil'>#</span>
                    <span class='create-chip-name'>general</span>
                    <span class='create-chip-lock'>Default</span>
                </div>
                <div id='channels-list' class='create-rows'></div>
                <button type='button' class='create-add' onclick='addChannel()'>+ Add channel</button>
            </div>

            <!-- Forum / thread sections (§): General is always created; extras are optional -->
            <div class='create-group'>
                <div class='create-group-head'>
                    <div class='create-group-title'><span class='sigil'>§</span> Forum sections</div>
                    <p class='create-group-desc'>Durable, indexed discussion areas, each with its own feed and sorting.</p>
                </div>
                <div class='create-chip'>
                    <span class='create-chip-sigil'>§</span>
                    <span class='create-chip-name'>General</span>
                    <span class='create-chip-lock'>Default</span>
                </div>
                <div id='sections-list' class='create-rows'></div>
                <button type='button' class='create-add' onclick='addSection()'>+ Add section</button>
            </div>

            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Create community</button>
            </div>
        </form>
      </div>
    </div>

    <script>
    /* Rows are renumbered to a contiguous 1..N on every add/remove so the field
       names (channel_name_N / section_name_N) always match the *_count the server
       loops over — removal order can never leave a gap the handler would skip. */
    function renumberChannels() {
        var rows = document.querySelectorAll('#channels-list > .create-row');
        rows.forEach(function(row, i) {
            row.querySelector('input').name = 'channel_name_' + (i + 1);
        });
        document.getElementById('channel_count').value = rows.length;
    }

    function addChannel() {
        var row = document.createElement('div');
        row.className = 'create-row';
        row.innerHTML =
            '<span class="create-row-sigil">#</span>'
            + '<input type="text" placeholder="channel-name" class="create-input create-row-name">'
            + '<button type="button" class="create-row-remove" title="Remove"'
            + ' onclick="this.closest(\'.create-row\').remove(); renumberChannels();">&times;</button>';
        document.getElementById('channels-list').appendChild(row);
        renumberChannels();
    }

    function buildSortSelect() {
        return '<select class="create-select create-row-sort">'
            + '<option value="new">New</option>'
            + '<option value="hot">Hot</option>'
            + '<option value="top">Top</option>'
            + '<option value="active">Active</option>'
            + '</select>';
    }

    function renumberSections() {
        var rows = document.querySelectorAll('#sections-list > .create-row');
        rows.forEach(function(row, i) {
            var idx = i + 1;
            var inputs = row.querySelectorAll('input');
            inputs[0].name = 'section_name_' + idx;
            inputs[1].name = 'section_desc_' + idx;
            row.querySelector('select').name = 'section_sort_' + idx;
        });
        document.getElementById('section_count').value = rows.length;
    }

    function addSection() {
        var row = document.createElement('div');
        row.className = 'create-row';
        row.innerHTML =
            '<span class="create-row-sigil">&sect;</span>'
            + '<input type="text" placeholder="Section name" class="create-input create-row-name">'
            + '<input type="text" placeholder="Description (optional)" class="create-input create-row-desc">'
            + buildSortSelect()
            + '<button type="button" class="create-row-remove" title="Remove"'
            + ' onclick="this.closest(\'.create-row\').remove(); renumberSections();">&times;</button>';
        document.getElementById('sections-list').appendChild(row);
        renumberSections();
    }
    </script>
    |html}
      [ csrf_token ]
  in
  let context =
    Html.static
      "<div class='launch-newcomm-context'><span \
       class='launch-newcomm-context-label'>Administrator utility</span><span \
       class='launch-newcomm-context-path'>/new-community</span></div>"
  in
  Page_shell.launch_app_page ?user ~request ~rail_communities
    ~page_class:"launch-new-community" ~title:"New Community"
    ~content:
      (context ++ Html.template "<div class='create-shell'>%s</div>" [ content ])
    ()

(* [connected_projects] is the pre-rendered "Connected projects" management fragment
   (Project_home_removal_pages.community_side_management_section) supplied by the settings
   route, or "" when the viewer is not on the top-mod/admin surface — the route never loads
   the read model for anyone else, and this page never loads it at all. An empty fragment
   also removes the panel from the navigation, so no ordinary moderator can reach an empty
   management surface by typing ?panel=projects. *)
let community_settings_page ?user ?(connected_projects = Html.empty)
    ?(rail_communities = []) ~is_admin ~is_top_mod ~open_reports_count
    ~(community : Community_types.community) ~(mods : User_store.user list)
    ~(banned_users : User_store.user list) ~(members : User_store.user list)
    ~(sections : Section_store.community_section list)
    ~(channels : Channel_store.channel list) request =
  let csrf_token = Csrf_field.tag request in
  let slug = Html.text community.slug in
  (* Active (non-archived) channel count — used both in the status strip and to gate the
     archive control in the UI (the server enforces the same guards regardless). *)
  let active_channels =
    List.filter (fun (c : Channel_store.channel) -> not c.is_archived) channels
  in
  let active_channel_count = List.length active_channels in

  (* The settings surface is a control panel: a left nav of panels, one panel rendered
     at a time. The selected panel comes from ?panel=…; unknown or missing values fall
     back to visibility. Pure server-side rendering — each nav link is a normal GET back
     to this same route with a different query value. *)
  (* The connected-projects panel exists only when the route supplied its fragment, which it
     does only for the top-mod/admin surface. Anyone else asking for ?panel=projects falls
     back to visibility exactly like any unknown value. *)
  let has_connected_projects = not (Html.is_empty connected_projects) in
  let panel =
    match Dream.query request "panel" with
    | Some "projects" when has_connected_projects -> "projects"
    | Some (("profile" | "channels" | "members" | "moderation" | "bans") as p)
      ->
        p
    | _ -> "visibility"
  in

  let is_private = community.visibility = Community_types.Community_private in
  let can_edit_vis = is_top_mod || is_admin in

  (* Network-community lifecycle state. A provisioned setup draft is not
     configured through the legacy settings controls: its identity,
     visibility, indexing, and discovery are all decided at once by the
     canonical publication flow, and the matching legacy POST routes now
     refuse a network community outright. Rendering controls that the server
     would reject would be a lie, so they are replaced by a pointer to the
     canonical surface. *)
  let is_network_draft =
    community.is_network_community
    && community.onboarding_state = Community_types.Community_draft
  in
  (* The setup surface independently reauthorizes (current top_mod of this
     community, or a durable users.is_admin holder), so this only decides
     whether the affordance is worth showing on a surface that already knows
     the answer for the common cases. The predicate (network draft +
     authorized viewer + canonical slug) lives in the shared settings shell
     so every shell surface gates the setup link identically. *)
  let can_complete_setup =
    Community_settings_shell.can_complete_setup ~community
      ~authorized:can_edit_vis
  in
  let setup_pointer_note =
    if not can_complete_setup then
      Html.static
        "<p class='cm-muted-note'>This community is still a setup draft. Its \
         identity and publication are decided together by an authorized \
         publisher.</p>"
    else
      Html.template
        "<p class='cm-muted-note'>This community is still a private setup \
         draft. Its name, address, description, visibility, and discovery are \
         decided together when you <a href='/c/%s/setup'>complete setup and \
         publish</a>.</p>"
        [ slug ]
  in

  let stat label cls value =
    Html.template
      "<div class='cm-stat'><span class='cm-stat-label'>%s</span><span \
       class='cm-stat-val %s'>%s</span></div>"
      [ label; cls; value ]
  in

  let sort_option value label selected_val =
    let sel =
      if selected_val = value then Html.static " selected" else Html.empty
    in
    Html.template "<option value='%s'%s>%s</option>"
      [ Html.text value; sel; Html.text label ]
  in

  (* Per-child indexability state + (TM/A only) toggle. Indexability is sensitive
     (it governs public discovery + chat provenance leakage), so the editing form renders only
     for top_mod/admin — regular mods get the read-only badge. The matching POST handlers re-check
     TM/A regardless. The hidden field carries the explicit next state. In the compact-row layout
     the badge lives in the always-visible summary and the toggle in the expanded ops strip. *)
  let can_edit_idx = is_top_mod || is_admin in
  let idx_badge ~indexable =
    if indexable then
      Html.static "<span class='cm-badge cm-badge--active'>Indexable</span>"
    else
      Html.static
        "<span class='cm-badge cm-badge--archived'>Not indexable</span>"
  in
  let idx_toggle_form ~action ~indexable =
    if not can_edit_idx then Html.empty
    else
      let next_val, label =
        if indexable then ("false", "Make non-indexable")
        else ("true", "Make indexable")
      in
      Html.template
        "<form action='%s' method='POST' class='cm-form-inline'>%s<input \
         type='hidden' name='indexable' value='%s'><button type='submit' \
         class='cm-btn-sm'>%s</button></form>"
        [
          Html.internal_path action;
          csrf_token;
          Html.text next_val;
          Html.text label;
        ]
  in
  (* Community-level private / non-indexable dominates child flags (effective_indexable_child):
     while dominated, a child's stored flag has no public effect. We still allow toggling so the flag
     is ready when the community becomes public+indexable; the note below says so plainly. *)
  let community_dominates_children = is_private || not community.indexable in
  let dominated_note =
    if not community_dominates_children then Html.empty
    else if is_private then
      Html.static
        "<p class='cm-muted-note' style='margin:0 0 10px'>This community is \
         private, so everything in it is non-indexable no matter what these \
         flags say. They take effect only once the community is public and \
         indexable.</p>"
    else
      Html.static
        "<p class='cm-muted-note' style='margin:0 0 10px'>This community is \
         public but excluded from discovery (noindex), so everything in it is \
         non-indexable no matter what these flags say. They take effect only \
         once the community is also indexable.</p>"
  in

  (* ---- Panel: Visibility & discovery (default) ---- *)
  (* Replaces the old Overview + Visibility pair. The status strip is read-only and derived
     from data the handler already passes (no fabricated metric). While private, indexability
     is never an actionable control — private communities are never indexed, so the stored
     flag is shown only as inactive secondary copy. The community-level routes/inputs are unchanged. *)
  let visibility_panel =
    let downvotes_row =
      if community.allow_downvotes then
        stat (Html.static "downvotes")
          (Html.static "cm-stat-val--ok")
          (Html.static "enabled")
      else
        stat (Html.static "downvotes")
          (Html.static "cm-stat-val--off")
          (Html.static "disabled")
    in
    let discovery_row =
      if is_private then
        stat (Html.static "discovery")
          (Html.static "cm-stat-val--off")
          (Html.static "noindex &middot; private")
      else if community.indexable then
        stat (Html.static "discovery")
          (Html.static "cm-stat-val--ok")
          (Html.static "indexable")
      else
        stat (Html.static "discovery")
          (Html.static "cm-stat-val--off")
          (Html.static "noindex")
    in
    let status_strip =
      Html.template
        "<div class='cm-stats' style='margin-bottom:14px'>%s%s%s%s%s%s</div>"
        [
          stat (Html.static "visibility") Html.empty
            (if is_private then Html.static "Private" else Html.static "Public");
          discovery_row;
          stat
            (Html.static "live channels")
            Html.empty
            (Html.text (string_of_int active_channel_count));
          (if community.sections_enabled then
             stat (Html.static "sections") Html.empty
               (Html.int (List.length sections))
           else
             stat (Html.static "sections")
               (Html.static "cm-stat-val--off")
               (Html.static "off"));
          stat (Html.static "moderators") Html.empty
            (Html.text (string_of_int (List.length mods)));
          downvotes_row;
        ]
    in
    let explanation =
      if is_private then
        Html.static
          "<p class='cm-panel-desc'>Private communities are only readable by \
           members, moderators, and admins. <strong>Private communities are \
           never indexed.</strong></p>"
      else if community.indexable then
        Html.static
          "<p class='cm-panel-desc'>This community can appear in the public \
           feed, search, and discovery, and may be indexed by search \
           engines.</p>"
      else
        Html.static
          "<p class='cm-panel-desc'>This community is public by link, but \
           <strong>excluded from the public feed, search, and \
           discovery</strong>, and marked <code>noindex</code>.</p>"
    in
    let visibility_control =
      if is_network_draft then setup_pointer_note
      else if not can_edit_vis then Html.empty
      else
        (* Segmented submit control: each segment is its own one-input POST form to the
           existing route, so the control stays fully server-rendered. Clicking the
           already-active segment re-submits the current value (idempotent). *)
        let seg value label active =
          Html.template
            "<form action='/c/%s/settings/visibility' method='POST' \
             class='cm-form-inline'>%s<input type='hidden' name='visibility' \
             value='%s'><button type='submit' \
             class='cm-seg-btn%s'>%s</button></form>"
            [
              slug;
              csrf_token;
              value;
              (if active then Html.static " cm-seg-btn--active" else Html.empty);
              label;
            ]
        in
        Html.template
          "\n\
          \          <div class='cm-field' style='margin-top:2px'>\n\
          \            <label class='cm-label'>Visibility</label>\n\
          \            <div class='cm-seg'>%s%s</div>\n\
          \          </div>"
          [
            seg (Html.static "public") (Html.static "Public") (not is_private);
            seg (Html.static "private") (Html.static "Private") is_private;
          ]
    in
    let indexable_control =
      (* Indexing on a network community is not an independent switch: the
         published shapes are "indexable and discoverable" or neither, and
         the legacy route can only move one of the two. It refuses a network
         community for exactly that reason, so no control is offered. *)
      if community.is_network_community then
        if is_network_draft then Html.empty
        else
          Html.static
            "<p class='cm-muted-note' style='margin-top:12px'>Discovery for \
             this community follows the Public or Unlisted choice made when it \
             was published.</p>"
      else if not can_edit_vis then Html.empty
      else if is_private then
        (* No actionable indexability control while private; surface the stored flag as
           inactive copy so it doesn't look clickable. *)
        Html.template
          "<p class='cm-muted-note' style='margin-top:12px'>Private \
           communities are never indexed. The stored indexability flag is \
           <strong>%s</strong> &mdash; it takes effect only if the community \
           becomes public.</p>"
          [
            (if community.indexable then Html.static "indexable"
             else Html.static "non-indexable");
          ]
      else
        let seg value label active =
          Html.template
            "<form action='/c/%s/settings/indexability' method='POST' \
             class='cm-form-inline'>%s<input type='hidden' name='indexable' \
             value='%s'><button type='submit' \
             class='cm-seg-btn%s'>%s</button></form>"
            [
              slug;
              csrf_token;
              value;
              (if active then Html.static " cm-seg-btn--active" else Html.empty);
              label;
            ]
        in
        Html.template
          "\n\
          \          <div class='cm-field' style='margin-top:14px'>\n\
          \            <label class='cm-label'>Discovery</label>\n\
          \            <div class='cm-seg'>%s%s</div>\n\
          \            <p class='cm-muted-note' style='margin:6px 0 \
           0'>Non-indexable keeps the community readable by link but marked \
           <code>noindex</code> and out of the public feed, search, and \
           discovery.</p>\n\
          \          </div>"
          [
            seg (Html.static "true") (Html.static "Indexable")
              community.indexable;
            seg (Html.static "false")
              (Html.static "Non-indexable")
              (not community.indexable);
          ]
    in
    let editor_note =
      if can_edit_vis then Html.empty
      else
        Html.static
          "<p class='cm-muted-note'>Only Top Mods and admins can change \
           visibility and discovery settings.</p>"
    in
    Html.template
      "\n\
      \      <section class='cm-panel'>\n\
      \        <h2 class='cm-panel-title'>Visibility &amp; discovery</h2>\n\
      \        <p class='cm-panel-desc'>Who can read this community, and \
       whether it appears in the public feed, search, and discovery.</p>\n\
      \        %s\n\
      \        %s\n\
      \        %s\n\
      \        %s\n\
      \        %s\n\
      \      </section>"
      [
        status_strip;
        explanation;
        visibility_control;
        indexable_control;
        editor_note;
      ]
  in

  (* ---- Panel: Profile ---- *)
  (* Native file inputs can't reflect an existing upload ("No file chosen" even when an
     avatar exists), so each upload field gets an explicit current-state row: a preview via
     the shared Html.image_src helpers when an asset exists, or a "none yet" note.

     The preview is display only. The form carries no avatar/banner URL of its own: when
     no file is uploaded, /update-community keeps the values it read from the community
     row itself, so what a client submits can never become a community's stored image. *)
  let avatar_status =
    match community.avatar_url with
    | Some u when String.trim u <> "" ->
        Html.template
          "<div class='cm-asset'>%s<span class='cm-asset-note'>Current avatar \
           uploaded &mdash; choose a file to replace it.</span></div>"
          [
            Components.community_avatar ~img_class:"cm-asset-avatar"
              ~tile_class:"cm-asset-avatar" ~name:community.name (Some u);
          ]
    | _ ->
        Html.static
          "<div class='cm-asset'><span class='cm-asset-note'>No avatar \
           uploaded yet.</span></div>"
  in
  let banner_status =
    match community.banner_url with
    | Some u when String.trim u <> "" ->
        Html.template
          "<div class='cm-asset'>%s<span class='cm-asset-note'>Current banner \
           uploaded &mdash; choose a file to replace it.</span></div>"
          [
            Components.community_banner ~wrap_class:"cm-asset-banner"
              ~img_class:"cm-asset-banner-img" ~fallback_class:"cm-asset-banner"
              (Some u);
          ]
    | _ ->
        Html.static
          "<div class='cm-asset'><span class='cm-asset-note'>No banner \
           uploaded yet.</span></div>"
  in
  let profile_panel =
    if is_network_draft then
      (* The legacy multipart route writes the description straight to the
         community row; for a setup draft that is a canonical-identity edit
         outside the publication flow, and the route now refuses it. No form
         is rendered rather than one the server would reject. *)
      Html.template
        "\n\
        \      <section class='cm-panel'>\n\
        \        <h2 class='cm-panel-title'>Profile</h2>\n\
        \        <p class='cm-panel-desc'>Description, rules, and imagery \
         shown on the public community page.</p>\n\
        \        %s\n\
        \      </section>"
        [ setup_pointer_note ]
    else
      Html.template
        "\n\
        \      <section class='cm-panel'>\n\
        \        <h2 class='cm-panel-title'>Profile</h2>\n\
        \        <p class='cm-panel-desc'>Description, rules, and imagery \
         shown on the public community page.</p>\n\
        \        <form action='/update-community' method='POST' \
         enctype='multipart/form-data' class='cm-form'>\n\
        \          %s\n\
        \          <input type='hidden' name='community_id' value='%s'>\n\
        \          <input type='hidden' name='community_slug' value='%s'>\n\
        \          <div class='cm-field'>\n\
        \            <label class='cm-label'>Description</label>\n\
        \            <textarea name='description' rows='3' \
         class='cm-textarea'>%s</textarea>\n\
        \          </div>\n\
        \          <div class='cm-field'>\n\
        \            <label class='cm-label'>Community rules</label>\n\
        \            <textarea name='rules' rows='5' \
         class='cm-textarea'>%s</textarea>\n\
        \          </div>\n\
        \          <div class='cm-assets2'>\n\
        \            <div class='cm-field'>\n\
        \              <label class='cm-label'>Avatar image</label>\n\
        \              %s\n\
        \              <input type='file' name='avatar_url' accept='image/*' \
         class='cm-file'>\n\
        \            </div>\n\
        \            <div class='cm-field'>\n\
        \              <label class='cm-label'>Banner image</label>\n\
        \              %s\n\
        \              <input type='file' name='banner_url' accept='image/*' \
         class='cm-file'>\n\
        \            </div>\n\
        \          </div>\n\
        \          <button type='submit' class='cm-btn'>Save changes</button>\n\
        \        </form>\n\
        \      </section>"
        [
          csrf_token;
          Html.int community.id;
          slug;
          Html.text (Option.value ~default:"" community.description);
          Html.text (Option.value ~default:"" community.rules);
          avatar_status;
          banner_status;
        ]
  in

  (* ---- Panel: Channels & sections ---- *)
  (* One compact <details> row per channel/section: current order, name, slug, badges in the
     always-visible summary; the update form and the indexability/archive/delete actions in the
     collapsed body. All routes, methods, and input names are unchanged from the old
     always-expanded cards. idx is the 0-based list index — the position columns exist in the
     schema but have no reorder endpoint, so we only SHOW the current order here; explicit
     reordering is a follow-up. *)
  let render_channel_item idx (c : Channel_store.channel) =
    let status_badge =
      if c.is_archived then
        Html.static "<span class='cm-badge cm-badge--archived'>Archived</span>"
      else Html.static "<span class='cm-badge cm-badge--active'>Active</span>"
    in
    (* Active channels link to their live view; archived ones are not navigable. *)
    let name_html =
      if c.is_archived then
        Html.template "<span class='cm-section-name'>%s</span>"
          [ Html.text c.name ]
      else
        Html.template
          "<a class='cm-section-name cm-channel-link' href='/c/%s/ch/%s'>%s</a>"
          [ slug; Html.text c.slug; Html.text c.name ]
    in
    let row_note =
      if c.slug = "general" then
        Html.static "<span class='cm-item-note'>Default channel</span>"
      else Html.empty
    in
    (* Archive is reversible (never a hard-delete). The default #general channel and the
       last remaining active channel cannot be archived — noted in the ops strip, enforced
       server-side in archive_channel_handler either way. *)
    let archive_control =
      if c.is_archived then
        Html.template
          "\n\
          \          <form action='/c/%s/channels/%s/unarchive' method='POST' \
           class='cm-form-inline'>\n\
          \            %s\n\
          \            <button type='submit' class='cm-btn-sm \
           cm-btn-sm--ok'>Unarchive</button>\n\
          \          </form>"
          [ slug; Html.int c.id; csrf_token ]
      else if c.slug = "general" then
        Html.static
          "<span class='cm-muted-note'>The default #general channel cannot be \
           archived.</span>"
      else if active_channel_count <= 1 then
        Html.static
          "<span class='cm-muted-note'>The last active channel cannot be \
           archived.</span>"
      else
        Html.template
          "\n\
          \          <form action='/c/%s/channels/%s/archive' method='POST' \
           class='cm-form-inline'\n\
          \                onsubmit=\"confirmModal(event, 'Archive this \
           channel? Members will no longer see it. You can unarchive it \
           later.')\">\n\
          \            %s\n\
          \            <button type='submit' class='cm-btn-sm \
           cm-btn-sm--danger'>Archive</button>\n\
          \          </form>"
          [ slug; Html.int c.id; csrf_token ]
    in
    Html.template
      "\n\
      \      <details class='cm-item'>\n\
      \        <summary>\n\
      \          <span class='cm-item-pos'>%s</span>\n\
      \          <span class='cm-item-name'>%s<span \
       class='cm-section-slug'>/ch/%s</span></span>\n\
      \          <span class='cm-item-badges'>%s</span>\n\
      \          <span class='cm-item-meta'>%s<span \
       class='cm-item-hint'>edit</span></span>\n\
      \        </summary>\n\
      \        <div class='cm-item-body'>\n\
      \          <form action='/c/%s/channels/%s/update' method='POST' \
       class='cm-form'>\n\
      \            %s\n\
      \            <div class='cm-field'>\n\
      \              <input type='text' name='name' value='%s' required \
       placeholder='Channel name' class='cm-input'>\n\
      \            </div>\n\
      \            <div class='cm-field'>\n\
      \              <input type='text' name='topic' value='%s' \
       placeholder='Topic (optional)' class='cm-input'>\n\
      \            </div>\n\
      \            <button type='submit' class='cm-btn \
       cm-btn--secondary'>Save</button>\n\
      \          </form>\n\
      \          <div class='cm-item-ops'>\n\
      \            %s\n\
      \          </div>\n\
      \        </div>\n\
      \      </details>"
      [
        Html.text (Printf.sprintf "%02d" (idx + 1));
        name_html;
        Html.text c.slug;
        status_badge;
        row_note;
        slug;
        Html.int c.id;
        csrf_token;
        Html.text c.name;
        Html.text (Option.value ~default:"" c.topic);
        archive_control;
      ]
  in
  let channels_block =
    let rows =
      if channels = [] then
        Html.static "<p class='cm-empty'>No channels yet.</p>"
      else
        Html.template "<div class='cm-items'>%s</div>"
          [
            (Html.join (Html.static "\n"))
              (List.mapi render_channel_item channels);
          ]
    in
    Html.template
      "\n\
      \      <h3 class='cm-subhead'>Live chat channels</h3>\n\
      \      <p class='cm-panel-desc'>Live chat channels for real-time \
       discussion in this community. Archive a channel to remove it from the \
       live channel list; you can unarchive it later. The default \
       <code>#general</code> channel and the last active channel cannot be \
       archived.</p>\n\
      \      %s\n\
      \      %s\n\
      \      <details class='cm-add'>\n\
      \        <summary>+ Add channel</summary>\n\
      \        <div class='cm-add-body'>\n\
      \          <form action='/c/%s/channels/add' method='POST' \
       class='cm-form'>\n\
      \            %s\n\
      \            <input type='text' name='name' required \
       placeholder='Channel name' class='cm-input'>\n\
      \            <input type='text' name='topic' placeholder='Topic \
       (optional)' class='cm-input'>\n\
      \            <button type='submit' class='cm-btn'>Add channel</button>\n\
      \          </form>\n\
      \        </div>\n\
      \      </details>"
      [ dominated_note; rows; slug; csrf_token ]
  in
  let sections_inner =
    if not community.sections_enabled then Html.empty
    else begin
      let render_section_item idx (s : Section_store.community_section) =
        Html.template
          "\n\
          \          <details class='cm-item'>\n\
          \            <summary>\n\
          \              <span class='cm-item-pos'>%s</span>\n\
          \              <span class='cm-item-name'><span \
           class='cm-section-name'>%s</span><span \
           class='cm-section-slug'>/s/%s</span></span>\n\
          \              <span class='cm-item-badges'>%s</span>\n\
          \              <span class='cm-item-meta'><span \
           class='cm-item-note'>sort: %s</span><span \
           class='cm-item-hint'>edit</span></span>\n\
          \            </summary>\n\
          \            <div class='cm-item-body'>\n\
          \              <form action='/c/%s/sections/%s/update' method='POST' \
           class='cm-form'>\n\
          \                %s\n\
          \                <div class='cm-row2'>\n\
          \                  <input type='text' name='name' value='%s' \
           required class='cm-input'>\n\
          \                  <select name='default_sort' class='cm-select'>\n\
          \                    %s%s%s%s\n\
          \                  </select>\n\
          \                </div>\n\
          \                <textarea name='description' rows='2' \
           placeholder='Description (optional)' \
           class='cm-textarea'>%s</textarea>\n\
          \                <button type='submit' class='cm-btn \
           cm-btn--secondary'>Save</button>\n\
          \              </form>\n\
          \              <div class='cm-item-ops'>\n\
          \                %s\n\
          \                <form action='/c/%s/sections/%s/delete' \
           method='POST' class='cm-form-inline'\n\
          \                      onsubmit=\"return confirm('Delete this \
           section? Posts will not be deleted. They will be moved to \
           Uncategorized.')\">\n\
          \                  %s\n\
          \                  <button type='submit' class='cm-btn-sm \
           cm-btn-sm--danger'>Delete section</button>\n\
          \                </form>\n\
          \                <span class='cm-muted-note'>Posts move to \
           Uncategorized on delete.</span>\n\
          \              </div>\n\
          \            </div>\n\
          \          </details>"
          [
            Html.text (Printf.sprintf "%02d" (idx + 1));
            Html.text s.name;
            Html.text s.slug;
            idx_badge ~indexable:s.indexable;
            Html.text s.default_sort;
            slug;
            Html.int s.section_id;
            csrf_token;
            Html.text s.name;
            sort_option "hot" "Hot" s.default_sort;
            sort_option "new" "New" s.default_sort;
            sort_option "top" "Top" s.default_sort;
            sort_option "active" "Active" s.default_sort;
            Html.text (Option.value ~default:"" s.description);
            idx_toggle_form
              ~action:
                (Printf.sprintf "/c/%s/sections/%d/indexability" community.slug
                   s.section_id)
              ~indexable:s.indexable;
            slug;
            Html.int s.section_id;
            csrf_token;
          ]
      in
      let next_position = List.length sections + 1 in
      let rows =
        if sections = [] then
          Html.static "<p class='cm-empty'>No sections yet.</p>"
        else
          Html.template "<div class='cm-items'>%s</div>"
            [
              (Html.join (Html.static "\n"))
                (List.mapi render_section_item sections);
            ]
      in
      Html.template
        "\n\
        \        <h3 class='cm-subhead' style='margin-top:24px'>Forum \
         sections</h3>\n\
        \        <p class='cm-panel-desc'>Organize posts into sections. \
         Deleting a section moves its posts to Uncategorized. Indexable \
         sections and their threads can appear in the public feed, search, and \
         discovery; non-indexable ones are marked <code>noindex</code> and \
         excluded from public discovery.</p>\n\
        \        %s\n\
        \        %s\n\
        \        <details class='cm-add'>\n\
        \          <summary>+ Add section</summary>\n\
        \          <div class='cm-add-body'>\n\
        \            <form action='/c/%s/sections/add' method='POST' \
         class='cm-form'>\n\
        \              %s\n\
        \              <div class='cm-row2'>\n\
        \                <input type='text' name='name' required \
         placeholder='Section name' class='cm-input'>\n\
        \                <select name='default_sort' class='cm-select'>\n\
        \                  <option value='hot'>Hot</option>\n\
        \                  <option value='new'>New</option>\n\
        \                  <option value='top'>Top</option>\n\
        \                  <option value='active'>Active</option>\n\
        \                </select>\n\
        \              </div>\n\
        \              <textarea name='description' rows='2' \
         placeholder='Description (optional)' class='cm-textarea'></textarea>\n\
        \              <input type='hidden' name='position' value='%s'>\n\
        \              <button type='submit' class='cm-btn'>Add section</button>\n\
        \            </form>\n\
        \          </div>\n\
        \        </details>"
        [ dominated_note; rows; slug; csrf_token; Html.int next_position ]
    end
  in
  let channels_panel =
    Html.template
      "\n\
      \      <section class='cm-panel'>\n\
      \        <h2 class='cm-panel-title'>Channels &amp; sections</h2>\n\
      \        <p class='cm-panel-desc'>Live chat channels and forum sections \
       for this community. Rows are shown in their current order.</p>\n\
      \        %s\n\
      \        %s\n\
      \      </section>"
      [ channels_block; sections_inner ]
  in

  (* ---- Panel: Members ---- *)
  (* Member allow-list management. Adding/removing members is TM/A-only (same gate as
     visibility/indexability), so the controls render only for top_mod/admin; the matching POST
     handlers re-check TM/A. Regular mods see a read-only list + a note. This manages ONLY
     community_members — moderators/admins keep read access through their role, so they may not
     appear here. The add form renders only while the community is private: for a public
     community the allow-list has no access effect, so offering an active add control would be
     misleading. Existing rows keep their remove action either way (same route/inputs). *)
  let members_panel =
    let can_edit = is_top_mod || is_admin in
    let desc =
      if is_private then
        Html.static
          "<p class='cm-panel-desc'>Members can read this private community. \
           Top Mods and admins can add or remove members. Removing a member is \
           not a ban &mdash; it only removes them from the member list. \
           Moderators and admins may still have access through their role, so \
           they may not appear in this list.</p>"
      else
        Html.static
          "<p class='cm-panel-desc'>This community is public. Members listed \
           here are saved community members, but access is not restricted \
           while the community is public. Removing a member only deletes the \
           saved member entry &mdash; it does not ban them. The member list \
           takes effect again if the community becomes private.</p>"
    in
    let member_rows =
      if members = [] then
        Html.static "<p class='cm-empty'>No members in the allow-list yet.</p>"
      else
        let rows =
          (Html.join (Html.static "\n"))
            (List.map
               (fun (m : User_store.user) ->
                 let remove_btn =
                   if not can_edit then Html.empty
                   else
                     Html.template
                       "\n\
                       \                <form \
                        action='/c/%s/settings/members/remove' method='POST' \
                        class='cm-form-inline'>\n\
                       \                  %s\n\
                       \                  <input type='hidden' \
                        name='target_user_id' value='%s'>\n\
                       \                  <button type='submit' \
                        class='cm-btn-sm cm-btn-sm--danger'>Remove \
                        member</button>\n\
                       \                </form>"
                       [ slug; csrf_token; Html.int m.id ]
                 in
                 Html.template
                   "\n\
                   \            <div class='cm-list-row'>\n\
                   \              <a href='/u/%s' class='cm-user-link'>u/%s</a>\n\
                   \              %s\n\
                   \            </div>"
                   [ Html.text m.username; Html.text m.username; remove_btn ])
               members)
        in
        Html.template "<div class='cm-list'>%s</div>" [ rows ]
    in
    let add_form =
      if not is_private then Html.empty
      else if not can_edit then
        Html.static
          "<p class='cm-muted-note'>Only Top Mods and admins can manage \
           members.</p>"
      else
        Html.template
          "\n\
          \          <form action='/c/%s/settings/members/add' method='POST' \
           class='cm-inline-form' style='margin-top:16px'>\n\
          \            %s\n\
          \            <input type='text' name='username' required \
           placeholder='Username to add' class='cm-input'>\n\
          \            <button type='submit' class='cm-btn'>Add member</button>\n\
          \          </form>"
          [ slug; csrf_token ]
    in
    Html.template
      "\n\
      \      <section class='cm-panel'>\n\
      \        <h2 class='cm-panel-title'>Members</h2>\n\
      \        %s\n\
      \        %s\n\
      \        %s\n\
      \      </section>"
      [ desc; member_rows; add_form ]
  in

  (* ---- Panel: Moderation ---- *)
  (* The cross-surface cardlinks (manage-mods, reports, modlog) plus the downvote
     enable/disable, grouped as one moderation area. The toggle is display-gated to
     top_mod/admin as before; toggle_downvotes_handler remains the authority and re-checks
     top_mod/admin. Route, POST method, CSRF, and the allow_downvotes input name/values are
     preserved exactly. *)
  let moderation_panel =
    let modtools_block =
      if not (is_top_mod || is_admin) then Html.empty
      else
        (* Same segmented idiom as visibility/discovery; each segment POSTs the explicit
           allow_downvotes value to the existing route. *)
        let seg value label active =
          Html.template
            "<form action='/c/%s/toggle_downvotes' method='POST' \
             class='cm-form-inline'>%s<input type='hidden' \
             name='allow_downvotes' value='%s'><button type='submit' \
             class='cm-seg-btn%s'>%s</button></form>"
            [
              slug;
              csrf_token;
              value;
              (if active then Html.static " cm-seg-btn--active" else Html.empty);
              label;
            ]
        in
        Html.template
          "\n\
          \          <h3 class='cm-subhead' style='margin-top:0'>Moderation \
           tools</h3>\n\
          \          <div class='cm-field'>\n\
          \            <label class='cm-label'>Downvotes</label>\n\
          \            <div class='cm-seg'>%s%s</div>\n\
          \            <p class='cm-muted-note' style='margin:6px 0 0'>Only \
           Top Mods and admins can change this.</p>\n\
          \          </div>"
          [
            seg (Html.static "true") (Html.static "Enabled")
              community.allow_downvotes;
            seg (Html.static "false") (Html.static "Disabled")
              (not community.allow_downvotes);
          ]
    in
    Html.template
      "\n\
      \      <section class='cm-panel'>\n\
      \        <h2 class='cm-panel-title'>Moderation</h2>\n\
      \        <p class='cm-panel-desc'>Governance, flagged content, and the \
       public audit trail for this community.</p>\n\
      \        <div class='cm-modlinks'>\n\
      \          <a href='/c/%s/manage-mods' class='cm-cardlink \
       cm-cardlink--sm'>\n\
      \            <div>\n\
      \              <p class='cm-cardlink-title'>Manage moderators</p>\n\
      \              <p class='cm-cardlink-desc'>Add, promote, and remove \
       moderators &mdash; Council of Equals governance.</p>\n\
      \            </div>\n\
      \            <span class='cm-cardlink-go'>&rarr;</span>\n\
      \          </a>\n\
      \          <a href='/c/%s/reports' class='cm-cardlink cm-cardlink--sm'>\n\
      \            <div>\n\
      \              <p class='cm-cardlink-title'>Reports%s</p>\n\
      \              <p class='cm-cardlink-desc'>Review posts and comments \
       flagged by members &mdash; spam, abuse, and rule-breaking content.</p>\n\
      \            </div>\n\
      \            <span class='cm-cardlink-go'>&rarr;</span>\n\
      \          </a>\n\
      \          <a href='/c/%s/modlog' class='cm-cardlink cm-cardlink--sm'>\n\
      \            <div>\n\
      \              <p class='cm-cardlink-title'>Mod log</p>\n\
      \              <p class='cm-cardlink-desc'>Public audit trail of \
       moderation actions in this community.</p>\n\
      \            </div>\n\
      \            <span class='cm-cardlink-go'>&rarr;</span>\n\
      \          </a>\n\
      \        </div>\n\
      \        %s\n\
      \      </section>"
      [
        slug;
        slug;
        (if open_reports_count > 0 then
           Html.template
             " <span class='cm-badge cm-badge--active'>%s open</span>"
             [ Html.int open_reports_count ]
         else Html.empty);
        slug;
        modtools_block;
      ]
  in

  (* ---- Panel: Bans ---- *)
  let bans_panel =
    let banned_section =
      if banned_users = [] then
        Html.static
          "<p class='cm-empty'>No users are currently banned from this \
           community.</p>"
      else
        let rows =
          (Html.join (Html.static "\n"))
            (List.map
               (fun (b : User_store.user) ->
                 Html.template
                   "\n\
                   \            <div class='cm-list-row'>\n\
                   \              <a href='/u/%s' class='cm-user-link'>u/%s</a>\n\
                   \              <form action='/unban-community-user' \
                    method='POST' class='cm-form-inline'>\n\
                   \                %s\n\
                   \                <input type='hidden' name='target_user_id' \
                    value='%s'>\n\
                   \                <input type='hidden' name='community_id' \
                    value='%s'>\n\
                   \                <input type='hidden' name='community_slug' \
                    value='%s'>\n\
                   \                <button type='submit' class='cm-btn-sm \
                    cm-btn-sm--ok'>Unban</button>\n\
                   \              </form>\n\
                   \            </div>"
                   [
                     Html.text b.username;
                     Html.text b.username;
                     csrf_token;
                     Html.int b.id;
                     Html.int community.id;
                     slug;
                   ])
               banned_users)
        in
        Html.template "<div class='cm-list'>%s</div>" [ rows ]
    in
    Html.template
      "\n\
      \      <section class='cm-panel cm-danger'>\n\
      \        <h2 class='cm-panel-title'>Bans</h2>\n\
      \        <p class='cm-panel-desc'>Banned users cannot post or comment in \
       this community. Bans are logged to the mod log.</p>\n\
      \        <form action='/ban-community-user' method='POST' \
       class='cm-inline-form'>\n\
      \          %s\n\
      \          <input type='hidden' name='community_id' value='%s'>\n\
      \          <input type='text' name='target_username' required \
       placeholder='Username to ban' class='cm-input'>\n\
      \          <button type='submit' class='cm-btn'>Ban user</button>\n\
      \        </form>\n\
      \        <h3 class='cm-subhead'>Banned users</h3>\n\
      \        %s\n\
      \      </section>"
      [ csrf_token; Html.int community.id; banned_section ]
  in

  (* Already-escaped, form-bearing HTML from the pure removal-pages module; this page adds
     only the panel chrome and never inspects, rewrites, or re-escapes it. *)
  let projects_panel =
    Html.template
      "<section class='cm-panel'>\n\
      \        <h2 class='cm-panel-title'>Connected projects</h2>\n\
      \        <p class='cm-panel-desc'>Open-source projects that use this \
       community as their Earde home.</p>\n\
      \        %s\n\
      \      </section>"
      [ connected_projects ]
  in

  let main_panel =
    match panel with
    | "profile" -> profile_panel
    | "channels" -> channels_panel
    | "members" -> members_panel
    | "moderation" -> moderation_panel
    | "projects" -> projects_panel
    | "bans" -> bans_panel
    | _ -> visibility_panel
  in

  (* Left nav + header + panel wrapper: the shared settings shell. The nav
     is the one grouped index every settings/management surface renders; the
     dedicated-route entries (Project home requests, Connections, Shared
     threads, Manage moderators) still grant nothing — each route
     reauthorizes from scratch — and the whole Network group plus Manage
     moderators is display-gated to the top-mod/admin surface this page
     already proved, so regular mods never see entries they cannot open. *)
  let active : Community_settings_shell.item =
    match panel with
    | "profile" -> Profile
    | "channels" -> Channels
    | "members" -> Members
    | "moderation" -> Moderation
    | "bans" -> Bans
    | "projects" -> Connected_projects
    | _ -> Visibility
  in
  let content =
    Community_settings_shell.wrap ~slug:community.slug ~active
      ~can_complete_setup ~network_manager:(is_top_mod || is_admin)
      ~panel:main_panel ()
  in
  (* Cartographic launch shell: the same four-pane chrome as the
     overview/channel/section/thread routes, wrapping the settings content
     verbatim — every cm-* fragment above is test-pinned or treated as such,
     so only this outer document changed. The sidebar reuses the shared
     knowledge grammar with Settings active; can_manage is true by
     construction (the handler already 403'd everyone below moderator). The
     Home requests queue entry stays exclusively in the settings index above:
     its canonical link is pinned to exactly one occurrence per document, so
     the sidebar must not repeat it. [rail_communities] carries the viewer's
     joined communities exactly as the handler loaded them (post-authorization
     only); ordering, dedup by slug, and the single active marker stay owned
     by the shared launch doc builder, so the settings rail can never diverge
     from the overview/channel/section/thread rails. Replay privacy keeps the
     existing inner ph-no-capture guard for private communities (the launch
     shell additionally marks .shell, exactly like the sibling routes). *)
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections
      ~settings_active:true ~can_manage:true ()
  in
  Community_shell.launch_community_page ?user ~request ~rail_communities
    ~community ~sidebar ~page_class:"launch-community-settings"
    ~title:(Printf.sprintf "Settings — /c/%s" community.slug)
    ~content:(Page_shell.private_replay_guard ~community content)
    ()
