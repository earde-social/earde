(* The community-connections management surface: the three-section
   management page and the two-step connect flow. Pure rendering over
   handler-supplied view models — no Caqti, no session access, no authority
   decision. See the .mli for the full contract.

   Two rules shape everything here. An accepted connection is symmetric, so
   it is rendered as a connected community and never as an act by a person:
   no requester, reviewer, or remover username exists in this module's
   vocabulary. And the private request note is private workflow text: it
   renders only inside the two pending sections, labelled, escaped, and never
   parsed as Markdown or HTML. *)

type community = { name : string; slug : string; eligible : bool }

type counterpart = { counterpart_name : string; counterpart_slug : string }

type accepted = { accepted_id : string; accepted_with : counterpart }

type pending = {
  pending_id : string;
  pending_with : counterpart;
  pending_note : string option;
}

type state = {
  community : community;
  accepted : accepted list;
  incoming : pending list;
  outgoing : pending list;
}

type target = { target_name : string; target_slug : string }

type feedback =
  | Stale_form
  | Already_connected
  | Review_unavailable
  | Removal_unavailable
  | Target_unavailable
  | Source_ineligible
  | Note_invalid
  | Action_failed

let esc = Components.html_escape

(* A community slug in an action path must be a single non-empty URL path
   segment; anything else drops every actionable form for that row. *)
let valid_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* A connection id reaches the page as a decimal string and only ever becomes
   a path segment; anything but plain digits is refused, so no route value
   can be smuggled through it. It identifies a record and grants nothing —
   the handler re-resolves and re-authorizes it. *)
let valid_connection_id value =
  String.length value > 0
  && String.length value <= 19
  && String.for_all (fun c -> c >= '0' && c <= '9') value

let base_path ~community_slug =
  Printf.sprintf "/c/%s/settings/connections" community_slug

let feedback_copy = function
  | Stale_form ->
      "This page had been open too long, so the action could no longer be \
       submitted. Nothing was changed. Try again."
  | Already_connected ->
      "These communities are already connected, or a request between them is \
       already waiting."
  | Review_unavailable -> "That request is no longer pending."
  | Removal_unavailable -> "That connection is no longer active."
  | Target_unavailable -> "That community is not available to connect."
  | Source_ineligible ->
      "This community cannot create or accept new connections right now."
  | Note_invalid ->
      "That note could not be saved. Notes are limited to 2,000 characters \
       of ordinary text."
  | Action_failed -> "We couldn't complete that action. Try again."

let feedback_html = function
  | None -> ""
  | Some feedback ->
      Printf.sprintf "<div class='ccn-alert'><p>%s</p></div>"
        (feedback_copy feedback)

(* One generic page-level notice. It states the rule, never which of the
   three facts this community currently fails. *)
let ineligible_notice_html (community : community) =
  if community.eligible then ""
  else
    "<div class='ccn-ineligible'><p>This community cannot create or accept \
     new connections right now. Existing connections and requests can still \
     be reviewed and removed below.</p></div>"

(* Counterpart identity: the name, with the address underneath. It links to
   the community only when the slug is addressable. *)
let counterpart_html (c : counterpart) =
  let name = esc c.counterpart_name in
  if valid_community_slug c.counterpart_slug then
    Printf.sprintf
      "<div class='ccn-identity'><a class='ccn-name' href='/c/%s'>%s</a><span \
       class='ccn-slug'>/c/%s</span></div>"
      (esc c.counterpart_slug) name (esc c.counterpart_slug)
  else
    Printf.sprintf
      "<div class='ccn-identity'><span class='ccn-name'>%s</span></div>" name

(* Private workflow text: labelled as private, HTML-escaped, and never
   parsed as Markdown or HTML. *)
let note_html = function
  | None -> ""
  | Some note ->
      Printf.sprintf
        "<div class='ccn-note'><p class='ccn-note-label'>Private note — \
         visible only to the moderators of the two communities.</p><p \
         class='ccn-note-body'>%s</p></div>"
        (esc note)

(* One nameless single-purpose form per action: the route path carries both
   subjects, so no application field and no hidden identifier exist. Dream's
   framework CSRF field is emitted only when a live request is supplied. *)
let action_form ?request ~action ~cls ~label () =
  let csrf_field =
    match request with None -> "" | Some request -> Dream.csrf_tag request
  in
  Printf.sprintf
    "<form method='POST' action='%s' class='ccn-form %s'>%s<button \
     type='submit' class='ccn-btn'>%s</button></form>"
    action cls csrf_field label

let action_path ~community_slug ~connection_id ~verb =
  esc
    (Printf.sprintf "%s/%s/%s"
       (base_path ~community_slug)
       connection_id verb)

(* --- Section 1: connected communities --- *)

let accepted_row ?request ~(community : community) (a : accepted) =
  let actions =
    if
      valid_community_slug community.slug && valid_connection_id a.accepted_id
    then
      action_form ?request
        ~action:
          (action_path ~community_slug:community.slug
             ~connection_id:a.accepted_id ~verb:"remove")
        ~cls:"ccn-remove" ~label:"Remove connection" ()
    else ""
  in
  Printf.sprintf "<li class='ccn-row ccn-row--accepted'>%s%s</li>"
    (counterpart_html a.accepted_with)
    actions

let accepted_section ?request ~(community : community) accepted =
  let body =
    match accepted with
    | [] ->
        "<p class='ccn-empty'>No connected communities yet.</p>"
    | rows ->
        Printf.sprintf "<ul class='ccn-list'>%s</ul>"
          (String.concat "\n"
             (List.map (accepted_row ?request ~community) rows))
  in
  Printf.sprintf
    "<section class='ccn-section'><h2 class='ccn-section-title'>Connected \
     communities</h2>%s</section>"
    body

(* --- Section 2: incoming requests --- *)

let incoming_row ?request ~(community : community) (p : pending) =
  let actionable =
    valid_community_slug community.slug && valid_connection_id p.pending_id
  in
  let accept =
    if not actionable then ""
    else if not community.eligible then
      "<p class='ccn-accept-unavailable'>Accepting is unavailable while this \
       community cannot connect.</p>"
    else
      action_form ?request
        ~action:
          (action_path ~community_slug:community.slug
             ~connection_id:p.pending_id ~verb:"accept")
        ~cls:"ccn-accept" ~label:"Accept" ()
  in
  let reject =
    if not actionable then ""
    else
      (* Rejection stays available on an ineligible community: a pending
         request must always be closable. *)
      action_form ?request
        ~action:
          (action_path ~community_slug:community.slug
             ~connection_id:p.pending_id ~verb:"reject")
        ~cls:"ccn-reject" ~label:"Reject" ()
  in
  Printf.sprintf
    "<li class='ccn-row ccn-row--incoming'>%s%s<div \
     class='ccn-actions'>%s%s</div></li>"
    (counterpart_html p.pending_with)
    (note_html p.pending_note)
    accept reject

let incoming_section ?request ~(community : community) incoming =
  let body =
    match incoming with
    | [] -> "<p class='ccn-empty'>No incoming requests.</p>"
    | rows ->
        Printf.sprintf "<ul class='ccn-list'>%s</ul>"
          (String.concat "\n"
             (List.map (incoming_row ?request ~community) rows))
  in
  Printf.sprintf
    "<section class='ccn-section'><h2 class='ccn-section-title'>Incoming \
     requests</h2>%s</section>"
    body

(* --- Section 3: outgoing requests --- *)

let outgoing_row (p : pending) =
  Printf.sprintf
    "<li class='ccn-row ccn-row--outgoing'>%s%s<p \
     class='ccn-state'>Waiting for a reply</p></li>"
    (counterpart_html p.pending_with)
    (note_html p.pending_note)

let outgoing_section outgoing =
  let body =
    match outgoing with
    | [] -> "<p class='ccn-empty'>No outgoing requests.</p>"
    | rows ->
        Printf.sprintf "<ul class='ccn-list'>%s</ul>"
          (String.concat "\n" (List.map outgoing_row rows))
  in
  Printf.sprintf
    "<section class='ccn-section'><h2 class='ccn-section-title'>Outgoing \
     requests</h2>%s</section>"
    body

(* --- The entry point into the search flow --- *)

(* Rendered first on the management page: starting a connection is what a
   moderator comes here to do, and the three lists below are the record of
   what that produced. *)
let connect_section ~(community : community) =
  if not (valid_community_slug community.slug) then ""
  else if not community.eligible then
    "<section class='ccn-section'><h2 class='ccn-section-title'>Connect a \
     community</h2><p class='ccn-empty'>Unavailable while this community \
     cannot connect.</p></section>"
  else
    (* The panel's primary action, on the shared .btn/.btn--primary control
       so it reads as an action rather than as prose. Still an ordinary
       link to the same destination: no JavaScript, and nothing outside the
       anchor itself is clickable. *)
    Printf.sprintf
      "<section class='ccn-section'><h2 class='ccn-section-title'>Connect a \
       community</h2><p class='ccn-section-desc'>Find a community to connect \
       with. Both sides must agree: they review your request.</p><p \
       class='ccn-cta'><a class='btn btn--primary' href='%s/new'>Find a \
       community</a></p></section>"
      (esc (base_path ~community_slug:community.slug))

let heading_html =
  "<div class='create-head'><h1 class='create-title'>Connections</h1><p \
   class='create-sub ccn-intro'>Mutual connections between this community \
   and others.</p></div>"

(* Action first, then the record: Connect a community, then the connected
   communities, then the two pending lists. The order is fixed, so an empty
   page and a busy one read the same way. Row order inside each section is
   the read model's. *)
let management_body ?request ~(state : state) () =
  Printf.sprintf "%s%s%s%s%s%s" heading_html
    (ineligible_notice_html state.community)
    (connect_section ~community:state.community)
    (accepted_section ?request ~community:state.community state.accepted)
    (incoming_section ?request ~community:state.community state.incoming)
    (outgoing_section state.outgoing)

(* --- Search (step one) --- *)

let search_form ~(community : community) ~query =
  Printf.sprintf
    "<form method='GET' action='%s/new' class='ccn-search-form'><label \
     class='ccn-label' for='ccn-q'>Search by name or address</label><input \
     id='ccn-q' type='text' name='q' value='%s' maxlength='120' \
     class='ccn-input'><button type='submit' \
     class='ccn-btn'>Search</button></form>"
    (esc (base_path ~community_slug:community.slug))
    (esc query)

(* A result is a link into the confirmation step, never a submit control and
   never a note field: the note is asked once, on the next page. *)
let result_row ~(community : community) ~query (t : target) =
  let identity =
    Printf.sprintf
      "<span class='ccn-name'>%s</span><span class='ccn-slug'>/c/%s</span>"
      (esc t.target_name) (esc t.target_slug)
  in
  if not (valid_community_slug t.target_slug) then
    Printf.sprintf "<li class='ccn-row'>%s</li>" identity
  else
    let href =
      Uri.to_string
        (Uri.add_query_params'
           (Uri.of_string (base_path ~community_slug:community.slug ^ "/new"))
           [ ("q", query); ("target", t.target_slug) ])
    in
    Printf.sprintf
      "<li class='ccn-row'><div class='ccn-identity'>%s</div><a \
       class='ccn-link' href='%s'>Continue</a></li>"
      identity (esc href)

let search_body ~(community : community) ~query ~results ~searched =
  let results_html =
    if not searched then
      "<p class='ccn-empty'>Enter a name or address to search.</p>"
    else
      match results with
      | [] -> "<p class='ccn-empty'>No communities matched.</p>"
      | rows ->
          Printf.sprintf "<ul class='ccn-list'>%s</ul>"
            (String.concat "\n"
               (List.map (result_row ~community ~query) rows))
  in
  Printf.sprintf
    "<div class='create-head'><h1 class='create-title'>Connect a \
     community</h1><p class='create-sub ccn-intro'>Only public, published, \
     discoverable communities you are not already connected to can be \
     found.</p></div>%s%s<p><a class='ccn-link' href='%s'>Back to \
     connections</a></p>"
    (search_form ~community ~query)
    results_html
    (esc (base_path ~community_slug:community.slug))

(* --- Confirmation (step two) --- *)

let confirm_body ?request ~(community : community) ~(target : target) ~note
    () =
  let csrf_field =
    match request with None -> "" | Some request -> Dream.csrf_tag request
  in
  let form =
    if
      valid_community_slug community.slug
      && valid_community_slug target.target_slug
    then
      Printf.sprintf
        "<form method='POST' action='%s/request' \
         class='ccn-confirm-form'>%s<input type='hidden' name='target' \
         value='%s'><label class='ccn-label' for='ccn-note'>Private note \
         (optional)</label><p class='ccn-hint'>Visible only to the \
         moderators of the two communities.</p><textarea id='ccn-note' \
         name='note' rows='5' maxlength='2000' \
         class='ccn-textarea'>%s</textarea><button type='submit' \
         class='ccn-btn'>Send request</button></form>"
        (esc (base_path ~community_slug:community.slug))
        csrf_field
        (esc target.target_slug)
        (esc note)
    else "<p class='ccn-empty'>This community is not available to connect.</p>"
  in
  Printf.sprintf
    "<div class='create-head'><h1 class='create-title'>Send a connection \
     request</h1><p class='create-sub ccn-intro'>%s will be asked to accept \
     or reject this request.</p></div><div class='ccn-identity'><span \
     class='ccn-name'>%s</span><span class='ccn-slug'>/c/%s</span></div>%s<p><a \
     class='ccn-link' href='%s/new'>Choose a different community</a></p>"
    (esc target.target_name) (esc target.target_name)
    (esc target.target_slug) form
    (esc (base_path ~community_slug:community.slug))

(* --- Documents --- *)

(* Identical shell handling to the sibling review surface: with launch chrome
   the panel renders inside the shared community-settings shell (header band
   + grouped settings navigation, Connections active — the viewer is
   top_mod-or-durable-admin by the read model's SQL, so the full permitted
   nav is honest); without it (only when the durable community record could
   not be re-read AFTER authorization succeeded) the same body renders inside
   the chrome-free launch message document rather than fabricating community
   data. *)
let document ?user ?request ?shell ~title ~body () =
  let wrapped =
    Printf.sprintf
      "<div class='create-wrap community-connections'><div \
       class='create-panel'>%s</div></div>"
      body
  in
  match shell with
  | None ->
      Components.launch_message_page ?request ~noindex:true ~title
        ~content:(Printf.sprintf "<div class='create-shell'>%s</div>" wrapped)
        ()
  | Some ((community_record : Community_types.community), rail_communities, sidebar) ->
      let content =
        Community_settings_shell.wrap ~slug:community_record.slug
          ~active:Community_settings_shell.Connections
          ~can_complete_setup:
            (Community_settings_shell.can_complete_setup
               ~community:community_record ~authorized:true)
          ~network_manager:true ~panel:wrapped ()
      in
      Components.launch_community_page ?user ?request ~noindex:true
        ~rail_communities ~community:community_record ~sidebar
        ~page_class:"launch-community-connections" ~title ~content ()

let management_page ?user ?request ?shell ~state ~feedback () =
  document ?user ?request ?shell ~title:"Connections"
    ~body:(feedback_html feedback ^ management_body ?request ~state ())
    ()

let target_search_page ?user ?request ?shell ~community ~query ~results ~searched
    ~feedback () =
  document ?user ?request ?shell ~title:"Connect a community"
    ~body:
      (feedback_html feedback
      ^ search_body ~community ~query ~results ~searched)
    ()

let confirm_page ?user ?request ?shell ~community ~target ~note ~feedback () =
  document ?user ?request ?shell ~title:"Send a connection request"
    ~body:
      (feedback_html feedback
      ^ confirm_body ?request ~community ~target ~note ())
    ()
