(** The shared-threads surfaces: the per-thread "Share thread" page and the
    community-level "Shared threads" management page.

    Pure rendering over handler-supplied view models: no Caqti, no
    read-model dependency, no session access, no authority decision of its
    own. Every caller-controlled string is escaped at the template boundary,
    and a value that would carry a non-addressable slug or a non-decimal id
    into an action path degrades to plain text rather than becoming
    actionable. No JavaScript is emitted anywhere.

    Language rules: the canonical discussion stays one thread — these pages
    speak of sharing a thread with a community, never of copies, mirrors, or
    synchronization, and never of placements, transitions, or relations. No
    requester, reviewer, or remover username appears anywhere. The private
    request note renders only where the handler already authorized it,
    labelled as private, escaped, and never parsed as Markdown or HTML.

    Both pages live inside the connections management skin whole (the
    [ccn-*] fragment classes beneath the [launch-community-connections]
    body scope), so no new stylesheet family exists for this surface; the
    [community-shared-threads] wrap class inside the panel is its
    distinguishing hook. *)

type candidate = { candidate_name : string; candidate_slug : string }

type share_placement = {
  share_placement_id : string;
      (** The placement's row id as a decimal string — an opaque record
          locator in an action path, never a grant of authority: the
          handler re-resolves and re-authorizes it server-side. *)
  share_destination_name : string;
  share_destination_slug : string;
  share_pending : bool;  (** Pending request vs accepted placement. *)
  share_can_withdraw : bool;
      (** Whether the viewer may withdraw this pending request — decided by
          the handler, rendered here as a control gate only. *)
  share_can_remove : bool;
  share_note : string option;
      (** Already authorized by the handler; [None] both for absence and
          for a viewer who may not read it. *)
}

type share_state = {
  share_thread_title : string;
  share_origin_name : string;
  share_origin_slug : string;
  share_thread_path : string;
      (** The canonical thread path, built server-side — the POST target is
          this path plus [/share]. *)
  share_candidates : candidate list;
  share_placements : share_placement list;
  share_manage_connections : bool;
      (** Whether to offer the origin manager's cross-link to connection
          management inside the no-candidates empty state. *)
}

type section_option = { section_id : string; section_name : string }

type pending_entry = {
  pending_id : string;
  pending_title : string;
  pending_thread_path : string;
  pending_counterpart_name : string;
  pending_counterpart_slug : string;
  pending_note : string option;
  pending_requested_at : string;  (** Raw timestamp text, rendered relative. *)
}

type accepted_entry = {
  accepted_id : string;
  accepted_title : string;
  accepted_thread_path : string;
  accepted_counterpart_name : string;
  accepted_counterpart_slug : string;
  accepted_section : string option;
      (** The destination section name; [None] renders as the flat
          "Uncategorized" label. *)
  accepted_at : string;
}

type management_state = {
  community_name : string;
  community_slug : string;
  community_eligible : bool;
      (** While [false], accept controls render as unavailable; reject,
          withdraw, and remove stay actionable. *)
  sections_enabled : bool;
  section_options : section_option list;
      (** One list for every incoming row's accept form — supplied once,
          never per row. *)
  incoming : pending_entry list;
  outgoing : pending_entry list;
  shared_into : accepted_entry list;
  shared_from : accepted_entry list;
}

(** What a completed action's redirect leaves on the reloaded page. Each is
    one stable outcome; the handler maps only a closed set of query values
    onto these, and an unknown value renders nothing. *)
type notice =
  | Request_sent
  | Request_accepted
  | Request_rejected
  | Request_withdrawn
  | Placement_removed

(** What a refused action leaves on the re-rendered page. None names a
    community's visibility, a moderator, or an internal lifecycle detail. *)
type feedback =
  | Stale_form
  | Destination_required
  | Destination_unavailable
  | Already_shared
  | Note_invalid
  | Thread_unavailable
  | Section_invalid
  | Source_ineligible
      (** The acting community itself may not take part right now. *)
  | Origin_unavailable
      (** The other side of an incoming request — its community or the
          standing connection — is no longer available; which never says. *)
  | Review_unavailable
  | Withdrawal_unavailable
  | Removal_unavailable

val share_page :
  ?user:string ->
  ?request:Dream.request ->
  ?shell:Community_types.community *
         Community_types.community list * Html.t ->
  state:share_state ->
  notice:notice option -> feedback:feedback option -> unit -> string
(** The per-thread Share page: the request form (destination select and the
    optional private note) over the thread's current placements. When no
    destination is available the form gives way to a quiet empty state —
    never an empty select — that does not imply withheld communities exist.
    [shell] is the Cartographic Civic launch chrome; without it the page
    falls back to the chrome-free launch message document, exactly as the
    sibling management surfaces do. *)

val management_page :
  ?user:string ->
  ?request:Dream.request ->
  ?shell:Community_types.community *
         Community_types.community list * Html.t ->
  state:management_state ->
  notice:notice option -> feedback:feedback option -> unit -> string
(** The community management page, four sections in fixed order: incoming
    requests, outgoing requests, shared into this community, shared from
    this community. Sectioned destinations render one section select per
    accept form from the single supplied list; flat destinations render
    none. *)
