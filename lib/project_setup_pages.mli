(** /projects/new — server-rendered page/view models for the future project
    setup GET and POST flow. Pure rendering: the closed page state and
    one-time feedback arrive as arguments; this module never reads the
    environment, the session, the query string, or the database, and it
    deliberately does not depend on the draft read model or selection store
    — the later handler translates read-model values into these view
    models. *)

type account_type =
  | Personal
  | Organization

type feedback =
  | Selection_saved  (** The previous POST replaced the selection. *)
  | Selection_stale
      (** The submission named snapshot ids a GitHub refresh replaced. *)
  | Selection_invalid  (** The submission failed the form parser. *)
  | Draft_unavailable
      (** The draft is gone for any reason — nonexistent, foreign, expired,
          terminal, or revoked installation stay indistinguishable. *)
  | Identity_form_invalid
      (** The identity submission failed the strict form parser. *)
  | Identity_name_invalid
  | Identity_slug_invalid
  | Identity_slug_reserved
  | Identity_description_invalid
  | Identity_website_invalid
  | Identity_primary_invalid
      (** The primary is not among the currently selected repositories. *)
  | Identity_primary_required  (** Kind [project] demands a primary. *)
  | Identity_namespace_mismatch
      (** An organization project needs an organization installation. *)
  | Identity_slug_unavailable  (** Finalization lost the slug race. *)
  | Identity_repository_already_connected
      (** Some selected repository is claimed by another project; which one
          stays unidentified. *)
  | Identity_creation_failed
      (** Any other finalization failure, indistinguishably. *)

(** One usable draft as the chooser card needs it. [draft_id] is an internal
    routing identifier, not a bearer secret — the future handler
    re-authorizes it against the current user. *)
type draft_option = {
  draft_id : int64;
  account_login : string;
  account_type : account_type;
  repository_count : int;
  selected_repository_count : int;
}

(** One verified public-repository snapshot row as the selection form needs
    it. [snapshot_id] is the local snapshot row id — used deliberately so a
    GitHub refresh invalidates stale form submissions. [html_url] is the
    previously validated canonical GitHub URL; it is still escaped normally
    at render time. [default_branch] may contain ['/'] and is rendered
    strictly as text, never placed into a URL. *)
type repository_option = {
  snapshot_id : int64;
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_archived : bool;
  is_selected : bool;
}

type configuration = {
  draft : draft_option;
  repositories : repository_option list;
}

(** One already-selected repository as the identity step needs it: display
    name, archived flag, and the local snapshot id used only as a
    [primary_snapshot_id] option value. No GitHub repository id, owner or
    installation identifier, and no hidden selected-set field ever reaches
    this step — the selection is server-owned. *)
type identity_repository = {
  snapshot_id : int64;
  full_name : string;
  is_archived : bool;
}

(** Identity form values: either initial prefills chosen by the later
    handler or a failed submission being re-rendered. Every string is
    escaped normally at render time. *)
type identity_values = {
  kind : Project_identity.kind;
  name : string;
  slug : string;
  description : string;
  website_url : string;
  primary_snapshot_id : int64 option;
}

type identity_configuration = {
  draft : draft_option;
  selected_repositories : identity_repository list;
  values : identity_values;
}

type state =
  | No_available_drafts
  | Choose_draft of draft_option list
  | Configure_repositories of configuration
  | Configure_identity of identity_configuration

val project_setup_page :
  ?user:string ->
  ?request:Dream.request ->
  state:state ->
  feedback:feedback option ->
  unit ->
  string
(** The complete "Create a project" page in the shared create-flow layout.

    Rendering is total and defensive: [Choose_draft []] renders like
    [No_available_drafts]; a [Configure_repositories] value with an empty
    repository list or a non-positive draft id renders a generic
    unavailable state with no selection form; a non-positive draft or
    snapshot id never produces an actionable link or form control; negative
    counts are clamped rather than shown.

    [Configure_repositories] renders exactly one
    [POST /projects/new/repositories] form whose only application-owned
    hidden field is [draft_id]; each repository is one [repository] checkbox
    valued by its local snapshot id, and the nameless submit button never
    enters the field set. When [request] is supplied, the form additionally
    contains Dream's framework CSRF hidden field ([Dream.csrf_tag]) — it is
    framework data, never an application form field, and it is absent from
    pure rendering calls where no request exists. Chooser links are built
    structurally ([Uri]) as [/projects/new?draft=<id>].

    [Configure_identity] renders a visibly separate "Project details" step:
    a read-only summary of the selected repositories (with a structural link
    back to [/projects/new?draft=<id>]) and exactly one [POST /projects]
    form whose application field set is exactly [draft_id] (hidden), [kind]
    (a select of the six canonical wire values), [name], [slug],
    [description], [website_url], and [primary_snapshot_id] (a select with
    a blank "no primary" option plus one option per valid selected
    repository, valued by local snapshot id). The same CSRF rule applies.
    No repository checkboxes, hidden snapshot ids, community chooser, or
    JavaScript appear; archived repositories stay offered with an [Archived]
    marker. Defensively: an empty or all-corrupt selected-repository list
    renders no identity form (only an explanation and the back link), a
    non-positive snapshot id never becomes an option value, and a
    [values.primary_snapshot_id] that matches no rendered repository falls
    back to the blank option.

    Feedback is cosmetic only — it never changes which state or form is
    rendered — and [None] renders no alert element at all. All copy stays
    factual ("verified through GitHub"); no endorsement claims, no claim
    that a community exists or that synchronization is active. *)
