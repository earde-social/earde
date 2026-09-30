(** Community settings: the settings page, details and media updates,
    downvotes, visibility and indexability, and the legacy mutations that
    network-community setup drafts must not reach. *)

val community_settings_handler : Dream.handler
(** Pure lifecycle gate for the /c/:slug/settings/visibility POST, extracted for
    testing: [None] lets the update proceed; [Some message] is the user-facing
    rejection (published network communities must remain public). Delegates to
    {!Network_communities.visibility_change_allowed}. *)

val update_community_handler : Dream.handler

val toggle_downvotes_handler : Dream.handler

val visibility_update_rejection :
  Community_types.community -> requested_visibility:Community_types.community_visibility -> string option

val update_community_visibility_handler : Dream.handler

val update_community_indexability_handler : Dream.handler

val update_channel_indexability_handler : Dream.handler

val update_section_indexability_handler : Dream.handler
