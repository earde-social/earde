(** Report form, report queue, moderator management and moderation log pages. *)

val manage_mods_page :
  ?user:string ->
  ?rail_communities:Community_types.community list ->
  is_admin:bool ->
  current_user_role:string option ->
  channels:Channel_store.channel list ->
  sections:Section_store.community_section list ->
  community:Community_types.community ->
  mods:Moderator_store.moderator_entry list ->
  Dream.request ->
  string

val report_form_page :
  ?user:string ->
  ?rail_communities:Community_types.community list ->
  channels:Channel_store.channel list ->
  sections:Section_store.community_section list ->
  can_manage:bool ->
  community:Community_types.community ->
  target_type:Report_store.report_target ->
  target_id:int ->
  target_title:string ->
  return_url:string ->
  Dream.request ->
  string

val reports_queue_page :
  ?user:string ->
  ?rail_communities:Community_types.community list ->
  is_admin:bool ->
  is_top_mod:bool ->
  channels:Channel_store.channel list ->
  sections:Section_store.community_section list ->
  community:Community_types.community ->
  status:Report_store.report_status ->
  reports:Report_store.report_row list ->
  previews:(int * (string * string)) list ->
  Dream.request ->
  string

val mod_log_page :
  ?user:string ->
  ?noindex:bool ->
  ?rail_communities:Community_types.community list ->
  can_access_settings:bool ->
  channels:Channel_store.channel list ->
  sections:Section_store.community_section list ->
  community:Community_types.community ->
  Mod_log_store.mod_action list ->
  Dream.request ->
  string
