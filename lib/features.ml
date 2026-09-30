(* Community-level feature capabilities.

   V1 is deliberately minimal: no schema change, no settings UI. A capability
   is an allow-list of community slugs resolved server-side at render time, so
   the browser can never enable a feature by editing a query string or its own
   DOM — pages simply don't render the feature's UI outside enabled
   communities.

   Shared cursors: the Beryl community is enabled by being in the default
   allow-list below. When EARDE_SHARED_CURSOR_COMMUNITIES is set (comma-
   separated slugs) it replaces the default list entirely — that is how
   local/dev environments point the feature at a test community. *)

let shared_cursors_env = "EARDE_SHARED_CURSOR_COMMUNITIES"
let default_shared_cursor_slugs = [ "beryl" ]

let slugs_of_string raw =
  String.split_on_char ',' raw
  |> List.map String.trim
  |> List.map String.lowercase_ascii
  |> List.filter (fun slug -> slug <> "")

let enabled_in ~slugs ~community_slug =
  List.mem (String.lowercase_ascii community_slug) slugs

let shared_cursor_slugs () =
  match Sys.getenv_opt shared_cursors_env with
  | Some raw when String.trim raw <> "" -> slugs_of_string raw
  | _ -> default_shared_cursor_slugs

let shared_cursors_enabled ~community_slug =
  enabled_in ~slugs:(shared_cursor_slugs ()) ~community_slug
