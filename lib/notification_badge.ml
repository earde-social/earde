(* The authenticated top bar's unread-notification badge. See the .mli.

   This replaces a client-side arrangement that produced three different
   answers to one question. The badge used to be server-rendered always, as
   a literal <span ...>0</span> carrying a `hidden` class, and then revealed
   by a per-page-load fetch of /api/unread-notifs. Nothing in the document
   hid it: `hidden` was deliberately not a global utility, so every launch
   page class had to repeat its own `#notif-badge.hidden { display: none }`
   rule, and a page class that forgot the rule showed a hard-coded zero. The
   endpoint also answered "0" when the count query failed, which the browser
   could not tell apart from a real zero.

   So the count is resolved on the server, once, from the same query on every
   page, and the element only exists when there is something to show. *)

let unread_field : int Dream.field =
  Dream.new_field ~name:"earde.unread_notifications" ()

(* Only a request that renders a document can render a badge. Everything else
   would pay for the count and throw it away, so it is excluded structurally:
   assets by prefix, and the two authenticated non-document GET routes by
   suffix. The latter matter more than they look — the live-chat page re-reads
   messages.json after every burst of messages and refreshes its realtime
   token on a timer, so counting there would attach an unread-count query to
   the chat polling loop rather than to page loads. Every non-GET is likewise
   uncounted: its response is a redirect or a re-render the following GET
   counts for itself.

   The query string is dropped first — messages.json arrives as
   ".../messages.json?after_id=N" — so the suffix test sees the route path. *)
let counted_path target =
  let path =
    match String.index_opt target '?' with
    | Some i -> String.sub target 0 i
    | None -> (
        match String.index_opt target '#' with
        | Some i -> String.sub target 0 i
        | None -> target)
  in
  let has_prefix p =
    String.length path >= String.length p
    && String.sub path 0 (String.length p) = p
  in
  let has_suffix s =
    let n = String.length s and l = String.length path in
    l >= n && String.sub path (l - n) n = s
  in
  not
    (has_prefix "/static/" || has_prefix "/css/" || has_prefix "/js/"
   || has_suffix ".json"
    || has_suffix "/realtime-token")

let middleware inner_handler request =
  let%lwt () =
    match (Dream.method_ request, Dream.session_field request "user_id") with
    | `GET, Some uid_str when counted_path (Dream.target request) -> (
        match int_of_string_opt uid_str with
        (* A session carrying a non-numeric user_id is already broken; the
           badge is not the place to raise about it. *)
        | None -> Lwt.return_unit
        | Some user_id ->
            Dream.sql request (fun db ->
                match%lwt Notification_store.count_unread_notifs db user_id with
                | Ok count ->
                    Dream.set_field request unread_field count;
                    Lwt.return_unit
                (* Leave the field unset: the document renders no badge
                   rather than a count it does not have. *)
                | Error _ -> Lwt.return_unit))
    | _ -> Lwt.return_unit
  in
  inner_handler request

let display_cap = 99

let render count =
  if count <= 0 then Html.empty
  else
    let label =
      if count > display_cap then Printf.sprintf "%d+" display_cap
      else string_of_int count
    in
    Html.template "<span id='notif-badge' class='bell__count'>%s</span>"
      [ Html.text label ]

let badge_html ?request () =
  match request with
  | None -> Html.empty
  | Some request -> (
      match Dream.field request unread_field with
      | None -> Html.empty
      | Some count -> render count)
