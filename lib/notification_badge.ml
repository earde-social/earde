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

(* Asset requests render no document, so counting for them would be a pure
   extra round trip; the same is true of every non-GET, whose response is a
   redirect or a re-render the following GET will count for itself. *)
let counted_path path =
  let has_prefix p =
    String.length path >= String.length p && String.sub path 0 (String.length p) = p
  in
  not (has_prefix "/static/" || has_prefix "/css/" || has_prefix "/js/")

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
                match%lwt Db.count_unread_notifs db user_id with
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
  if count <= 0 then ""
  else
    let label =
      if count > display_cap then Printf.sprintf "%d+" display_cap
      else string_of_int count
    in
    Printf.sprintf "<span id='notif-badge' class='bell__count'>%s</span>" label

let badge_html ?request () =
  match request with
  | None -> ""
  | Some request -> (
      match Dream.field request unread_field with
      | None -> ""
      | Some count -> render count)
