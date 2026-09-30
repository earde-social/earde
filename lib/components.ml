(* Reusable rendering primitives. Escaping and URL policy live in Html. *)

(* First-letter glyph for a name/username; "?" when empty. String.sub 0 1
   is byte-based: a leading multibyte character degrades rather than
   crashing. *)
let initial_glyph name =
  let n = String.trim name in
  if n = "" then (Html.static "?")
  else Html.text (String.uppercase_ascii (String.sub n 0 1))

(* The single letter-tile fallback. [class_] carries the surface's existing
   classes so each call site keeps its exact look. *)
let initial_tile ?(class_ = "") name =
  if class_ = "" then Html.template "<div>%s</div>" [ initial_glyph name ]
  else
    Html.template "<div class='%s'>%s</div>"
      [ Html.text class_; initial_glyph name ]

(* <img> when the avatar URL passes the image policy, else the letter tile:
   a stored value that fails it (an injected javascript: payload, say)
   falls back to the tile rather than rendering a dead src='#'. *)
let user_avatar ?(alt = "") ~img_class ~tile_class ~username avatar_url =
  match Option.bind avatar_url Html.image_src_opt with
  | Some src ->
      Html.template "<img src='%s' class='%s' alt='%s'>"
        [ src; Html.text img_class; Html.text alt ]
  | None -> initial_tile ~class_:tile_class username

(* Community counterpart of user_avatar; the tile glyph is the community
   name's first letter. *)
let community_avatar ?(alt = "") ~img_class ~tile_class ~name avatar_url =
  match Option.bind avatar_url Html.image_src_opt with
  | Some src ->
      Html.template "<img src='%s' class='%s' alt='%s'>"
        [ src; Html.text img_class; Html.text alt ]
  | None -> initial_tile ~class_:tile_class name

(* Banner image when present and safe, else the caller's fallback element. *)
let community_banner ~wrap_class ~img_class ~fallback_class banner_url =
  match Option.bind banner_url Html.image_src_opt with
  | Some src ->
      Html.template
        "<div class='%s'><img src='%s' class='%s' alt='banner'></div>"
        [ Html.text wrap_class; src; Html.text img_class ]
  | None -> Html.template "<div class='%s'></div>" [ Html.text fallback_class ]

let is_deleted_user u = String.length u >= 9 && String.sub u 0 9 = "[deleted_"

(* mod_usernames/admin_usernames enable badge rendering at call sites that know the community;
   callers without context omit the params, defaulting to [] so badge logic is a no-op. *)
let render_author ?(mod_usernames=[]) ?(admin_usernames=[]) username =
  if is_deleted_user username then
    (Html.static "<span class='text-gray-400 italic'>[deleted]</span>")
  else
    let mod_badge =
      if List.mem username mod_usernames then
        (Html.static "<span class='mod-badge ml-1 text-[10px] font-semibold bg-green-100 text-green-700 px-1.5 py-0.5 rounded'>[MOD]</span>")
      else Html.empty
    in
    (* Admin badge is always site-wide; rendered after MOD so both appear side-by-side
       for the rare case where a site admin is also a local moderator. *)
    let admin_badge =
      if List.mem username admin_usernames then
        (Html.static "<span class='mod-badge ml-1 text-[10px] font-semibold bg-red-100 text-red-700 px-1.5 py-0.5 rounded'>[ADMIN]</span>")
      else Html.empty
    in
    Html.template "<a href='/u/%s' class='hover:text-[#C94C4C] hover:underline font-medium transition'>u/%s</a>%s%s"
  [ Html.text (username)
  ; Html.text (username)
  ; mod_badge
  ; admin_badge ]

(* Parse and diff in OCaml rather than casting in SQL to keep DB queries generic
   and avoid timezone drift when the DB and app server are in different locales. *)
let time_ago date_str =
  try
    let clean_str = if String.length date_str >= 19 then String.sub date_str 0 19 else date_str in
    let (y, m, d, h, min, s) =
      Scanf.sscanf clean_str "%d-%d-%d %d:%d:%d" (fun y m d h min s -> (y, m, d, h, min, s))
    in
    let tm = { Unix.tm_sec = s; tm_min = min; tm_hour = h; tm_mday = d;
               tm_mon = m - 1; tm_year = y - 1900; tm_wday = 0; tm_yday = 0; tm_isdst = false } in
    (* mktime treats tm as local time; DB timestamps are UTC.
       Compute UTC offset: mktime(gmtime(now)) returns now interpreted as local → offset = now - mktime(gmtime(now)) *)
    let epoch_local, _ = Unix.mktime tm in
    let now = Unix.gettimeofday () in
    let epoch_gm, _ = Unix.mktime (Unix.gmtime now) in
    let tz_offset = now -. epoch_gm in
    let epoch = epoch_local +. tz_offset in
    let diff = int_of_float (now -. epoch) in

    if diff < 60 then "just now"
    else if diff < 3600 then Printf.sprintf "%d min ago" (diff / 60)
    else if diff < 86400 then Printf.sprintf "%d hr ago" (diff / 3600)
    else if diff < 2592000 then Printf.sprintf "%d days ago" (diff / 86400)
    else if diff < 31536000 then Printf.sprintf "%d mo ago" (diff / 2592000)
    else Printf.sprintf "%d yr ago" (diff / 31536000)
  with _ ->
    date_str

let format_month_year date_str =
  try
    let (y, m) = Scanf.sscanf date_str "%d-%d" (fun y m -> (y, m)) in
    let months = [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|] in
    if m >= 1 && m <= 12 then Printf.sprintf "%s %d" months.(m-1) y
    else date_str
  with _ -> date_str
