let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '&'  -> Buffer.add_string buf "&amp;"
    | '<'  -> Buffer.add_string buf "&lt;"
    | '>'  -> Buffer.add_string buf "&gt;"
    | '"'  -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#39;"
    | c    -> Buffer.add_char buf c) s;
  Buffer.contents buf

(* Block javascript: and data: URLs — only http(s) are safe as user-supplied link targets.
   Falls back to "#" so the anchor renders but is inert. *)
let safe_url url =
  let lower = String.lowercase_ascii url in
  if (String.length lower >= 7 && String.sub lower 0 7 = "http://")
  || (String.length lower >= 8 && String.sub lower 0 8 = "https://")
  then html_escape url else "#"

(* Internal nav targets (sidebar channel/section links, rail tiles) are server-built app paths
   like "/c/europe/ch/general" — NOT user input. They must NOT go through safe_url: that only
   passes http(s):// and would collapse every relative path to "#" (the bug that made the
   community sidebar links inert). This is the dedicated check for rooted internal paths:
   require a single leading "/", reject "" / "#", and reject protocol-relative "//host" (and the
   "/\\host" backslash variant some engines normalise to it) so it can never become an open
   redirect. javascript:/data: can't start with "/" so they're rejected implicitly.

   It is also the gate the shared message page applies to its "Go back"
   destination, which several handlers build from a percent-decoded route
   parameter — so the value reaching it can be fully attacker-controlled, and
   escaping-plus-refusal here is what makes that one call site cover ~70.

   The site root "/" is a rooted internal path like any other and is accepted
   explicitly: the length>=2 guard below exists only so path.[1] can be read,
   and rejecting "/" would have collapsed the most common back link on the
   shared message page (194 call sites pass exactly "/") to an inert "#". *)
let safe_internal_path path =
  if String.equal path "/" then path
  else if
    String.length path >= 2
    && path.[0] = '/'
    && path.[1] <> '/' && path.[1] <> '\\'
  then html_escape path
  else "#"

(* Escaping for a value interpolated into a JavaScript single-quoted string
   literal that itself lives inside an HTML event-handler attribute (the
   confirmModal onsubmit hooks). html_escape alone is NOT sufficient there:
   the HTML parser decodes entities before the JavaScript parser sees the
   source, so &#39; becomes a real apostrophe again and terminates the
   literal early — turning the rest of the value into executable code.

   The two escapes therefore have to compose in this order:
     1. JavaScript-escape, so the post-decode source is a valid literal;
     2. html_escape, so the attribute delimiter itself is safe.
   html_escape is exactly reversible, so step 2 is undone by the parser and
   step 1's output is what JavaScript actually parses. Backslash first (else
   it would double-escape the escapes we add), then the quote characters,
   then the line terminators — an unescaped newline is a syntax error inside
   a literal — then < and & as \xNN so no markup or entity survives, then
   the remaining C0/DEL control bytes. *)
let js_single_quoted_attr s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\'' -> Buffer.add_string buf "\\'"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '<' -> Buffer.add_string buf "\\x3C"
      | '>' -> Buffer.add_string buf "\\x3E"
      | '&' -> Buffer.add_string buf "\\x26"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string buf (Printf.sprintf "\\x%02X" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  html_escape (Buffer.contents buf)

(* === IMAGES ===
   One escaping gate + a few render primitives so every image surface treats stored URLs the
   same way. Before this, image src was rendered three different ways (raw, html_escape, and
   safe_url), and safe_url — correct for href — silently broke local uploads (it only passes
   http(s):// and collapses "/static/uploads/x.webp" to "#"). *)

(* The dedicated gate for image src attributes. Unlike safe_url (link href, http(s) only) it
   ALSO passes rooted local app paths so uploaded images render — the upload pipeline stores
   "/static/uploads/<name>.webp". Rejects (→ "#"): javascript:/data:, protocol-relative
   "//host" and the "/\\host" variant, empty/whitespace, AND any candidate containing a quote,
   angle bracket, backtick, ASCII whitespace, or control char. Rather than leaning on
   html_escape to neutralise an injection payload after the fact, we refuse to emit it at all —
   a legitimate upload path / URL never contains these chars, so this only ever rejects hostile
   or malformed input. (html_escape stays as belt-and-suspenders on the accepted value.) *)
let safe_img_src raw =
  let url = String.trim raw in
  let has_dangerous_char =
    String.exists (fun c ->
      match c with
      | '\'' | '"' | '<' | '>' | '`' -> true
      | ' ' | '\t' | '\n' | '\r' -> true
      | c when Char.code c < 0x20 || Char.code c = 0x7f -> true
      | _ -> false) url
  in
  if url = "" || has_dangerous_char then "#"
  else
    let lower = String.lowercase_ascii url in
    let is_http =
      (String.length lower >= 7 && String.sub lower 0 7 = "http://")
      || (String.length lower >= 8 && String.sub lower 0 8 = "https://")
    in
    (* Rooted local path: single leading "/", not "//host" and not "/\\host". javascript:/data:
       can't start with "/", so they're rejected implicitly here. *)
    let is_local =
      String.length url >= 2 && url.[0] = '/' && url.[1] <> '/' && url.[1] <> '\\'
    in
    if is_http || is_local then html_escape url else "#"

(* First-letter glyph for a name/username; "?" when empty. html_escape'd so a one-char name
   like "<" can't inject. String.sub 0 1 matches the existing letter-tile code (byte-based;
   a leading multibyte char degrades to "?"-ish like before, not a crash). *)
let initial_glyph name =
  let n = String.trim name in
  if n = "" then "?"
  else html_escape (String.uppercase_ascii (String.sub n 0 1))

(* The single letter-tile fallback. [class_] carries the surface's existing utility classes
   (size/shape/color/centering) so each call site keeps its exact look — this only centralizes
   the first-letter extraction + escaping that was hand-rolled per surface. *)
let initial_tile ?(class_="") name =
  let cls = if class_ = "" then "" else " class='" ^ class_ ^ "'" in
  Printf.sprintf "<div%s>%s</div>" cls (initial_glyph name)

(* <img> when the avatar URL is safe and non-empty, else the letter tile. A stored value that
   fails safe_img_src (e.g. an injected javascript: payload) falls back to the tile rather than
   rendering a dead src='#'. [img_class] styles the <img>, [tile_class] the fallback <div>. *)
let user_avatar ?(alt="") ~img_class ~tile_class ~username avatar_url =
  match avatar_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then initial_tile ~class_:tile_class username
      else Printf.sprintf "<img src='%s' class='%s' alt='%s'>" src img_class (html_escape alt)
  | _ -> initial_tile ~class_:tile_class username

(* Community counterpart of user_avatar; the tile glyph is the community name's first letter. *)
let community_avatar ?(alt="") ~img_class ~tile_class ~name avatar_url =
  match avatar_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then initial_tile ~class_:tile_class name
      else Printf.sprintf "<img src='%s' class='%s' alt='%s'>" src img_class (html_escape alt)
  | _ -> initial_tile ~class_:tile_class name

(* Banner image when present & safe, else the caller's existing fallback element (e.g. a
   gradient block). [wrap_class] wraps the <img> case; [fallback_class] is the empty
   placeholder div — both preserve the current home-hero markup when passed its class strings. *)
let community_banner ~wrap_class ~img_class ~fallback_class banner_url =
  match banner_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then Printf.sprintf "<div class='%s'></div>" fallback_class
      else Printf.sprintf "<div class='%s'><img src='%s' class='%s' alt='banner'></div>"
             wrap_class src img_class
  | _ -> Printf.sprintf "<div class='%s'></div>" fallback_class
let is_deleted_user u = String.length u >= 9 && String.sub u 0 9 = "[deleted_"

(* mod_usernames/admin_usernames enable badge rendering at call sites that know the community;
   callers without context omit the params, defaulting to [] so badge logic is a no-op. *)
let render_author ?(mod_usernames=[]) ?(admin_usernames=[]) username =
  if is_deleted_user username then
    "<span class='text-gray-400 italic'>[deleted]</span>"
  else
    let mod_badge =
      if List.mem username mod_usernames then
        "<span class='mod-badge ml-1 text-[10px] font-semibold bg-green-100 text-green-700 px-1.5 py-0.5 rounded'>[MOD]</span>"
      else ""
    in
    (* Admin badge is always site-wide; rendered after MOD so both appear side-by-side
       for the rare case where a site admin is also a local moderator. *)
    let admin_badge =
      if List.mem username admin_usernames then
        "<span class='mod-badge ml-1 text-[10px] font-semibold bg-red-100 text-red-700 px-1.5 py-0.5 rounded'>[ADMIN]</span>"
      else ""
    in
    Printf.sprintf "<a href='/u/%s' class='hover:text-[#C94C4C] hover:underline font-medium transition'>u/%s</a>%s%s" (html_escape username) (html_escape username) mod_badge admin_badge

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
