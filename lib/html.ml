type t = string

let empty = ""

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let text = escape
let int = string_of_int
let int64 = Int64.to_string

let has_prefix ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let is_http url =
  let lower = String.lowercase_ascii url in
  has_prefix ~prefix:"http://" lower || has_prefix ~prefix:"https://" lower

(* Only http(s) is safe as a user-supplied link target; the inert "#" keeps
   the anchor rendering. *)
let external_url_opt url = if is_http url then Some (escape url) else None
let external_url url = Option.value (external_url_opt url) ~default:"#"

(* A server-built app path such as "/c/europe/ch/general". It must not go
   through [external_url], which would collapse every relative path to "#".
   The single leading "/" rule refuses protocol-relative "//host" and the
   "/\\host" variant some engines normalize to it, so the value can never
   become an open redirect; javascript: and data: cannot start with "/".
   The site root is accepted explicitly: the length guard only exists so
   the second character can be read. The shared message page applies this
   to a "Go back" target that handlers build from percent-decoded route
   parameters, so the input can be fully attacker-controlled. *)
let internal_path_opt path =
  if String.equal path "/" then Some path
  else if
    String.length path >= 2
    && path.[0] = '/'
    && path.[1] <> '/'
    && path.[1] <> '\\'
  then Some (escape path)
  else None

let internal_path path = Option.value (internal_path_opt path) ~default:"#"

(* Image sources pass rooted local paths (uploads are stored as
   "/static/uploads/<name>.webp") as well as http(s). A candidate carrying
   a quote, angle bracket, backtick, whitespace or control character is
   refused outright rather than escaped: a legitimate upload path or URL
   never contains one, so only hostile or malformed input is rejected.
   Escaping the accepted value is belt and braces. *)
let image_src_opt raw =
  let url = String.trim raw in
  let dangerous =
    String.exists
      (fun c ->
        match c with
        | '\'' | '"' | '<' | '>' | '`' | ' ' | '\t' | '\n' | '\r' -> true
        | c -> Char.code c < 0x20 || Char.code c = 0x7f)
      url
  in
  if url = "" || dangerous then None
  else
    let is_local =
      String.length url >= 2
      && url.[0] = '/'
      && url.[1] <> '/'
      && url.[1] <> '\\'
    in
    if is_http url || is_local then Some (escape url) else None

let image_src raw = Option.value (image_src_opt raw) ~default:"#"

(* The HTML parser decodes entities before the script engine reads the
   attribute, so &#39; would become a real apostrophe and end the literal
   early. JavaScript escaping therefore comes first and HTML escaping
   second; the HTML escaping is exactly reversed by the parser. Backslash
   first (else the added escapes would be doubled), then quotes, line
   terminators (a raw newline is a syntax error inside a literal), then
   < > & as \xNN so no markup or entity survives, then the remaining
   C0/DEL control bytes. *)
let js_string s =
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
  escape (Buffer.contents buf)

let template markup holes =
  let n = String.length markup in
  let buf = Buffer.create (n + 64) in
  let rec go i holes =
    if i >= n then (
      if holes <> [] then invalid_arg "Html.template: more holes than markers")
    else if i + 1 < n && markup.[i] = '%' && markup.[i + 1] = 's' then (
      match holes with
      | h :: rest ->
          Buffer.add_string buf h;
          go (i + 2) rest
      | [] -> invalid_arg "Html.template: more markers than holes")
    else (
      Buffer.add_char buf markup.[i];
      go (i + 1) holes)
  in
  go 0 holes;
  Buffer.contents buf

let static markup = markup
let trusted markup = markup
let concat = String.concat ""
let join = String.concat
module Infix = struct
  let ( ++ ) = ( ^ )
end
let is_empty s = s = ""
let to_string s = s
