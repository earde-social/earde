(* The shipped stylesheet as a browser assembles it: static/css/earde.css with
   its @import partials inlined in order, plus a minimal rule reader, so
   census cases can ask which page classes a rule reaches whichever partial
   holds it and however many routes share it. *)

let entrypoint = "static/css/earde.css"

(* Where a CSS string token ends: at its closing quote, or before an
   unescaped newline (a bad string), as the CSS tokenizer does. *)
let skip_string s i =
  let q = s.[i] and n = String.length s in
  let rec go j =
    if j >= n then j
    else if s.[j] = q then j + 1
    else if s.[j] = '\n' then j
    else if s.[j] = '\\' then go (j + 2)
    else go (j + 1)
  in
  go (i + 1)

let skip_comment s i =
  match Html_assert.index_from s "*/" (i + 2) with
  | Some j -> j + 2
  | None -> String.length s

let strip_comments s =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && s.[i] = '/' && s.[i + 1] = '*' then
      go (skip_comment s i)
    else if s.[i] = '"' || s.[i] = '\'' then (
      let j = skip_string s i in
      Buffer.add_string b (String.sub s i (j - i));
      go j)
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let squash s =
  String.split_on_char ' '
    (String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s)
  |> List.filter (( <> ) "")
  |> String.concat " "

(* Top-level items of comment-free CSS: [`Stmt text] for @import and the
   like, [`Block (prelude, body)] for rules and at-rule blocks. *)
let items s =
  let n = String.length s in
  let rec prelude_end j =
    if j >= n then j
    else
      match s.[j] with
      | '{' | ';' -> j
      | '"' | '\'' -> prelude_end (skip_string s j)
      | _ -> prelude_end (j + 1)
  in
  let rec block_end k depth =
    if k >= n || depth = 0 then k
    else
      match s.[k] with
      | '"' | '\'' -> block_end (skip_string s k) depth
      | '{' -> block_end (k + 1) (depth + 1)
      | '}' -> block_end (k + 1) (depth - 1)
      | _ -> block_end (k + 1) depth
  in
  let rec go i acc =
    let i =
      let rec ws i =
        if i < n && String.contains " \t\r\n" s.[i] then ws (i + 1) else i
      in
      ws i
    in
    if i >= n then List.rev acc
    else
      let j = prelude_end i in
      if j >= n then List.rev acc
      else if s.[j] = ';' then
        go (j + 1) (`Stmt (squash (String.sub s i (j - i))) :: acc)
      else
        let k = block_end (j + 1) 1 in
        go k
          (`Block
             (squash (String.sub s i (j - i)), String.sub s (j + 1) (k - j - 2))
          :: acc)
  in
  go 0 []

let import_target stmt =
  let prefix = "@import url(\"" in
  let pl = String.length prefix in
  if String.length stmt > pl && String.sub stmt 0 pl = prefix then
    match String.index_from_opt stmt pl '"' with
    | Some j -> Some (String.sub stmt pl (j - pl))
    | None -> None
  else None

(* The partials in cascade order, as paths relative to the repository. *)
let partials =
  let dir = Filename.dirname entrypoint in
  List.filter_map
    (function
      | `Stmt st ->
          Option.map (fun rel -> Filename.concat dir rel) (import_target st)
      | `Block _ -> None)
    (items (strip_comments (Source_census.read entrypoint)))

(* Entrypoint and partials, comments included, in cascade order. *)
let stylesheet =
  String.concat "\n"
    (Source_census.read entrypoint :: List.map Source_census.read partials)

type rule = {
  context : string list;  (** enclosing at-rule preludes, outermost first *)
  selectors : string list;
      (** complex selectors, a leading [:is(...)] expanded into one selector per
          alternative *)
  body : string;  (** declarations, whitespace squashed *)
}

let split_top_level_commas s =
  let n = String.length s in
  let rec go i depth start acc =
    if i >= n then List.rev (String.sub s start (n - start) :: acc)
    else
      match s.[i] with
      | '(' | '[' -> go (i + 1) (depth + 1) start acc
      | ')' | ']' -> go (i + 1) (depth - 1) start acc
      | ',' when depth = 0 ->
          go (i + 1) depth (i + 1) (String.sub s start (i - start) :: acc)
      | _ -> go (i + 1) depth start acc
  in
  go 0 0 0 [] |> List.map String.trim |> List.filter (( <> ) "")

(* ":is(a, b) rest" matches exactly what "a rest" and "b rest" match. *)
let expand selector =
  let is_ = ":is(" in
  let il = String.length is_ in
  if String.length selector > il && String.sub selector 0 il = is_ then
    let n = String.length selector in
    let rec close i depth =
      if i >= n then None
      else
        match selector.[i] with
        | '(' -> close (i + 1) (depth + 1)
        | ')' -> if depth = 1 then Some i else close (i + 1) (depth - 1)
        | _ -> close (i + 1) depth
    in
    match close il 1 with
    | Some j ->
        let rest = String.sub selector (j + 1) (n - j - 1) in
        List.map
          (fun alt -> alt ^ rest)
          (split_top_level_commas (String.sub selector il (j - il)))
    | None -> [ selector ]
  else [ selector ]

let rules =
  let rec of_items context acc = function
    | [] -> acc
    | `Stmt _ :: rest -> of_items context acc rest
    | `Block (prelude, body) :: rest ->
        let acc =
          if
            String.length prelude > 0
            && prelude.[0] = '@'
            && not
                 (String.length prelude >= 10
                 && String.sub prelude 0 10 = "@keyframes")
          then of_items (context @ [ prelude ]) acc (items body)
          else
            {
              context;
              selectors =
                List.concat_map expand (split_top_level_commas prelude);
              body = squash body;
            }
            :: acc
        in
        of_items context acc rest
  in
  List.concat_map
    (fun path ->
      List.rev
        (of_items [] [] (items (strip_comments (Source_census.read path)))))
    partials

(* Every expanded selector of every rule, in cascade order. *)
let selectors = List.concat_map (fun r -> r.selectors) rules
let has_selector s = List.mem s selectors

let has_selector_prefix prefix =
  let pl = String.length prefix in
  List.exists
    (fun s -> String.length s >= pl && String.sub s 0 pl = prefix)
    selectors
