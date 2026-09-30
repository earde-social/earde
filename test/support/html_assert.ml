(* Substring and HTML-fragment assertions over rendered markup. Absence
   checks on whole pages should drop the framework CSRF inputs first
   ([without_csrf_inputs]): their random token bytes can spell any short
   literal. *)

let contains haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  if nl = 0 then true
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let index_of haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  let rec loop i =
    if nl = 0 || i > hl - nl then None
    else if String.sub haystack i nl = needle then Some i
    else loop (i + 1)
  in
  loop 0

(* Extracts the single-quoted value of [name='value'] from rendered HTML. *)
let attr_value html name =
  match index_of html (name ^ "='") with
  | None -> None
  | Some i -> (
      let start = i + String.length name + 2 in
      match String.index_from_opt html start '\'' with
      | None -> None
      | Some j -> Some (String.sub html start (j - start)))

(* Counts occurrences of a substring — used to prove no unexpected
   data-analytics-* attribute sneaks in. *)
let count_sub haystack needle =
  let nl = String.length needle in
  let rec loop i acc =
    match
      index_of (String.sub haystack i (String.length haystack - i)) needle
    with
    | None -> acc
    | Some j -> loop (i + j + nl) (acc + 1)
  in
  if nl = 0 then 0 else loop 0 0

let contains_nonempty ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n > 0 && at 0

let occurs ~needle hay =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let index_from haystack needle from =
  let hl = String.length haystack and nl = String.length needle in
  let rec go i =
    if i + nl > hl then None
    else if String.sub haystack i nl = needle then Some i
    else go (i + 1)
  in
  go from

let occurrences haystack needle =
  let nl = String.length needle in
  let rec go from acc =
    match index_from haystack needle from with
    | None -> acc
    | Some i -> go (i + nl) (acc + 1)
  in
  go 0 0

let panel_fragment html =
  (* The feature panel: the degraded (chrome-free) documents keep the legacy
     create-shell marker; the launch documents now render the panel inside
     the shared settings shell's cm-main column. Either way the fragment is
     the panel content, never the chrome. *)
  let start =
    match index_from html "<div class='create-shell'>" 0 with
    | Some s -> Some s
    | None -> index_from html "<div class='cm-main'>" 0
  in
  match start with
  | None -> Alcotest.fail "create shell missing from page"
  | Some s -> (
      match index_from html "</main>" s with
      | None -> Alcotest.fail "unterminated main element"
      | Some e -> String.sub html s (e - s))

let must frag s =
  Alcotest.(check bool) ("contains: " ^ s) true (contains frag s)

let must_not frag s =
  Alcotest.(check bool) ("must not contain: " ^ s) false (contains frag s)

let order frag first second =
  match (index_from frag first 0, index_from frag second 0) with
  | Some i, Some j ->
      Alcotest.(check bool)
        (Printf.sprintf "'%s' before '%s'" first second)
        true (i < j)
  | _ -> Alcotest.fail "expected both order markers present"

let csrf_input_prefix = "<input name=\"dream.csrf\" type=\"hidden\" value=\""

(* Slices the first [<form ...>...</form>] region out of rendered HTML. *)
let form_region html =
  match index_from html "<form" 0 with
  | None -> Alcotest.fail "no form in the rendered markup"
  | Some s -> (
      match index_from html "</form>" s with
      | None -> Alcotest.fail "unterminated form element"
      | Some e -> String.sub html s (e - s))

(* Every [<input ...>] tag of a region, as raw tag text, in source order. *)
let input_tags region =
  let rec go from acc =
    match index_from region "<input" from with
    | None -> List.rev acc
    | Some s -> (
        match String.index_from_opt region s '>' with
        | None -> Alcotest.fail "unterminated input element"
        | Some e -> go (e + 1) (String.sub region s (e + 1 - s) :: acc))
  in
  go 0 []

(* True when [tag] is exactly the framework CSRF field: the framework
   prefix, then one opaque value that is the tag's last attribute. The
   value is bounded but never read, so an extra attribute smuggled in
   after it fails the check. *)
let is_csrf_input tag =
  let pl = String.length csrf_input_prefix and tl = String.length tag in
  tl >= pl + 2
  && String.sub tag 0 pl = csrf_input_prefix
  && String.sub tag (tl - 2) 2 = "\">"
  && not (String.contains (String.sub tag pl (tl - pl - 2)) '"')

(* Rendered HTML with every framework CSRF field dropped, so a whole-page
   absence assertion cannot trip over random token bytes. *)
let without_csrf_inputs html =
  let len = String.length html in
  let buf = Buffer.create len in
  let rec go from =
    match index_from html csrf_input_prefix from with
    | None -> Buffer.add_string buf (String.sub html from (len - from))
    | Some s -> (
        Buffer.add_string buf (String.sub html from (s - from));
        match String.index_from_opt html s '>' with
        | None -> Alcotest.fail "unterminated CSRF input element"
        | Some e -> go (e + 1))
  in
  go 0;
  Buffer.contents buf
