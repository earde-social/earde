(* The production OCaml sources, read for census assertions. The set is
   discovered, so a new module cannot escape a census. *)

(* dune test runs in test/, dune exec from the project root. *)
let root = if Sys.file_exists "../bin/main.ml" then ".." else "."

let read path =
  let ic = open_in_bin (Filename.concat root path) in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Every production OCaml source, discovered rather than listed, so a new
   module cannot quietly escape the census. *)
let production_sources =
  let lib = Filename.concat root "lib" in
  let lib_files =
    Sys.readdir lib |> Array.to_list
    |> List.filter (fun f ->
        Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli")
    |> List.sort compare
    |> List.map (fun f -> ("lib/" ^ f, read ("lib/" ^ f)))
  in
  ("bin/main.ml", read "bin/main.ml") :: lib_files

let absent_everywhere label needle =
  List.iter
    (fun (path, body) ->
      if Html_assert.contains body needle then
        Alcotest.failf "%s: %s still contains %S" label path needle)
    production_sources

(* The implementations only: interfaces repeat names, and dune leaves a
   .pp.ml beside each preprocessed source, the same code twice. *)
let implementations =
  List.filter
    (fun (path, _) ->
      Filename.check_suffix path ".ml"
      && not (Filename.check_suffix path ".pp.ml"))
    production_sources

(* Occurrences of [needle] across every implementation, so a definition or
   call site counts wherever the module layout puts it. *)
let count_everywhere needle =
  List.fold_left
    (fun acc (_, body) -> acc + Html_assert.count_sub body needle)
    0 implementations

(* A backslash-newline inside a string literal continues it without the
   newline or the next line's leading blanks. Joining those continuations
   reads the markup as it renders, wherever the formatter breaks a line. *)
let join_continuations src =
  let b = Buffer.create (String.length src) in
  let n = String.length src in
  let rec go i =
    if i >= n then ()
    else if src.[i] = '\\' && i + 1 < n && src.[i + 1] = '\n' then (
      let j = ref (i + 2) in
      while !j < n && (src.[!j] = ' ' || src.[!j] = '\t') do
        incr j
      done;
      go !j)
    else (
      Buffer.add_char b src.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* The modules that build the launch documents: their top bars, shared
   scripts and badge calls, with literal continuations joined. *)
let launch_shells =
  join_continuations
    (String.concat "\n"
       [ read "lib/page_shell.ml"; read "lib/community_shell.ml" ])
