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
