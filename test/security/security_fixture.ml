(* Upload files and uploads-directory helpers for the security cases. *)

let must label body needle =
  if not (Html_assert.contains body needle) then
    Alcotest.failf "%s: missing fragment %S" label needle

let must_not label body needle =
  if Html_assert.contains body needle then
    Alcotest.failf "%s: forbidden fragment %S" label needle

(* The uploads directory the handlers write to is resolved relative to the
   process CWD, which under dune is the test's own build directory — never
   the repository's static/uploads. *)
let uploads_dir = "static/uploads"

let ensure_uploads_dir () =
  let rec mk path =
    if not (Sys.file_exists path) then begin
      mk (Filename.dirname path);
      try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  mk uploads_dir

let uploads_listing () =
  if Sys.file_exists uploads_dir then
    Array.to_list (Sys.readdir uploads_dir) |> List.sort String.compare
  else []

let avatar_url_of name = "/static/uploads/" ^ name

let make_upload_file name =
  let path = Filename.concat uploads_dir name in
  let oc = open_out_bin path in
  output_string oc "not really a webp, but a real file";
  close_out oc;
  path

(* A genuine 1x1 PNG — the pipeline must be able to decode it, so it cannot
   be a placeholder. *)
let real_png =
  "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a\x00\x00\x00\x0d\x49\x48\x44\x52\x00\x00\
   \x00\x01\x00\x00\x00\x01\x08\x06\x00\x00\x00\x1f\x15\xc4\x89\x00\x00\x00\
   \x0d\x49\x44\x41\x54\x78\xda\x63\x64\x60\xf8\x5f\x0f\x00\x02\x87\x01\x80\
   \xeb\x47\xba\x92\x00\x00\x00\x00\x49\x45\x4e\x44\xae\x42\x60\x82"
