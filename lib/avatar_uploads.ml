(* See avatar_uploads.mli. The validation is deliberately shape-exact rather
   than merely prefix-based: proving the basename is one the upload pipeline
   could have minted (earde_<digits>_<digits>.webp) is what makes deletion
   safe — no separator, no dot outside the fixed suffix, and no traversal can
   survive the digits-and-underscore alphabet, and bundled assets live under
   a different directory entirely. A stored value that fails the shape check
   is simply not ours to delete. *)

let url_prefix = "/static/uploads/"
let uploads_dir = "static/uploads"
let base_prefix = "earde_"
let base_suffix = ".webp"

(* <digits>_<digits>: at least one digit, exactly one underscore separator. *)
let is_pipeline_middle middle =
  let n = String.length middle in
  n >= 3
  && String.for_all (fun c -> (c >= '0' && c <= '9') || c = '_') middle
  && middle.[0] <> '_'
  && middle.[n - 1] <> '_'
  && String.fold_left (fun acc c -> if c = '_' then acc + 1 else acc) 0 middle
     = 1

(* Uploads are served statically with no access check, and a post image may
   belong to a private community, so the name is the only thing keeping the
   file private: it must be unguessable. The digits come from the
   cryptographic generator (the stdlib Random is not seeded here and repeats
   the same sequence after every restart). Rejection sampling (bytes below
   250) keeps each digit uniform. 32 digits carry about 106 bits. *)
let random_digit_count = 32

let random_digits ~random n =
  let buf = Buffer.create n in
  let rec fill () =
    if Buffer.length buf < n then begin
      String.iter
        (fun c ->
          let b = Char.code c in
          if b < 250 && Buffer.length buf < n then
            Buffer.add_char buf (Char.chr (Char.code '0' + (b mod 10))))
        (random (n + 8));
      fill ()
    end
  in
  fill ();
  Buffer.contents buf

let fresh_basename ~now_ms ~random =
  Printf.sprintf "%s%Ld_%s" base_prefix now_ms
    (random_digits ~random random_digit_count)

let is_pipeline_basename base =
  let pl = String.length base_prefix and sl = String.length base_suffix in
  let n = String.length base in
  n > pl + sl
  && String.sub base 0 pl = base_prefix
  && String.sub base (n - sl) sl = base_suffix
  && is_pipeline_middle (String.sub base pl (n - pl - sl))

let local_file_of_url url =
  let pl = String.length url_prefix in
  if String.length url > pl && String.sub url 0 pl = url_prefix then
    let base = String.sub url pl (String.length url - pl) in
    if is_pipeline_basename base then Some (Filename.concat uploads_dir base)
    else None
  else None

let remove_local_file path =
  try
    Sys.remove path;
    `Removed
  with Sys_error _ ->
    (* ENOENT and a real failure both raise Sys_error; only a file that
       verifiably still exists counts as a failure worth logging. *)
    if (try Sys.file_exists path with Sys_error _ -> false) then `Failed
    else `Absent

let cleanup_deleted_account_avatar avatar_url =
  match avatar_url with
  | None -> `Not_local
  | Some url -> (
      match local_file_of_url url with
      | None -> `Not_local
      | Some path -> remove_local_file path)
