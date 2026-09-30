(* Avatar_uploads: the strict url→file mapping that makes account-deletion
   file cleanup safe. Only the exact pipeline shape
   /static/uploads/earde_<digits>_<digits>.webp maps to a local path;
   external URLs, bundled assets, traversal shapes and free-text values all
   map to None and never reach the filesystem. The removal test uses a
   system temp file (outside the repository), not a repo path. *)

let case name f = Alcotest.test_case name `Quick f

let accepts =
  case "pipeline-shaped urls map to their uploads file" (fun () ->
      Alcotest.(check (option string))
        "canonical shape"
        (Some "static/uploads/earde_1722779100123_042917.webp")
        (Earde.Avatar_uploads.local_file_of_url
           "/static/uploads/earde_1722779100123_042917.webp");
      Alcotest.(check (option string))
        "short digit runs still match the shape"
        (Some "static/uploads/earde_1_2.webp")
        (Earde.Avatar_uploads.local_file_of_url "/static/uploads/earde_1_2.webp"))

let rejects =
  case "everything else maps to None" (fun () ->
      let refuse label url =
        Alcotest.(check (option string))
          label None
          (Earde.Avatar_uploads.local_file_of_url url)
      in
      refuse "external absolute URL" "https://cdn.example.com/earde_1_2.webp";
      refuse "protocol-relative URL" "//evil.example/earde_1_2.webp";
      refuse "bundled static asset" "/static/images/logo-mark.svg";
      refuse "traversal into images" "/static/uploads/../images/logo-mark.svg";
      refuse "encoded traversal" "/static/uploads/..%2F..%2Fetc%2Fpasswd";
      refuse "nested separator" "/static/uploads/evil/earde_1_2.webp";
      refuse "empty basename" "/static/uploads/";
      refuse "bare prefix" "/static/uploads";
      refuse "wrong suffix" "/static/uploads/earde_1_2.webp.sh";
      refuse "wrong prefix" "/static/uploads/avatar_1_2.webp";
      refuse "letters in the middle" "/static/uploads/earde_1_x2.webp";
      refuse "missing separator" "/static/uploads/earde_12.webp";
      refuse "two separators" "/static/uploads/earde_1_2_3.webp";
      refuse "leading underscore" "/static/uploads/earde__2.webp";
      refuse "dotted middle" "/static/uploads/earde_1_2.2.webp";
      refuse "empty string" "";
      refuse "plain text" "not a url at all")

let cleanup_dispatch =
  case "cleanup composition: absent and non-local values touch nothing"
    (fun () ->
      let show = function
        | `Removed -> "removed"
        | `Absent -> "absent"
        | `Failed -> "failed"
        | `Not_local -> "not_local"
      in
      Alcotest.(check string)
        "no avatar" "not_local"
        (show (Earde.Avatar_uploads.cleanup_deleted_account_avatar None));
      Alcotest.(check string)
        "external avatar" "not_local"
        (show
           (Earde.Avatar_uploads.cleanup_deleted_account_avatar
              (Some "https://cdn.example.com/earde_1_2.webp")));
      Alcotest.(check string)
        "traversal avatar" "not_local"
        (show
           (Earde.Avatar_uploads.cleanup_deleted_account_avatar
              (Some "/static/uploads/../images/logo-mark.svg"))))

let removal =
  case "remove_local_file: unlink once, absent afterwards" (fun () ->
      let path = Filename.temp_file "earde_avatar_test" ".webp" in
      let show = function
        | `Removed -> "removed"
        | `Absent -> "absent"
        | `Failed -> "failed"
      in
      Alcotest.(check string)
        "existing file is removed" "removed"
        (show (Earde.Avatar_uploads.remove_local_file path));
      Alcotest.(check bool) "file is gone" false (Sys.file_exists path);
      Alcotest.(check string)
        "second attempt is absent, not a failure" "absent"
        (show (Earde.Avatar_uploads.remove_local_file path)))

(* Uploads are served with no access check, so a private community's post
   image is private only while its name is unguessable. The old pipeline
   took the name's random part from the unseeded stdlib Random, which
   repeats the same sequence after every restart: with the same Random
   state it minted the same suffix. *)
let fresh_names =
  case "fresh upload names are unguessable and keep the pipeline shape"
    (fun () ->
      let digits_of base =
        match String.split_on_char '_' base with
        | [ "earde"; _ms; d ] -> d
        | _ -> Alcotest.failf "unexpected shape %S" base
      in
      let mint () =
        Earde.Avatar_uploads.fresh_basename ~now_ms:1722779100123L
          ~random:Dream.random
      in
      Random.init 7;
      let a = mint () in
      Random.init 7;
      let b = mint () in
      Alcotest.(check bool)
        "independent of the stdlib Random state" true
        (digits_of a <> digits_of b);
      Alcotest.(check int) "32 random digits" 32 (String.length (digits_of a));
      Alcotest.(check bool)
        "digits only" true
        (String.for_all (fun c -> c >= '0' && c <= '9') (digits_of a));
      Alcotest.(check (option string))
        "the deletion validator accepts it"
        (Some ("static/uploads/" ^ a ^ ".webp"))
        (Earde.Avatar_uploads.local_file_of_url
           ("/static/uploads/" ^ a ^ ".webp"));
      (* Bytes 250..255 are skipped so every digit stays uniform. *)
      let feed =
        ref
          [
            String.make 40 '\255';
            String.init 40 (fun i -> Char.chr (i mod 250));
          ]
      in
      let scripted n =
        match !feed with
        | x :: rest ->
            feed := rest;
            String.sub x 0 (min n (String.length x))
        | [] -> String.make n '\000'
      in
      let c = Earde.Avatar_uploads.fresh_basename ~now_ms:5L ~random:scripted in
      Alcotest.(check string)
        "rejection sampling" "earde_5_01234567890123456789012345678901" c)

let suite = [ accepts; rejects; cleanup_dispatch; removal; fresh_names ]
let suites = [ ("avatar_upload_paths", suite) ]
