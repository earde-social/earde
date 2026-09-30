(* === IMAGE UPLOADS ===

   The pipeline's policy — which payloads are accepted and the exact argument
   vector ImageMagick runs with — lives in the pure [Image_upload] module.
   What is left here is the IO: the rate-limit check, writing the temporary
   file, running the process off the event loop, and moving the result into
   public storage.

   Callers MUST perform authentication, ban and resource-authorization checks
   before calling this. Nothing below re-derives who may upload; it only
   refuses work that is already authorized but abusive or malformed. *)

(* ImageMagick 7 installs `magick` and (usually) a `convert` compatibility
   alias; ImageMagick 6 installs `convert` only. Resolved once per process
   rather than per request, and deliberately NOT configurable from the
   environment — the binary that decodes hostile bytes is not something a
   request or a stray env var should be able to redirect. *)
let imagemagick_binary =
  lazy
    (let exists name =
       Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote name)) = 0
     in
     if exists "magick" then "magick" else "convert")

(* Bounds how many conversions may run at once. Without this, the move to a
   non-blocking process would replace "one upload stalls everyone" with
   "N concurrent uploads fork N ImageMagick processes", which is a worse
   failure mode on a single small instance. Requests beyond the bound wait
   for a slot rather than being refused. *)
let image_workers = 2

let image_worker_pool =
  lazy (Lwt_pool.create image_workers (fun () -> Lwt.return_unit))

(* Wall-clock ceiling on one conversion, independent of ImageMagick's own
   `-limit time`: that limit governs decode work, and cannot end a process
   wedged on IO. Comfortably above the limit so the in-process one is what
   normally fires. *)
let image_convert_timeout_seconds = 30.0

let run_image_convert argv =
  Lwt_pool.use (Lazy.force image_worker_pool) (fun () ->
      Lwt.catch
        (fun () ->
          let process = Lwt_process.open_process_none ("", argv) in
          (* Lwt.protected keeps the losing branch of the pick from
             cancelling the status promise the terminate path still needs. *)
          let status = Lwt.protected process#status in
          let%lwt outcome =
            Lwt.pick
              [ (let%lwt s = status in
                 Lwt.return (`Exited s));
                (let%lwt () = Lwt_unix.sleep image_convert_timeout_seconds in
                 Lwt.return `Timeout) ]
          in
          match outcome with
          | `Exited (Unix.WEXITED 0) -> Lwt.return true
          | `Exited _ -> Lwt.return false
          | `Timeout ->
              process#terminate;
              let%lwt _ = process#close in
              Lwt.return false)
        (fun _ -> Lwt.return false))

(* Returns Ok None when no file was submitted (the field is present but
   empty on every one of these forms). Every failure path removes both
   temporary files, so a refused or crashed conversion leaves nothing
   behind and nothing ever reaches static/uploads. *)
let process_image_upload ~db ~ip ~purpose image_bytes =
  if image_bytes = "" then Lwt.return (Ok None)
  else if String.length image_bytes > Image_upload.max_bytes then
    Lwt.return (Error Image_upload.too_large_message)
  else
    match Image_upload.detect_format image_bytes with
    | None ->
        (* Refused on the payload's own leading bytes, before a temporary
           file exists and before any decoder is invoked. *)
        Lwt.return (Error Image_upload.rejected_message)
    | Some format -> (
        match%lwt Rate_limit_store.check_upload db ip with
        | Ok `Blocked -> Lwt.return (Error Image_upload.rate_limited_message)
        | Ok `Allowed | Error _ ->
            (* Deliberately fail-open on a storage error, unlike the
               request-path middleware (which refuses): this budget only
               meters image processing for an already-authenticated member,
               and a database problem must not make uploads impossible. *)
            let base =
              Avatar_uploads.fresh_basename
                ~now_ms:(Int64.of_float (Unix.gettimeofday () *. 1000.0))
                ~random:Dream.random
            in
            let tmp_path = Filename.concat (Filename.get_temp_dir_name ()) (base ^ ".tmp") in
            let webp_path = Filename.concat (Filename.get_temp_dir_name ()) (base ^ ".webp") in
            let dest_name = base ^ ".webp" in
            let dest_path = "static/uploads/" ^ dest_name in
            let url_path = "/static/uploads/" ^ dest_name in
            let cleanup () =
              (try Sys.remove tmp_path with _ -> ());
              (try Sys.remove webp_path with _ -> ())
            in
            Lwt.catch
              (fun () ->
                (* Exclusive create: never write through a file or link
                   that already exists in the shared temp directory. *)
                let oc =
                  open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] 0o600 tmp_path
                in
                output_string oc image_bytes;
                close_out oc;
                let argv =
                  Image_upload.convert_argv
                    ~binary:(Lazy.force imagemagick_binary)
                    ~format ~purpose ~input:tmp_path ~output:webp_path
                in
                let%lwt converted = run_image_convert argv in
                (try Sys.remove tmp_path with _ -> ());
                if not converted then begin
                  cleanup ();
                  Lwt.return (Error Image_upload.rejected_message)
                end
                else
                  (* Rename rather than shelling out to mv. Falls back to a
                     copy when /tmp and static/uploads are on different
                     filesystems, which Sys.rename cannot cross. *)
                  match
                    (try
                       Sys.rename webp_path dest_path;
                       `Ok
                     with _ -> (
                       try
                         let ic = open_in_bin webp_path in
                         let len = in_channel_length ic in
                         let data = really_input_string ic len in
                         close_in ic;
                         let oc = open_out_bin dest_path in
                         output_string oc data;
                         close_out oc;
                         (try Sys.remove webp_path with _ -> ());
                         `Ok
                       with _ -> `Failed))
                  with
                  | `Ok -> Lwt.return (Ok (Some url_path))
                  | `Failed ->
                      cleanup ();
                      Lwt.return (Error "Failed to store the processed image."))
              (fun _ ->
                cleanup ();
                (* The exception text is not reflected: it can name host
                   paths and errno detail the uploader has no business
                   seeing. *)
                Lwt.return (Error Image_upload.rejected_message)))
