(* See image_upload.mli. Pure policy only: the accept decision and the argv
   construction, so both are unit-testable without ImageMagick present and
   without a hostile payload ever reaching a decoder. *)

type purpose =
  | Profile_avatar
  | Community_avatar
  | Community_banner
  | Post_image

type format =
  | Jpeg
  | Png
  | Gif
  | Webp

let max_bytes = 5 * 1024 * 1024

(* Kept byte-identical to the message the previous pipeline produced. *)
let too_large_message =
  Printf.sprintf "Image exceeds the %d MB limit." (max_bytes / (1024 * 1024))

(* One message for every refusal reason. "Not an image", "unsupported
   format" and "the decoder failed" are indistinguishable to the uploader, so
   a payload can never be used to probe what the pipeline recognises. *)
let rejected_message =
  "Image processing failed. Please upload a valid image (JPEG, PNG, GIF, \
   WebP)."

let rate_limited_message =
  "Too many image uploads. Please wait a moment and try again."

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

(* The accept decision is made from the payload's own bytes. The multipart
   Content-Type and the submitted filename are attacker-controlled and are
   never consulted — the previous pipeline consulted neither either, but it
   also checked nothing at all and handed raw bytes to a content-sniffing
   ImageMagick. *)
let detect_format bytes =
  if starts_with ~prefix:"\xff\xd8\xff" bytes then Some Jpeg
  else if starts_with ~prefix:"\x89PNG\r\n\x1a\n" bytes then Some Png
  else if starts_with ~prefix:"GIF87a" bytes || starts_with ~prefix:"GIF89a" bytes
  then Some Gif
  else if
    (* RIFF<4-byte little-endian length>WEBP *)
    String.length bytes >= 12
    && starts_with ~prefix:"RIFF" bytes
    && String.sub bytes 8 4 = "WEBP"
  then Some Webp
  else None

(* Pinning the decoder to the format the magic bytes proved is the control
   that keeps a crafted payload out of the URL:/MSL:/MSVG:/EPHEMERAL:/PS:/PDF:
   coders — ImageMagick picks a coder by sniffing content unless the path is
   explicitly qualified. *)
let coder = function
  | Jpeg -> "JPEG:"
  | Png -> "PNG:"
  | Gif -> "GIF:"
  | Webp -> "WEBP:"

let resize_geometry = function
  | Profile_avatar -> "512x512>"
  | Community_avatar -> "512x512>"
  | Community_banner -> "1920x480>"
  | Post_image -> "1920x1080>"

(* Sized for the 5 MiB byte cap. area/width/height bound the DECODED image, so
   a small payload that expands into an enormous canvas (the decompression-bomb
   shape) is refused by ImageMagick instead of being materialised; memory/map/
   disk bound what one conversion may consume; time bounds a decoder that makes
   no progress. These are per-invocation and cannot be widened by a payload —
   but they are not a substitute for the deployment's policy.xml, which is the
   only place the delegate coders themselves can be turned off. *)
let resource_limits =
  [ "-limit"; "memory"; "256MiB";
    "-limit"; "map"; "512MiB";
    "-limit"; "disk"; "1GiB";
    "-limit"; "area"; "64MP";
    "-limit"; "width"; "16KP";
    "-limit"; "height"; "16KP";
    "-limit"; "time"; "20";
    "-limit"; "thread"; "2" ]

(* An argument VECTOR, not a command string: Lwt_process passes it to execvp
   directly, so no shell exists to interpret a quote, a semicolon, or a
   backtick in any component. -strip drops EXIF (which carries GPS and device
   identifiers on phone photos); -auto-orient applies the rotation flag before
   the strip removes it; [0] takes the first frame so an animated GIF or
   multi-frame payload cannot fan out into a directory of files the way the
   previous mogrify invocation did. *)
let convert_argv ~binary ~format ~purpose ~input ~output =
  Array.of_list
    (((binary :: resource_limits)
     @ [ coder format ^ input ^ "[0]";
         "-auto-orient";
         "-strip";
         "-resize";
         resize_geometry purpose;
         "-quality";
         "80";
         "webp:" ^ output ]))
