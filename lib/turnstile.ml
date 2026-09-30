(* Cloudflare Turnstile verification for public signup. Modeled on email.ml's
   Cohttp/Yojson HTTP pattern. The design contract is FAIL CLOSED: the only path
   that lets signup proceed is an affirmative "success": true from Cloudflare.
   Anything else — no keys, no token, timeout, transport error, non-2xx, garbage
   body — yields false so the handler creates no pending row and sends no email. *)

let siteverify_url = "https://challenges.cloudflare.com/turnstile/v0/siteverify"

(* A signup request must not hang a worker waiting on Cloudflare. If verification
   does not return within this budget we fail closed (treat the token as invalid). *)
let verify_timeout_seconds = 5.0

(* Trim and treat empty-after-trim as absent, so a key set to "" or "  " (a common
   deploy/.env mishap) does NOT count as configured. *)
let trimmed_env name =
  match Sys.getenv_opt name with
  | Some v ->
      let v = String.trim v in
      if v = "" then None else Some v
  | None -> None

let site_key () = trimmed_env "TURNSTILE_SITE_KEY"
let secret_key () = trimmed_env "TURNSTILE_SECRET_KEY"

(* Same truthy vocabulary as EARDE_SIGNUPS_ENABLED, for operator consistency. *)
let required () =
  match Sys.getenv_opt "EARDE_TURNSTILE_REQUIRED" with
  | Some v -> (
      match String.lowercase_ascii (String.trim v) with
      | "1" | "true" | "yes" | "on" -> true
      | _ -> false)
  | None -> false

type status = Configured of string | Disabled | Misconfigured

let status () =
  match (site_key (), secret_key ()) with
  | Some sk, Some _ -> Configured sk
  (* Required but a key is missing: do not silently fall back to an unprotected
     form — signal Misconfigured so the handler fails closed. *)
  | _ -> if required () then Misconfigured else Disabled

(* Pure: true iff Cloudflare's JSON reports success. Separated from IO so it is
   unit-testable offline. Any non-object body, missing/false "success", or parse
   error is false. *)
let parse_siteverify body =
  match Yojson.Safe.from_string body with
  | `Assoc fields -> (
      match List.assoc_opt "success" fields with
      | Some (`Bool b) -> b
      | _ -> false)
  | _ -> false
  | exception _ -> false

let verify ~response =
  match secret_key () with
  | None ->
      Lwt.return false (* Defensive: verify is only called when Configured. *)
  | Some secret ->
      (* x-www-form-urlencoded per the siteverify API. remoteip is intentionally
         omitted: behind Nginx, Dream.client is the proxy address, which would
         produce false negatives. Add it later via a trusted X-Forwarded-For
         helper in a dedicated infra/security branch. *)
      let body =
        Uri.encoded_of_query
          [ ("secret", [ secret ]); ("response", [ response ]) ]
      in
      let headers =
        Cohttp.Header.of_list
          [
            ("content-type", "application/x-www-form-urlencoded");
            ("accept", "application/json");
          ]
      in
      let do_request () =
        let%lwt resp, resp_body =
          Cohttp_lwt_unix.Client.post ~headers
            ~body:(Cohttp_lwt.Body.of_string body)
            (Uri.of_string siteverify_url)
        in
        let code =
          resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status
        in
        let%lwt raw = Cohttp_lwt.Body.to_string resp_body in
        if code >= 200 && code < 300 then Lwt.return (parse_siteverify raw)
        else begin
          (* The body may echo request fields; never log it (mirrors email.ml). *)
          Dream.log "Turnstile siteverify HTTP %d" code;
          Lwt.return false
        end
      in
      let timeout () =
        let%lwt () = Lwt_unix.sleep verify_timeout_seconds in
        Dream.log "Turnstile siteverify timed out after %.0fs"
          verify_timeout_seconds;
        Lwt.return false
      in
      Lwt.catch
        (fun () -> Lwt.pick [ do_request (); timeout () ])
        (fun exn ->
          Dream.log "Turnstile siteverify exception: %s"
            (Printexc.to_string exn);
          Lwt.return false)
