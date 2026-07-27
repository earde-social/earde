(* GitHub App OAuth code-for-token exchange. The endpoint is a fixed
   constant and the transport is injected, so callers can neither steer the
   credential-bearing request nor observe more than the closed error type.
   Every error constructor is payload-free except the HTTP status integer,
   and nothing in this module logs — see the .mli for the privacy
   contract. *)

(* Shared with codes and both tokens: GitHub credentials are opaque, so no
   prefix, length or alphabet is imposed, but whitespace and control bytes
   (including NUL and DEL) can only be transport or paste damage. Damaged
   values are rejected, never trimmed or repaired. *)
let is_opaque_credential value =
  value <> "" && not (String.exists (fun c -> c <= ' ' || c = '\x7f') value)

type authorization_code = string

type code_error = Invalid_code

let authorization_code_of_callback raw =
  if is_opaque_credential raw then Ok raw else Error Invalid_code

type token_set = {
  access_token : string;
  expires_in : int option;
  refresh_token : string option;
  refresh_token_expires_in : int option;
}

let access_token t = t.access_token
let expires_in t = t.expires_in
let refresh_token t = t.refresh_token
let refresh_token_expires_in t = t.refresh_token_expires_in

module type TRANSPORT = sig
  val post :
    uri:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (int * string, unit) result Lwt.t
end

module Cohttp_transport : TRANSPORT = struct
  (* One budget covers connect, request, and body read: a token exchange
     that cannot finish quickly is treated as failed rather than holding a
     worker. *)
  let timeout_seconds = 10.0

  (* A real token response is a few hundred bytes; anything approaching this
     cap is not a token response and reading it further only buys an
     adversary memory. *)
  let max_body_bytes = 65_536

  exception Body_too_large

  let read_bounded body =
    let stream = Cohttp_lwt.Body.to_stream body in
    let buffer = Buffer.create 1024 in
    let%lwt () =
      Lwt_stream.iter_s
        (fun chunk ->
          if Buffer.length buffer + String.length chunk > max_body_bytes then
            raise Body_too_large
          else (
            Buffer.add_string buffer chunk;
            Lwt.return_unit))
        stream
    in
    Lwt.return (Buffer.contents buffer)

  let post ~uri ~headers ~body =
    (* Cohttp's plain client performs exactly the one request — it has no
       redirect following to disable. *)
    let request () =
      let headers = Cohttp.Header.of_list headers in
      let%lwt response, response_body =
        Cohttp_lwt_unix.Client.post ~headers
          ~body:(Cohttp_lwt.Body.of_string body)
          uri
      in
      let status =
        Cohttp.Code.code_of_status (Cohttp.Response.status response)
      in
      let%lwt raw = read_bounded response_body in
      Lwt.return (Ok (status, raw))
    in
    let timeout () =
      let%lwt () = Lwt_unix.sleep timeout_seconds in
      Lwt.return (Error ())
    in
    (* The exception itself must not escape (it may carry remote detail) and
       must not be stringified or logged; only cancellation keeps its
       meaning and propagates. *)
    Lwt.catch
      (fun () -> Lwt.pick [ request (); timeout () ])
      (function
        | Lwt.Canceled -> Lwt.reraise Lwt.Canceled
        | _ -> Lwt.return (Error ()))
end

type error =
  | Transport_error
  | Unexpected_http_status of int
  | OAuth_rejected
  | Invalid_response

(* Fixed constant: deriving the token endpoint from configuration or input
   would let a misconfiguration or an attacker aim the client secret at an
   arbitrary host. *)
let endpoint =
  Uri.make ~scheme:"https" ~host:"github.com"
    ~path:"/login/oauth/access_token" ()

let request_headers =
  [
    ("accept", "application/json");
    ("content-type", "application/x-www-form-urlencoded");
  ]

let recognized_keys =
  [
    "access_token";
    "token_type";
    "scope";
    "expires_in";
    "refresh_token";
    "refresh_token_expires_in";
    "error";
  ]

(* Presence of any of these alongside "error" is contradictory, and a
   contradictory response is rejected outright rather than interpreted. *)
let success_keys =
  [
    "access_token";
    "token_type";
    "scope";
    "expires_in";
    "refresh_token";
    "refresh_token_expires_in";
  ]

let occurrences fields key =
  List.length (List.filter (fun (k, _) -> String.equal k key) fields)

(* Positive OCaml int only: floats, numeric strings, null, and integers
   Yojson could not represent as an int (`Intlit`) are all rejected. *)
let positive_seconds = function
  | `Int n when n > 0 -> Some n
  | _ -> None

let parse_success fields =
  match
    ( List.assoc_opt "access_token" fields,
      List.assoc_opt "token_type" fields,
      List.assoc_opt "scope" fields )
  with
  | Some (`String token), Some (`String "bearer"), Some (`String "")
    when is_opaque_credential token -> (
      (* The three expiration-related fields travel together: all absent is
         the non-expiring App configuration, all present the expiring one.
         A partial triplet means someone's model of the response is wrong,
         so it is rejected rather than half-trusted. *)
      match
        ( List.assoc_opt "expires_in" fields,
          List.assoc_opt "refresh_token" fields,
          List.assoc_opt "refresh_token_expires_in" fields )
      with
      | None, None, None ->
          Ok
            {
              access_token = token;
              expires_in = None;
              refresh_token = None;
              refresh_token_expires_in = None;
            }
      | Some expires, Some (`String refresh), Some refresh_expires -> (
          match (positive_seconds expires, positive_seconds refresh_expires) with
          | Some expires_in, Some refresh_token_expires_in
            when is_opaque_credential refresh ->
              Ok
                {
                  access_token = token;
                  expires_in = Some expires_in;
                  refresh_token = Some refresh;
                  refresh_token_expires_in = Some refresh_token_expires_in;
                }
          | _ -> Error Invalid_response)
      | _ -> Error Invalid_response)
  | _ -> Error Invalid_response

let parse_body body =
  match Yojson.Safe.from_string body with
  | exception _ -> Error Invalid_response
  | `Assoc fields ->
      (* Yojson preserves duplicate keys in `Assoc`; a duplicated
         recognized key makes the response ambiguous, so it is rejected
         before any field is interpreted. Unknown keys stay ignored. *)
      if List.exists (fun key -> occurrences fields key > 1) recognized_keys
      then Error Invalid_response
      else (
        match List.assoc_opt "error" fields with
        | Some error_value -> (
            let contradicted =
              List.exists (fun key -> List.mem_assoc key fields) success_keys
            in
            (* The remote error string is checked for shape and then
               dropped: every well-formed rejection collapses to the same
               payload-free constructor. *)
            match error_value with
            | `String reason
              when String.trim reason <> "" && not contradicted ->
                Error OAuth_rejected
            | _ -> Error Invalid_response)
        | None -> parse_success fields)
  | _ -> Error Invalid_response

let exchange ~transport:(module Transport : TRANSPORT) ~config ~credentials
    ~code ~verifier =
  (* All credential material rides only in the form body; the Uri library's
     encoder owns the escaping so reserved bytes round-trip exactly. *)
  let body =
    Uri.encoded_of_query
      [
        ("client_id", [ Github_app_config.client_id config ]);
        ( "client_secret",
          [ Github_oauth_credentials.client_secret credentials ] );
        ("code", [ code ]);
        ("redirect_uri", [ Github_app_config.callback_url config ]);
        ( "code_verifier",
          [ Github_onboarding_pkce.verifier_to_string verifier ] );
      ]
  in
  let%lwt result =
    Transport.post ~uri:endpoint ~headers:request_headers ~body
  in
  match result with
  | Error () -> Lwt.return (Error Transport_error)
  | Ok (200, body) -> Lwt.return (parse_body body)
  | Ok (status, _) ->
      (* The body of a non-200 answer is never parsed or preserved; the
         status integer is the only remote datum an error may carry. *)
      Lwt.return (Error (Unexpected_http_status status))
