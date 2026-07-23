(* GitHub App user-installations lookup. Confirms that a pending
   installation ID is visible to the user access token produced by the OAuth
   exchange, by paging through GET /user/installations at a fixed endpoint.
   The transport is injected, so callers can neither steer the
   credential-bearing request nor observe more than the closed error type.
   Every error constructor is payload-free except the HTTP status integer,
   and nothing in this module logs — see the .mli for the privacy
   contract. *)

type verified_installation = { id : int64 }

let installation_id verified = verified.id

module type TRANSPORT = sig
  val get :
    uri:Uri.t ->
    headers:(string * string) list ->
    (int * string, unit) result Lwt.t
end

module Cohttp_transport : TRANSPORT = struct
  (* One budget covers connect, request, and body read: a listing that
     cannot finish quickly is treated as failed rather than holding a
     worker. *)
  let timeout_seconds = 10.0

  (* A full 100-entry installations page is tens of kilobytes; anything
     approaching this cap is not an installations page and reading it
     further only buys an adversary memory. *)
  let max_body_bytes = 2_097_152

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

  let get ~uri ~headers =
    (* Cohttp's plain client performs exactly the one request — it has no
       redirect following to disable. *)
    let request () =
      let headers = Cohttp.Header.of_list headers in
      let%lwt response, response_body =
        Cohttp_lwt_unix.Client.get ~headers uri
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
  | Invalid_installation_id
  | Transport_error
  | Unexpected_http_status of int
  | Invalid_response
  | Installation_not_accessible
  | Pagination_limit

let per_page = 100

(* Five pages of one hundred: enough for any plausible user, and a hard
   stop so a hostile total_count cannot drive unbounded requests. *)
let max_pages = 5

(* Fixed constants: deriving the API endpoint from configuration or input
   would let a misconfiguration or an attacker aim the user token at an
   arbitrary host. The requested installation ID is deliberately absent
   from the URI — it is verified by searching the returned list. *)
let page_uri page =
  Uri.make ~scheme:"https" ~host:"api.github.com" ~path:"/user/installations"
    ~query:
      [ ("per_page", [ string_of_int per_page ]); ("page", [ string_of_int page ]) ]
    ()

(* The token rides only in the Authorization header, never in the URI. *)
let request_headers token_set =
  [
    ("accept", "application/vnd.github+json");
    ( "authorization",
      "Bearer " ^ Github_oauth_token_exchange.access_token token_set );
    ("x-github-api-version", "2026-03-10");
    ("user-agent", "Earde-GitHub-Onboarding");
  ]

let recognized_keys = [ "total_count"; "installations" ]

let occurrences fields key =
  List.length (List.filter (fun (k, _) -> String.equal k key) fields)

(* Exact-value int64 only: floats, numeric strings, null, booleans, and
   literals outside the int64 range are all rejected. `Intlit` carries
   integers Yojson could not fit in an OCaml int, whose exactness
   Int64.of_string_opt then decides. *)
let int64_of_json = function
  | `Int n -> Some (Int64.of_int n)
  | `Intlit literal -> Int64.of_string_opt literal
  | _ -> None

(* Exactly one positive-int64 "id" field; every other installation field
   (account, permissions, repository selection, URLs) is ignored and
   dropped, never retained. *)
let entry_id = function
  | `Assoc fields -> (
      match List.filter (fun (k, _) -> String.equal k "id") fields with
      | [ (_, id_json) ] -> (
          match int64_of_json id_json with
          | Some id when Int64.compare id 0L > 0 -> Some id
          | _ -> None)
      | _ -> None)
  | _ -> None

(* All entries validate or the page is rejected whole — a malformed entry
   anywhere poisons the page even when another entry already matched, so a
   match can never be returned from a page that was not fully understood. *)
let rec entry_ids validated = function
  | [] -> Some (List.rev validated)
  | entry :: rest -> (
      match entry_id entry with
      | Some id when not (List.exists (Int64.equal id) validated) ->
          entry_ids (id :: validated) rest
      | _ -> None)

let parse_page body =
  match Yojson.Safe.from_string body with
  | exception _ -> Error Invalid_response
  | `Assoc fields -> (
      (* Yojson preserves duplicate keys in `Assoc`; a duplicated recognized
         key makes the response ambiguous, so it is rejected before any
         field is interpreted. Unknown top-level keys stay ignored. *)
      if List.exists (fun key -> occurrences fields key > 1) recognized_keys
      then Error Invalid_response
      else
        match
          ( List.assoc_opt "total_count" fields,
            List.assoc_opt "installations" fields )
        with
        | Some total_json, Some (`List entries)
          when List.length entries <= per_page -> (
            match (int64_of_json total_json, entry_ids [] entries) with
            | Some total_count, Some ids
              when Int64.compare total_count 0L >= 0 ->
                Ok (total_count, ids)
            | _ -> Error Invalid_response)
        | _ -> Error Invalid_response)
  | _ -> Error Invalid_response

let verify ~transport:(module Transport : TRANSPORT) ~token_set
    ~installation_id =
  if Int64.compare installation_id 0L <= 0 then
    (* Rejected before any request: an invalid ID must not cost a network
       round-trip carrying the token. *)
    Lwt.return (Error Invalid_installation_id)
  else
    let headers = request_headers token_set in
    let rec fetch page =
      let%lwt result = Transport.get ~uri:(page_uri page) ~headers in
      match result with
      | Error () -> Lwt.return (Error Transport_error)
      | Ok (200, body) -> (
          match parse_page body with
          | Error error -> Lwt.return (Error error)
          | Ok (total_count, ids) ->
              if List.exists (Int64.equal installation_id) ids then
                Lwt.return (Ok { id = installation_id })
              else if List.length ids < per_page then
                (* A short page is the definitive end of the list. *)
                Lwt.return (Error Installation_not_accessible)
              else if
                Int64.compare
                  (Int64.mul (Int64.of_int page) (Int64.of_int per_page))
                  total_count
                >= 0
              then Lwt.return (Error Installation_not_accessible)
              else if page < max_pages then fetch (page + 1)
              else
                (* The search stopped because of our own page cap, not
                   because the list ended — saying "not accessible" here
                   would misreport an incomplete search as definitive. *)
                Lwt.return (Error Pagination_limit))
      | Ok (status, _) ->
          (* The body of a non-200 answer is never parsed or preserved; the
             status integer is the only remote datum an error may carry. *)
          Lwt.return (Error (Unexpected_http_status status))
    in
    fetch 1
