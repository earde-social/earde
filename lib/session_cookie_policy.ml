(* See session_cookie_policy.mli for why this is a response filter rather
   than a configuration: Dream's session back end emits its cookie with
   attributes inferred from Helpers.tls, sql_sessions exposes only ?lifetime,
   and set_tls is not exported — so the outgoing header is the only surface
   the application can reach. *)

let public_origin_env = "EARDE_PUBLIC_ORIGIN"

let secure_required = function
  | Some origin -> String.starts_with ~prefix:"https://" (String.trim origin)
  | None -> false

(* Anchored to ';'-separated attributes: a cookie whose name or value happens
   to contain "secure" must not read as already-secured. *)
let has_secure_attribute value =
  String.split_on_char ';' value
  |> List.exists (fun part ->
         String.equal (String.lowercase_ascii (String.trim part)) "secure")

let add_secure_attribute value =
  if has_secure_attribute value then value else value ^ "; Secure"

(* Read once per process: the public origin is deployment configuration, and
   re-reading it per response would let a stray runtime change silently
   downgrade live cookies. *)
let required = lazy (secure_required (Sys.getenv_opt public_origin_env))

let middleware inner_handler request =
  let%lwt response = inner_handler request in
  if not (Lazy.force required) then Lwt.return response
  else begin
    match Dream.headers response "Set-Cookie" with
    | [] -> Lwt.return response
    | cookies ->
        (* Drop and re-add: Dream appends Set-Cookie headers, so rewriting in
           place is not available. The order of the re-added headers matches
           the order they were read in. *)
        Dream.drop_header response "Set-Cookie";
        List.iter
          (fun value ->
            Dream.add_header response "Set-Cookie" (add_secure_attribute value))
          cookies;
        Lwt.return response
  end
