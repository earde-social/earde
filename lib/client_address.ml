(* See client_address.mli. The trust boundary for forwarded headers, and the
   normalization that makes a per-IP bucket actually per-IP. *)

let fallback_key = "unknown"
let default_trusted_proxies = [ "127.0.0.1"; "::1" ]
let trusted_proxies_env = "EARDE_TRUSTED_PROXIES"

(* Round-tripping through the system parser is what canonicalizes: two
   spellings of one address become one key, and anything the parser rejects
   is not an address. Never raises. *)
let normalize_ip raw =
  let candidate = String.trim raw in
  if String.equal candidate "" then None
  else
    match Unix.inet_addr_of_string candidate with
    | addr -> Some (Unix.string_of_inet_addr addr)
    | exception _ -> None

(* Dream renders the peer as "<address>:<port>" and does NOT bracket IPv6
   (its Adapt.address_to_string is a plain "%s:%i"), so the port is whatever
   follows the last colon and the address is everything before it. A
   Unix-socket peer is reported as the socket path, which has no port — it
   fails the address parse below and falls through to None. *)
let peer_ip peer =
  match String.rindex_opt peer ':' with
  | None -> normalize_ip peer
  | Some i -> (
      match normalize_ip (String.sub peer 0 i) with
      | Some ip -> Some ip
      (* An address with no port at all (some proxies and test harnesses hand
         one over bare) still has colons if it is IPv6, so retry whole. *)
      | None -> normalize_ip peer)

(* Strictly the LAST entry — not the last well-formed one. With nginx's
   proxy_add_x_forwarded_for the proxy APPENDS the address it observed, so
   the final entry is always the proxy's own observation and everything left
   of it is whatever the client chose to send.

   Scanning leftward past a malformed final entry would be the bug all over
   again: a client sending "198.51.100.44, garbage" would have its own chosen
   value adopted, because the entry our proxy was supposed to append is not
   there. If the last entry is not an address, the proxy did not append one,
   nothing in the header is trustworthy, and the caller falls back to the
   peer. *)
let last_forwarded value =
  match List.rev (String.split_on_char ',' value) with
  | [] -> None
  | last :: _ -> normalize_ip last

(* Only the LAST X-Forwarded-For header line is considered when a request
   carries several (bin/main.ml passes it): that is the one our own proxy
   wrote or appended to. *)
let client_ip ~trusted_proxies ~peer ~forwarded_for =
  match peer_ip peer with
  | None -> fallback_key
  | Some peer_ip -> (
      if not (List.mem peer_ip trusted_proxies) then
        (* Direct connection: forwarded headers are pure client input here
           and are ignored outright. *)
        peer_ip
      else
        match forwarded_for with
        | None -> peer_ip
        | Some value -> (
            match last_forwarded value with
            | Some ip -> ip
            (* Trusted peer but an unusable header: fall back to the proxy's
               own address. One shared, stable bucket — over-limiting, never
               a bypass. *)
            | None -> peer_ip))

let trusted_proxies_from_env () =
  match Sys.getenv_opt trusted_proxies_env with
  | None -> default_trusted_proxies
  | Some raw ->
      let parsed =
        String.split_on_char ',' raw |> List.filter_map normalize_ip
      in
      (* An empty or entirely unparseable setting means "no forwarded header
         is trusted", which is the safe reading of a misconfiguration — not a
         reason to silently restore the default. *)
      parsed
