(** The one canonical client identity, shared by the rate limiter and by every
    other place that records "where did this request come from".

    Two properties matter, and the previous inline middleware had neither:

    - {b Stability.} Dream reports the peer as ["address:port"] (see its
      [Adapt.address_to_string]), and the ephemeral source port changes on every
      connection. Keying anything on that string mints a fresh bucket per
      request, so a per-IP limiter silently enforces nothing on a direct
      connection. Everything here yields a bare, normalized address.

    - {b Authenticity.} Forwarded headers are attacker-supplied unless the
      immediate peer is one of our own proxies. The previous middleware took the
      LEFTMOST [X-Forwarded-For] value unconditionally — the one segment of that
      header a client fully controls — so rotating it produced an unlimited
      supply of buckets. Here a forwarded header is consulted only when the peer
      is a configured trusted proxy, and only its rightmost entry (the address
      that proxy itself observed) is believed.

    Everything is pure except [trusted_proxies_from_env]; no IO, no logging, and
    no address is ever returned to a client. *)

val normalize_ip : string -> string option
(** Canonical form of a textual IPv4 or IPv6 address, or [None] if it is not an
    address at all. Round-tripping through the system parser collapses
    alternative spellings of the same address ([::0001] and [::1], [010.1.1.1]
    and [10.1.1.1]) onto one key, so spelling variations cannot multiply
    buckets. *)

val peer_ip : string -> string option
(** The address part of Dream's ["address:port"] peer string, normalized. Splits
    at the LAST colon, which is correct for both families because Dream renders
    IPv6 unbracketed ([::1:44746]). [None] for a Unix-socket peer (Dream reports
    the socket path, which contains no port) and for anything unparseable. *)

val client_ip :
  trusted_proxies:string list ->
  peer:string ->
  forwarded_for:string option ->
  string
(** The bucket key for one request.

    When the peer is NOT a trusted proxy, forwarded headers are ignored entirely
    and the peer's own address is used — a direct client cannot nominate its own
    identity.

    When the peer IS a trusted proxy, strictly the LAST entry of [forwarded_for]
    wins — not the last well-formed one. With nginx's
    [proxy_add_x_forwarded_for] the proxy APPENDS the address it observed, so
    the final entry is always the proxy's own observation and everything to its
    left is client-supplied noise; with
    [proxy_set_header X-Forwarded-For $remote_addr] there is only one entry and
    it is the same value. Both configurations therefore give the real client,
    and neither can be steered by a client-sent header.

    Scanning leftward past a malformed final entry would reopen the bypass: a
    client sending ["198.51.100.44, garbage"] would get its own chosen value
    adopted. A final entry that is not an address means the proxy appended
    nothing, so nothing in the header is believed.

    Every failure path — malformed header, malformed peer, header absent — falls
    back to a value that is stable and shared rather than unique, so a parsing
    failure can only over-limit, never open a bypass. *)

val fallback_key : string
(** The key used when the peer address itself cannot be parsed. A constant, so
    such requests share one bucket instead of each receiving a fresh one. *)

val trusted_proxies_from_env : unit -> string list
(** Normalized trusted-proxy addresses from [EARDE_TRUSTED_PROXIES]
    (comma-separated). Defaults to loopback ([127.0.0.1], [::1]) — the
    documented production topology, where nginx terminates TLS on the same host
    and Dream binds loopback. Unparseable entries are dropped rather than
    trusted. *)

val default_trusted_proxies : string list
