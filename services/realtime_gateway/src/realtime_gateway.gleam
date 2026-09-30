import beryl
import beryl/channel
import beryl/presence
import beryl/socket
import beryl/transport/mist as ws
import beryl/wire
import cursors
import gleam/bit_array
import gleam/bytes_tree
import gleam/crypto
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/float
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/order
import gleam/result
import gleam/string
import mist.{type Connection, type ResponseData}
import typing

const port = 8090

const internal_secret_env = "REALTIME_INTERNAL_SECRET"

const token_secret_env = "REALTIME_TOKEN_SECRET"

const allowed_origins_env = "REALTIME_ALLOWED_ORIGINS"

/// shared_cursors is a per-community capability minted into the signed token
/// by Dream; legacy tokens without the claim default to False. Public (with
/// parse_claims_json) so tests can pin the trust boundary.
pub type AuthClaims {
  AuthClaims(
    user_id: Int,
    username: String,
    topic: String,
    exp: Int,
    shared_cursors: Bool,
  )
}

type AuthError {
  MissingSecret
  InvalidFormat
  InvalidSignature
  InvalidPayload
  ExpiredToken
}

@external(erlang, "realtime_gateway_ffi", "unix_now")
fn unix_now() -> Int

@external(erlang, "realtime_gateway_ffi", "getenv")
fn getenv(name: String) -> Result(String, Nil)

@external(erlang, "realtime_gateway_ffi", "safely")
fn safely(operation: fn() -> Nil) -> Result(Nil, Nil)

pub type PublishRequest {
  PublishRequest(topic: String, event: String, payload: json.Json)
}

/// Parse an internal-publish body. Public so tests can pin the new_msg
/// contract; the HTTP handler goes through the same decoder.
pub fn parse_publish_request(body: String) -> Result(PublishRequest, Nil) {
  json.parse(from: body, using: publish_request_decoder())
  |> result.map_error(fn(_) { Nil })
}

/// Internal-publish event allow-list. Only new_msg exists today; anything
/// else (even from a holder of the internal secret) is rejected so reserved
/// client-facing event names (presence_list, typing_list, cursor) can never
/// be spoofed through this endpoint.
pub fn publish_event_allowed(event: String) -> Bool {
  event == "new_msg"
}

/// Token freshness as used at connect AND at every join: a socket that
/// somehow outlives its token cannot re-join with the stale claims.
pub fn token_fresh(exp exp: Int, now now: Int) -> Bool {
  exp > now
}

/// How long a connection authenticated with this token may live, in
/// milliseconds. Clamped at zero: an already-expired token closes now.
pub fn expiry_delay_ms(exp exp: Int, now now: Int) -> Int {
  case exp > now {
    True -> { exp - now } * 1000
    False -> 0
  }
}

/// Beryl configuration with inbound rate limiting. Budgets cover typing
/// (~1 push / 2.5s) plus shared cursors (client-throttled to 10/s); chat
/// messages go over HTTP, so ordinary use stays below every limit.
/// Rate-limited frames are dropped by Beryl without disconnecting the socket.
/// Beryl has no per-event budgets — typing and cursor share the channel
/// bucket — so the cursors store adds its own 12/s movement-only limiter to
/// keep a cursor flood from starving typing.
pub fn gateway_config() -> beryl.Config {
  beryl.config(wire.phoenix_codec())
  // Transport-level, per socket, all inbound frames (incl. heartbeats):
  // generous ceiling against broken/malicious clients.
  |> beryl.with_message_rate(per_second: 20, burst: 40)
  // Per socket+topic after join — the combined typing + cursor budget.
  |> beryl.with_channel_rate(per_second: 15, burst: 30)
  // Reconnect/join churn guard; Phoenix rejoin backoff absorbs rejections.
  |> beryl.with_join_rate(per_second: 2, burst: 5)
  // Legitimate inbound frames (join, heartbeat, typing, cursor) are tiny;
  // oversized frames are closed before decoding and cannot affect others.
  |> beryl.with_max_inbound_frame_bytes(max_bytes: 4096)
}

pub fn main() -> Nil {
  let _gateway = start(gateway_config(), port)
  process.sleep_forever()
}

/// The processes a running gateway is made of, exposed so tests can inject
/// failures.
pub type Gateway {
  Gateway(
    channels: beryl.Channels,
    typing: typing.Typing,
    cursors: cursors.Cursors,
  )
}

/// Starts the gateway and serves HTTP and WebSockets on 127.0.0.1:`port`.
///
/// Recovery contract. Every essential subsystem (Beryl's coordinator and
/// handler registry, presence, typing, cursors and the HTTP server's
/// supervisor) is linked to the calling process, so none of them can die
/// alone: a crash takes the caller down with it. Under `gleam run` that ends
/// the VM with a non-zero status, and the service manager starts a fresh
/// gateway. The listening socket and its acceptors are restarted in place by
/// the HTTP server's own supervisor. Nothing here is durable: browsers
/// reconnect with backoff and catch up over HTTP, and PostgreSQL keeps every
/// committed message.
pub fn start(config: beryl.Config, port: Int) -> Gateway {
  let assert Ok(channels) = beryl.start(config)
  io.println("beryl started")

  let assert Ok(tracker) =
    presence.start(presence.default_config("realtime_gateway"))
  io.println("presence started")

  let assert Ok(typing_tracker) = typing.start(channels)
  io.println("typing tracker started")

  let assert Ok(cursor_tracker) = cursors.start(channels)
  io.println("cursor tracker started")

  let assert Ok(_registration) =
    beryl.register(
      channels,
      "chan:*",
      chat_channel(channels, tracker, typing_tracker, cursor_tracker),
    )
  io.println("registered channel pattern chan:*")

  let assert Ok(_server) =
    mist.new(fn(req) { handle_request(req, channels) })
    |> mist.port(port)
    |> mist.bind("127.0.0.1")
    |> mist.start

  io.println(
    "realtime gateway listening on http://localhost:" <> int.to_string(port),
  )

  Gateway(channels: channels, typing: typing_tracker, cursors: cursor_tracker)
}

fn chat_channel(
  channels: beryl.Channels,
  tracker: presence.Presence,
  typing_tracker: typing.Typing,
  cursor_tracker: cursors.Cursors,
) -> channel.Channel(AuthClaims, Nil) {
  channel.new(fn(topic: String, _payload, socket: socket.Socket(AuthClaims)) {
    let claims = socket.get_assigns(socket)

    case claims.topic == topic, token_fresh(exp: claims.exp, now: unix_now()) {
      True, True -> {
        io.println("client joined topic " <> topic)
        track_presence(channels, tracker, topic, claims, socket.id(socket))
        schedule_expiry_close(socket, claims.exp)
        channel.JoinOk(reply: None, socket: socket)
      }

      True, False -> {
        io.println("rejected join for topic " <> topic <> ": expired token")

        channel.JoinError(
          json.object([
            #("reason", json.string("expired token")),
          ]),
        )
      }

      False, _ -> {
        io.println(
          "rejected join for topic "
          <> topic
          <> " with token topic "
          <> claims.topic,
        )

        channel.JoinError(
          json.object([
            #("reason", json.string("unauthorized topic")),
          ]),
        )
      }
    }
  })
  |> channel.with_handle_in(
    fn(event, payload, socket: socket.Socket(AuthClaims)) {
      case event {
        "typing" -> handle_typing_event(typing_tracker, payload, socket)
        "cursor" -> handle_cursor_event(cursor_tracker, payload, socket)
        // Unknown client events are ignored so they can never affect
        // presence or new_msg fanout.
        _ -> channel.NoReply(socket)
      }
    },
  )
  |> channel.with_terminate(fn(_reason, socket: socket.Socket(AuthClaims)) {
    let claims = socket.get_assigns(socket)
    untrack_presence(channels, tracker, claims.topic, socket.id(socket))
    typing.socket_gone(typing_tracker, socket_id: socket.id(socket))
    cursors.socket_gone(cursor_tracker, socket_id: socket.id(socket))
  })
}

/// Enforce token expiry on the connection itself: an unlinked sleeper closes
/// the underlying transport when the token's exp is reached — the same close
/// path Beryl's heartbeat eviction uses, so mist runs the full disconnect
/// (terminate → presence/typing/cursor cleanup) and the browser's Phoenix
/// socket reconnects on its own with the refreshed token from its params
/// callback. Closing an already-dead connection is a no-op, so the sleeper
/// needs no cancellation on early disconnect; duplicate joins just add
/// another sleeper for the same instant.
fn schedule_expiry_close(sock: socket.Socket(AuthClaims), exp: Int) -> Nil {
  let close = socket.close(socket.transport(sock))
  let delay = expiry_delay_ms(exp: exp, now: unix_now())
  let _pid =
    process.spawn_unlinked(fn() {
      process.sleep(delay)
      let _ = close()
      Nil
    })
  Nil
}

/// Identity and topic come exclusively from the verified socket assigns —
/// the client controls only the boolean. Joins are already restricted to the
/// token's exact topic, so claims.topic is the joined channel. Malformed or
/// unversioned payloads are ignored.
fn handle_typing_event(
  typing_tracker: typing.Typing,
  payload: Dynamic,
  socket: socket.Socket(AuthClaims),
) -> channel.HandleResult(AuthClaims) {
  let claims = socket.get_assigns(socket)
  case decode_typing_payload(payload) {
    Ok(True) ->
      typing.active(
        typing_tracker,
        topic: claims.topic,
        socket_id: socket.id(socket),
        user_id: claims.user_id,
        username: claims.username,
      )
    Ok(False) ->
      typing.inactive(
        typing_tracker,
        topic: claims.topic,
        socket_id: socket.id(socket),
      )
    Error(Nil) -> Nil
  }
  channel.NoReply(socket)
}

pub fn decode_typing_payload(payload: Dynamic) -> Result(Bool, Nil) {
  let decoder = {
    use v <- decode.field("v", decode.int)
    use active <- decode.field("active", decode.bool)
    decode.success(#(v, active))
  }
  case channel.decode_payload(payload, decoder) {
    Ok(#(1, active)) -> Ok(active)
    _ -> Error(Nil)
  }
}

pub type CursorEvent {
  CursorActive(x: Float, y: Float)
  CursorInactive
}

/// Enforcement of the per-community capability at the trust boundary: only
/// the verified token claim decides. An active event without the capability
/// is dropped before any state or broadcast can exist; inactive stays a
/// harmless cleanup no-op. Payload fields (e.g. a forged shared_cursors)
/// can never override the claim — the payload decoder ignores them.
pub fn authorize_cursor_event(
  shared_cursors shared_cursors: Bool,
  event event: CursorEvent,
) -> Result(CursorEvent, Nil) {
  case shared_cursors, event {
    False, CursorActive(_, _) -> Error(Nil)
    _, _ -> Ok(event)
  }
}

/// Same trust model as typing: identity and topic come exclusively from the
/// verified socket assigns; the client controls only active/x/y. Malformed or
/// unversioned payloads are ignored, as are active events from tokens
/// without the shared_cursors capability.
fn handle_cursor_event(
  cursor_tracker: cursors.Cursors,
  payload: Dynamic,
  socket: socket.Socket(AuthClaims),
) -> channel.HandleResult(AuthClaims) {
  let claims = socket.get_assigns(socket)
  case
    decode_cursor_payload(payload)
    |> result.try(fn(event) {
      authorize_cursor_event(
        shared_cursors: claims.shared_cursors,
        event: event,
      )
    })
  {
    Ok(CursorActive(x, y)) ->
      cursors.active(
        cursor_tracker,
        topic: claims.topic,
        socket_id: socket.id(socket),
        user_id: claims.user_id,
        username: claims.username,
        x: x,
        y: y,
      )
    Ok(CursorInactive) ->
      cursors.inactive(
        cursor_tracker,
        topic: claims.topic,
        socket_id: socket.id(socket),
      )
    Error(Nil) -> Nil
  }
  channel.NoReply(socket)
}

/// Coordinates must be numeric (JSON integers 0 and 1 included — Erlang JSON
/// yields ints for them, so plain decode.float would wrongly reject the
/// edges) and are clamped to [0, 1]. Missing or non-numeric coordinates on an
/// active update reject the whole event.
pub fn decode_cursor_payload(payload: Dynamic) -> Result(CursorEvent, Nil) {
  let number =
    decode.one_of(decode.float, or: [decode.map(decode.int, int.to_float)])
  let decoder = {
    use v <- decode.field("v", decode.int)
    use active <- decode.field("active", decode.bool)
    case active {
      False -> decode.success(#(v, CursorInactive))
      True -> {
        use x <- decode.field("x", number)
        use y <- decode.field("y", number)
        decode.success(#(
          v,
          CursorActive(
            float.clamp(x, min: 0.0, max: 1.0),
            float.clamp(y, min: 0.0, max: 1.0),
          ),
        ))
      }
    }
  }
  case channel.decode_payload(payload, decoder) {
    Ok(#(1, event)) -> Ok(event)
    _ -> Error(Nil)
  }
}

pub type PresenceUser {
  PresenceUser(user_id: Int, username: String)
}

fn track_presence(
  channels: beryl.Channels,
  tracker: presence.Presence,
  topic: String,
  claims: AuthClaims,
  socket_id: String,
) -> Nil {
  case
    safely(fn() {
      let _ =
        presence.track(
          tracker,
          topic,
          int.to_string(claims.user_id),
          socket_id,
          json.object([#("username", json.string(claims.username))]),
        )
      broadcast_presence_list(channels, tracker, topic)
    })
  {
    Ok(Nil) -> Nil
    Error(Nil) -> io.println("presence track failed for topic " <> topic)
  }
}

fn untrack_presence(
  channels: beryl.Channels,
  tracker: presence.Presence,
  topic: String,
  socket_id: String,
) -> Nil {
  case
    safely(fn() {
      presence.untrack_all(tracker, socket_id)
      broadcast_presence_list(channels, tracker, topic)
    })
  {
    Ok(Nil) -> Nil
    Error(Nil) -> io.println("presence untrack failed for topic " <> topic)
  }
}

fn broadcast_presence_list(
  channels: beryl.Channels,
  tracker: presence.Presence,
  topic: String,
) -> Nil {
  let users =
    presence.list(tracker, topic)
    |> list.filter_map(presence_entry_user)
    |> unique_presence_users

  beryl.broadcast(
    channels,
    topic,
    "presence_list",
    presence_list_payload(users),
  )
}

fn presence_entry_user(
  entry: presence.PresenceEntry,
) -> Result(PresenceUser, Nil) {
  case int.parse(entry.key), decode_presence_username(entry.meta) {
    Ok(user_id), Ok(username) -> Ok(PresenceUser(user_id, username))
    _, _ -> Error(Nil)
  }
}

fn decode_presence_username(meta: json.Json) -> Result(String, Nil) {
  let decoder = {
    use username <- decode.field("username", decode.string)
    decode.success(username)
  }

  json.parse(from: json.to_string(meta), using: decoder)
  |> result.map_error(fn(_) { Nil })
}

pub fn unique_presence_users(users: List(PresenceUser)) -> List(PresenceUser) {
  users
  |> list.fold([], fn(seen, user) {
    case
      list.any(seen, fn(kept: PresenceUser) { kept.user_id == user.user_id })
    {
      True -> seen
      False -> [user, ..seen]
    }
  })
  |> list.sort(fn(a, b) {
    case
      string.compare(string.lowercase(a.username), string.lowercase(b.username))
    {
      order.Eq -> int.compare(a.user_id, b.user_id)
      username_order -> username_order
    }
  })
}

pub fn presence_list_payload(users: List(PresenceUser)) -> json.Json {
  json.object([
    #(
      "users",
      json.array(users, fn(user) {
        json.object([
          #("user_id", json.int(user.user_id)),
          #("username", json.string(user.username)),
        ])
      }),
    ),
  ])
}

fn handle_request(
  req: Request(Connection),
  channels: beryl.Channels,
) -> Response(ResponseData) {
  use <- ws.upgrade(req, channels, ws_config())

  case request.path_segments(req) {
    ["internal", "publish"] -> handle_internal_publish(req, channels)
    ["health"] -> text(200, "ok")
    _ -> text(404, "not found")
  }
}

fn ws_config() {
  ws.default_config("/socket/websocket")
  |> with_configured_origins
  |> ws.with_on_connect(fn(req) {
    case token_of(req) {
      Ok(token) -> {
        case verify_signed_token(token) {
          Ok(claims) -> Ok(claims)
          Error(error) -> reject_auth(error)
        }
      }

      Error(Nil) -> {
        io.println("realtime auth rejected: missing token")
        Error(ws.ConnectRejected)
      }
    }
  })
}

/// Beryl 1.x defaults WebSocket upgrades to a SameOrigin policy, which
/// rejects browsers whenever the page origin differs from the gateway's
/// host:port (the standard Dream-on-8080 / gateway-on-8090 topology). When
/// REALTIME_ALLOWED_ORIGINS is set (comma-separated full origins, e.g.
/// "https://earde.com,http://localhost:8080") pin an explicit allow-list;
/// when unset, keep the stricter SameOrigin default.
fn with_configured_origins(
  config: ws.TransportConfig(Nil),
) -> ws.TransportConfig(Nil) {
  case getenv(allowed_origins_env) {
    Ok(raw) -> {
      let origins =
        raw
        |> string.split(",")
        |> list.map(string.trim)
        |> list.filter(fn(origin) { origin != "" })
      case origins {
        [] -> config
        _ -> ws.with_allowed_origins(config, origins)
      }
    }
    Error(Nil) -> config
  }
}

fn token_of(req: Request(Connection)) -> Result(String, Nil) {
  case request.get_query(req) {
    Ok(params) -> list.key_find(params, "token")
    Error(Nil) -> Error(Nil)
  }
}

fn token_secret() -> Result(String, AuthError) {
  case getenv(token_secret_env) {
    Ok(secret) if secret != "" -> Ok(secret)
    _ -> Error(MissingSecret)
  }
}

fn internal_secret() -> Result(String, Nil) {
  case getenv(internal_secret_env) {
    Ok(secret) if secret != "" -> Ok(secret)
    _ -> Error(Nil)
  }
}

fn verify_signed_token(token: String) -> Result(AuthClaims, AuthError) {
  case string.split(token, ".") {
    [payload, signature] -> {
      use secret <- result.try(token_secret())
      use _ <- result.try(verify_signature(payload, signature, secret))
      use claims <- result.try(decode_claims(payload))

      case claims.exp > unix_now() {
        True -> Ok(claims)
        False -> Error(ExpiredToken)
      }
    }

    _ -> Error(InvalidFormat)
  }
}

fn verify_signature(
  payload: String,
  signature: String,
  secret: String,
) -> Result(Nil, AuthError) {
  let expected =
    crypto.hmac(<<payload:utf8>>, crypto.Sha256, <<secret:utf8>>)
    |> bit_array.base64_url_encode(False)

  case crypto.secure_compare(<<expected:utf8>>, <<signature:utf8>>) {
    True -> Ok(Nil)
    False -> Error(InvalidSignature)
  }
}

fn decode_claims(payload: String) -> Result(AuthClaims, AuthError) {
  use payload_bits <- result.try(
    bit_array.base64_url_decode(payload)
    |> result.map_error(fn(_) { InvalidPayload }),
  )

  use payload_json <- result.try(
    bit_array.to_string(payload_bits)
    |> result.map_error(fn(_) { InvalidPayload }),
  )

  json.parse(from: payload_json, using: claims_decoder())
  |> result.map_error(fn(_) { InvalidPayload })
}

fn claims_decoder() -> decode.Decoder(AuthClaims) {
  use _v <- decode.field("v", decode.int)
  use user_id <- decode.field("user_id", decode.int)
  use username <- decode.field("username", decode.string)
  use topic <- decode.field("topic", decode.string)
  use exp <- decode.field("exp", decode.int)
  // Absent on legacy tokens (minted before the capability existed): those
  // stay valid for presence/typing/chat but can never share cursors.
  use shared_cursors <- decode.optional_field(
    "shared_cursors",
    False,
    decode.bool,
  )

  decode.success(AuthClaims(user_id, username, topic, exp, shared_cursors))
}

/// Parse a raw claims JSON document (the decoded token payload). Signature
/// and expiry checks live in verify_signed_token; this is the pure decoding
/// step, public so tests can pin claim semantics (legacy default included).
pub fn parse_claims_json(payload_json: String) -> Result(AuthClaims, Nil) {
  json.parse(from: payload_json, using: claims_decoder())
  |> result.map_error(fn(_) { Nil })
}

fn handle_internal_publish(
  req: Request(Connection),
  channels: beryl.Channels,
) -> Response(ResponseData) {
  case req.method == http.Post {
    False -> text(405, "method not allowed")
    True -> {
      case internal_secret() {
        Error(Nil) -> {
          io.println(
            "internal publish rejected: missing REALTIME_INTERNAL_SECRET",
          )
          text(500, "missing REALTIME_INTERNAL_SECRET")
        }

        Ok(expected_secret) -> {
          case request.get_header(req, "x-earde-internal") {
            Ok(actual_secret) -> {
              case
                crypto.secure_compare(<<actual_secret:utf8>>, <<
                  expected_secret:utf8,
                >>)
              {
                True -> publish_from_body(req, channels)
                False -> {
                  io.println(
                    "internal publish rejected: invalid internal secret",
                  )
                  text(403, "forbidden")
                }
              }
            }

            Error(Nil) -> {
              io.println(
                "internal publish rejected: missing internal secret header",
              )
              text(403, "forbidden")
            }
          }
        }
      }
    }
  }
}

fn publish_from_body(
  req: Request(Connection),
  channels: beryl.Channels,
) -> Response(ResponseData) {
  case mist.read_body(req, max_body_limit: 64_000) {
    Ok(req_with_body) -> {
      case bit_array.to_string(req_with_body.body) {
        Ok(body) -> {
          case parse_publish_request(body) {
            Ok(PublishRequest(topic, event, payload)) ->
              case publish_event_allowed(event) {
                True -> {
                  beryl.broadcast(channels, topic, event, payload)
                  text(202, "published")
                }
                False -> {
                  io.println(
                    "internal publish rejected: unsupported event " <> event,
                  )
                  text(400, "unsupported event")
                }
              }
            Error(_) -> text(400, "invalid json")
          }
        }
        Error(_) -> text(400, "invalid utf8")
      }
    }
    Error(_) -> text(400, "could not read body")
  }
}

fn publish_request_decoder() -> decode.Decoder(PublishRequest) {
  use topic <- decode.field("topic", decode.string)
  use event <- decode.field("event", decode.string)
  use payload <- decode.field("payload", message_payload_decoder())

  decode.success(PublishRequest(topic, event, payload))
}

fn message_payload_decoder() -> decode.Decoder(json.Json) {
  use v <- decode.field("v", decode.int)
  use msg_type <- decode.field("type", decode.string)
  use id <- decode.field("id", decode.int)
  use channel_id <- decode.field("channel_id", decode.int)
  use community_id <- decode.field("community_id", decode.int)
  use user_id <- decode.field("user_id", decode.int)
  use username <- decode.field("username", decode.string)
  use content <- decode.field("content", decode.string)
  use created_at <- decode.field("created_at", decode.string)

  decode.success(
    json.object([
      #("v", json.int(v)),
      #("type", json.string(msg_type)),
      #("id", json.int(id)),
      #("channel_id", json.int(channel_id)),
      #("community_id", json.int(community_id)),
      #("user_id", json.int(user_id)),
      #("username", json.string(username)),
      #("content", json.string(content)),
      #("created_at", json.string(created_at)),
    ]),
  )
}

fn auth_error_to_string(error: AuthError) -> String {
  case error {
    MissingSecret -> "missing REALTIME_TOKEN_SECRET"
    InvalidFormat -> "invalid token format"
    InvalidSignature -> "invalid signature"
    InvalidPayload -> "invalid payload"
    ExpiredToken -> "expired token"
  }
}

fn reject_auth(error: AuthError) -> Result(AuthClaims, ws.ConnectError) {
  io.println("realtime auth rejected: " <> auth_error_to_string(error))
  Error(ws.ConnectRejected)
}

fn text(status: Int, body: String) -> Response(ResponseData) {
  response.new(status)
  |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
}
