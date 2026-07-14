import beryl
import beryl/channel
import beryl/presence
import beryl/socket
import beryl/transport/mist as ws
import beryl/wire
import gleam/bit_array
import gleam/bytes_tree
import gleam/crypto
import gleam/dynamic/decode
import gleam/erlang/process
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

const port = 8090

const internal_secret_env = "REALTIME_INTERNAL_SECRET"

const token_secret_env = "REALTIME_TOKEN_SECRET"

type AuthClaims {
  AuthClaims(user_id: Int, username: String, topic: String, exp: Int)
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

type PublishRequest {
  PublishRequest(topic: String, event: String, payload: json.Json)
}

pub fn main() -> Nil {
  let assert Ok(channels) = beryl.start(beryl.config(wire.phoenix_codec()))
  io.println("beryl started")

  let assert Ok(tracker) =
    presence.start(presence.default_config("realtime_gateway"))
  io.println("presence started")

  let assert Ok(_registration) =
    beryl.register(channels, "chan:*", chat_channel(channels, tracker))
  io.println("registered channel pattern chan:*")

  let assert Ok(_server) =
    mist.new(fn(req) { handle_request(req, channels) })
    |> mist.port(port)
    |> mist.bind("127.0.0.1")
    |> mist.start

  io.println(
    "realtime gateway listening on http://localhost:" <> int.to_string(port),
  )

  process.sleep_forever()
}

fn chat_channel(
  channels: beryl.Channels,
  tracker: presence.Presence,
) -> channel.Channel(AuthClaims, Nil) {
  channel.new(fn(topic: String, _payload, socket: socket.Socket(AuthClaims)) {
    let claims = socket.get_assigns(socket)

    case claims.topic == topic {
      True -> {
        io.println("client joined topic " <> topic)
        track_presence(channels, tracker, topic, claims, socket.id(socket))
        channel.JoinOk(reply: None, socket: socket)
      }

      False -> {
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
  |> channel.with_terminate(fn(_reason, socket: socket.Socket(AuthClaims)) {
    let claims = socket.get_assigns(socket)
    untrack_presence(channels, tracker, claims.topic, socket.id(socket))
  })
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

  decode.success(AuthClaims(user_id, username, topic, exp))
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
          case json.parse(from: body, using: publish_request_decoder()) {
            Ok(PublishRequest(topic, event, payload)) -> {
              beryl.broadcast(channels, topic, event, payload)
              text(202, "published")
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
