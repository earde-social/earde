//// Failure injection against the real gateway: the recovery contract
//// documented on `realtime_gateway.start`, connection cleanup and reconnect.
//// Each test serves on its own loopback port from a root process that stands
//// in for `main`, and kills that root when it is done.

import beryl
import cursors
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/list
import realtime_gateway
import typing

type Socket

@external(erlang, "gateway_test_ffi", "links")
fn links(pid: Pid) -> List(Pid)

@external(erlang, "gateway_test_ffi", "listener_owner")
fn listener_owner(port: Int) -> Result(Pid, Nil)

@external(erlang, "gateway_test_ffi", "connection_owner")
fn connection_owner(port: Int, client: Socket) -> Result(Pid, Nil)

@external(erlang, "gateway_test_ffi", "wait_down")
fn wait_down(pid: Pid, timeout_ms: Int) -> Bool

@external(erlang, "gateway_test_ffi", "http_status")
fn http_status(port: Int, path: String) -> Result(Int, Nil)

@external(erlang, "gateway_test_ffi", "ws_connect")
fn ws_connect(port: Int, target: String) -> Result(Socket, Nil)

@external(erlang, "gateway_test_ffi", "ws_send")
fn ws_send(socket: Socket, text: String) -> Nil

@external(erlang, "gateway_test_ffi", "ws_recv")
fn ws_recv(socket: Socket, timeout_ms: Int) -> Result(String, String)

@external(erlang, "gateway_test_ffi", "ws_close")
fn ws_close(socket: Socket) -> Nil

@external(erlang, "gateway_test_ffi", "putenv")
fn putenv(name: String, value: String) -> Nil

@external(erlang, "gateway_test_ffi", "now_ms")
fn now_ms() -> Int

@external(erlang, "realtime_gateway_ffi", "unix_now")
fn unix_now() -> Int

const token_secret = "recovery-test-token-secret"

const internal_secret = "recovery-test-internal-secret"

const topic = "chan:7:0"

fn setup_env() -> Nil {
  putenv("REALTIME_TOKEN_SECRET", token_secret)
  putenv("REALTIME_INTERNAL_SECRET", internal_secret)
}

/// Starts a gateway owned by a fresh, unlinked root process, as `main` owns
/// it in production.
fn spawn_gateway(
  config: beryl.Config,
  port: Int,
) -> #(Pid, realtime_gateway.Gateway) {
  setup_env()
  let started = process.new_subject()
  let root =
    process.spawn_unlinked(fn() {
      process.send(started, realtime_gateway.start(config, port))
      process.sleep_forever()
    })
  let assert Ok(gateway) = process.receive(started, 5000)
  #(root, gateway)
}

fn stop(root: Pid) -> Nil {
  process.kill(root)
  let _ = wait_down(root, 2000)
  Nil
}

fn token(user_id: Int, username: String) -> String {
  let payload =
    json.object([
      #("v", json.int(1)),
      #("user_id", json.int(user_id)),
      #("username", json.string(username)),
      #("topic", json.string(topic)),
      #("exp", json.int(unix_now() + 600)),
    ])
    |> json.to_string
    |> bit_array.from_string
    |> bit_array.base64_url_encode(False)
  let signature =
    crypto.hmac(<<payload:utf8>>, crypto.Sha256, <<token_secret:utf8>>)
    |> bit_array.base64_url_encode(False)
  payload <> "." <> signature
}

fn join(port: Int, user_id: Int, username: String) -> Socket {
  let assert Ok(socket) =
    ws_connect(
      port,
      "/socket/websocket?vsn=2.0.0&token=" <> token(user_id, username),
    )
  ws_send(socket, "[\"1\",\"1\",\"" <> topic <> "\",\"phx_join\",{}]")
  let assert Ok(_) = next_event(socket, "phx_reply", 2000)
  socket
}

type Frame {
  Frame(event: String, payload: decode.Dynamic)
}

fn frame_decoder() -> decode.Decoder(Frame) {
  use event <- decode.subfield([3], decode.string)
  use payload <- decode.subfield([4], decode.dynamic)
  decode.success(Frame(event, payload))
}

/// The payload of the next frame carrying `event`, skipping others.
fn next_event(
  socket: Socket,
  event: String,
  timeout_ms: Int,
) -> Result(decode.Dynamic, String) {
  next_event_until(socket, event, now_ms() + timeout_ms)
}

fn next_event_until(
  socket: Socket,
  event: String,
  deadline: Int,
) -> Result(decode.Dynamic, String) {
  let remaining = deadline - now_ms()
  case remaining > 0 {
    False -> Error("timeout waiting for " <> event)
    True ->
      case ws_recv(socket, remaining) {
        Error(reason) -> Error(reason)
        Ok(text) ->
          case json.parse(text, frame_decoder()) {
            Ok(Frame(name, payload)) if name == event -> Ok(payload)
            _ -> next_event_until(socket, event, deadline)
          }
      }
  }
}

fn user_ids(payload: decode.Dynamic) -> List(Int) {
  let decoder =
    decode.at(["users"], decode.list(decode.at(["user_id"], decode.int)))
  let assert Ok(ids) = decode.run(payload, decoder)
  list.sort(ids, int.compare)
}

/// The next `event` list whose user ids equal `expected`.
fn await_users(
  socket: Socket,
  event: String,
  expected: List(Int),
  timeout_ms: Int,
) -> Bool {
  await_users_until(socket, event, expected, now_ms() + timeout_ms)
}

fn await_users_until(
  socket: Socket,
  event: String,
  expected: List(Int),
  deadline: Int,
) -> Bool {
  case next_event_until(socket, event, deadline) {
    Ok(payload) ->
      user_ids(payload) == expected
      || await_users_until(socket, event, expected, deadline)
    Error(_) -> False
  }
}

/// Whether the server closes `socket` within the timeout; frames already
/// queued before the close are skipped.
fn closed(socket: Socket, timeout_ms: Int) -> Bool {
  closed_until(socket, now_ms() + timeout_ms)
}

fn closed_until(socket: Socket, deadline: Int) -> Bool {
  let remaining = deadline - now_ms()
  remaining > 0
  && case ws_recv(socket, remaining) {
    Ok(_) -> closed_until(socket, deadline)
    Error("timeout") -> False
    Error(_) -> True
  }
}

fn owner(result: Result(Pid, Nil)) -> Pid {
  let assert Ok(pid) = result
  pid
}

// Every process linked to the root is one the gateway cannot work without,
// and each takes the whole gateway down when it dies: the listener closes,
// so nothing keeps answering /health with a subsystem missing.
pub fn every_essential_subsystem_is_fatal_test() {
  let port = 18_091
  let #(root, gateway) = spawn_gateway(realtime_gateway.gateway_config(), port)
  let essential = links(root)
  // Beryl's coordinator and registry, presence, typing, cursors and the HTTP
  // server's supervisor.
  assert list.length(essential) == 6
  let named = [
    owner(process.subject_owner(beryl.coordinator_subject(gateway.channels))),
    owner(typing.owner(gateway.typing)),
    owner(cursors.owner(gateway.cursors)),
  ]
  assert list.all(named, list.contains(essential, _))
  stop(root)

  list.index_map(essential, fn(_, index) {
    let port = port + 1 + index
    let #(root, _) = spawn_gateway(realtime_gateway.gateway_config(), port)
    assert http_status(port, "/health") == Ok(200)
    let assert Ok(victim) = list.first(list.drop(links(root), index))
    process.kill(victim)
    assert wait_down(root, 2000)
    assert wait_for(fn() { http_status(port, "/health") == Error(Nil) }, 2000)
  })
}

// The listening socket is the HTTP server's own business: its supervisor
// restarts it in place, open connections keep working and new ones are
// accepted.
pub fn listener_is_restarted_in_place_test() {
  let port = 18_101
  let #(root, _) = spawn_gateway(realtime_gateway.gateway_config(), port)
  let open = join(port, 1, "ada")
  let listener = owner(listener_owner(port))
  process.kill(listener)
  assert wait_down(listener, 1000)
  assert wait_for(fn() { http_status(port, "/health") == Ok(200) }, 2000)
  assert process.is_alive(root)

  let later = join(port, 2, "bob")
  assert publish(port, "after the listener restart") == Ok(202)
  let content = decode.at(["content"], decode.string)
  let assert Ok(payload) = next_event(open, "new_msg", 2000)
  assert decode.run(payload, content) == Ok("after the listener restart")
  let assert Ok(payload) = next_event(later, "new_msg", 2000)
  assert decode.run(payload, content) == Ok("after the listener restart")
  ws_close(open)
  ws_close(later)
  stop(root)
}

// A socket that closes normally leaves presence and typing at once.
pub fn closed_socket_leaves_presence_and_typing_test() {
  let port = 18_102
  let #(root, _) = spawn_gateway(realtime_gateway.gateway_config(), port)
  let ada = join(port, 1, "ada")
  let bob = join(port, 2, "bob")
  assert await_users(bob, "presence_list", [1, 2], 2000)
  ws_send(
    ada,
    "[\"1\",\"2\",\"" <> topic <> "\",\"typing\",{\"v\":1,\"active\":true}]",
  )
  assert await_users(bob, "typing_list", [1], 2000)

  ws_close(ada)
  assert await_users(bob, "presence_list", [2], 2000)
  assert await_users(bob, "typing_list", [], 2000)
  ws_close(bob)
  stop(root)
}

// A connection whose process dies without closing (no disconnect message
// reaches Beryl) is evicted by the heartbeat check: every entry it held is
// released within one heartbeat timeout plus one check interval. Production
// uses Beryl's defaults (60 s timeout, 30 s checks); this test shortens them.
pub fn dead_connection_is_evicted_by_heartbeat_test() {
  let port = 18_103
  let config =
    realtime_gateway.gateway_config()
    |> beryl.with_heartbeat(interval_ms: 200, timeout_ms: 400)
  let #(root, _) = spawn_gateway(config, port)
  let ada = join(port, 1, "ada")
  let bob = join(port, 2, "bob")
  assert await_users(bob, "presence_list", [1, 2], 2000)

  process.kill(owner(connection_owner(port, ada)))
  // Bob keeps his own connection alive meanwhile.
  let heartbeat = "[null,\"9\",\"phoenix\",\"heartbeat\",{}]"
  ws_send(bob, heartbeat)
  process.sleep(250)
  ws_send(bob, heartbeat)
  assert await_users(bob, "presence_list", [2], 1500)
  ws_close(ada)
  ws_close(bob)
  stop(root)
}

// After the gateway dies, a restarted one accepts the same client again and
// delivers new messages; nothing from before the restart is replayed (that
// is the HTTP catch-up's job, served from PostgreSQL).
pub fn clients_reconnect_after_a_restart_test() {
  let port = 18_104
  let #(root, gateway) = spawn_gateway(realtime_gateway.gateway_config(), port)
  let socket = join(port, 1, "ada")
  process.kill(owner(typing.owner(gateway.typing)))
  assert wait_down(root, 2000)
  assert closed(socket, 2000)
  ws_close(socket)

  let #(root, _) = spawn_gateway(realtime_gateway.gateway_config(), port)
  let socket = join(port, 1, "ada")
  assert publish(port, "after the restart") == Ok(202)
  let assert Ok(payload) = next_event(socket, "new_msg", 2000)
  assert decode.run(payload, decode.at(["content"], decode.string))
    == Ok("after the restart")
  ws_close(socket)
  stop(root)
}

fn publish(port: Int, content: String) -> Result(Int, Nil) {
  let body =
    json.object([
      #("topic", json.string(topic)),
      #("event", json.string("new_msg")),
      #(
        "payload",
        json.object([
          #("v", json.int(1)),
          #("type", json.string("message")),
          #("id", json.int(1)),
          #("channel_id", json.int(7)),
          #("community_id", json.int(3)),
          #("user_id", json.int(2)),
          #("username", json.string("bob")),
          #("content", json.string(content)),
          #("created_at", json.string("2026-09-30T00:00:00Z")),
        ]),
      ),
    ])
    |> json.to_string
  http_post(port, "/internal/publish", internal_secret, body)
}

@external(erlang, "gateway_test_ffi", "http_post")
fn http_post(
  port: Int,
  path: String,
  secret: String,
  body: String,
) -> Result(Int, Nil)

fn wait_for(check: fn() -> Bool, timeout_ms: Int) -> Bool {
  wait_for_until(check, now_ms() + timeout_ms)
}

fn wait_for_until(check: fn() -> Bool, deadline: Int) -> Bool {
  case check() {
    True -> True
    False ->
      case now_ms() < deadline {
        True -> {
          process.sleep(25)
          wait_for_until(check, deadline)
        }
        False -> False
      }
  }
}
