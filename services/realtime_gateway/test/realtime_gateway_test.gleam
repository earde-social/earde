import beryl
import gleam/dynamic
import gleam/json
import gleeunit
import realtime_gateway.{PresenceUser, PublishRequest}
import typing.{TypingUser}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn placeholder_test() {
  let topic = "chan:" <> "8"
  assert topic == "chan:8"
}

pub fn unique_presence_users_dedupes_by_user_id_test() {
  let users = [
    PresenceUser(7, "tolwiz"),
    PresenceUser(7, "tolwiz"),
    PresenceUser(3, "ada"),
  ]

  assert realtime_gateway.unique_presence_users(users)
    == [PresenceUser(3, "ada"), PresenceUser(7, "tolwiz")]
}

pub fn unique_presence_users_sorts_case_insensitively_test() {
  let users = [
    PresenceUser(1, "Zed"),
    PresenceUser(2, "ada"),
    PresenceUser(3, "Bea"),
  ]

  assert realtime_gateway.unique_presence_users(users)
    == [PresenceUser(2, "ada"), PresenceUser(3, "Bea"), PresenceUser(1, "Zed")]
}

pub fn unique_presence_users_breaks_username_ties_by_user_id_test() {
  let users = [
    PresenceUser(9, "sam"),
    PresenceUser(4, "SAM"),
  ]

  assert realtime_gateway.unique_presence_users(users)
    == [PresenceUser(4, "SAM"), PresenceUser(9, "sam")]
}

pub fn presence_list_payload_test() {
  let payload =
    realtime_gateway.presence_list_payload([PresenceUser(123, "tolwiz")])

  assert json.to_string(payload)
    == "{\"users\":[{\"user_id\":123,\"username\":\"tolwiz\"}]}"
}

pub fn presence_list_payload_empty_test() {
  assert json.to_string(realtime_gateway.presence_list_payload([]))
    == "{\"users\":[]}"
}

// ── Typing event contract ───────────────────────────────────────────────────

fn typing_payload(v: Int, active: Bool) {
  dynamic.properties([
    #(dynamic.string("v"), dynamic.int(v)),
    #(dynamic.string("active"), dynamic.bool(active)),
  ])
}

pub fn decode_typing_payload_active_test() {
  assert realtime_gateway.decode_typing_payload(typing_payload(1, True))
    == Ok(True)
}

pub fn decode_typing_payload_inactive_test() {
  assert realtime_gateway.decode_typing_payload(typing_payload(1, False))
    == Ok(False)
}

pub fn decode_typing_payload_rejects_unknown_version_test() {
  assert realtime_gateway.decode_typing_payload(typing_payload(2, True))
    == Error(Nil)
}

pub fn decode_typing_payload_rejects_malformed_test() {
  assert realtime_gateway.decode_typing_payload(dynamic.string("nope"))
    == Error(Nil)
  assert realtime_gateway.decode_typing_payload(
      dynamic.properties([#(dynamic.string("active"), dynamic.bool(True))]),
    )
    == Error(Nil)
  assert realtime_gateway.decode_typing_payload(
      dynamic.properties([
        #(dynamic.string("v"), dynamic.int(1)),
        #(dynamic.string("active"), dynamic.string("yes")),
      ]),
    )
    == Error(Nil)
}

pub fn decode_typing_payload_ignores_identity_fields_test() {
  // A client smuggling identity/topic fields cannot influence anything: the
  // decoder only reads v/active, and the handler takes identity and topic
  // from the verified socket assigns.
  let payload =
    dynamic.properties([
      #(dynamic.string("v"), dynamic.int(1)),
      #(dynamic.string("active"), dynamic.bool(True)),
      #(dynamic.string("user_id"), dynamic.int(999)),
      #(dynamic.string("username"), dynamic.string("mallory")),
      #(dynamic.string("topic"), dynamic.string("chan:1")),
    ])
  assert realtime_gateway.decode_typing_payload(payload) == Ok(True)
}

// ── Typing store ────────────────────────────────────────────────────────────

pub fn typing_first_socket_starts_one_user_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)

  assert typing.snapshot(store, "chan:1") == [TypingUser(7, "alice")]
}

pub fn typing_second_socket_same_user_does_not_duplicate_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:1", "s2", 7, "alice", now: 101)

  assert typing.snapshot(store, "chan:1") == [TypingUser(7, "alice")]
}

pub fn typing_one_socket_stopping_keeps_other_active_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:1", "s2", 7, "alice", now: 101)
    |> typing.set_inactive("chan:1", "s1")

  assert typing.snapshot(store, "chan:1") == [TypingUser(7, "alice")]
}

pub fn typing_final_socket_stopping_clears_user_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:1", "s2", 7, "alice", now: 101)
    |> typing.set_inactive("chan:1", "s1")
    |> typing.set_inactive("chan:1", "s2")

  assert typing.snapshot(store, "chan:1") == []
}

pub fn typing_disconnect_clears_socket_state_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:1", "s2", 3, "bob", now: 100)
    |> typing.remove_socket("s1")

  assert typing.snapshot(store, "chan:1") == [TypingUser(3, "bob")]
  assert typing.topics_of_socket(store, "s1") == []
}

pub fn typing_channels_are_independent_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:2", "s2", 3, "bob", now: 100)
    |> typing.set_inactive("chan:1", "s1")

  assert typing.snapshot(store, "chan:1") == []
  assert typing.snapshot(store, "chan:2") == [TypingUser(3, "bob")]
}

pub fn typing_ttl_sweep_removes_stale_entries_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:1", "s2", 3, "bob", now: 105)
    |> typing.sweep(now: 106, ttl: 6)

  // alice (last active 100) is 6s stale at t=106 and swept; bob survives.
  assert typing.snapshot(store, "chan:1") == [TypingUser(3, "bob")]

  let swept = typing.sweep(store, now: 111, ttl: 6)
  assert typing.snapshot(swept, "chan:1") == []
  assert typing.topics(swept) == []
}

pub fn typing_refresh_extends_ttl_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 100)
    |> typing.set_active("chan:1", "s1", 7, "alice", now: 104)
    |> typing.sweep(now: 107, ttl: 6)

  assert typing.snapshot(store, "chan:1") == [TypingUser(7, "alice")]
}

pub fn typing_snapshot_sorts_deterministically_test() {
  let store =
    typing.new()
    |> typing.set_active("chan:1", "s1", 9, "sam", now: 100)
    |> typing.set_active("chan:1", "s2", 2, "ada", now: 100)
    |> typing.set_active("chan:1", "s3", 4, "SAM", now: 100)

  assert typing.snapshot(store, "chan:1")
    == [TypingUser(2, "ada"), TypingUser(4, "SAM"), TypingUser(9, "sam")]
}

pub fn typing_list_payload_test() {
  let payload =
    typing.typing_list_payload([TypingUser(123, "alice"), TypingUser(4, "bob")])

  assert json.to_string(payload)
    == "{\"v\":1,\"users\":[{\"user_id\":123,\"username\":\"alice\"},{\"user_id\":4,\"username\":\"bob\"}]}"
}

pub fn typing_list_payload_empty_test() {
  assert json.to_string(typing.typing_list_payload([]))
    == "{\"v\":1,\"users\":[]}"
}

// ── Rate limiting configuration ─────────────────────────────────────────────

pub fn gateway_config_rate_limits_test() {
  let config = realtime_gateway.gateway_config()

  // Per-channel budget covers typing (client throttles to ~1 push / 2.5s).
  assert beryl.config_channel_rate(config) == 2
  assert beryl.config_channel_burst(config) == 5
  assert beryl.config_join_rate(config) == 2
  assert beryl.config_join_burst(config) == 5
  // The per-socket message rate (10/s, burst 20) has no public getter in
  // beryl; it is exercised via the transport at runtime.
}

// ── new_msg publish contract stays unchanged ────────────────────────────────

pub fn parse_publish_request_new_msg_test() {
  let body =
    "{\"topic\":\"chan:8\",\"event\":\"new_msg\",\"payload\":{"
    <> "\"v\":1,\"type\":\"chat_message_created\",\"id\":42,\"channel_id\":8,"
    <> "\"community_id\":3,\"user_id\":7,\"username\":\"alice\","
    <> "\"content\":\"hi\",\"created_at\":\"2026-07-18 12:00:00\"}}"

  let assert Ok(PublishRequest(topic, event, payload)) =
    realtime_gateway.parse_publish_request(body)

  assert topic == "chan:8"
  assert event == "new_msg"
  assert json.to_string(payload)
    == "{\"v\":1,\"type\":\"chat_message_created\",\"id\":42,\"channel_id\":8,"
    <> "\"community_id\":3,\"user_id\":7,\"username\":\"alice\","
    <> "\"content\":\"hi\",\"created_at\":\"2026-07-18 12:00:00\"}"
}

pub fn parse_publish_request_rejects_malformed_test() {
  assert realtime_gateway.parse_publish_request("{\"topic\":\"chan:8\"}")
    == Error(Nil)
  assert realtime_gateway.parse_publish_request("not json") == Error(Nil)
}
