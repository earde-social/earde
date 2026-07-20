import beryl
import cursors.{type Cursor, Cursor, Gone, Moved}
import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleeunit
import realtime_gateway.{
  AuthClaims, CursorActive, CursorInactive, PresenceUser, PublishRequest,
}
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

  // Per-channel budget is shared by typing (~1 push / 2.5s) and cursor
  // movement (client-throttled to 10/s); the cursors store adds its own
  // movement-only 12/s limiter on top.
  assert beryl.config_channel_rate(config) == 15
  assert beryl.config_channel_burst(config) == 30
  assert beryl.config_join_rate(config) == 2
  assert beryl.config_join_burst(config) == 5
  // Legitimate inbound frames are tiny; oversized ones are dropped before
  // decoding and cannot affect presence, typing, or new_msg fanout.
  assert beryl.config_max_inbound_frame_bytes(config) == 4096
  // The per-socket message rate (20/s, burst 40) has no public getter in
  // beryl; it is exercised via the transport at runtime.
}

// ── Cursor event contract ───────────────────────────────────────────────────

fn cursor_active_payload(v: Int, x: dynamic.Dynamic, y: dynamic.Dynamic) {
  dynamic.properties([
    #(dynamic.string("v"), dynamic.int(v)),
    #(dynamic.string("active"), dynamic.bool(True)),
    #(dynamic.string("x"), x),
    #(dynamic.string("y"), y),
  ])
}

pub fn decode_cursor_payload_active_test() {
  assert realtime_gateway.decode_cursor_payload(cursor_active_payload(
      1,
      dynamic.float(0.42),
      dynamic.float(0.71),
    ))
    == Ok(CursorActive(0.42, 0.71))
}

pub fn decode_cursor_payload_inactive_test() {
  // Deactivation carries no coordinates and must decode without them.
  assert realtime_gateway.decode_cursor_payload(
      dynamic.properties([
        #(dynamic.string("v"), dynamic.int(1)),
        #(dynamic.string("active"), dynamic.bool(False)),
      ]),
    )
    == Ok(CursorInactive)
}

pub fn decode_cursor_payload_accepts_integer_edge_coordinates_test() {
  // JSON 0 and 1 arrive as Erlang integers, not floats; both edges are valid.
  assert realtime_gateway.decode_cursor_payload(cursor_active_payload(
      1,
      dynamic.int(0),
      dynamic.int(1),
    ))
    == Ok(CursorActive(0.0, 1.0))
}

pub fn decode_cursor_payload_clamps_out_of_range_test() {
  assert realtime_gateway.decode_cursor_payload(cursor_active_payload(
      1,
      dynamic.float(1.5),
      dynamic.float(-0.25),
    ))
    == Ok(CursorActive(1.0, 0.0))
}

pub fn decode_cursor_payload_rejects_unknown_version_test() {
  assert realtime_gateway.decode_cursor_payload(cursor_active_payload(
      2,
      dynamic.float(0.5),
      dynamic.float(0.5),
    ))
    == Error(Nil)
}

pub fn decode_cursor_payload_rejects_malformed_test() {
  assert realtime_gateway.decode_cursor_payload(dynamic.string("nope"))
    == Error(Nil)
  // Missing active flag.
  assert realtime_gateway.decode_cursor_payload(
      dynamic.properties([#(dynamic.string("v"), dynamic.int(1))]),
    )
    == Error(Nil)
  // Non-boolean active flag.
  assert realtime_gateway.decode_cursor_payload(
      dynamic.properties([
        #(dynamic.string("v"), dynamic.int(1)),
        #(dynamic.string("active"), dynamic.string("yes")),
      ]),
    )
    == Error(Nil)
}

pub fn decode_cursor_payload_rejects_missing_or_non_numeric_coordinates_test() {
  // Active update without coordinates.
  assert realtime_gateway.decode_cursor_payload(
      dynamic.properties([
        #(dynamic.string("v"), dynamic.int(1)),
        #(dynamic.string("active"), dynamic.bool(True)),
      ]),
    )
    == Error(Nil)
  // Non-numeric coordinate.
  assert realtime_gateway.decode_cursor_payload(cursor_active_payload(
      1,
      dynamic.string("0.5"),
      dynamic.float(0.5),
    ))
    == Error(Nil)
}

pub fn decode_cursor_payload_ignores_identity_fields_test() {
  // A client smuggling identity/topic/color fields cannot influence anything:
  // the decoder only reads v/active/x/y, and the handler takes identity and
  // topic from the verified socket assigns.
  let payload =
    dynamic.properties([
      #(dynamic.string("v"), dynamic.int(1)),
      #(dynamic.string("active"), dynamic.bool(True)),
      #(dynamic.string("x"), dynamic.float(0.5)),
      #(dynamic.string("y"), dynamic.float(0.5)),
      #(dynamic.string("user_id"), dynamic.int(999)),
      #(dynamic.string("username"), dynamic.string("mallory")),
      #(dynamic.string("topic"), dynamic.string("chan:1")),
      #(dynamic.string("socket_id"), dynamic.string("s99")),
      #(dynamic.string("color"), dynamic.string("#ff0000")),
    ])
  assert realtime_gateway.decode_cursor_payload(payload)
    == Ok(CursorActive(0.5, 0.5))
}

pub fn cursor_moved_payload_test() {
  assert json.to_string(cursors.moved_payload(Cursor(123, "alice", 0.5, 0.25)))
    == "{\"v\":1,\"user_id\":123,\"username\":\"alice\",\"active\":true,"
    <> "\"x\":0.5,\"y\":0.25}"
}

pub fn cursor_gone_payload_test() {
  assert json.to_string(cursors.gone_payload(123))
    == "{\"v\":1,\"user_id\":123,\"active\":false}"
}

// ── Signed-token capability claim ───────────────────────────────────────────

pub fn parse_claims_json_with_capability_test() {
  let assert Ok(claims) =
    realtime_gateway.parse_claims_json(
      "{\"v\":1,\"user_id\":7,\"username\":\"alice\",\"topic\":\"chan:8\","
      <> "\"exp\":99,\"shared_cursors\":true}",
    )
  assert claims == AuthClaims(7, "alice", "chan:8", 99, True)
}

pub fn parse_claims_json_without_capability_test() {
  let assert Ok(claims) =
    realtime_gateway.parse_claims_json(
      "{\"v\":1,\"user_id\":7,\"username\":\"alice\",\"topic\":\"chan:8\","
      <> "\"exp\":99,\"shared_cursors\":false}",
    )
  assert claims == AuthClaims(7, "alice", "chan:8", 99, False)
}

pub fn parse_claims_json_legacy_token_defaults_false_test() {
  // Tokens minted before the capability existed still authenticate (presence,
  // typing and chat keep working) but can never share cursors.
  let assert Ok(claims) =
    realtime_gateway.parse_claims_json(
      "{\"v\":1,\"user_id\":7,\"username\":\"alice\",\"topic\":\"chan:8\",\"exp\":99}",
    )
  assert claims == AuthClaims(7, "alice", "chan:8", 99, False)
}

pub fn parse_claims_json_rejects_malformed_test() {
  assert realtime_gateway.parse_claims_json("{\"v\":1}") == Error(Nil)
  assert realtime_gateway.parse_claims_json("not json") == Error(Nil)
}

pub fn authorize_cursor_event_allows_active_with_capability_test() {
  assert realtime_gateway.authorize_cursor_event(
      shared_cursors: True,
      event: CursorActive(0.5, 0.5),
    )
    == Ok(CursorActive(0.5, 0.5))
}

pub fn authorize_cursor_event_blocks_active_without_capability_test() {
  // No state, no broadcast: the event dies before reaching the cursor store.
  assert realtime_gateway.authorize_cursor_event(
      shared_cursors: False,
      event: CursorActive(0.5, 0.5),
    )
    == Error(Nil)
}

pub fn authorize_cursor_event_inactive_is_harmless_cleanup_test() {
  assert realtime_gateway.authorize_cursor_event(
      shared_cursors: False,
      event: CursorInactive,
    )
    == Ok(CursorInactive)
  assert realtime_gateway.authorize_cursor_event(
      shared_cursors: True,
      event: CursorInactive,
    )
    == Ok(CursorInactive)
}

pub fn forged_payload_capability_cannot_override_token_test() {
  // A client smuggling shared_cursors:true into the event payload changes
  // nothing: the payload decoder ignores the field and authorization reads
  // only the verified token claim.
  let payload =
    dynamic.properties([
      #(dynamic.string("v"), dynamic.int(1)),
      #(dynamic.string("active"), dynamic.bool(True)),
      #(dynamic.string("x"), dynamic.float(0.5)),
      #(dynamic.string("y"), dynamic.float(0.5)),
      #(dynamic.string("shared_cursors"), dynamic.bool(True)),
    ])
  let assert Ok(event) = realtime_gateway.decode_cursor_payload(payload)
  assert realtime_gateway.authorize_cursor_event(
      shared_cursors: False,
      event: event,
    )
    == Error(Nil)
}

// ── Cursor store: ownership ─────────────────────────────────────────────────

pub fn cursor_first_socket_creates_one_cursor_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.2, now: 1000)

  assert cursors.snapshot(store, "chan:1", now: 1000)
    == [Cursor(7, "alice", 0.1, 0.2)]
}

pub fn cursor_second_socket_same_user_does_not_duplicate_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 7, "alice", x: 0.2, y: 0.2, now: 1100)

  // One logical cursor per user; the freshest socket (s2) owns it.
  assert cursors.snapshot(store, "chan:1", now: 1100)
    == [Cursor(7, "alice", 0.2, 0.2)]
}

pub fn cursor_latest_moving_socket_becomes_owner_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 7, "alice", x: 0.2, y: 0.2, now: 1100)
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.3, y: 0.3, now: 1200)

  // s1 moved last, so ownership switches back to s1.
  assert cursors.snapshot(store, "chan:1", now: 1200)
    == [Cursor(7, "alice", 0.3, 0.3)]
}

pub fn cursor_ownership_tie_breaks_by_socket_id_test() {
  // Identical last_active: the greater socket_id wins, deterministically,
  // regardless of insertion order.
  let a =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 7, "alice", x: 0.2, y: 0.2, now: 1000)
  let b =
    cursors.new()
    |> cursors.set_active("chan:1", "s2", 7, "alice", x: 0.2, y: 0.2, now: 1000)
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)

  assert cursors.snapshot(a, "chan:1", now: 1000)
    == [Cursor(7, "alice", 0.2, 0.2)]
  assert cursors.snapshot(b, "chan:1", now: 1000)
    == [Cursor(7, "alice", 0.2, 0.2)]
}

pub fn cursor_owner_disconnect_falls_back_to_fresh_socket_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 7, "alice", x: 0.2, y: 0.2, now: 1500)
    |> cursors.remove_socket("s2")

  // s1 is 1s old at now=2500 — still within the 3s TTL, so it takes over.
  assert cursors.snapshot(store, "chan:1", now: 2500)
    == [Cursor(7, "alice", 0.1, 0.1)]
}

pub fn cursor_no_fallback_to_stale_socket_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 7, "alice", x: 0.2, y: 0.2, now: 3500)
    |> cursors.remove_socket("s2")

  // s1's entry is 3.5s old at now=4500 (past the 3s TTL) and may still be in
  // the store between sweeps — a stale background tab must never reappear.
  assert cursors.snapshot(store, "chan:1", now: 4500) == []
}

pub fn cursor_inactive_removes_entry_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_inactive("chan:1", "s1")

  assert cursors.snapshot(store, "chan:1", now: 1000) == []
  assert cursors.topics(store) == []
}

pub fn cursor_disconnect_clears_socket_state_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 3, "bob", x: 0.2, y: 0.2, now: 1000)
    |> cursors.remove_socket("s1")

  assert cursors.snapshot(store, "chan:1", now: 1000)
    == [Cursor(3, "bob", 0.2, 0.2)]
  assert cursors.topics_of_socket(store, "s1") == []
}

pub fn cursor_channels_are_independent_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:2", "s2", 3, "bob", x: 0.2, y: 0.2, now: 1000)
    |> cursors.set_inactive("chan:1", "s1")

  assert cursors.snapshot(store, "chan:1", now: 1000) == []
  assert cursors.snapshot(store, "chan:2", now: 1000)
    == [Cursor(3, "bob", 0.2, 0.2)]
}

pub fn cursor_snapshot_sorts_by_user_id_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 9, "sam", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 2, "ada", x: 0.2, y: 0.2, now: 1000)
    |> cursors.set_active("chan:1", "s3", 4, "bea", x: 0.3, y: 0.3, now: 1000)

  assert cursors.snapshot(store, "chan:1", now: 1000)
    == [
      Cursor(2, "ada", 0.2, 0.2),
      Cursor(4, "bea", 0.3, 0.3),
      Cursor(9, "sam", 0.1, 0.1),
    ]
}

// ── Cursor store: TTL ───────────────────────────────────────────────────────

pub fn cursor_ttl_sweep_removes_stale_entries_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s2", 3, "bob", x: 0.2, y: 0.2, now: 2500)
    |> cursors.sweep(now: 4000)

  // alice (last active 1000) is 3s stale at t=4000 and swept; bob survives.
  assert cursors.snapshot(store, "chan:1", now: 4000)
    == [Cursor(3, "bob", 0.2, 0.2)]

  let swept = cursors.sweep(store, now: 5500)
  assert cursors.snapshot(swept, "chan:1", now: 5500) == []
  assert cursors.topics(swept) == []
}

pub fn cursor_movement_extends_ttl_test() {
  let store =
    cursors.new()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.1, y: 0.1, now: 1000)
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.2, y: 0.2, now: 3500)
    |> cursors.sweep(now: 4500)

  assert cursors.snapshot(store, "chan:1", now: 4500)
    == [Cursor(7, "alice", 0.2, 0.2)]
}

// ── Cursor store: capacity ──────────────────────────────────────────────────

fn full_topic() -> cursors.Store {
  int.range(
    from: 1,
    to: cursors.max_entries_per_topic + 1,
    with: cursors.new(),
    run: fn(store, i) {
      cursors.set_active(
        store,
        "chan:1",
        "s" <> int.to_string(i),
        i,
        "user" <> int.to_string(i),
        x: 0.5,
        y: 0.5,
        now: 1000,
      )
    },
  )
}

pub fn cursor_topic_at_capacity_ignores_new_entries_test() {
  let store =
    full_topic()
    |> cursors.set_active("chan:1", "s-extra", 999, "eve", x: 0.5, y: 0.5, now: 1001)

  assert list.length(cursors.snapshot(store, "chan:1", now: 1001))
    == cursors.max_entries_per_topic
  assert cursors.topics_of_socket(store, "s-extra") == []
}

pub fn cursor_topic_at_capacity_accepts_updates_to_existing_entries_test() {
  let store =
    full_topic()
    |> cursors.set_active("chan:1", "s1", 1, "user1", x: 0.9, y: 0.9, now: 1200)

  let assert Ok(updated) =
    cursors.snapshot(store, "chan:1", now: 1200)
    |> list.find(fn(cursor: Cursor) { cursor.user_id == 1 })
  assert updated == Cursor(1, "user1", 0.9, 0.9)
}

pub fn cursor_topic_at_capacity_still_processes_inactive_test() {
  let store =
    full_topic()
    |> cursors.set_inactive("chan:1", "s1")

  assert list.length(cursors.snapshot(store, "chan:1", now: 1000))
    == cursors.max_entries_per_topic - 1

  // Freed capacity is usable again.
  let store =
    cursors.set_active(store, "chan:1", "s-new", 999, "eve", x: 0.5, y: 0.5, now: 1001)
  assert list.length(cursors.snapshot(store, "chan:1", now: 1001))
    == cursors.max_entries_per_topic
}

pub fn cursor_topic_at_capacity_still_processes_disconnect_and_sweep_test() {
  let after_disconnect = full_topic() |> cursors.remove_socket("s1")
  assert list.length(cursors.snapshot(after_disconnect, "chan:1", now: 1000))
    == cursors.max_entries_per_topic - 1

  let after_sweep = full_topic() |> cursors.sweep(now: 4000)
  assert cursors.snapshot(after_sweep, "chan:1", now: 4000) == []
  assert cursors.topics(after_sweep) == []
}

// ── Cursor store: movement-only rate limiting ───────────────────────────────

/// Exhaust s1's flood bucket at a fixed instant: the burst allows exactly
/// `rate_burst` accepted movements, so the last accepted x is rate_burst/100.
fn flooded_store() -> cursors.Store {
  int.range(
    from: 1,
    to: cursors.rate_burst + 6,
    with: cursors.new(),
    run: fn(store, i) {
      cursors.set_active(
        store,
        "chan:1",
        "s1",
        7,
        "alice",
        x: int.to_float(i) /. 100.0,
        y: 0.5,
        now: 1000,
      )
    },
  )
}

pub fn cursor_rate_limited_movement_is_dropped_test() {
  // Updates beyond the burst are dropped: the position freezes at the last
  // accepted movement instead of the last sent one.
  let last_accepted = int.to_float(cursors.rate_burst) /. 100.0
  assert cursors.snapshot(flooded_store(), "chan:1", now: 1000)
    == [Cursor(7, "alice", last_accepted, 0.5)]
}

pub fn cursor_rate_limiter_recovers_over_time_test() {
  // 1 second later the bucket has refilled; movement is accepted again.
  let store =
    flooded_store()
    |> cursors.set_active("chan:1", "s1", 7, "alice", x: 0.9, y: 0.9, now: 2000)

  assert cursors.snapshot(store, "chan:1", now: 2000)
    == [Cursor(7, "alice", 0.9, 0.9)]
}

pub fn cursor_inactive_bypasses_rate_limiter_test() {
  // Deactivation must work even with an empty movement bucket.
  let store = flooded_store() |> cursors.set_inactive("chan:1", "s1")
  assert cursors.snapshot(store, "chan:1", now: 1000) == []
}

pub fn cursor_disconnect_and_sweep_bypass_rate_limiter_test() {
  let after_disconnect = flooded_store() |> cursors.remove_socket("s1")
  assert cursors.snapshot(after_disconnect, "chan:1", now: 1000) == []

  let after_sweep = flooded_store() |> cursors.sweep(now: 4000)
  assert cursors.snapshot(after_sweep, "chan:1", now: 4000) == []
}

// ── Cursor diff (broadcast coalescing model) ────────────────────────────────

pub fn cursor_diff_emits_removals_then_updates_test() {
  let before = [Cursor(1, "ada", 0.1, 0.1), Cursor(2, "bob", 0.2, 0.2)]
  let after = [Cursor(2, "bob", 0.25, 0.2), Cursor(3, "cyd", 0.3, 0.3)]

  assert cursors.diff(before, after)
    == [
      Gone(1),
      Moved(Cursor(2, "bob", 0.25, 0.2)),
      Moved(Cursor(3, "cyd", 0.3, 0.3)),
    ]
}

pub fn cursor_diff_unchanged_snapshot_is_silent_test() {
  // Non-owner movement and no-op removals leave the derived snapshot equal,
  // so nothing is broadcast.
  let snapshot = [Cursor(1, "ada", 0.1, 0.1)]
  assert cursors.diff(snapshot, snapshot) == []
  assert cursors.diff([], []) == []
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
