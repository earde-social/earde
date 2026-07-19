//// Ephemeral, channel-scoped typing state.
////
//// A pure store (all functions take an explicit `now`, so the model is fully
//// unit-testable) wrapped in a small OTP actor that owns the state, sweeps
//// stale entries on a timer, and broadcasts full `typing_list` snapshots via
//// beryl. Typing is advisory: nothing here is durable, a gateway restart
//// clears everything, and no failure in this module may affect chat fanout.

import beryl
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/order
import gleam/otp/actor
import gleam/result
import gleam/string

@external(erlang, "realtime_gateway_ffi", "unix_now")
fn unix_now() -> Int

@external(erlang, "realtime_gateway_ffi", "safely")
fn safely(operation: fn() -> Nil) -> Result(Nil, Nil)

/// Safety-net TTL: a socket that stops sending anything (crashed tab, dead
/// network) is considered no longer typing this many seconds after its last
/// active=true, even if the disconnect is never observed.
pub const ttl_seconds = 6

const sweep_interval_ms = 2000

pub type TypingUser {
  TypingUser(user_id: Int, username: String)
}

type Entry {
  Entry(user_id: Int, username: String, last_active: Int)
}

/// topic -> socket_id -> entry. One entry per active socket, so multi-tab
/// semantics fall out of the shape: a logical user is typing while at least
/// one of their sockets has an entry, and `snapshot` dedupes by user_id.
pub opaque type Store {
  Store(topics: Dict(String, Dict(String, Entry)))
}

pub fn new() -> Store {
  Store(dict.new())
}

pub fn set_active(
  store: Store,
  topic topic: String,
  socket_id socket_id: String,
  user_id user_id: Int,
  username username: String,
  now now: Int,
) -> Store {
  let sockets =
    dict.get(store.topics, topic)
    |> result.unwrap(dict.new())
    |> dict.insert(socket_id, Entry(user_id, username, now))
  Store(dict.insert(store.topics, topic, sockets))
}

pub fn set_inactive(
  store: Store,
  topic topic: String,
  socket_id socket_id: String,
) -> Store {
  case dict.get(store.topics, topic) {
    Error(Nil) -> store
    Ok(sockets) -> put_topic(store, topic, dict.delete(sockets, socket_id))
  }
}

/// Disconnect cleanup: drop this socket's entries from every topic.
pub fn remove_socket(store: Store, socket_id: String) -> Store {
  dict.fold(store.topics, store, fn(acc, topic, sockets) {
    put_topic(acc, topic, dict.delete(sockets, socket_id))
  })
}

/// Topics currently holding an entry for this socket (the only topics whose
/// snapshot can change when the socket goes away).
pub fn topics_of_socket(store: Store, socket_id: String) -> List(String) {
  dict.fold(store.topics, [], fn(acc, topic, sockets) {
    case dict.has_key(sockets, socket_id) {
      True -> [topic, ..acc]
      False -> acc
    }
  })
}

pub fn topics(store: Store) -> List(String) {
  dict.keys(store.topics)
}

/// Drop entries whose last activity is older than `ttl` seconds.
pub fn sweep(store: Store, now now: Int, ttl ttl: Int) -> Store {
  dict.fold(store.topics, store, fn(acc, topic, sockets) {
    let kept = dict.filter(sockets, fn(_, entry) { now - entry.last_active < ttl })
    put_topic(acc, topic, kept)
  })
}

fn put_topic(store: Store, topic: String, sockets: Dict(String, Entry)) -> Store {
  case dict.size(sockets) {
    0 -> Store(dict.delete(store.topics, topic))
    _ -> Store(dict.insert(store.topics, topic, sockets))
  }
}

/// Deduplicated, deterministically ordered typing users for one topic.
/// Ordering matches the presence list: case-insensitive username, then
/// user_id as a tie-break.
pub fn snapshot(store: Store, topic: String) -> List(TypingUser) {
  dict.get(store.topics, topic)
  |> result.unwrap(dict.new())
  |> dict.values
  |> list.fold([], fn(seen, entry: Entry) {
    case
      list.any(seen, fn(kept: TypingUser) { kept.user_id == entry.user_id })
    {
      True -> seen
      False -> [TypingUser(entry.user_id, entry.username), ..seen]
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

pub fn typing_list_payload(users: List(TypingUser)) -> json.Json {
  json.object([
    #("v", json.int(1)),
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

// ── Actor ───────────────────────────────────────────────────────────────────

pub type Msg {
  Active(topic: String, socket_id: String, user_id: Int, username: String)
  Inactive(topic: String, socket_id: String)
  SocketGone(socket_id: String)
  Sweep
}

pub opaque type Typing {
  Typing(subject: Subject(Msg))
}

type ActorState {
  ActorState(store: Store, channels: beryl.Channels, self: Subject(Msg))
}

pub fn start(channels: beryl.Channels) -> Result(Typing, actor.StartError) {
  actor.new_with_initialiser(5000, fn(subject) {
    let _timer = process.send_after(subject, sweep_interval_ms, Sweep)
    actor.initialised(ActorState(new(), channels, subject))
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { Typing(started.data) })
}

pub fn active(
  typing: Typing,
  topic topic: String,
  socket_id socket_id: String,
  user_id user_id: Int,
  username username: String,
) -> Nil {
  process.send(typing.subject, Active(topic, socket_id, user_id, username))
}

pub fn inactive(typing: Typing, topic topic: String, socket_id socket_id: String) -> Nil {
  process.send(typing.subject, Inactive(topic, socket_id))
}

pub fn socket_gone(typing: Typing, socket_id socket_id: String) -> Nil {
  process.send(typing.subject, SocketGone(socket_id))
}

fn handle_message(state: ActorState, msg: Msg) -> actor.Next(ActorState, Msg) {
  case msg {
    Active(topic, socket_id, user_id, username) ->
      actor.continue(
        mutate(state, [topic], fn(store) {
          set_active(store, topic, socket_id, user_id, username, unix_now())
        }),
      )

    Inactive(topic, socket_id) ->
      actor.continue(
        mutate(state, [topic], fn(store) {
          set_inactive(store, topic, socket_id)
        }),
      )

    SocketGone(socket_id) ->
      actor.continue(
        mutate(state, topics_of_socket(state.store, socket_id), fn(store) {
          remove_socket(store, socket_id)
        }),
      )

    Sweep -> {
      let now = unix_now()
      let next =
        mutate(state, topics(state.store), fn(store) {
          sweep(store, now: now, ttl: ttl_seconds)
        })
      let _timer = process.send_after(state.self, sweep_interval_ms, Sweep)
      actor.continue(next)
    }
  }
}

/// Apply a store update and broadcast a fresh snapshot to each listed topic
/// whose visible typing list actually changed — throttled active=true
/// refreshes and no-op removals stay silent.
fn mutate(
  state: ActorState,
  affected_topics: List(String),
  update: fn(Store) -> Store,
) -> ActorState {
  let before =
    list.map(affected_topics, fn(topic) {
      #(topic, snapshot(state.store, topic))
    })
  let store = update(state.store)
  list.each(before, fn(pair) {
    let #(topic, old_snapshot) = pair
    let new_snapshot = snapshot(store, topic)
    case new_snapshot == old_snapshot {
      True -> Nil
      False ->
        case
          safely(fn() {
            beryl.broadcast(
              state.channels,
              topic,
              "typing_list",
              typing_list_payload(new_snapshot),
            )
          })
        {
          Ok(Nil) -> Nil
          Error(Nil) -> Nil
        }
    }
  })
  ActorState(..state, store: store)
}
