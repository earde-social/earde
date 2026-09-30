//// Ephemeral, channel-scoped shared-cursor state.
////
//// Same shape as `typing`: a pure store (every function takes an explicit
//// `now` in milliseconds, so the model is fully unit-testable) wrapped in a
//// small OTP actor that owns the state, sweeps stale entries on a timer, and
//// broadcasts per-user `cursor` deltas via beryl. Cursors are advisory:
//// nothing here is durable, a gateway restart clears everything, and no
//// failure in this module may affect chat fanout, presence, or typing.
////
//// Unlike typing (rare changes, full-snapshot broadcasts), cursor updates
//// arrive at ~10/s per user, so the actor broadcasts per-user deltas and
//// diffs against the last broadcast state — non-owner movement, duplicate
//// positions, and no-op removals stay silent.

import beryl
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/order
import gleam/otp/actor
import gleam/result
import gleam/string

@external(erlang, "realtime_gateway_ffi", "unix_now_ms")
fn unix_now_ms() -> Int

@external(erlang, "realtime_gateway_ffi", "safely")
fn safely(operation: fn() -> Nil) -> Result(Nil, Nil)

/// A pointer that stops moving is gone after this long: the client sends no
/// keep-alive for a stationary cursor, so TTL expiry is the normal way an
/// idle cursor disappears (not just a crash safety net).
pub const ttl_ms = 3000

pub const sweep_interval_ms = 1000

/// Hard cap on socket entries per topic. At capacity, updates to existing
/// entries and all cleanup keep working; only brand-new active entries are
/// ignored until capacity frees up.
pub const max_entries_per_topic = 256

/// Cursor-specific flood budget, per socket entry. Applies only to active
/// movement updates — inactive events, disconnect cleanup, and TTL expiry
/// are never rate limited. The client sends at most 10/s, so ordinary
/// traffic never hits this.
pub const rate_per_second = 12

pub const rate_burst = 15

// ── Token bucket (pure, millisecond clock) ──────────────────────────────────

/// Tokens are stored in millitokens (1 token = 1000) so refill arithmetic
/// stays in integers: `rate_per_second` millitokens accrue per millisecond.
type Bucket {
  Bucket(millitokens: Int, last_refill_ms: Int)
}

fn new_bucket(now: Int) -> Bucket {
  Bucket(millitokens: rate_burst * 1000, last_refill_ms: now)
}

fn bucket_take(bucket: Bucket, now: Int) -> #(Bucket, Result(Nil, Nil)) {
  let elapsed = int.max(now - bucket.last_refill_ms, 0)
  let refilled =
    int.min(bucket.millitokens + elapsed * rate_per_second, rate_burst * 1000)
  let bucket = Bucket(millitokens: refilled, last_refill_ms: now)
  case refilled >= 1000 {
    True -> #(Bucket(..bucket, millitokens: refilled - 1000), Ok(Nil))
    False -> #(bucket, Error(Nil))
  }
}

// ── Store ───────────────────────────────────────────────────────────────────

type Entry {
  Entry(
    user_id: Int,
    username: String,
    x: Float,
    y: Float,
    last_active: Int,
    bucket: Bucket,
  )
}

/// topic -> socket_id -> entry. One entry per active socket; the visible
/// cursor per user is derived (`snapshot`), never stored, so multi-tab
/// ownership and fallback fall out of the derivation.
pub opaque type Store {
  Store(topics: Dict(String, Dict(String, Entry)))
}

pub fn new() -> Store {
  Store(dict.new())
}

/// Record an active movement update. Drops the update (returning the store
/// otherwise unchanged) when the per-entry flood bucket is empty, or when the
/// topic is at capacity and this socket has no entry yet.
pub fn set_active(
  store: Store,
  topic topic: String,
  socket_id socket_id: String,
  user_id user_id: Int,
  username username: String,
  x x: Float,
  y y: Float,
  now now: Int,
) -> Store {
  let sockets = dict.get(store.topics, topic) |> result.unwrap(dict.new())
  case dict.get(sockets, socket_id) {
    Ok(entry) -> {
      case bucket_take(entry.bucket, now) {
        #(bucket, Ok(Nil)) ->
          put_entry(
            store,
            topic,
            sockets,
            socket_id,
            Entry(user_id, username, x, y, now, bucket),
          )
        // Rate limited: keep the old position and last_active, but persist
        // the refilled bucket so the budget clock stays honest.
        #(bucket, Error(Nil)) ->
          put_entry(store, topic, sockets, socket_id, Entry(..entry, bucket:))
      }
    }
    Error(Nil) ->
      case dict.size(sockets) >= max_entries_per_topic {
        True -> store
        False -> {
          let #(bucket, _) = bucket_take(new_bucket(now), now)
          put_entry(
            store,
            topic,
            sockets,
            socket_id,
            Entry(user_id, username, x, y, now, bucket),
          )
        }
      }
  }
}

fn put_entry(
  store: Store,
  topic: String,
  sockets: Dict(String, Entry),
  socket_id: String,
  entry: Entry,
) -> Store {
  let sockets = dict.insert(sockets, socket_id, entry)
  Store(dict.insert(store.topics, topic, sockets))
}

/// Explicit deactivation — never rate limited or capacity checked.
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

/// Topics currently holding an entry for this socket.
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

/// Drop entries whose last accepted movement is older than `ttl_ms`.
pub fn sweep(store: Store, now now: Int) -> Store {
  dict.fold(store.topics, store, fn(acc, topic, sockets) {
    let kept =
      dict.filter(sockets, fn(_, entry) { now - entry.last_active < ttl_ms })
    put_topic(acc, topic, kept)
  })
}

fn put_topic(
  store: Store,
  topic: String,
  sockets: Dict(String, Entry),
) -> Store {
  case dict.size(sockets) {
    0 -> Store(dict.delete(store.topics, topic))
    _ -> Store(dict.insert(store.topics, topic, sockets))
  }
}

// ── Derived visible cursors ─────────────────────────────────────────────────

pub type Cursor {
  Cursor(user_id: Int, username: String, x: Float, y: Float)
}

/// One visible cursor per user: the freshest still-fresh socket entry wins,
/// ordered by last_active then socket_id (deterministic tie-break). Filtering
/// by TTL here — not only in `sweep` — is what makes a stale background tab
/// unable to reappear as owner between sweeps.
pub fn snapshot(store: Store, topic: String, now now: Int) -> List(Cursor) {
  dict.get(store.topics, topic)
  |> result.unwrap(dict.new())
  |> dict.fold([], fn(acc, socket_id, entry) {
    case now - entry.last_active < ttl_ms {
      True -> [#(socket_id, entry), ..acc]
      False -> acc
    }
  })
  |> list.fold(dict.new(), fn(owners, candidate) {
    let #(socket_id, entry) = candidate
    case dict.get(owners, entry.user_id) {
      Error(Nil) -> dict.insert(owners, entry.user_id, candidate)
      Ok(#(kept_socket_id, kept)) -> {
        let wins = case int.compare(entry.last_active, kept.last_active) {
          order.Gt -> True
          order.Lt -> False
          order.Eq -> string.compare(socket_id, kept_socket_id) == order.Gt
        }
        case wins {
          True -> dict.insert(owners, entry.user_id, candidate)
          False -> owners
        }
      }
    }
  })
  |> dict.values
  |> list.map(fn(candidate) {
    let #(_, entry) = candidate
    Cursor(entry.user_id, entry.username, entry.x, entry.y)
  })
  |> list.sort(fn(a, b) { int.compare(a.user_id, b.user_id) })
}

/// What must be broadcast to move clients from `before` to `after`.
/// Deterministic order: removals first, then updates, each sorted by user_id.
pub type Change {
  Moved(cursor: Cursor)
  Gone(user_id: Int)
}

pub fn diff(before: List(Cursor), after: List(Cursor)) -> List(Change) {
  let gone =
    before
    |> list.filter(fn(old: Cursor) {
      !list.any(after, fn(new: Cursor) { new.user_id == old.user_id })
    })
    |> list.map(fn(old: Cursor) { Gone(old.user_id) })
  let moved =
    after
    |> list.filter(fn(new) { !list.contains(before, new) })
    |> list.map(Moved)
  list.append(gone, moved)
}

pub fn moved_payload(cursor: Cursor) -> json.Json {
  json.object([
    #("v", json.int(1)),
    #("user_id", json.int(cursor.user_id)),
    #("username", json.string(cursor.username)),
    #("active", json.bool(True)),
    #("x", json.float(cursor.x)),
    #("y", json.float(cursor.y)),
  ])
}

pub fn gone_payload(user_id: Int) -> json.Json {
  json.object([
    #("v", json.int(1)),
    #("user_id", json.int(user_id)),
    #("active", json.bool(False)),
  ])
}

fn change_payload(change: Change) -> json.Json {
  case change {
    Moved(cursor) -> moved_payload(cursor)
    Gone(user_id) -> gone_payload(user_id)
  }
}

// ── Actor ───────────────────────────────────────────────────────────────────

pub type Msg {
  Active(
    topic: String,
    socket_id: String,
    user_id: Int,
    username: String,
    x: Float,
    y: Float,
  )
  Inactive(topic: String, socket_id: String)
  SocketGone(socket_id: String)
  Sweep
}

pub opaque type Cursors {
  Cursors(subject: Subject(Msg))
}

/// The actor's process, for failure-injection tests.
pub fn owner(cursors: Cursors) -> Result(Pid, Nil) {
  process.subject_owner(cursors.subject)
}

/// `visible` is the per-topic snapshot as last broadcast. Diffing against it
/// (rather than the pre-mutation store) is what makes TTL expiry between
/// events still produce a Gone broadcast at sweep time.
type ActorState {
  ActorState(
    store: Store,
    visible: Dict(String, List(Cursor)),
    channels: beryl.Channels,
    self: Subject(Msg),
  )
}

pub fn start(channels: beryl.Channels) -> Result(Cursors, actor.StartError) {
  actor.new_with_initialiser(5000, fn(subject) {
    let _timer = process.send_after(subject, sweep_interval_ms, Sweep)
    actor.initialised(ActorState(new(), dict.new(), channels, subject))
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { Cursors(started.data) })
}

pub fn active(
  cursors: Cursors,
  topic topic: String,
  socket_id socket_id: String,
  user_id user_id: Int,
  username username: String,
  x x: Float,
  y y: Float,
) -> Nil {
  process.send(
    cursors.subject,
    Active(topic, socket_id, user_id, username, x, y),
  )
}

pub fn inactive(
  cursors: Cursors,
  topic topic: String,
  socket_id socket_id: String,
) -> Nil {
  process.send(cursors.subject, Inactive(topic, socket_id))
}

pub fn socket_gone(cursors: Cursors, socket_id socket_id: String) -> Nil {
  process.send(cursors.subject, SocketGone(socket_id))
}

fn handle_message(state: ActorState, msg: Msg) -> actor.Next(ActorState, Msg) {
  let now = unix_now_ms()
  case msg {
    Active(topic, socket_id, user_id, username, x, y) ->
      actor.continue(
        mutate(state, [topic], now, fn(store) {
          set_active(store, topic, socket_id, user_id, username, x, y, now)
        }),
      )

    Inactive(topic, socket_id) ->
      actor.continue(
        mutate(state, [topic], now, fn(store) {
          set_inactive(store, topic, socket_id)
        }),
      )

    SocketGone(socket_id) ->
      actor.continue(
        mutate(state, topics_of_socket(state.store, socket_id), now, fn(store) {
          remove_socket(store, socket_id)
        }),
      )

    Sweep -> {
      let next =
        mutate(state, topics(state.store), now, fn(store) {
          sweep(store, now: now)
        })
      let _timer = process.send_after(state.self, sweep_interval_ms, Sweep)
      actor.continue(next)
    }
  }
}

/// Apply a store update, broadcast the delta between what clients were last
/// told and the new derived snapshot, and remember the new snapshot.
fn mutate(
  state: ActorState,
  affected_topics: List(String),
  now: Int,
  update: fn(Store) -> Store,
) -> ActorState {
  let store = update(state.store)
  let visible =
    list.fold(affected_topics, state.visible, fn(visible, topic) {
      let before = dict.get(visible, topic) |> result.unwrap([])
      let after = snapshot(store, topic, now)
      list.each(diff(before, after), fn(change) {
        case
          safely(fn() {
            beryl.broadcast(
              state.channels,
              topic,
              "cursor",
              change_payload(change),
            )
          })
        {
          Ok(Nil) -> Nil
          Error(Nil) -> Nil
        }
      })
      case after {
        [] -> dict.delete(visible, topic)
        _ -> dict.insert(visible, topic, after)
      }
    })
  ActorState(..state, store:, visible:)
}
