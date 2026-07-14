import gleam/json
import gleeunit
import realtime_gateway.{PresenceUser}

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
