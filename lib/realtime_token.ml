let secret_env = "REALTIME_TOKEN_SECRET"

let getenv_nonempty name =
  match Sys.getenv_opt name with
  | None -> None
  | Some value ->
      let value = String.trim value in
      if value = "" then None else Some value

let base64url_of_string value =
  let encoded = Base64.encode_exn value in
  encoded
  |> String.map (function
       | '+' -> '-'
       | '/' -> '_'
       | c -> c)
  |> fun s ->
  let rec strip_padding s =
    let len = String.length s in
    if len > 0 && s.[len - 1] = '=' then
      strip_padding (String.sub s 0 (len - 1))
    else
      s
  in
  strip_padding s

let hmac_sha256_base64url ~secret value =
  Digestif.SHA256.hmac_string ~key:secret value
  |> Digestif.SHA256.to_raw_string
  |> base64url_of_string

let unix_now () =
  Unix.time () |> int_of_float

let payload_json ~user_id ~username ~topic ~ttl_seconds =
  let exp = unix_now () + ttl_seconds in
  `Assoc
    [ ("v", `Int 1)
    ; ("user_id", `Int user_id)
    ; ("username", `String username)
    ; ("topic", `String topic)
    ; ("exp", `Int exp)
    ]

let sign ~secret payload_json =
  let payload =
    payload_json
    |> Yojson.Safe.to_string
    |> base64url_of_string
  in
  let signature = hmac_sha256_base64url ~secret payload in
  payload ^ "." ^ signature

let create_for_topic ~user_id ~username ~topic =
  match getenv_nonempty secret_env with
  | None -> None
  | Some secret ->
      payload_json ~user_id ~username ~topic ~ttl_seconds:3600
      |> sign ~secret
      |> Option.some
