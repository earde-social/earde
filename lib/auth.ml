let hash_password password =
  let salt = Dream.random 16 in
  let result =
    Argon2.hash
      ~t_cost:2
      ~m_cost:65536
      ~parallelism:1
      ~pwd:password
      ~salt:salt
      ~kind:Argon2.ID
      ~hash_len:32
      ~encoded_len:128
      ~version:Argon2.VERSION_13
  in
  match result with
  | Ok (_, encoded) -> Lwt.return (Ok encoded)
  | Error _ -> Lwt.return (Error "Failed to hash password")

let verify_password ~password ~hash =
  let result = 
    Argon2.verify 
      ~pwd:password 
      ~encoded:hash
      ~kind:Argon2.ID
  in
  match result with
  | Ok true -> Lwt.return (Ok true)
  | _ -> Lwt.return (Error "Invalid password")
