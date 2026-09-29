(* Production Argon2id cost. Named so the login dummy-hash fixture can be
   checked against exactly these values: a cheaper dummy would make a
   missing account measurably faster to reject than a wrong password. *)
let t_cost = 2
let m_cost = 65536
let parallelism = 1

let hash_password password =
  let salt = Dream.random 16 in
  let result =
    Argon2.hash
      ~t_cost
      ~m_cost
      ~parallelism
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
