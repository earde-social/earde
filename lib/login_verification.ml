type verifier = password:string -> hash:string -> bool Lwt.t

let argon2_verifier ~password ~hash =
  match%lwt Auth.verify_password ~password ~hash with
  | Ok true -> Lwt.return true
  | Ok false | Error _ -> Lwt.return false

(* Fixed rather than generated at startup or on the first miss: a first-miss
   generation would itself be a one-off timing signal, and a fixture keeps
   startup free of a 64 MiB hash. It carries no secret — see the .mli. *)
let dummy_password = "earde-login-dummy-verification-not-a-credential"

let dummy_hash =
  "$argon2id$v=19$m=65536,t=2,p=1$XCNXzKZa6nueVtfSPY7SOw$OCGq7tTVRXoGIxrwFJ0iRUvQDDmxyxdEqadJXQDAtvk"

let verified ~verify ~password ~hash =
  Lwt.catch (fun () -> verify ~password ~hash) (fun _ -> Lwt.return false)

let authenticate ~verify ~password = function
  | Some (hash, account) ->
      let%lwt ok = verified ~verify ~password ~hash in
      Lwt.return (if ok then Some account else None)
  | None ->
      (* The result is discarded by construction: this branch has no account
         to return, so even a dummy match cannot authenticate anyone. *)
      let%lwt (_ : bool) = verified ~verify ~password ~hash:dummy_hash in
      Lwt.return None
