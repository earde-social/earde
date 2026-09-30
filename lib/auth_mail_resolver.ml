type 'endp t = {
  permits : int;
  run : Uri.t -> 'endp Lwt.t;
  mutable outstanding : int;
}

let default_permits = 2

let create ~permits ~resolve =
  if permits <= 0 then
    invalid_arg "Auth_mail_resolver.create: permits must be positive";
  { permits; run = resolve; outstanding = 0 }

let outstanding t = t.outstanding

let resolve t uri =
  if t.outstanding >= t.permits then Lwt.return (Error `Busy)
  else begin
    t.outstanding <- t.outstanding + 1;
    let released = ref false in
    let release () =
      if not !released then begin
        released := true;
        t.outstanding <- t.outstanding - 1
      end
    in
    let physical = Lwt.apply t.run uri in
    (* The permit follows the physical promise alone. This callback captures
       nothing but the counter, so an abandoned lookup keeps no caller state
       (message, payload, key) alive while the OS finishes it. *)
    Lwt.on_termination physical release;
    (* [protected]: a caller that gives up rejects only its own view. Its
       callback on [physical] is removed, and the permit stays held. *)
    Lwt.map Result.ok (Lwt.protected physical)
  end
