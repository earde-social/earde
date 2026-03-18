open Lwt.Infix

(* ======================================================= *)
(* === Helpers                                           === *)
(* ======================================================= *)

(* Safety guardrail: reject any URL whose host is not a loopback address.
   Prevents accidental truncation of staging/production databases. *)
let assert_localhost uri =
  let host = Option.value (Uri.host uri) ~default:"" in
  if host <> "localhost" && host <> "127.0.0.1" then begin
    Printf.eprintf
      "ABORT: DATABASE_URL host is '%s'. Only localhost/127.0.0.1 is permitted for seeding.\n%!"
      host;
    exit 1
  end

let require_confirmation () =
  Printf.printf
    "\n\
     WARNING: This will TRUNCATE ALL DATA in the target database.\n\
     Type 'I_AM_SURE' and press Enter to continue: %!";
  let input = input_line stdin in
  if input <> "I_AM_SURE" then begin
    Printf.printf "Aborted. No changes were made.\n%!";
    exit 0
  end

(* Lower t_cost than production — seeding speed over KDF hardness. *)
let seed_pw_hash () =
  (* 16-byte fixed seed salt — reproducibility over security for dev data *)
  let salt = "earde_seed_16byt" in
  match
    Argon2.hash ~t_cost:1 ~m_cost:4096 ~parallelism:1
      ~pwd:"password123" ~salt
      ~kind:Argon2.ID ~hash_len:32 ~encoded_len:128
      ~version:Argon2.VERSION_13
  with
  | Ok (_, encoded) -> encoded
  | Error _ ->
      Printf.eprintf "Argon2 hash failed during seed setup.\n%!";
      exit 1

let ok_or_fail r = match r with Ok x -> Lwt.return x | Error s -> failwith s
let ok_caqti r = match r with Ok x -> Lwt.return x | Error e -> failwith (Caqti_error.show e)

(* ======================================================= *)
(* === Seed queries (raw SQL not exposed in Earde.Db)   === *)
(* ======================================================= *)

(* Schema is managed by dbmate migrations in db/migrations/.
   seed_body assumes all tables already exist. *)

(* Truncate all user-generated tables in FK-safe order, reset sequences.
   CASCADE handles any FK constraints we haven't ordered explicitly. *)
let truncate_all_q =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
  "TRUNCATE TABLE community_bans, page_views, notifications, comment_votes, post_votes,
    comments, community_moderators, community_members, posts, communities, users RESTART IDENTITY CASCADE"

let verify_all_users_q =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
  "UPDATE users SET is_email_verified = TRUE"

let get_community_id_q =
  let open Caqti_request.Infix in
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT id FROM communities WHERE slug = $1"

let get_user_id_q =
  let open Caqti_request.Infix in
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT id FROM users WHERE username = $1"

(* ======================================================= *)
(* === Main seed routine                                 === *)
(* ======================================================= *)

(* All errors inside the with_connection callback are raised as Failure so the
   callback's return type stays polymorphic in its error component, satisfying
   Caqti_lwt_unix.with_connection's type constraint. Lwt.catch at the call site
   converts exceptions back to (Error string) for clean propagation. *)
let seed_body (module C : Caqti_lwt.CONNECTION) =
  let db = (module C : Caqti_lwt.CONNECTION) in

  (* --- Truncate --------------------------------------------------------- *)
  let%lwt () = C.exec truncate_all_q () >>= ok_caqti in
  Printf.printf "  Tables truncated.\n%!";

  (* --- Communities ------------------------------------------------------ *)
  let%lwt () = Earde.Db.create_community db "Science"    "science"    (Some "Scientific discoveries and research.") >>= ok_or_fail in
  let%lwt () = Earde.Db.create_community db "Technology" "technology" (Some "Tech news, tools, and engineering.")    >>= ok_or_fail in
  let%lwt () = Earde.Db.create_community db "Philosophy" "philosophy" (Some "Ideas, ethics, and the examined life.") >>= ok_or_fail in
  Printf.printf "  Communities created.\n%!";

  (* --- Users ------------------------------------------------------------ *)
  let pw = seed_pw_hash () in
  let%lwt () = Earde.Db.create_user db "alice"   "alice@example.com"   pw "tok_alice"   >>= ok_or_fail in
  let%lwt () = Earde.Db.create_user db "bob"     "bob@example.com"     pw "tok_bob"     >>= ok_or_fail in
  let%lwt () = Earde.Db.create_user db "charlie" "charlie@example.com" pw "tok_charlie" >>= ok_or_fail in
  let%lwt () = C.exec verify_all_users_q () >>= ok_caqti in
  Printf.printf "  Users created and email-verified.\n%!";

  (* --- Fetch IDs for FK references ------------------------------------- *)
  let%lwt science_id    = C.find get_community_id_q "science"    >>= ok_caqti in
  let%lwt technology_id = C.find get_community_id_q "technology" >>= ok_caqti in
  let%lwt philosophy_id = C.find get_community_id_q "philosophy" >>= ok_caqti in
  let%lwt alice_id      = C.find get_user_id_q  "alice"      >>= ok_caqti in
  let%lwt bob_id        = C.find get_user_id_q  "bob"        >>= ok_caqti in
  let%lwt charlie_id    = C.find get_user_id_q  "charlie"    >>= ok_caqti in

  (* --- Memberships ------------------------------------------------------ *)
  let join u a = Earde.Db.join_community db u a >>= ok_or_fail in
  let%lwt () = join alice_id   science_id    in
  let%lwt () = join alice_id   technology_id in
  let%lwt () = join alice_id   philosophy_id in
  let%lwt () = join bob_id     science_id    in
  let%lwt () = join bob_id     technology_id in
  let%lwt () = join bob_id     philosophy_id in
  let%lwt () = join charlie_id science_id    in
  let%lwt () = join charlie_id technology_id in
  let%lwt () = join charlie_id philosophy_id in
  Printf.printf "  Memberships created.\n%!";

  (* --- Moderators ------------------------------------------------------- *)
  (* Alice is the canonical test moderator: she created nothing in seed but is
     manually promoted to verify is_moderator checks in integration tests. *)
  let%lwt () = Earde.Db.add_moderator db alice_id science_id >>= ok_or_fail in
  Printf.printf "  Moderators seeded (alice → science).\n%!";

  (* --- Posts ------------------------------------------------------------ *)
  let mkpost title community_id user_id content =
    (* create_post now returns the new post id; discard it here since seed only cares about success. *)
    Earde.Db.create_post db title None (Some content) community_id user_id >>= ok_or_fail >>= fun _ -> Lwt.return ()
  in
  let%lwt () = mkpost "Scientists discover new deep-sea bioluminescent species"
                 science_id alice_id
                 "A team of marine biologists found a species off the Mariana Trench." in
  let%lwt () = mkpost "New study links sleep patterns to cognitive performance"
                 science_id bob_id
                 "Researchers at MIT published findings on REM disruption and memory consolidation." in
  let%lwt () = mkpost "CRISPR-Cas12 enables single-base precision in somatic cells"
                 science_id charlie_id
                 "A refined variant reduces off-target edits by 95% in liver tissue trials." in
  let%lwt () = mkpost "OCaml 5.2 performance benchmarks vs 5.1"
                 technology_id alice_id
                 "Effect handlers improve tail-call throughput significantly on multicore." in
  let%lwt () = mkpost "Postgres full-text search beats Elasticsearch for most apps"
                 technology_id bob_id
                 "With tsvector indexes and GIN, Postgres covers 90% of search use cases." in
  let%lwt () = mkpost "Building a real-time forum with Dream + Htmx"
                 technology_id charlie_id
                 "Dream's Lwt model pairs well with Htmx's swap-based UI updates." in
  let%lwt () = mkpost "Does free will survive a deterministic universe?"
                 philosophy_id alice_id
                 "Compatibilism argues agency and physical causation are not mutually exclusive." in
  let%lwt () = mkpost "Stoicism vs Epicureanism: a practical comparison"
                 philosophy_id bob_id
                 "Both agree on inner tranquility; they diverge on the role of pleasure." in
  let%lwt () = mkpost "The Ship of Theseus applied to software"
                 philosophy_id charlie_id
                 "If you rewrite every module one by one, is it still the same codebase?" in
  Printf.printf "  Posts created.\n%!";

  (* --- Comments --------------------------------------------------------- *)
  (* Post IDs restart at 1 after TRUNCATE RESTART IDENTITY *)
  let%lwt () = Earde.Db.create_comment db
                 "The ocean depths never cease to surprise — great find!" 1 alice_id None >>= ok_or_fail in
  let%lwt () = Earde.Db.create_comment db
                 "This is exactly why OCaml deserves more mainstream attention." 4 bob_id None >>= ok_or_fail in
  Printf.printf "  Comments created.\n%!";

  Lwt.return_ok ()

(* ======================================================= *)
(* === Entry point                                       === *)
(* ======================================================= *)

let () =
  let database_url =
    try Sys.getenv "DATABASE_URL"
    with Not_found ->
      Printf.eprintf "ABORT: DATABASE_URL environment variable is not set.\n%!";
      exit 1
  in
  let uri = Uri.of_string database_url in
  assert_localhost uri;
  require_confirmation ();
  Printf.printf "\nSeeding %s...\n%!" database_url;
  let result =
    Lwt_main.run (
      Lwt.catch
        (fun () ->
          Caqti_lwt_unix.with_connection uri seed_body >>= function
          | Ok ()   -> Lwt.return (Ok ())
          | Error e -> Lwt.return (Error (Caqti_error.show e))
        )
        (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
    )
  in
  match result with
  | Ok ()   ->
      Printf.printf "\nSeeding complete. Login: alice / bob / charlie, password: password123\n%!"
  | Error e ->
      Printf.eprintf "\nSeeding failed: %s\n%!" e;
      exit 1
