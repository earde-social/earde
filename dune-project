(lang dune 3.11)
(name earde)
(generate_opam_files true)

(package
 (name earde)
 (synopsis "earde mvp")
 (depends
  (ocaml (>= 5.1.1))
  (dune (>= 3.11))
  dream
  dream-pure
  (caqti (>= 2.0.1))
  (caqti-lwt (>= 2.0.1))
  (caqti-driver-postgresql (>= 2.0.1))
  lwt
  (lwt_ppx (>= 2.1.0))
  (argon2 (>= 1.0.0))
  base64
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  logs
  uri
  yojson
  digestif
  (alcotest :with-test)
 ))
