(lang dune 3.11)
(name earde)
(generate_opam_files true)

(source
 (github earde-social/earde))

(license MIT)

(authors tolwiz)

(maintainers tolwiz)

(package
 (name earde)
 (synopsis "Community platform for technical communities")
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
  (mtime (>= 2.0.0))
  (alcotest :with-test)
 ))
