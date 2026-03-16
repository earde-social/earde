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
  (caqti (>= 2.0.1))                   
  (caqti-driver-postgresql (>= 2.0.1)) 
  (lwt_ppx (>= 2.1.0)) 
  (argon2 (>= 1.0.0))
  cohttp-lwt-unix
 ))
