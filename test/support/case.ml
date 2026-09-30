(* Alcotest case constructors shared by every domain. *)

let quick name f = Alcotest.test_case name `Quick f
