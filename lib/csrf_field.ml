(* Dream builds the complete hidden input and escapes the token it holds. *)
let tag request = Html.trusted (Dream.csrf_tag request)
