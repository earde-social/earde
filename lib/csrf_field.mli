(** The framework CSRF field as markup. *)

val tag : Dream.request -> Html.t
(** Dream's hidden CSRF [<input>] for [request]. Dream renders the whole
    element and escapes its token, so this is the one place its markup enters
    {!Html.t} through {!Html.trusted}. *)
