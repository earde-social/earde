(* A rendered community page and position lookups within it. *)

let index_of html needle =
  let n = String.length needle and h = String.length html in
  let rec go i =
    if i + n > h then None
    else if String.sub html i n = needle then Some i
    else go (i + 1)
  in
  go 0

let community : Earde.Db.community =
  { id = 7711; slug = "cmia";
    name = "Cartographic Mapping Interest Assembly";
    description = Some "Two ways to take part."; rules = None;
    avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = true; visibility = Earde.Db.Community_public;
    indexable = true; is_network_community = false;
    onboarding_state = Earde.Db.Community_published; discoverable = true }
