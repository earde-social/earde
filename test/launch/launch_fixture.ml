(* Launch-shell fixtures shared by the launch cases. *)

let nav_test_community : Earde.Community_types.community =
  {
    id = 1;
    slug = "ocaml";
    name = "OCaml";
    description = None;
    rules = None;
    avatar_url = None;
    banner_url = None;
    allow_downvotes = true;
    sections_enabled = true;
    visibility = Earde.Community_types.Community_public;
    indexable = true;
    is_network_community = false;
    onboarding_state = Earde.Community_types.Community_published;
    discoverable = true;
  }

(* The exact element, as the wrapper emits it. Byte-exact on purpose: copy,
   destination, element type, accessible name and icon size are all product
   decisions, and a diff here should be a deliberate edit, not a surprise.
   The accessible name is the visible label itself — no aria-label to
   drift from it. *)
let cta_open =
  "<a class='btn btn--connect-github' href='/bring' title='Connect an \
   open-source project'>"
