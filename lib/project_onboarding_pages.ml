(* /bring — server-rendered entry point for the GitHub-anchored open-source
   onboarding. Pure rendering: the closed mode and viewer context arrive as
   arguments (the handler owns env/session reads), so every mode/viewer state
   is testable without a running server.

   Language rules: verification wording stays factual ("Verified through
   GitHub", "Project connected through GitHub") — never an "Official ..."
   claim, because a connected repository proves maintainership, not that a
   community speaks for a project. No links to routes outside this slice: the
   GitHub install/callback routes do not exist yet, and the generic
   community-creation route is deliberately not offered here — which is also
   why this page uses the chrome-free auth layout instead of the App command
   bar (whose logged-in topbar carries the "Connect a project" link). *)

(* Uppercase micro-heading + paragraph in the auth-card idiom; all inputs are
   static literals, so no escaping is needed at this call site. *)
let section ~heading body =
  Printf.sprintf
    "<h2 class='auth-label'>%s</h2>\n<p class='auth-sub'>%s</p>"
    heading body

let intro =
  "<h1 class='auth-title'>Bring your project to Earde</h1>\n\
   <p class='auth-sub'>Earde is becoming a community network for open source: \
   live chat plus durable, searchable threads for the projects people build \
   together.</p>"

let members_section =
  section ~heading:"Members never need GitHub"
    "Reading, chatting, and posting work with a normal Earde account. \
     Ordinary community members do not need a GitHub account. GitHub is used \
     only by maintainers, to verify an open-source project they maintain."

let choices_section =
  section ~heading:"What a verified project can choose"
    "Once a project is verified through GitHub, its maintainers can either \
     create its own Earde community as a dedicated home for the project, or \
     connect the project to an existing broader Earde community where its \
     discussion already happens."

let meaning_section =
  section ~heading:"What verification means"
    "&ldquo;Verified through GitHub&rdquo; and &ldquo;Project connected \
     through GitHub&rdquo; mean exactly that: a maintainer proved control of \
     the repository. Connecting a repository does not by itself make any \
     Earde community the authoritative or endorsed place for a project."

(* The mode-specific status panel. Only the Admins-mode admin view carries an
   action-shaped element, and it is a disabled informational button — there is
   no GitHub connection route to link to yet, and a dead link would be worse
   than an honest "coming soon". *)
let status_html mode ~is_admin =
  let panel body = Printf.sprintf "<div class='auth-alert'>%s</div>" body in
  match (mode : Project_onboarding.mode) with
  | Off ->
      panel
        "Maintainer onboarding is not currently enabled. This page will be \
         updated when project verification opens."
  | Admins when is_admin ->
      panel
        "Private administrator testing is enabled for your account. The \
         GitHub connection flow is not wired up yet, so there is nothing to \
         start from this page."
      ^ "<button type='button' class='auth-btn' disabled>Connect a \
         repository &mdash; coming soon</button>"
  | Admins ->
      (* Deliberately vague for non-admins: says availability is limited
         without describing who is testing or how. *)
      panel
        "Maintainer onboarding is currently limited to a small private \
         group. Check back soon."
  | Public ->
      panel
        "Public onboarding is enabled at the configuration level, but the \
         GitHub connection flow is still being prepared. A start action will \
         appear here when it is ready."

(* Context-aware navigation, real routes only. Logged-in viewers already have
   an account, so signup is never shown to them. *)
let nav_html ~user =
  match user with
  | Some _ ->
      "<div class='auth-foot'><a href='/feed' class='auth-link'>Back to your \
       feed</a></div>"
  | None ->
      "<div class='auth-foot'><a href='/feed' class='auth-link'>Browse the \
       public feed</a></div>\n\
       <div class='auth-foot'>Already on Earde? <a href='/login' \
       class='auth-link'>Log in</a> &middot; New here? <a href='/signup' \
       class='auth-link'>Sign up</a></div>"

let bring_page ?user ?request ~is_admin ~mode () =
  let card =
    String.concat "\n"
      [ intro
      ; members_section
      ; choices_section
      ; meaning_section
      ; status_html mode ~is_admin
      ; nav_html ~user
      ]
  in
  (* noindex: a transitional onboarding surface whose copy will change as the
     flow ships — not worth putting in search indexes. *)
  Components.auth_page ?user ?request ~noindex:true
    ~title:"Bring your project to Earde" ~card ()
