(* /bring — server-rendered entry and return page for GitHub project
   onboarding. Pure rendering: the closed access state and one-time callback
   feedback arrive as arguments (the handler owns env, session, and query
   reads), so every state renders without a server or database.

   Language rules: verification wording stays factual ("Verified through
   GitHub", "Project connected through GitHub") — never an "Official ..."
   claim, because a connected repository proves maintainership, not that a
   community speaks for a project. Every string below is a static literal,
   so no escaping is needed at these call sites; caller-controlled data
   (notably the callback query value) must never be passed in. *)

type access =
  | Onboarding_disabled
  | Login_required
  | Rollout_limited
  | Ready

type feedback =
  | Connected
  | Failed

(* Uppercase micro-heading + paragraph in the auth-card idiom. *)
let section ~heading body =
  Printf.sprintf
    "<h2 class='auth-label'>%s</h2>\n<p class='auth-sub'>%s</p>"
    heading body

let intro =
  "<h1 class='auth-title'>Bring your project to Earde</h1>\n\
   <p class='auth-sub'>Earde is becoming a community network for open \
   source: live chat plus durable, searchable threads for the projects \
   people build together.</p>"

let members_section =
  section ~heading:"Members never need GitHub"
    "Reading, chatting, and posting work with a normal Earde account. \
     Ordinary community members do not need a GitHub account. GitHub is used \
     only by maintainers, to verify an open-source project they maintain."

let connecting_section =
  section ~heading:"What connecting GitHub does"
    "Connecting GitHub installs the Earde GitHub App on a project you \
     maintain. GitHub is used to verify project maintainers and their \
     public repositories — nothing else. A project verified this way is \
     shown as &ldquo;Project connected through GitHub&rdquo;."

let meaning_section =
  section ~heading:"What verification means"
    "&ldquo;Verified through GitHub&rdquo; means exactly that: a maintainer \
     proved control of the repository. Connecting an installation does not \
     automatically grant moderation rights in an existing Earde community, \
     and it does not make any community the authoritative or endorsed place \
     for a project."

(* One-time callback feedback. At most one banner; None renders no element
   at all. The success copy claims only the connection itself — never a
   created community or synchronized repositories — and the failure copy is
   one generic sentence, matching the callback's anti-oracle redirects:
   which internal stage failed must stay indistinguishable here too. *)
let feedback_html = function
  | None -> ""
  | Some Connected ->
      "<div class='auth-alert'>GitHub installation connected \
       successfully.</div>"
  | Some Failed ->
      "<div class='auth-alert auth-alert--error'>We couldn't complete the \
       GitHub connection. Please try again.</div>"

(* The one start action of this page. Deliberately parameter-free: no
   hidden user, state, installation, client, callback, or redirect field —
   the start handler re-derives everything from the session and repeats all
   access and same-origin checks. *)
let start_form =
  "<form method='POST' action='/integrations/github/install/start'>\
   <button type='submit' class='auth-btn'>Connect a GitHub \
   project</button></form>"

let access_html = function
  | Onboarding_disabled ->
      "<div class='auth-alert'>Project onboarding is currently unavailable. \
       This page will be updated when project verification opens.</div>"
  | Login_required ->
      "<div class='auth-alert'>An Earde account is required to connect a \
       project. <a href='/login' class='auth-link'>Log in</a> to \
       continue.</div>"
  | Rollout_limited ->
      (* Deliberately about the rollout, not the viewer: nothing here may
         imply missing GitHub permissions or name a configuration flag. *)
      "<div class='auth-alert'>GitHub project onboarding is currently \
       limited to the early-access rollout. Check back soon.</div>"
  | Ready -> start_form

(* Context-aware navigation, real routes only. Logged-in viewers already
   have an account, so signup is never shown to them. *)
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

let bring_page ?user ?request ~access ~feedback () =
  let card =
    String.concat "\n"
      (List.filter
         (fun part -> not (String.equal part ""))
         [ intro
         ; feedback_html feedback
         ; members_section
         ; connecting_section
         ; meaning_section
         ; access_html access
         ; nav_html ~user
         ])
  in
  (* noindex: a session- and rollout-dependent onboarding surface — not
     worth putting in search indexes. *)
  Components.auth_page ?user ?request ~noindex:true
    ~title:"Bring your project to Earde" ~card ()
