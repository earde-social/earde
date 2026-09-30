(* /bring — server-rendered entry and return page for GitHub project
   onboarding. Pure rendering: the closed access state and one-time callback
   feedback arrive as arguments (the handler owns env, session, and query
   reads), so every state renders without a server or database.

   Rendered as a Cartographic Civic launch document
   (Components.launch_entry_page): hero, OPTION A/B cards, centred CTA
   stack, then the factual explainer blocks. The chrome and this content
   are form-free except for the single Ready-state start form — the
   non-ready states are asserted to contain no <form> document-wide.

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

(* Centred hero: 46px mark, serif 32 title, one-paragraph lead. *)
let hero =
  "<div class='launch-hero'>\
   <img src='/static/images/logo-mark.svg' alt='' width='46' height='46'>\
   <h1 class='launch-hero__title'>Bring your open-source community</h1>\
   <p class='launch-hero__lead'>Verify a project you maintain through \
   GitHub, then give it a home on Earde &mdash; either a dedicated \
   community, or as a connected project inside a broader existing \
   community.</p>\
   </div>"

(* The two home paths, explained side by side. Explainers only — the real
   choice is made later in the onboarding flow, so neither card links
   anywhere (and this page must never link the post-install route). *)
let option_cards =
  "<div class='choice-grid launch-options'>\
   <div class='card card--pad launch-option'>\
   <p class='mono launch-option__kicker launch-option__kicker--moss'>OPTION A</p>\
   <p class='launch-option__title'>Create a community home</p>\
   <p class='card__blurb'>A dedicated public community for your project: \
   live channels, forum sections and governance you steward.</p>\
   </div>\
   <div class='card card--pad launch-option'>\
   <p class='mono launch-option__kicker launch-option__kicker--ochre'>OPTION B</p>\
   <p class='launch-option__title'>Connect to an existing community</p>\
   <p class='card__blurb'>Join a broader existing community as a verified \
   connected project &mdash; no empty duplicate \
   community.</p>\
   </div></div>"

(* Uppercase mono kicker + paragraph explainer block. *)
let section ~heading body =
  Printf.sprintf
    "<div class='launch-explain__block'><h2 class='kicker'>%s</h2>\n\
     <p class='launch-explain__text'>%s</p></div>"
    heading body

let connecting_section =
  section ~heading:"What connecting GitHub does"
    "Connecting GitHub installs the Earde GitHub App on a project you \
     maintain. GitHub is used to verify project maintainers and their \
     public repositories &mdash; nothing else. A project verified this way is \
     shown as &ldquo;Project connected through GitHub&rdquo;."

let meaning_section =
  section ~heading:"What verification means"
    "&ldquo;Verified through GitHub&rdquo; means exactly that: within the \
     last 30 days, a steward proved through GitHub that they can access the \
     repository. After that the label reads &ldquo;Verification stale&rdquo; \
     until a steward connects the project again. Connecting an installation does not \
     automatically grant moderation rights in an existing Earde community, \
     and it does not make any community the authoritative or endorsed place \
     for a project."

let explainers =
  Printf.sprintf "<div class='panel launch-explain'>%s%s</div>"
    connecting_section meaning_section

(* One-time callback feedback. At most one banner; None renders no element
   at all. The success copy claims only the connection itself — never a
   created community or synchronized repositories — and the failure copy is
   one generic sentence, matching the callback's anti-oracle redirects:
   which internal stage failed must stay indistinguishable here too. The
   auth-alert class names are the stable contract (tests assert them and
   their absence); earde.css restyles them under the launch scope. *)
let feedback_html = function
  | None -> ""
  | Some Connected ->
      "<div class='auth-alert'>GitHub installation connected \
       successfully.</div>"
  | Some Failed ->
      "<div class='auth-alert auth-alert--error'>We couldn't complete the \
       GitHub connection. Please try again.</div>"

let github_icon =
  "<svg width='20' height='20' viewBox='0 0 16 16' fill='currentColor' \
   aria-hidden='true'><path d='M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 \
   5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 \
   1.08.58 1.23.82.72 1.21 1.87.87 \
   2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 \
   0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82a7.6 \
   7.6 0 0 1 4 0c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 \
   2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 \
   3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 \
   .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8Z'/></svg>"

(* The one start action of this page. Deliberately parameter-free: no
   hidden user, state, installation, client, callback, or redirect field —
   the start handler re-derives everything from the session and repeats all
   access and same-origin checks. The opening <form> tag is byte-exact test
   contract (Gh_bring_browser_post posts the rendered form). *)
let start_form =
  "<form method='POST' action='/integrations/github/install/start'>\
   <button type='submit' class='btn btn--dark'>" ^ github_icon
  ^ " Connect a GitHub project</button></form>"

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

let footnote =
  "<p class='mono launch-footnote'>Reads public-repository metadata only \
   &middot; no source-code or write access</p>"

let bring_page ?user ?request ~access ~feedback () =
  let cta =
    Printf.sprintf "<div class='launch-cta'>%s\n%s\n%s</div>"
      (access_html access) (nav_html ~user) footnote
  in
  let content =
    String.concat "\n"
      (List.filter
         (fun part -> not (String.equal part ""))
         [ hero; feedback_html feedback; option_cards; cta; explainers ])
  in
  (* noindex: a session- and rollout-dependent onboarding surface — not
     worth putting in search indexes. Entry_viewer: this page IS /bring, so
     the compact topbar Connect CTA would self-link; the topbar carries the
     viewer's auth controls instead (form-free in both arms).

     desktop_only: everything this page opens — GitHub App installation,
     repository selection, project setup — is a desktop flow, and the
     downstream surfaces (/projects/new and the rest) already carry the
     canonical gate. Gating the entry point too means a phone-width visitor
     never enters the funnel at all, in every access state and whether they
     arrive cold, from the landing CTA, or back from login/signup: the state
     lives in the session and the query string, never in the viewport, so
     authenticating changes nothing about which document the width shows. *)
  Page_shell.launch_entry_page ?request ~noindex:true
    ~topbar:(Page_shell.Entry_viewer user) ~desktop_only:true
    ~page_class:"launch-bring" ~title:"Bring your open-source community"
    ~content ()
