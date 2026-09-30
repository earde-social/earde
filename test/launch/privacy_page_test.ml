(* /privacy — the 2026-08-04 Article-13-style rewrite. The suite pins the
   wrapper contract (single local stylesheet, no Tailwind CDN, no Google
   Fonts, no auth.css, no notification chrome, no forms, viewer-independent,
   indexable), the required disclosures (controller contact, data categories,
   purpose/legal-basis rows, public-content/indexing, Shared Threads, GitHub,
   PostHog + consent cookie, retention, rights, complaint route, automated
   decisions), the anchor TOC's id/href pairing, the analytics-preference
   control anatomy (the same data-analytics-* hooks analytics.js drives on
   /settings — buttons + fetch, never a <form>), and the ABSENCE of the old
   page's unsupportable claims (future-tense analytics disclosure,
   "anonymous" analytics wording, absolute security/anonymity language). *)

let case name f = Alcotest.test_case name `Quick f

let render_privacy ?user () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered := Earde.Site_pages.privacy_page ?user req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/privacy" ""))
  in
  !rendered

(* The 16 anchor sections, in document order. The TOC must link every one
   and every one must exist as <section id='…'>. *)
let section_ids =
  [
    "controller";
    "data-we-collect";
    "how-we-use";
    "public-content";
    "shared-threads";
    "github";
    "cookies-analytics";
    "recipients";
    "transfers";
    "retention";
    "security";
    "your-rights";
    "deletion";
    "automated-decisions";
    "changes";
    "contact";
  ]

let wrapper_case =
  case "wrapper: launch entry shell, only local launch assets" (fun () ->
      let page = render_privacy () in
      Html_assert.must page "<body class='launch-privacy'>";
      Html_assert.must page "<title>Privacy Policy - Earde</title>";
      Html_assert.must page
        "<link rel='stylesheet' href='/static/css/earde.css'>";
      Alcotest.(check int)
        "exactly one stylesheet" 1
        (Html_assert.occurrences page "<link rel='stylesheet'");
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "auth.css";
      Html_assert.must_not page "mobile-gate.css";
      Html_assert.must_not page "unread-notifs";
      Html_assert.must_not page "notif-badge";
      Html_assert.must_not page "noindex";
      Html_assert.must_not page "href='#'";
      (* Entry chrome is form-free and the document carries none. *)
      Html_assert.must_not page "<form")

let structure_case =
  case "title, date, TOC anchors and section skeleton" (fun () ->
      let page = render_privacy () in
      Html_assert.must page "<h1>Privacy Policy</h1>";
      Html_assert.must page "Last updated: 4 August 2026";
      Alcotest.(check int) "one h1" 1 (Html_assert.occurrences page "<h1");
      Alcotest.(check int) "sixteen h2" 16 (Html_assert.occurrences page "<h2");
      Alcotest.(check int)
        "sixteen sections" 16
        (Html_assert.occurrences page "<section id='");
      Alcotest.(check int) "toc entries" 16 (List.length section_ids);
      List.iter
        (fun id ->
          Html_assert.must page (Printf.sprintf "href='#%s'" id);
          Html_assert.must page (Printf.sprintf "<section id='%s'>" id))
        section_ids;
      (* The self-service links: settings twice (rights + deletion),
         export once. *)
      Alcotest.(check int)
        "settings links" 2
        (Html_assert.occurrences page "href='/settings'");
      Alcotest.(check int)
        "export link" 1
        (Html_assert.occurrences page "href='/export-data'"))

let disclosure_case =
  case "required Article-13 disclosures are present" (fun () ->
      let page = render_privacy () in
      (* Controller and contact: named contact channel, three times
         (controller, rights, contact). *)
      Html_assert.must page "is the data controller";
      Alcotest.(check int)
        "contact mailto" 3
        (Html_assert.occurrences page "mailto:metacirculardispatches@gmail.com");
      (* Data categories and accuracy about credentials. *)
      Html_assert.must page "salted argon2id hash";
      Html_assert.must page "IP address";
      Html_assert.must page "user agent";
      (* Purpose / legal-basis mapping. *)
      Html_assert.must page "Performance of a contract (Art. 6(1)(b) GDPR)";
      Html_assert.must page "Legitimate interest";
      Html_assert.must page "Consent (Art. 6(1)(a) GDPR)";
      Html_assert.must page "Legal obligation";
      (* Public content and indexing: surface-based, never
         community-categorical. *)
      Html_assert.must page "may be indexed by search engines";
      Html_assert.must page "moderation log";
      Html_assert.must page
        "publicly accessible communities, channels and sections";
      Html_assert.must page "limited to users authorized to view that area";
      (* Shared Threads. *)
      Html_assert.must page "one canonical thread";
      Html_assert.must page "top moderators of the origin and destination";
      (* GitHub: durable wording, no exact API-call count. *)
      Html_assert.must page "never stores your GitHub tokens";
      Html_assert.must page "Only repositories that are public on GitHub";
      Html_assert.must page
        "only for the read-only requests needed to verify the installation";
      (* Cookies + PostHog + consent. *)
      Html_assert.must page "dream.session";
      Html_assert.must page "earde_analytics_consent";
      Html_assert.must page "eu.i.posthog.com";
      Html_assert.must page "Session Replay";
      Html_assert.must page "the PostHog script is not downloaded";
      (* Recipients, transfers, no-sale statement. *)
      Html_assert.must page "Brevo";
      Html_assert.must page "Cloudflare";
      Html_assert.must page "Hetzner";
      Html_assert.must page "does not sell personal data";
      Html_assert.must page "European Economic Area";
      (* Retention. *)
      Html_assert.must page "How long we keep data";
      Html_assert.must page "24 hours";
      Html_assert.must page "about two weeks";
      Html_assert.must page "one-minute request window";
      Html_assert.must page "uploaded avatar image file is deleted";
      (* Rights, withdrawal, complaint. *)
      Html_assert.must page "withdraw consent";
      Html_assert.must page "lodge a complaint";
      Html_assert.must page "supervisory authority";
      (* Automated decisions and deletion behavior. *)
      Html_assert.must page
        "does not make automated decisions about you that produce legal or \
         similarly significant effects";
      Html_assert.must page "[deleted]")

let consent_controls_case =
  case "analytics-preference control: settings anatomy, no form" (fun () ->
      let page = render_privacy () in
      (* Exactly the data-analytics-* anatomy analytics.js drives on
         /settings, shipped hidden so a deployment without analytics
         renders no dead control. Buttons, not a form. *)
      Html_assert.must page
        "<div class='privacy-consent' data-analytics-settings hidden>";
      Html_assert.must page "data-analytics-state";
      Html_assert.must page "data-analytics-accept";
      Html_assert.must page "data-analytics-refuse";
      Html_assert.must page "data-analytics-error";
      Alcotest.(check int)
        "two buttons" 2
        (Html_assert.occurrences page "<button type='button' data-analytics-");
      Html_assert.must_not page "<form";
      (* One panel and one global footer — the footer's
         Analytics-preferences link may not duplicate the control, and the
         anchor it targets is this section's id. *)
      Alcotest.(check int)
        "one preferences panel" 1
        (Html_assert.occurrences page "data-analytics-settings");
      Alcotest.(check int)
        "one global footer" 1
        (Html_assert.occurrences page "<footer class='launch-footer'>");
      Html_assert.must page "<section id='cookies-analytics'>")

let removed_claims_case =
  case "old unsupportable claims are gone" (fun () ->
      let page = render_privacy () in
      (* The pre-rewrite page deferred analytics to the future; PostHog is
         live and consent-gated, so that sentence must never return. *)
      Html_assert.must_not page
        "If analytics or tracking tools are added in the future";
      (* Authenticated analytics uses internal user:<id> identities, so no
         "anonymous" claim may appear anywhere in the document. *)
      Html_assert.must_not page "anonymous";
      Html_assert.must_not page "Anonymous";
      (* Absolute or template claims the audit could not support. *)
      Html_assert.must_not page "never share";
      Html_assert.must_not page "100% secure";
      Html_assert.must_not page "military-grade";
      Html_assert.must_not page "industry-standard";
      Html_assert.must_not page "end-to-end encrypted";
      Html_assert.must_not page "passwords are encrypted";
      Html_assert.must_not page "we value your privacy";
      (* The categorical everything-in-a-public-community-is-public claim,
         the fragile exact GitHub API-call count, and the indefinite
         rate-limit retention wording are all retired. *)
      Html_assert.must_not page "Public communities are public";
      Html_assert.must_not page "exactly two read-only API calls";
      Html_assert.must_not page "not currently expired on a fixed schedule";
      (* Passwords are hashed; the word "encrypted" may only describe the
         GitHub flow cookie. *)
      Html_assert.must page "hashes, never in a readable form")

let viewer_independence_case =
  case "ignored ?user changes nothing" (fun () ->
      Alcotest.(check string)
        "anonymous = authenticated" (render_privacy ())
        (render_privacy ~user:"qa-viewer" ()))

(* The signup consent link is the page's primary inbound route: pin its
   exact destination and security attributes. *)
let signup_consent_link_case =
  case "signup consent link still targets /privacy" (fun () ->
      let rendered = ref "" in
      let (_ : Dream.response) =
        Lwt_main.run
          (Dream.memory_sessions
             (fun req ->
               rendered := Earde.Auth_pages.signup_form req;
               Dream.html "")
             (Dream.request ~method_:`GET ~target:"/signup" ""))
      in
      Html_assert.must !rendered
        "<a href='/privacy' target='_blank'>Privacy Policy</a>")

let suite =
  [
    wrapper_case;
    structure_case;
    disclosure_case;
    consent_controls_case;
    removed_claims_case;
    viewer_independence_case;
    signup_consent_link_case;
  ]

let suites = [ ("privacy_launch_page", suite) ]
