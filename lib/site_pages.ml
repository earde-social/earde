(* Complete Article-13-style notice, rewritten 2026-08-04. Every factual claim
   is grounded in the current implementation: schema + migrations (users,
   pending_signups, rate_limits, page_views, dream_session, reports,
   mod_actions, shared_thread_placements, community_connections, the github_*
   and project_* tables), auth.ml (argon2id), email.ml (Brevo), turnstile.ml,
   analytics.ml/.js (consent-gated PostHog EU, masked replay, deletion jobs),
   the GitHub onboarding modules (no persisted tokens, public repos only,
   read-only calls) and the delete-account / export handlers. No invented
   entity, address, DPO, retention period or safeguard — where a fact is not
   configured, the copy states a criterion instead of a number.

   The document stays static and viewer-independent (?user accepted and
   ignored; the entry chrome is deterministic), form-free (the consent
   controls are the same data-analytics-* button anatomy analytics.js already
   drives on /settings — button + fetch, no <form>), and indexable. Anchors
   use real ids (never href='#'). The analytics-preferences panel ships
   hidden; analytics.js reveals it only when the deployment has a valid
   analytics configuration, so a deployment without PostHog renders no dead
   control. Styling hooks live in the "privacy policy only" section of
   earde.css, scoped under body.launch-privacy. *)
let privacy_page ?user:_ request =
  let content = (Html.static "
    <div class='privacy-doc'>

      <h1>Privacy Policy</h1>
      <p class='privacy-updated'>Last updated: 4 August 2026</p>

      <div class='privacy-summary'>
        <p>The short version:</p>
        <ul>
          <li>Visibility follows the surface you post on. Content in publicly accessible communities, channels and sections &mdash; posts, comment threads, chat archives, moderation logs and profiles &mdash; can be read by anyone and indexed by search engines. Content in private, members-only or moderator-only areas is limited to the people authorized to view it.</li>
          <li>An account needs a username, an email address and a password. The password is stored only as a salted hash; your email address is never displayed publicly.</li>
          <li>Optional product analytics (PostHog) runs only after you explicitly allow it, and you can withdraw that choice at any time on this page.</li>
          <li>If you connect an open-source project, Earde stores public GitHub metadata about it. It never stores your GitHub tokens and never reads repository contents.</li>
          <li>You can export your profile, posts and comments as JSON, and delete your account at any time from your settings.</li>
        </ul>
        <p>This policy covers the Earde service at earde.com. It is not a terms-of-service document.</p>
      </div>

      <nav class='privacy-toc' aria-label='Sections of this policy'>
        <ul>
          <li><a href='#controller'>Who controls your data</a></li>
          <li><a href='#data-we-collect'>Data we collect</a></li>
          <li><a href='#how-we-use'>How and why we use data</a></li>
          <li><a href='#public-content'>Public content and search engines</a></li>
          <li><a href='#shared-threads'>Connected communities and Shared Threads</a></li>
          <li><a href='#github'>GitHub integration</a></li>
          <li><a href='#cookies-analytics'>Cookies and analytics</a></li>
          <li><a href='#recipients'>Who receives data</a></li>
          <li><a href='#transfers'>International transfers</a></li>
          <li><a href='#retention'>How long we keep data</a></li>
          <li><a href='#security'>Security</a></li>
          <li><a href='#your-rights'>Your rights</a></li>
          <li><a href='#deletion'>Account and content deletion</a></li>
          <li><a href='#automated-decisions'>Automated decisions</a></li>
          <li><a href='#changes'>Changes to this policy</a></li>
          <li><a href='#contact'>Contact</a></li>
        </ul>
      </nav>

      <div class='privacy-sections'>

        <section id='controller'>
          <h2>Who controls your data</h2>
          <p>Earde is a small, independently operated service. The operator of Earde decides how and why the personal data described in this policy is processed, and is the data controller for it.</p>
          <p>For any question or request about your data, contact the operator at <a href='mailto:metacirculardispatches@gmail.com'>metacirculardispatches@gmail.com</a>. Earde has not appointed a data protection officer.</p>
        </section>

        <section id='data-we-collect'>
          <h2>Data we collect</h2>
          <h3>Account and profile</h3>
          <p>Creating an account requires a username, an email address and a password. Without them you can read public content but cannot post, comment, chat, vote or join communities. The password is stored only as a salted argon2id hash &mdash; Earde cannot read it back. Your email address is used to confirm your account and to reset your password; it is never displayed publicly and is not included in analytics. You can optionally add a bio and an avatar image to your profile; both are public.</p>
          <h3>Signup confirmation</h3>
          <p>When you sign up, Earde stores a pending record with your chosen username, email address, password hash, a hashed confirmation token, and the IP address and browser identifier (user agent) of the signup request. The confirmation link expires after 24 hours. Signup may also include a Cloudflare Turnstile bot check (see <a href='#recipients'>Who receives data</a>).</p>
          <h3>Service and technical data</h3>
          <ul>
            <li>Login sessions, stored server-side in Earde's database; the browser cookie holds only a session identifier.</li>
            <li>Rate-limiting records keyed by IP address and endpoint, for signup, login and password-reset requests.</li>
            <li>First-party page-view statistics: the page path, the referring site's host name, and a pseudonymous identifier rebuilt each day from IP address, browser and date &mdash; it cannot link your visits across days, and the raw IP address is not stored with it.</li>
            <li>Server request logs for operating and debugging the service; security-sensitive values (tokens, authorization codes) are redacted before logging.</li>
          </ul>
          <h3>Community content and activity</h3>
          <p>Posts, comments, chat messages, votes, karma, community memberships, reports you file, moderation actions that concern you, and notifications addressed to you. Live presence, typing and cursor indicators are transient signals that are broadcast to other viewers of the page and are not stored.</p>
          <h3>GitHub project data</h3>
          <p>Only if you connect an open-source project &mdash; see <a href='#github'>GitHub integration</a>.</p>
          <h3>Analytics data</h3>
          <p>Only after you allow it &mdash; see <a href='#cookies-analytics'>Cookies and analytics</a>.</p>
          <h3>Where this data comes from</h3>
          <p>From you (forms and the content you write), from your browser (technical data that accompanies each request), and from GitHub (public metadata, when you connect a project).</p>
        </section>

        <section id='how-we-use'>
          <h2>How and why we use data</h2>
          <div class='privacy-table-wrap'>
            <table class='privacy-table'>
              <thead><tr><th>Purpose</th><th>Data</th><th>Legal basis</th></tr></thead>
              <tbody>
                <tr><td>Providing your account and the service: signing you in, publishing the content you write, memberships, notifications, data export</td><td>Account, profile, content and activity</td><td>Performance of a contract (Art. 6(1)(b) GDPR)</td></tr>
                <tr><td>Sending transactional email: signup confirmation and password reset</td><td>Email address</td><td>Performance of a contract</td></tr>
                <tr><td>Security and abuse prevention: rate limiting, the signup bot check, bans, moderation and report handling</td><td>IP address, technical data, moderation records</td><td>Legitimate interest: keeping the service and its communities secure and usable. You can object (see <a href='#your-rights'>Your rights</a>).</td></tr>
                <tr><td>Aggregate first-party usage statistics (page-view counts with a daily-rotating pseudonymous identifier)</td><td>Technical data</td><td>Legitimate interest: understanding aggregate usage of a public service without profiling individuals across days. You can object.</td></tr>
                <tr><td>Optional product analytics and session replay (PostHog)</td><td>Analytics data</td><td>Consent (Art. 6(1)(a) GDPR), withdrawable at any time</td></tr>
                <tr><td>Complying with the law when a competent authority lawfully requires information</td><td>As required</td><td>Legal obligation</td></tr>
              </tbody>
            </table>
          </div>
          <p>Earde does not rely on legitimate interest for optional analytics, and does not treat publishing content as blanket consent to unrelated processing.</p>
        </section>

        <section id='public-content'>
          <h2>Public content and search engines</h2>
          <p>How visible your content is depends on the visibility of the specific surface you post it on, not just on the community it belongs to. Content posted in publicly accessible communities, channels and sections &mdash; posts, comment threads and chat channel archives &mdash; can be read by anyone, including signed-out visitors, and may be indexed by search engines. Your profile page &mdash; username, bio, avatar, join date and your posts and comments on publicly accessible surfaces &mdash; is public too. A community's moderation log (the action taken, the acting moderator's username and the reason given) is visible to anyone who can view that community.</p>
          <p>Vote totals on posts and comments are public; which way you voted is stored but not shown to other users. When a chat conversation is promoted into a thread, the thread displays the referenced chat messages with their authors, subject to the source surface's access rules.</p>
          <p>Content posted in private, members-only or moderator-only areas &mdash; private communities and everything inside them, moderation queues, and the private workflow records described elsewhere in this policy &mdash; is limited to users authorized to view that area (which always includes Earde's administrators). Pages of private communities also ask search engines not to index them.</p>
          <p>Anything published publicly can be crawled, cached, quoted or copied by search engines and other third parties. Deleting content on Earde stops Earde from displaying it, but does not reach copies that others have already made outside Earde's control.</p>
        </section>

        <section id='shared-threads'>
          <h2>Connected communities and Shared Threads</h2>
          <p>Independently governed communities on Earde can connect with each other, and a thread from one community can be shared into another. There is always one canonical thread, owned and moderated by its origin community; accepted destination communities display that same thread, and their members can read and comment on it according to the current access rules. A shared thread stops being shown in destinations if its origin community becomes private.</p>
          <p>A share or connection request can carry an optional private note. That note is visible only to the person who wrote it, the top moderators of the origin and destination communities involved, and Earde's administrators. It never appears publicly, and it is not shown to the thread's author unless they made the request themselves.</p>
          <p>Top moderators of the affected communities (and, for project requests, the project's stewards) receive structured notifications about these requests. Earde also keeps internal lifecycle records of who requested, accepted, rejected or removed a connection or placement and when; these records contain no free-text content.</p>
        </section>

        <section id='github'>
          <h2>GitHub integration</h2>
          <p>Connecting an open-source project through GitHub is optional, and it is not a way to sign in: your Earde account remains a separate username-and-password account.</p>
          <p>When you connect a project, Earde receives from GitHub and stores: the numeric ID, login name and type (user or organization) of the GitHub account the app is installed on; the installation's identifier and status; and, for the public repositories you select, their repository IDs, names, owners, descriptions, default branches, archived status and public github.com URLs. Only repositories that are public on GitHub are stored &mdash; private and internal repositories are never kept. Project pages and connected community pages display this metadata publicly.</p>
          <p>Earde never stores your GitHub tokens. The short-lived token GitHub issues during the flow is used only for the read-only requests needed to verify the installation and list the public repositories available through it, and is then discarded. The integration does not read repository contents and does not change anything on GitHub. During the flow, a temporary encrypted cookie (15 minutes, scoped to the connect pages) carries the flow's security material.</p>
          <p>GitHub itself independently processes what you do on github.com &mdash; including the install and authorize screens &mdash; under its own <a href='https://docs.github.com/en/site-policy/privacy-policies/github-general-privacy-statement' target='_blank' rel='noopener'>privacy statement</a>.</p>
          <p>If you uninstall the Earde app on GitHub, no further access is possible, but the project metadata already stored on Earde is not removed automatically &mdash; <a href='#contact'>contact the operator</a> to have it removed.</p>
        </section>

        <section id='cookies-analytics'>
          <h2>Cookies and analytics</h2>
          <h3>Strictly necessary storage</h3>
          <div class='privacy-table-wrap'>
            <table class='privacy-table'>
              <thead><tr><th>Name</th><th>Purpose</th><th>Duration</th></tr></thead>
              <tbody>
                <tr><td>dream.session</td><td>Keeps you signed in. Holds only a session identifier; the session itself lives in Earde's database.</td><td>Up to two weeks; removed on logout.</td></tr>
                <tr><td>earde_analytics_consent</td><td>Remembers your analytics choice: exactly granted or denied.</td><td>About 180 days.</td></tr>
                <tr><td>Per-flow GitHub connect cookie</td><td>Carries encrypted security material during the GitHub connect flow only.</td><td>15 minutes.</td></tr>
              </tbody>
            </table>
          </div>
          <p>Forms are protected against cross-site request forgery by a signed token embedded in the page rather than a cookie.</p>
          <h3>Optional analytics (PostHog)</h3>
          <p>Nothing analytics-related loads before you choose: until you select Allow, the PostHog script is not downloaded and no request of any kind is made to PostHog. If you allow it, PostHog stores its own state in your browser (cookies and local storage prefixed ph_), which Earde clears again when you withdraw.</p>
          <p>After consent, analytics collects: page views with query strings, fragments and page titles stripped; structural click data that never includes on-page text; performance measurements; heatmaps; and Session Replay &mdash; a recording of page interactions in which everything you type and all user-generated text (posts, messages, usernames) is masked in your browser before anything is sent. Search queries are excluded from analytics, and JavaScript error capture is switched off.</p>
          <p>Signed-out visitors are identified by a random identifier. When you are signed in, analytics is linked to an internal pseudonymous identifier of the form user:&lt;number&gt;, together with your username, signup date and whether the account is an administrator &mdash; never your email address. Community context is reported by internal numeric identifiers; for private communities no name or slug is sent, and pages of private communities do not run analytics at all, even with consent. Server-side product events (for example that a thread was created or a project connected) are likewise sent only while your consent cookie says granted.</p>
          <p>Analytics data is sent to PostHog's EU cloud endpoints (eu.i.posthog.com). See <a href='https://posthog.com/privacy' target='_blank' rel='noopener'>PostHog's privacy notice</a> for how PostHog processes data.</p>
          <h3>Analytics preferences</h3>
          <p>Withdrawing consent is as easy as granting it. When optional analytics is active on this deployment, your current choice and the controls to change it appear below; they work for signed-in and signed-out visitors alike.</p>
          <div class='privacy-consent' data-analytics-settings hidden>
            <span data-analytics-state class='privacy-consent__state'></span>
            <div class='privacy-consent__actions'>
              <button type='button' data-analytics-accept class='privacy-consent__btn privacy-consent__btn--allow'>Allow analytics</button>
              <button type='button' data-analytics-refuse class='privacy-consent__btn'>Turn off analytics</button>
            </div>
            <span data-analytics-error hidden class='privacy-consent__error'>Couldn&#39;t save your choice &mdash; please try again.</span>
          </div>
          <p>When you withdraw, the analytics script stops collecting, its browser storage is cleared, and from the next page load it is not downloaded at all. Withdrawal does not retroactively erase data already collected; you can ask the operator to delete it, and deleting your account automatically requests deletion of your analytics profile and its events at PostHog.</p>
        </section>

        <section id='recipients'>
          <h2>Who receives data</h2>
          <ul>
            <li><strong>Brevo</strong> (transactional email delivery): receives your email address and the confirmation or password-reset message sent to you. See <a href='https://www.brevo.com/legal/privacypolicy/' target='_blank' rel='noopener'>Brevo's privacy policy</a>.</li>
            <li><strong>Cloudflare</strong> (Turnstile bot check on signup, when enabled): your browser loads the challenge widget directly from Cloudflare, which processes that interaction under <a href='https://www.cloudflare.com/privacypolicy/' target='_blank' rel='noopener'>Cloudflare's privacy policy</a>. Earde's own server sends Cloudflare only the challenge token to verify &mdash; not your IP address.</li>
            <li><strong>PostHog</strong> (EU cloud): the consented analytics data described above.</li>
            <li><strong>GitHub</strong>: when you connect a project, you interact with GitHub directly; GitHub acts as an independent service, not on Earde's behalf.</li>
            <li><strong>Hosting infrastructure</strong>: Earde runs on a server hosted with Hetzner; the service data described in this policy is stored there, in Earde's own PostgreSQL database.</li>
            <li><strong>Other users, moderators and communities</strong>: content according to its visibility; reports you file go to the moderators of the community concerned; private request notes reach the limited audience described under <a href='#shared-threads'>Shared Threads</a>.</li>
            <li><strong>Authorities</strong>: only if disclosure is lawfully required.</li>
          </ul>
          <p>Earde does not sell personal data and does not share it for cross-context behavioral advertising.</p>
        </section>

        <section id='transfers'>
          <h2>International transfers</h2>
          <p>Earde's analytics is configured to use PostHog's EU region endpoints. GitHub, Cloudflare and Brevo are independent global providers; when you interact with them as described above, they may process data outside the European Economic Area, as described in their own privacy notices linked in this policy.</p>
        </section>

        <section id='retention'>
          <h2>How long we keep data</h2>
          <ul>
            <li><strong>Account data</strong>: while your account exists. On deletion, your identifying details are removed as described under <a href='#deletion'>Account and content deletion</a>.</li>
            <li><strong>Login sessions</strong>: expire after about two weeks, or immediately on logout.</li>
            <li><strong>Signup confirmation records</strong> (including the signup IP address and browser identifier): valid for 24 hours, then removed during routine cleanup.</li>
            <li><strong>Password-reset records</strong>: valid for 2 hours; removed when used, unusable afterwards.</li>
            <li><strong>Rate-limiting records</strong> (IP address and endpoint): used only for the current one-minute request window; routine cleanup deletes records shortly after their window has lapsed.</li>
            <li><strong>First-party page-view statistics</strong>: kept as pseudonymous usage statistics; the daily-rotating identifier cannot link visits across days.</li>
            <li><strong>Posts and comments</strong>: until you or a moderator deletes them. Deleting a post or comment replaces its text with a neutral placeholder; the placeholder row remains so that surrounding discussion stays coherent.</li>
            <li><strong>Chat messages</strong>: until deleted. A deleted chat message is hidden from every reader, though the original text currently remains in the database record.</li>
            <li><strong>Reports, moderation logs and lifecycle records</strong>: retained while needed for community safety, moderation accountability and dispute handling.</li>
            <li><strong>Notifications</strong>: stored with your account; removed when the thing they point to is deleted.</li>
            <li><strong>Project and GitHub metadata</strong>: for as long as the project remains on Earde. It is not removed automatically when the connecting account is deleted; contact the operator to remove it.</li>
            <li><strong>Analytics data at PostHog</strong>: held by PostHog until deleted; deleting your Earde account triggers a deletion request for your analytics profile and its events.</li>
            <li><strong>Server logs</strong>: kept for operating and troubleshooting the service.</li>
          </ul>
        </section>

        <section id='security'>
          <h2>Security</h2>
          <ul>
            <li>Passwords are stored only as salted argon2id hashes, never in a readable form.</li>
            <li>One-time email tokens (signup confirmation, password reset) are stored only as SHA-256 hashes.</li>
            <li>Sessions are kept server-side; the browser cookie carries only an identifier. Every state-changing form is protected by a signed anti-forgery token.</li>
            <li>The GitHub connect flow uses PKCE, single-use state values stored only as hashes, and an encrypted, short-lived flow cookie.</li>
            <li>The production service is served over HTTPS, and security-sensitive values are redacted from server logs.</li>
          </ul>
          <p>No online service can promise perfect security. If a breach affecting your data occurs, the operator will handle it as applicable law requires.</p>
        </section>

        <section id='your-rights'>
          <h2>Your rights</h2>
          <p>Under the GDPR you can ask for access to your data, correction, deletion, restriction of processing, and a portable copy; you can object to processing based on legitimate interest; and you can withdraw consent (for analytics, directly via the <a href='#cookies-analytics'>controls above</a>) at any time without affecting past processing.</p>
          <p>You can exercise several of these yourself: edit your profile in <a href='/settings'>account settings</a>, download your profile, posts and comments as JSON via <a href='/export-data'>data export</a>, delete individual posts, comments and chat messages, and delete your whole account. For everything else &mdash; including access to data the export does not cover &mdash; email <a href='mailto:metacirculardispatches@gmail.com'>metacirculardispatches@gmail.com</a>; the operator may need to verify that you control the account concerned.</p>
          <p>You also have the right to lodge a complaint with a data protection supervisory authority, in particular in the EU member state where you live, where you work, or where you believe an infringement occurred.</p>
        </section>

        <section id='deletion'>
          <h2>Account and content deletion</h2>
          <p>You can delete your account at any time from <a href='/settings'>account settings</a> (Danger zone &rarr; Delete account). This is irreversible. When you do:</p>
          <ul>
            <li>Your username is replaced by a neutral placeholder, shown as [deleted]; your email address, password hash, bio and avatar are removed from the account record; the uploaded avatar image file is deleted from Earde's storage; and your sessions are ended.</li>
            <li>Earde automatically requests deletion of your analytics profile and its events at PostHog.</li>
            <li>Your posts, comments and chat messages remain in their communities, no longer attributed to you. Delete any of them individually first if you do not want them to remain.</li>
            <li>Records needed for community safety &mdash; reports, moderation logs, lifecycle records &mdash; and project or GitHub metadata you connected are retained as described under <a href='#retention'>How long we keep data</a>.</li>
            <li>Copies of formerly public content held by search engines or other third parties are outside Earde's control.</li>
          </ul>
        </section>

        <section id='automated-decisions'>
          <h2>Automated decisions</h2>
          <p>Earde does not make automated decisions about you that produce legal or similarly significant effects. Automated protections exist &mdash; rate limiting, the signup bot check and a spam trap &mdash; but they only limit form submissions; if you believe one blocked you in error, <a href='#contact'>contact the operator</a>.</p>
        </section>

        <section id='changes'>
          <h2>Changes to this policy</h2>
          <p>When this policy changes, the new version is published on this page with an updated date at the top, and material changes are summarized here. This page is always reachable without an account.</p>
        </section>

        <section id='contact'>
          <h2>Contact</h2>
          <p>Questions, requests, objections, or anything unclear: <a href='mailto:metacirculardispatches@gmail.com'>metacirculardispatches@gmail.com</a>.</p>
        </section>

      </div>
    </div>")
  in
  Page_shell.launch_entry_page ~request ~page_class:"launch-privacy"
    ~title:"Privacy Policy" ~content ()

(* === MESSAGE PAGE === *)

(* Single shell for errors, successes, and info — avoids per-handler inline HTML
   fragments that diverge in style and don't inherit the shared layout/nav.

   Cartographic Civic (pass 17): both historical ~auth branches — which had
   already converged on one byte-identical focused panel — now render one
   neutral launch message sheet via Components.launch_message_page. The
   caller contract is untouched: same signature, [title] and [message] are
   always escaped text (never trusted HTML), [return_url] is the renderer's
   own "Go back" destination interpolated exactly as before, and
   [alert_type] keeps its success / info / everything-else-is-error mapping
   onto the same three SVG glyphs. ?user and ?auth are accepted and ignored:
   the old `Auth chrome already rendered no viewer-dependent bytes, and the
   document must stay viewer-independent because anti-enumeration pins
   require byte-identical denials. No handler, status, header, redirect or
   message string changes.

   [return_url] is gated by Html.internal_path at this ONE shared
   sink rather than at each of the ~70 call sites. Several of those build the
   destination from a route parameter (e.g. "/c/" ^ Dream.param "slug"), and
   Dream percent-decodes parameters, so a crafted slug put attacker bytes
   straight into the href — reachable without a session and, via the
   CSRF-failure arm of the settings POSTs, without a token either. The gate
   escapes the value and collapses anything that is not a rooted internal
   path (foreign origins, "//host", javascript:, quote/markup payloads) to
   the inert "#". Every real caller passes a server-built rooted path, so no
   legitimate back link changes. *)
let msg_page ?user:_ ?auth:_ ~title ~message ~alert_type ~return_url request =
  let icon_html = match alert_type with
    | "success" ->
        (Html.static "<div class='launch-msg__icon launch-msg__icon--success'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'><path d='M5 13l4 4L19 7'/></svg></div>")
    | "info" ->
        (Html.static "<div class='launch-msg__icon launch-msg__icon--info'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><path d='M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z'/></svg></div>")
    | _ ->
        (Html.static "<div class='launch-msg__icon launch-msg__icon--error'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'><path d='M6 18L18 6M6 6l12 12'/></svg></div>")
  in
  let content = (Html.template "
        <div class='auth launch-msg'>
          <div class='auth__head'>
            <a class='launch-msg__brand' href='/feed' aria-label='Earde feed'><img class='auth__mark' src='/static/images/logo-mark.svg' alt=''></a>
            <p class='launch-msg__kicker'>Earde &middot; notice</p>
            <h1 class='auth__title'>%s</h1>
          </div>
          <div class='auth__card launch-msg__card'>
            %s
            <p class='launch-msg__text'>%s</p>
            <div class='launch-msg__foot'><a href='%s' class='launch-msg__back'>Go back</a></div>
          </div>
        </div>"
  [ (Html.text (title))
  ; icon_html
  ; (Html.text (message))
  ; (Html.internal_path (return_url)) ])
  in
  Page_shell.launch_message_page ~request ~title ~content ()
