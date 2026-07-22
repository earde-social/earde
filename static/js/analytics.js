/* PostHog browser integration (spec: docs/features/posthog-analytics.md §2/§6/§9).
 *
 * This local file loads on every layout page, but connects to PostHog only
 * AFTER analytics consent is granted: no SDK download, no preconnect, no
 * request of any kind beforehand. Consent state is the plaintext
 * earde_analytics_consent cookie, owned by the Dream endpoint
 * POST /analytics/consent — this script never writes the cookie itself.
 * Every failure path is swallowed into controlled UI state; analytics must
 * never break a page. IIFE, no dependencies, matching chat_live.js style. */
(function () {
  "use strict";

  var banner = document.getElementById("analytics-consent");
  if (!banner) return;
  var token = banner.getAttribute("data-ph-token");
  var apiHost = banner.getAttribute("data-ph-api-host");
  if (!token || !apiHost) return;

  /* §4.2/§5.3 page context, rendered by the Dream layout: the authenticated
     identity is exactly "user:<database_id>" (absent on anonymous pages); the
     community group key is exactly "community:<database_id>" (absent on
     global pages). No other identity or group data ever reaches the DOM. */
  var identityAttr = banner.getAttribute("data-analytics-user");
  var groupAttr = banner.getAttribute("data-analytics-group");

  /* §13 fully private communities: the server-rendered marker (derived from
     the authoritative community visibility, value only — never a name or
     slug) makes this document analytics-inert. Consent controls still work
     and the consent endpoint still records the choice, but the SDK is never
     loaded or initialized here: no pageview, no autocapture, no replay, no
     group call, no custom event, no PostHog network request. Analytics
     resumes on the next eligible public/global page load. */
  var privateCommunity =
    banner.getAttribute("data-analytics-private-community") === "true";

  var COOKIE_NAME = "earde_analytics_consent";

  /* Exact parse mirroring the server: only the exact values granted/denied
     count; anything else means "no choice yet". */
  function consentState() {
    var parts = document.cookie ? document.cookie.split(";") : [];
    for (var i = 0; i < parts.length; i++) {
      var part = parts[i].trim();
      if (part.indexOf(COOKIE_NAME + "=") === 0) {
        var value = part.slice(COOKIE_NAME.length + 1);
        return value === "granted" || value === "denied" ? value : null;
      }
    }
    return null;
  }

  /* §2.3 URL rule (mirrors Analytics.sanitize_url_for_analytics): origin +
     pathname only — never query strings (tokens, search text), never
     fragments. */
  function stripUrl(raw) {
    if (!raw) return raw;
    try {
      var u = new URL(raw, window.location.href);
      return u.origin + u.pathname;
    } catch (e) {
      return null;
    }
  }

  /* Autocapture text protection (§13): autocapture keeps structural click
     metadata only — tag, classes, event type, position. Visible text (post
     titles, search results, usernames) must never enter a payload. The SDK's
     mask_all_text option removes element text at the source; the scrubbers
     below are defense in depth on every payload shape the SDK can emit
     ($el_text, objects inside $elements, the encoded $elements_chain
     string), and also strip query strings/fragments from URL-bearing
     element attributes. */
  function stripUrlValue(raw) {
    return String(raw).split("?")[0].split("#")[0];
  }

  var ELEMENT_TEXT_KEYS = [
    "$el_text",
    "text",
    "attr__title",
    "attr__aria-label",
    "attr__alt",
    "attr__placeholder",
    "attr__value",
    "attr__label"
  ];
  var ELEMENT_URL_KEYS = ["attr__href", "attr__src", "attr__action"];

  function sanitizeElement(el) {
    if (!el || typeof el !== "object") return el;
    ELEMENT_TEXT_KEYS.forEach(function (key) {
      if (key in el) delete el[key];
    });
    ELEMENT_URL_KEYS.forEach(function (key) {
      if (typeof el[key] === "string") el[key] = stripUrlValue(el[key]);
    });
    return el;
  }

  function sanitizeElementsChain(chain) {
    return String(chain)
      .replace(/text="[^"]*"/g, "")
      .replace(/attr__(?:title|aria-label|alt|placeholder|value|label)="[^"]*"/g, "")
      .replace(
        /((?:attr__)?(?:href|src|action)=")([^"]*)"/g,
        function (_match, prefix, url) {
          return prefix + stripUrlValue(url) + '"';
        }
      );
  }

  function sanitizeProperties(props) {
    if (!props) return props;
    if (props.$current_url) props.$current_url = stripUrl(props.$current_url);
    if (props.$referrer) props.$referrer = stripUrl(props.$referrer);
    if (props.$pathname)
      props.$pathname = String(props.$pathname).split("?")[0].split("#")[0];
    /* Document titles embed user content (post titles; historically search
       terms): removed from every event, never replaced with another
       user-controlled value. */
    if ("$title" in props) delete props.$title;
    if ("$el_text" in props) delete props.$el_text;
    if (Array.isArray(props.$elements))
      props.$elements = props.$elements.map(sanitizeElement);
    if (typeof props.$elements_chain === "string")
      props.$elements_chain = sanitizeElementsChain(props.$elements_chain);
    return props;
  }

  /* Replay masking (§6): covers SSR nodes and the identical classes
     chat_live.js uses for live-inserted messages/typing/presence. Inputs and
     textareas are all masked via maskAllInputs. "title" masks the document
     <title> text in snapshots — verified against the rrweb source bundled by
     posthog-js: maskTextSelector matches a text node's parent element
     (el.closest), and only STYLE/SCRIPT are excluded, so titles carrying
     user-generated content (posts, communities, profiles) never reach
     recordings in clear text. */
  var MASK_TEXT_SELECTOR = [
    "title",
    ".ph-mask",
    ".cs-msg-text",
    ".cs-msg-author",
    "#chat-typing",
    ".cs-presence-name",
    ".ft-title",
    ".ft-preview",
    ".th-title",
    ".th-body",
    ".sr-row-title",
    ".sr-row-excerpt",
    ".ctext",
    '[id^="comment-content-"]',
    ".account-notif-msg",
    ".cm-table-reason",
    ".admin-cell-muted",
    ".account-bio"
  ].join(", ");

  /* Module-owned idempotent initialization: one shared Promise; repeated
     calls (banner + settings, double clicks) reuse the same attempt, so
     identify/group/pageview each run at most once per document load. No
     undocumented PostHog internals are consulted. */
  var initPromise = null;
  var pageviewSent = false;
  var searchPerformedSent = false;

  /* §4.2 identity reconciliation. Runs after init, before anything else:
     - identity attribute present: identify only when the persisted distinct
       id differs (a matching id must not identify again). No person
       properties are ever sent from the browser ($set is server-owned).
     - attribute absent (anonymous page): a persisted "user:"-prefixed id
       means the user logged out or was deleted → reset() to a fresh
       anonymous id; an already-anonymous id is preserved untouched. */
  function reconcileIdentity() {
    var current = window.posthog.get_distinct_id();
    if (identityAttr) {
      if (current !== identityAttr) {
        window.posthog.identify(identityAttr);
      }
    } else if (typeof current === "string" && current.indexOf("user:") === 0) {
      window.posthog.reset();
    }
  }

  /* §5.3 group reconciliation. Runs after identity reconciliation (so it
     applies to the post-reset identity) and before the pageview:
     - group attribute present: session-sticky group set by key only — group
       properties are owned by the server via $groupidentify (later step);
     - absent (global pages): actively clear the sticky group so a previously
       visited community never leaks onto /feed, search, settings, etc. */
  function reconcileGroup() {
    if (groupAttr) {
      window.posthog.group("community", groupAttr);
    } else {
      window.posthog.resetGroups();
    }
  }

  /* §2.4 search_performed: the ONLY UI event. Reads the closed
     server-rendered metadata container (#sr-analytics, present only when a
     non-empty search actually executed) and captures at most one event per
     document load, immediately after the manual $pageview. Strict allowlist
     parsing: the tab must belong to the exact closed UI set, the count must
     be a non-negative integer, the page a positive integer — anything
     missing or malformed means NO event, never a guessed value. A fresh
     object with exactly the three permitted properties is captured; the raw
     dataset is never passed through, and the query text has no path in: the
     container carries none and nothing else is read. */
  var SEARCH_TABS = ["posts", "communities", "comments", "people"];

  function captureSearchPerformed() {
    if (searchPerformedSent) return;
    var meta = document.getElementById("sr-analytics");
    if (!meta) return;
    var tab = meta.getAttribute("data-analytics-search-tab");
    var countRaw = meta.getAttribute("data-analytics-search-result-count");
    var pageRaw = meta.getAttribute("data-analytics-search-page");
    if (SEARCH_TABS.indexOf(tab) === -1) return;
    if (!/^[0-9]+$/.test(countRaw || "") || !/^[0-9]+$/.test(pageRaw || ""))
      return;
    var resultCount = parseInt(countRaw, 10);
    var page = parseInt(pageRaw, 10);
    if (page < 1) return;
    searchPerformedSent = true;
    window.posthog.capture("search_performed", {
      result_count: resultCount,
      active_tab: tab,
      page: page
    });
  }

  function loadSdk() {
    return new Promise(function (resolve, reject) {
      if (window.posthog && typeof window.posthog.init === "function") {
        resolve();
        return;
      }
      var script = document.createElement("script");
      script.async = true;
      /* Official snippet CDN path: the SDK bundle is served from the assets
         host derived from the ingest host (eu.i.posthog.com →
         eu-assets.i.posthog.com), legacy latest-1.x path. */
      script.src =
        apiHost.replace(".i.posthog.com", "-assets.i.posthog.com") +
        "/static/array.js";
      script.onload = function () {
        resolve();
      };
      script.onerror = function () {
        reject(new Error("analytics sdk failed to load"));
      };
      document.head.appendChild(script);
    });
  }

  function initAnalytics() {
    if (initPromise) return initPromise;
    if (privateCommunity) {
      /* §13: one centralized gate — every code path that could load or talk
         to PostHog funnels through initAnalytics, so a private-community
         document resolves to an inert no-op while staying idempotent. */
      initPromise = Promise.resolve();
      return initPromise;
    }
    initPromise = loadSdk()
      .then(function () {
        if (!(window.posthog && typeof window.posthog.init === "function"))
          return;
        window.posthog.init(token, {
          api_host: apiHost,
          persistence: "localStorage+cookie",
          capture_pageview: false,
          capture_pageleave: true,
          autocapture: true,
          /* §13: autocapture is structural-only — element text never leaves
             the page. The sanitizeProperties scrubbers above are the
             defense-in-depth layer for the same rule. */
          mask_all_text: true,
          enable_heatmaps: true,
          capture_performance: { web_vitals: true },
          /* §13: automatic exception capture is DISABLED. Raw JS error
             messages/stacks are arbitrary data and violate the closed
             property contract; Error Tracking returns only with a closed
             error_code/error_stage allowlist and explicit sanitization.
             This file installs no global error or rejection handler. */
          capture_exceptions: false,
          sanitize_properties: function (props) {
            return sanitizeProperties(props);
          },
          session_recording: {
            maskAllInputs: true,
            maskTextSelector: MASK_TEXT_SELECTOR,
            recordHeaders: false,
            recordBody: false
          }
        });
        /* Required order (§4.2/§5.3/§2.4): init → identify/reset → group
           set/reset → the single manual pageview → search_performed, so both
           events are attributed to the correct person and (for search: no)
           community. This block runs once per document load (shared
           initPromise), so neither event can be duplicated by repeated
           initialization calls. */
        reconcileIdentity();
        reconcileGroup();
        if (!pageviewSent) {
          pageviewSent = true;
          window.posthog.capture("$pageview", {
            $current_url: window.location.origin + window.location.pathname
          });
        }
        captureSearchPerformed();
      })
      .catch(function () {
        /* Swallowed: a blocked/failed SDK load must not break the page. */
      });
    return initPromise;
  }

  /* Revocation cleanup: drop PostHog persistence (ph_* cookies and
     localStorage). Safe to call whether or not the SDK ever loaded. */
  function clearPosthogPersistence() {
    try {
      (document.cookie ? document.cookie.split(";") : []).forEach(function (part) {
        var name = part.trim().split("=")[0];
        if (name.indexOf("ph_") === 0) {
          document.cookie = name + "=; Max-Age=0; path=/";
        }
      });
      var doomed = [];
      for (var i = 0; i < window.localStorage.length; i++) {
        var key = window.localStorage.key(i);
        if (key && key.indexOf("ph_") === 0) doomed.push(key);
      }
      doomed.forEach(function (key) {
        window.localStorage.removeItem(key);
      });
    } catch (e) {
      /* Storage may be unavailable; never break the page. */
    }
  }

  function postConsent(state) {
    return fetch("/analytics/consent", {
      method: "POST",
      credentials: "same-origin",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ state: state })
    }).then(function (response) {
      if (!response.ok) throw new Error("consent endpoint " + response.status);
    });
  }

  /* One in-flight submission across banner + settings; controls re-enable on
     failure with a concise retryable error. */
  var submitting = false;

  function bindControls(root, hooks) {
    var accept = root.querySelector("[data-analytics-accept]");
    var refuse = root.querySelector("[data-analytics-refuse]");
    var error = root.querySelector("[data-analytics-error]");
    function setBusy(busy) {
      if (accept) accept.disabled = busy;
      if (refuse) refuse.disabled = busy;
    }
    function submit(state, after) {
      if (submitting) return;
      submitting = true;
      setBusy(true);
      if (error) error.hidden = true;
      postConsent(state)
        .then(function () {
          submitting = false;
          setBusy(false);
          after();
        })
        .catch(function () {
          submitting = false;
          setBusy(false);
          if (error) error.hidden = false;
          if (hooks.onError) hooks.onError();
        });
    }
    if (accept)
      accept.addEventListener("click", function () {
        submit("granted", hooks.onGranted);
      });
    if (refuse)
      refuse.addEventListener("click", function () {
        submit("denied", hooks.onDenied);
      });
  }

  /* --- Settings control (account page) ------------------------------------ */
  var settings = document.querySelector("[data-analytics-settings]");

  function refreshSettings() {
    if (!settings) return;
    var state = consentState();
    var accept = settings.querySelector("[data-analytics-accept]");
    var refuse = settings.querySelector("[data-analytics-refuse]");
    var label = settings.querySelector("[data-analytics-state]");
    if (accept) accept.hidden = state === "granted";
    if (refuse) refuse.hidden = state !== "granted";
    if (label)
      label.textContent =
        state === "granted"
          ? "Analytics is currently enabled."
          : state === "denied"
            ? "Analytics is currently disabled."
            : "You have not made a choice yet.";
    settings.hidden = false;
  }

  if (settings) {
    bindControls(settings, {
      onGranted: function () {
        banner.hidden = true;
        refreshSettings();
        initAnalytics();
      },
      onDenied: function () {
        banner.hidden = true;
        refreshSettings();
        /* Revocation: opt out + reset only if the SDK actually loaded — the
           remote SDK is never fetched just to process a denial. */
        if (
          window.posthog &&
          typeof window.posthog.opt_out_capturing === "function"
        ) {
          try {
            window.posthog.opt_out_capturing();
            window.posthog.reset();
          } catch (e) {
            /* swallowed */
          }
        }
        clearPosthogPersistence();
      },
      onError: function () {}
    });
    refreshSettings();
  }

  /* --- Banner -------------------------------------------------------------- */
  bindControls(banner, {
    onGranted: function () {
      banner.hidden = true;
      refreshSettings();
      initAnalytics();
    },
    onDenied: function () {
      banner.hidden = true;
      refreshSettings();
    },
    onError: function () {
      banner.hidden = false;
    }
  });

  var state = consentState();
  if (state === "granted") {
    initAnalytics();
  } else if (state === null) {
    banner.hidden = false;
  }
  /* denied: SDK never loads, banner stays hidden. */
})();
