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
          enable_heatmaps: true,
          capture_performance: { web_vitals: true },
          capture_exceptions: true,
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
        /* Required order (§4.2/§5.3): init → identify/reset → group
           set/reset → the single manual pageview, so the pageview is
           attributed to the correct person and community. */
        reconcileIdentity();
        reconcileGroup();
        if (!pageviewSent) {
          pageviewSent = true;
          window.posthog.capture("$pageview", {
            $current_url: window.location.origin + window.location.pathname
          });
        }
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
