(function () {
  const root = document.getElementById("chat-live-root");

  if (!root) {
    return;
  }

  const bottomThresholdPx = 100;

  function getChatScroller() {
    if (root.matches(".cs-main-body, [data-chat-scroll-container]")) {
      return root;
    }

    const candidates = [
      root.closest("[data-chat-scroll-container]"),
      root.closest(".cs-main-body"),
      root.querySelector("[data-chat-scroll-container]"),
      root.querySelector(".cs-main-body"),
    ].filter(Boolean);

    return candidates[0] || root;
  }

  function isNearBottom(scroller) {
    return scroller.scrollHeight - scroller.scrollTop - scroller.clientHeight <= bottomThresholdPx;
  }

  function scrollToBottom(scroller) {
    scroller.scrollTop = scroller.scrollHeight;
  }

  function scrollToBottomSoon(scroller) {
    if (!scroller) {
      return;
    }

    requestAnimationFrame(() => {
      scrollToBottom(scroller);
      requestAnimationFrame(() => scrollToBottom(scroller));
      window.setTimeout(() => scrollToBottom(scroller), 100);
    });
  }

  const chatScroller = getChatScroller();

  // Reverse navigation (?source_thread=…): the server renders the anchored window plus
  // data-source-anchor-id / data-source-highlight-ids. Scroll to the first source
  // message instead of the bottom, flash every loaded source row as one group, then
  // return to completely normal chat behavior. Rows missing from the window (deleted or
  // out of range) are skipped silently.
  const sourceAnchorId = root.dataset.sourceAnchorId || "";
  let inSourceFocus = false;

  if (sourceAnchorId !== "") {
    const anchorRow = root.querySelector(
      `[data-message-id="${sourceAnchorId}"]`
    );

    if (anchorRow) {
      inSourceFocus = true;

      const highlightIds = (root.dataset.sourceHighlightIds || "")
        .split(",")
        .filter(Boolean);

      const highlighted = [];
      highlightIds.forEach((id) => {
        const row = root.querySelector(`[data-message-id="${id}"]`);
        if (row) {
          row.classList.add("cs-msg--source-flash");
          highlighted.push(row);
        }
      });

      // A per-message "view in chat" link carries #msg-<id>; honor that specific
      // target when it is loaded, otherwise center the first source message.
      let scrollTarget = anchorRow;
      if (/^#msg-\d+$/.test(window.location.hash)) {
        const hashRow = document.getElementById(window.location.hash.slice(1));
        if (hashRow) {
          scrollTarget = hashRow;
        }
      }

      requestAnimationFrame(() => {
        scrollTarget.scrollIntoView({ block: "center" });
      });

      window.setTimeout(() => {
        highlighted.forEach((row) => row.classList.remove("cs-msg--source-flash"));
      }, 4000);
    }
  }

  if (!inSourceFocus) {
    scrollToBottomSoon(chatScroller);
  }

  // --- Shared page constants ------------------------------------------------
  // Everything the rendering core and the composer need, independent of
  // whether the realtime socket ever initializes on this page.
  const channelBasePath = window.location.pathname.replace(/\/$/, "");
  const catchUpUrl = `${channelBasePath}/messages.json`;
  const canStart = root.dataset.canStart === "true";

  function readLastMessageIdFromDom() {
    let maxId = 0;

    root.querySelectorAll(".cs-msg[data-message-id]").forEach((el) => {
      const id = Number(el.dataset.messageId);
      if (Number.isFinite(id) && id > maxId) {
        maxId = id;
      }
    });

    return maxId;
  }

  // Highest rendered message id — drives display-side decisions only.
  let lastMessageId = readLastMessageIdFromDom();

  // Recovery cursor for HTTP catch-up. Deliberately separate from
  // lastMessageId: it starts at the highest SSR id and advances ONLY through
  // successful catch-up responses, never through realtime events. A message
  // that was persisted but whose gateway publish was lost therefore stays
  // recoverable even after later live messages have rendered — the next
  // catch-up re-covers the gap from the last HTTP-confirmed point.
  let recoveryCursor = lastMessageId;

  // --- Ordered insertion ----------------------------------------------------
  // SSR, composer-response, catch-up and realtime rows all converge into
  // ascending message-id order. The scan walks from the tail because the
  // overwhelmingly common insert is an append; mid-stream inserts (catch-up
  // backfilling a gap) compensate scrollTop when they land above the viewport
  // so the reader's visual position never moves.
  function insertRowOrdered(row, id) {
    let ref = null;

    if (Number.isFinite(id)) {
      let node = root.lastElementChild;
      while (node) {
        const nodeId = Number(node.dataset ? node.dataset.messageId : NaN);
        if (Number.isFinite(nodeId) && nodeId < id) {
          break;
        }
        if (Number.isFinite(nodeId)) {
          ref = node;
        }
        node = node.previousElementSibling;
      }
    }

    const prevTop = chatScroller.scrollTop;
    const prevHeight = chatScroller.scrollHeight;

    if (ref) {
      root.insertBefore(row, ref);
      if (row.offsetTop < prevTop) {
        chatScroller.scrollTop = prevTop + (chatScroller.scrollHeight - prevHeight);
      }
    } else {
      root.appendChild(row);
    }
  }

  function appendMessage(payload, options = {}) {
    if (payload.id !== undefined && payload.id !== null) {
      const existing = root.querySelector(`[data-message-id="${payload.id}"]`);
      if (existing) {
        return;
      }
    }

    const shouldStayAtBottom =
      options.forceScroll === true ||
      (options.wasNearBottom !== undefined ? options.wasNearBottom : isNearBottom(chatScroller));

    const empty = root.querySelector(".cs-msg-empty");
    if (empty) {
      empty.remove();
    }

    const name = payload.username || "[deleted]";
    const initial =
      name.length > 0 && name[0] !== "[" ? name[0].toUpperCase() : "?";

    const row = document.createElement("div");
    row.className = "cs-msg";

    const numericId = Number(payload.id);

    if (payload.id !== undefined && payload.id !== null) {
      row.dataset.messageId = String(payload.id);

      if (Number.isFinite(numericId) && numericId > lastMessageId) {
        lastMessageId = numericId;
      }
    }

    const avatar = document.createElement("div");
    avatar.className = "cs-msg-avatar";
    avatar.textContent = initial;

    const body = document.createElement("div");
    body.className = "cs-msg-body";

    const meta = document.createElement("div");
    meta.className = "cs-msg-meta";

    const author = document.createElement("span");
    author.className = "cs-msg-author";
    author.textContent = name;

    const time = document.createElement("span");
    time.className = "cs-msg-time";
    time.textContent = payload.created_at || "";

    const text = document.createElement("div");
    text.className = "cs-msg-text";
    text.textContent = payload.content || "";

    meta.appendChild(author);

    const hasId = payload.id !== undefined && payload.id !== null;
    const hasAuthor = payload.user_id !== undefined && payload.user_id !== null;
    const isDeleted = payload.deleted === true;
    const isPromoted = payload.thread_id !== undefined && payload.thread_id !== null;
    row.dataset.hasThread = isPromoted ? "true" : "false";

    if (canStart && hasId && hasAuthor && !isDeleted && !isPromoted) {
      const slot = document.createElement("span");
      slot.className = "cs-msg-time-slot";

      const startLink = document.createElement("a");
      startLink.className = "cs-msg-start";
      startLink.href = `${channelBasePath}/messages/${encodeURIComponent(
        String(payload.id)
      )}/start-thread`;
      startLink.dataset.promoteUrl = startLink.href;
      row.dataset.promoteUrl = startLink.href;
      startLink.textContent = "Start thread";

      slot.appendChild(time);
      slot.appendChild(startLink);
      meta.appendChild(slot);
    } else {
      meta.appendChild(time);
    }

    body.appendChild(meta);
    body.appendChild(text);
    row.appendChild(avatar);
    row.appendChild(body);

    insertRowOrdered(row, numericId);

    if (shouldStayAtBottom) {
      scrollToBottomSoon(chatScroller);
    }
  }

  // --- HTTP catch-up --------------------------------------------------------
  // Requests start after the recovery cursor and merge through the same
  // ordered, deduplicated insertion as every other row. The cursor advances
  // only when a page arrives successfully; timeout, abort and HTTP errors
  // leave it (and catchUpInFlight) reset so the next cycle retries the same
  // range.
  const catchUpPageSize = 100; // server-side LIMIT of messages.json
  const catchUpMaxPagesPerCycle = 5;
  const catchUpTimeoutMs = 10000;
  const catchUpContinuationDelayMs = 250;
  const catchUpReconcileDelayMs = 4000;
  let catchUpInFlight = false;
  let catchUpTimer = null;

  function scheduleCatchUp(delayMs) {
    window.clearTimeout(catchUpTimer);
    catchUpTimer = window.setTimeout(catchUp, delayMs);
  }

  async function catchUp() {
    if (catchUpInFlight) {
      return;
    }

    catchUpInFlight = true;
    let continueLater = false;

    try {
      for (let page = 0; page < catchUpMaxPagesPerCycle; page++) {
        const url = `${catchUpUrl}?after_id=${encodeURIComponent(String(recoveryCursor))}`;
        const controller = new AbortController();
        const timeoutTimer = window.setTimeout(() => controller.abort(), catchUpTimeoutMs);

        let data;
        try {
          const response = await fetch(url, {
            method: "GET",
            headers: { Accept: "application/json" },
            credentials: "same-origin",
            signal: controller.signal,
          });

          if (!response.ok) {
            console.warn("[chat_live] catch-up failed", response.status);
            return;
          }

          data = await response.json();
        } finally {
          window.clearTimeout(timeoutTimer);
        }

        const messages = Array.isArray(data.messages) ? data.messages : [];
        const wasNearBottom = isNearBottom(chatScroller);

        messages.forEach((message) => {
          appendMessage(message, { wasNearBottom });

          const id = Number(message.id);
          if (Number.isFinite(id) && id > recoveryCursor) {
            recoveryCursor = id;
          }
        });

        if (messages.length < catchUpPageSize) {
          return;
        }
      }

      // Page cap reached on a full page: more may remain, so schedule a
      // continuation instead of silently treating recovery as complete.
      continueLater = true;
    } catch (err) {
      console.warn("[chat_live] catch-up exception", err);
    } finally {
      catchUpInFlight = false;
      if (continueLater) {
        scheduleCatchUp(catchUpContinuationDelayMs);
      }
    }
  }

  // --- Typing emission ------------------------------------------------------
  // Emission state lives up here so the composer can clear typing on send
  // even when realtime never initializes; the realtime section plugs the
  // actual channel transport into realtimeTypingPush.
  const typingRefreshMs = 2500;
  const typingIdleMs = 4000;
  let typingActive = false;
  let lastTypingSentAt = 0;
  let typingIdleTimer = null;
  let realtimeTypingPush = null;

  function pushTyping(active) {
    if (realtimeTypingPush) {
      realtimeTypingPush(active);
    }
  }

  function sendTypingActive() {
    const now = Date.now();

    if (!typingActive || now - lastTypingSentAt >= typingRefreshMs) {
      typingActive = true;
      lastTypingSentAt = now;
      pushTyping(true);
    }

    window.clearTimeout(typingIdleTimer);
    typingIdleTimer = window.setTimeout(sendTypingStop, typingIdleMs);
  }

  function sendTypingStop() {
    window.clearTimeout(typingIdleTimer);
    typingIdleTimer = null;

    if (!typingActive) {
      return;
    }

    typingActive = false;
    lastTypingSentAt = 0;
    pushTyping(false);
  }

  // --- Composer -------------------------------------------------------------
  // Submission is intercepted and sent over fetch with Accept:
  // application/json; the server answers with the canonical persisted row
  // (same shape as a realtime new_msg), which is inserted immediately — the
  // later realtime echo is absorbed by id deduplication. The <form> itself is
  // untouched, so with JavaScript disabled it still POSTs and redirects.
  const composerForm = document.querySelector(".cs-composer form[action='/messages']");

  if (composerForm) {
    const textarea = composerForm.querySelector("textarea[name='content']");
    const sendButton = composerForm.querySelector("button[type='submit'], .cs-send");

    // Bounded inline error line; cleared on the next input or success.
    const composerError = document.createElement("div");
    composerError.className = "cs-composer-error";
    composerError.setAttribute("role", "alert");
    composerError.hidden = true;
    composerForm.parentElement.appendChild(composerError);

    function showComposerError(message) {
      composerError.textContent = String(message || "Could not send message.").slice(0, 200);
      composerError.hidden = false;
    }

    function clearComposerError() {
      composerError.textContent = "";
      composerError.hidden = true;
    }

    function setComposerPending(pending) {
      composerForm.dataset.submitting = pending ? "true" : "false";
      if (sendButton) {
        sendButton.disabled = pending;
      }
    }

    // Focus restoration guard: after the request settles, return focus to the
    // composer only when focus is still on the composer itself, on the (now
    // disabled) send button, or fell back to <body> because we disabled that
    // button. If the user deliberately moved to any other control while the
    // request was pending, their focus is left alone.
    function shouldRestoreComposerFocus() {
      const active = document.activeElement;
      return (
        active === null ||
        active === textarea ||
        active === sendButton ||
        active === document.body
      );
    }

    function composerErrorMessage(data, status) {
      if (data && typeof data.message === "string" && data.message !== "") {
        return data.message;
      }
      if (status >= 500) {
        return "Something went wrong. Please try again.";
      }
      return "Could not send message.";
    }

    composerForm.addEventListener("submit", (event) => {
      event.preventDefault();

      if (!textarea || textarea.value.trim() === "") {
        return;
      }

      if (composerForm.dataset.submitting === "true") {
        return;
      }

      setComposerPending(true);
      clearComposerError();
      // Submitted text is no longer "being typed" regardless of the outcome.
      sendTypingStop();

      const body = new URLSearchParams(new FormData(composerForm));
      const sentValue = textarea.value;

      fetch(composerForm.getAttribute("action") || "/messages", {
        method: "POST",
        headers: { Accept: "application/json" },
        body,
        credentials: "same-origin",
      })
        .then(async (response) => {
          let data = null;
          try {
            data = await response.json();
          } catch (err) {
            // Non-JSON body (proxy error page): fall through to status text.
          }

          if (!response.ok) {
            // Failure: the textarea keeps its value so the user can retry.
            showComposerError(composerErrorMessage(data, response.status));
            return;
          }

          // Success: insert the persisted row and end at the latest message.
          if (data && data.id !== undefined && data.id !== null) {
            appendMessage(data, { forceScroll: true });
          } else {
            scrollToBottomSoon(chatScroller);
          }

          // Clear only what was sent: anything typed while the request was
          // pending survives in the composer.
          if (textarea.value === sentValue) {
            textarea.value = "";
          } else if (textarea.value.startsWith(sentValue)) {
            textarea.value = textarea.value.slice(sentValue.length);
          }
        })
        .catch(() => {
          showComposerError("Could not send. Check your connection and try again.");
        })
        .finally(() => {
          setComposerPending(false);

          if (textarea && shouldRestoreComposerFocus()) {
            const cleared = textarea.value === "";
            textarea.focus();
            if (cleared) {
              try {
                textarea.setSelectionRange(0, 0);
              } catch (err) {
                // Non-text inputs throw; the textarea never should.
              }
            }
          }
        });
    });

    if (textarea) {
      textarea.addEventListener("input", () => {
        clearComposerError();

        if (textarea.value.trim() === "") {
          sendTypingStop();
          return;
        }

        sendTypingActive();
      });

      textarea.addEventListener("keydown", (event) => {
        if (event.key !== "Enter" || event.shiftKey || event.isComposing) {
          return;
        }

        event.preventDefault();

        if (textarea.value.trim() === "" || composerForm.dataset.submitting === "true") {
          return;
        }

        composerForm.requestSubmit();
      });
    }
  }

  const presenceHeading = document.getElementById("chat-presence-heading");
  const presenceStatus = document.getElementById("chat-presence-status");
  const presenceList = document.getElementById("chat-presence-list");
  const hasPresencePane = Boolean(presenceHeading && presenceStatus && presenceList);

  function setPresenceStatus(text) {
    if (!hasPresencePane) {
      return;
    }

    try {
      presenceList.textContent = "";
      presenceStatus.textContent = text;
      presenceStatus.hidden = false;
    } catch (err) {
      console.warn("[chat_live] presence status failed", err);
    }
  }

  function renderPresenceList(payload) {
    if (!hasPresencePane) {
      return;
    }

    try {
      const users =
        payload && Array.isArray(payload.users) ? payload.users : [];

      presenceHeading.textContent = `In this channel — ${users.length}`;
      presenceList.textContent = "";

      if (users.length === 0) {
        presenceStatus.textContent = "No one is here";
        presenceStatus.hidden = false;
        return;
      }

      presenceStatus.hidden = true;

      users.forEach((user) => {
        const username =
          user && typeof user.username === "string" ? user.username : "";

        if (username === "") {
          return;
        }

        const row = document.createElement("li");
        row.className = "cs-presence-user";

        const avatar = document.createElement("span");
        avatar.className = "cs-presence-avatar";
        avatar.textContent = username[0].toUpperCase();

        const link = document.createElement("a");
        link.className = "cs-presence-name";
        link.href = `/u/${encodeURIComponent(username)}`;
        link.textContent = username;

        row.appendChild(avatar);
        row.appendChild(link);
        presenceList.appendChild(row);
      });
    } catch (err) {
      console.warn("[chat_live] presence render failed", err);
      setPresenceStatus("Presence unavailable");
    }
  }

  const channelId = root.dataset.channelId;

  if (!channelId) {
    console.warn("[chat_live] missing data-channel-id");
    setPresenceStatus("Presence unavailable");
    return;
  }

  if (!window.Phoenix || !window.Phoenix.Socket) {
    console.warn("[chat_live] phoenix.js not loaded");
    setPresenceStatus("Presence unavailable");
    return;
  }

  const topic = `chan:${channelId}`;
  const socketUrl = root.dataset.socketUrl;
  const token = root.dataset.signedToken;

  if (!socketUrl) {
    console.warn("[chat_live] missing data-socket-url");
    setPresenceStatus("Presence unavailable");
    return;
  }

  if (!token) {
    console.warn("[chat_live] missing data-signed-token");
    setPresenceStatus("Presence unavailable");
    return;
  }

  // --- Realtime token lifecycle -------------------------------------------
  // The signed token payload is transparent (base64url JSON): decode it to
  // learn our own user_id (for typing-line self-exclusion) and the expiry.
  // `currentToken` lives in a closure read by the socket's params function on
  // every (re)connect, so refreshing it never requires a second socket.

  let currentToken = token;

  function decodeTokenClaims(tok) {
    try {
      const payload = String(tok).split(".")[0];
      const b64 = payload.replace(/-/g, "+").replace(/_/g, "/");
      const padded = b64 + "=".repeat((4 - (b64.length % 4)) % 4);
      const claims = JSON.parse(atob(padded));
      return claims && typeof claims === "object" ? claims : null;
    } catch (err) {
      return null;
    }
  }

  const initialClaims = decodeTokenClaims(currentToken);
  const viewerId =
    initialClaims && typeof initialClaims.user_id === "number"
      ? initialClaims.user_id
      : null;

  function tokenExpiryMs(tok) {
    const claims = decodeTokenClaims(tok);
    return claims && typeof claims.exp === "number" ? claims.exp * 1000 : null;
  }

  const tokenRefreshUrl = `${channelBasePath}/realtime-token`;
  const tokenRefreshLeadMs = 2 * 60 * 1000;
  const tokenRefreshFallbackDelayMs = 45 * 60 * 1000;
  const tokenRefreshBackoffMs = [10000, 30000, 60000, 120000, 300000];
  const maxTokenRefreshAttempts = 6;
  let tokenRefreshTimer = null;
  let tokenRefreshInFlight = false;
  let tokenRefreshAttempts = 0;
  let tokenRefreshStopped = false;

  function scheduleTokenRefresh() {
    if (tokenRefreshStopped) {
      return;
    }

    window.clearTimeout(tokenRefreshTimer);
    const expMs = tokenExpiryMs(currentToken);
    let delay =
      expMs !== null ? expMs - Date.now() - tokenRefreshLeadMs : tokenRefreshFallbackDelayMs;
    if (delay < 30000) {
      delay = 30000;
    }
    tokenRefreshTimer = window.setTimeout(refreshRealtimeToken, delay);
  }

  async function refreshRealtimeToken() {
    if (tokenRefreshInFlight || tokenRefreshStopped) {
      return;
    }

    tokenRefreshInFlight = true;

    try {
      const response = await fetch(tokenRefreshUrl, {
        method: "GET",
        headers: { Accept: "application/json" },
        credentials: "same-origin",
      });

      if (response.status === 401 || response.status === 403 || response.status === 404) {
        // Authorization is gone (logged out, membership revoked, …): a new
        // token will never arrive, so stop retrying until a page reload.
        tokenRefreshStopped = true;
        console.warn("[chat_live] realtime token refresh unauthorized", response.status);
        return;
      }

      if (!response.ok) {
        throw new Error(`status ${response.status}`);
      }

      const data = await response.json();
      if (!data || typeof data.token !== "string" || data.token === "") {
        throw new Error("malformed token response");
      }

      currentToken = data.token;
      tokenRefreshAttempts = 0;
      scheduleTokenRefresh();
    } catch (err) {
      console.warn("[chat_live] realtime token refresh failed", err);
      tokenRefreshAttempts += 1;
      if (tokenRefreshAttempts >= maxTokenRefreshAttempts) {
        tokenRefreshStopped = true;
        return;
      }
      window.clearTimeout(tokenRefreshTimer);
      const backoff =
        tokenRefreshBackoffMs[
          Math.min(tokenRefreshAttempts - 1, tokenRefreshBackoffMs.length - 1)
        ];
      tokenRefreshTimer = window.setTimeout(refreshRealtimeToken, backoff);
    } finally {
      tokenRefreshInFlight = false;
    }
  }

  scheduleTokenRefresh();

  let hasJoined = false;

  console.log("[chat_live] channel_id", channelId);
  console.log("[chat_live] topic", topic);
  console.log("[chat_live] connecting", socketUrl);

  const { Socket } = window.Phoenix;

  // params is a function so every reconnect attempt picks up the freshest
  // token — phoenix.js re-evaluates it when building the websocket URL.
  const socket = new Socket(socketUrl, {
    params: () => ({ token: currentToken }),
  });

  socket.onOpen(() => {
    console.log("[chat_live] socket open");

    if (hasJoined) {
      catchUp();
    }
  });

  socket.onError(() => {
    console.warn("[chat_live] socket error");
    setPresenceStatus("Presence unavailable");
    clearTypingLine();

    // The proactive timer can miss (laptop asleep past expiry): if reconnects
    // are failing with an already-expired token, fetch a fresh one now. The
    // in-flight/stopped guards keep repeated socket errors from stampeding.
    const expMs = tokenExpiryMs(currentToken);
    if (expMs !== null && expMs <= Date.now()) {
      refreshRealtimeToken();
    }
  });

  socket.onClose(() => {
    console.warn("[chat_live] socket closed");
    setPresenceStatus("Presence unavailable");
    clearTypingLine();
  });

  socket.connect();

  const channel = socket.channel(topic, {});

  channel
    .join()
    .receive("ok", () => {
      hasJoined = true;
      console.log("[chat_live] joined", topic);
      root.dataset.liveStatus = "joined";
      // Typing state is ephemeral: after a (re)join, show nothing until the
      // gateway broadcasts a fresh snapshot.
      clearTypingLine();
      catchUp();
    })
    .receive("error", (resp) => {
      console.warn("[chat_live] join failed", resp);
      root.dataset.liveStatus = "error";
      setPresenceStatus("Presence unavailable");
    });

  channel.on("new_msg", (payload) => {
    console.log("[chat_live] new_msg", payload);
    const shouldStayAtBottom = isNearBottom(chatScroller);
    appendMessage(payload, { wasNearBottom: shouldStayAtBottom });

    // Live events never advance the recovery cursor, so a live id ahead of it
    // means an HTTP-unconfirmed range exists. Reconcile shortly after the
    // burst settles: the catch-up either confirms the range or backfills a
    // message whose gateway publish was lost.
    const id = Number(payload && payload.id);
    if (Number.isFinite(id) && id > recoveryCursor) {
      scheduleCatchUp(catchUpReconcileDelayMs);
    }
  });

  channel.on("presence_list", (payload) => {
    renderPresenceList(payload);
  });

  // --- Typing indicators ---------------------------------------------------
  // Client sends only { v: 1, active: bool }; identity and channel come from
  // the gateway's verified socket state. Advisory feature: every failure path
  // degrades to "no typing line" without touching the composer or chat.
  // Emission state and the composer input listeners live in the composer
  // section; only the channel transport is plugged in here.

  const typingLine = document.getElementById("chat-typing");

  realtimeTypingPush = (active) => {
    if (!hasJoined || !socket.isConnected() || !channel.isJoined()) {
      return;
    }

    try {
      channel.push("typing", { v: 1, active });
    } catch (err) {
      console.warn("[chat_live] typing push failed", err);
    }
  };

  function clearTypingLine() {
    if (!typingLine) {
      return;
    }

    typingLine.textContent = "";
    typingLine.hidden = true;
  }

  function typingLineText(usernames) {
    if (usernames.length === 1) {
      return `${usernames[0]} is typing…`;
    }

    if (usernames.length === 2) {
      return `${usernames[0]} and ${usernames[1]} are typing…`;
    }

    if (usernames.length === 3) {
      return `${usernames[0]}, ${usernames[1]} and ${usernames[2]} are typing…`;
    }

    return `${usernames[0]}, ${usernames[1]} and ${usernames.length - 2} others are typing…`;
  }

  function renderTypingList(payload) {
    if (!typingLine) {
      return;
    }

    try {
      const users =
        payload && Array.isArray(payload.users) ? payload.users : [];

      const usernames = users
        .filter(
          (user) =>
            user &&
            typeof user.username === "string" &&
            user.username !== "" &&
            (viewerId === null || user.user_id !== viewerId)
        )
        .map((user) => user.username);

      if (usernames.length === 0) {
        clearTypingLine();
        return;
      }

      typingLine.textContent = typingLineText(usernames);
      typingLine.hidden = false;
    } catch (err) {
      console.warn("[chat_live] typing render failed", err);
      clearTypingLine();
    }
  }

  channel.on("typing_list", (payload) => {
    renderTypingList(payload);
  });

  // --- Shared cursors ------------------------------------------------------
  // Fine-pointer clients only. The client sends only { v: 1, active, x, y }
  // (normalized to the visible chat viewport); identity comes from the
  // gateway's verified socket state. Viewport-relative coordinates describe a
  // position within each user's own visible viewport — not the same message
  // when scroll positions differ. Advisory feature: every failure path
  // degrades to "no cursors" without touching chat, typing, or presence.

  const cursorOverlay = document.getElementById("chat-cursor-overlay");
  const finePointer =
    typeof window.matchMedia === "function" &&
    window.matchMedia("(pointer: fine)").matches;
  let cursorDebug = null;

  // The opt-in control exists only where the server enabled the feature for
  // this community; on coarse-pointer devices it would be inert, so hide it.
  const cursorShareLabel = document.getElementById("chat-cursor-share");
  if (cursorShareLabel && !finePointer) {
    cursorShareLabel.hidden = true;
  }

  if (cursorOverlay && finePointer) {
    const cursorSendIntervalMs = 100;
    const cursorMinMoveNorm = 0.005;
    const cursorStaleMs = 5000;
    const cursorStaleSweepMs = 2000;
    const maxRenderedCursors = 128;
    const cursorHueCount = 8;

    function clamp01(value) {
      return Math.min(1, Math.max(0, value));
    }

    // One cached rect serves emission and rendering; per-event
    // getBoundingClientRect calls would force layout at 10 Hz per user.
    let overlayRect = cursorOverlay.getBoundingClientRect();

    function refreshOverlayRect() {
      overlayRect = cursorOverlay.getBoundingClientRect();
      cursorEls.forEach((entry) => positionCursorEl(entry));
    }

    if (window.ResizeObserver) {
      new ResizeObserver(refreshOverlayRect).observe(cursorOverlay);
    }
    window.addEventListener("resize", refreshOverlayRect);

    // -- Emission (opt-in) --
    // Broadcasting is off by default and only possible where the server
    // rendered the "Share cursor" control. The checkbox governs emission
    // only: remote cursors keep rendering for everyone regardless.

    const shareToggle = document.getElementById("chat-cursor-share-toggle");
    const shareCommunitySlug =
      cursorShareLabel && typeof cursorShareLabel.dataset.communitySlug === "string"
        ? cursorShareLabel.dataset.communitySlug
        : "";
    const shareStorageKey =
      shareCommunitySlug !== "" ? `earde:share-cursor:${shareCommunitySlug}` : null;

    // localStorage can be unavailable (privacy modes); degrade to a
    // session-only, default-off preference.
    function readSharePreference() {
      if (!shareStorageKey) {
        return false;
      }

      try {
        return window.localStorage.getItem(shareStorageKey) === "true";
      } catch (err) {
        return false;
      }
    }

    function writeSharePreference(value) {
      if (!shareStorageKey) {
        return;
      }

      try {
        window.localStorage.setItem(shareStorageKey, value ? "true" : "false");
      } catch (err) {
        // Preference stays session-only.
      }
    }

    // True only while the server-rendered toggle exists AND is checked; a
    // stored "true" from another community can never flip it here because the
    // key is community-scoped and the toggle only exists where enabled.
    let sharingEnabled = false;

    let pointerClientX = 0;
    let pointerClientY = 0;
    let pointerDirty = false;
    let cursorActive = false;
    let lastSentX = null;
    let lastSentY = null;

    function pushCursor(payload) {
      if (!hasJoined || !socket.isConnected() || !channel.isJoined()) {
        return false;
      }

      try {
        channel.push("cursor", payload);
        return true;
      } catch (err) {
        console.warn("[chat_live] cursor push failed", err);
        return false;
      }
    }

    function sendCursorInactive() {
      pointerDirty = false;

      if (!cursorActive) {
        return;
      }

      cursorActive = false;
      lastSentX = null;
      lastSentY = null;
      pushCursor({ v: 1, active: false });
    }

    // Raw pointermove events only record the position; this timer coalesces
    // them to at most one push per interval, skipping micro-jitter. A
    // stationary pointer sends nothing and its remote cursor expires via the
    // gateway TTL by design.
    const cursorSendTimer = window.setInterval(() => {
      if (!sharingEnabled || !pointerDirty) {
        return;
      }

      pointerDirty = false;
      const rect = overlayRect;
      if (!rect || rect.width <= 0 || rect.height <= 0) {
        return;
      }

      const x = clamp01((pointerClientX - rect.left) / rect.width);
      const y = clamp01((pointerClientY - rect.top) / rect.height);

      if (cursorActive && lastSentX !== null) {
        if (Math.hypot(x - lastSentX, y - lastSentY) < cursorMinMoveNorm) {
          return;
        }
      }

      if (pushCursor({ v: 1, active: true, x, y })) {
        cursorActive = true;
        lastSentX = x;
        lastSentY = y;
      }
    }, cursorSendIntervalMs);

    function onCursorPointerMove(event) {
      if (!sharingEnabled || event.pointerType === "touch") {
        return;
      }

      pointerClientX = event.clientX;
      pointerClientY = event.clientY;
      pointerDirty = true;
    }

    function onCursorVisibilityChange() {
      if (document.visibilityState === "hidden") {
        sendCursorInactive();
      }
    }

    root.addEventListener("pointermove", onCursorPointerMove);
    root.addEventListener("pointerleave", sendCursorInactive);
    document.addEventListener("visibilitychange", onCursorVisibilityChange);

    if (composerForm) {
      composerForm.addEventListener("submit", sendCursorInactive);
    }

    if (shareToggle) {
      shareToggle.checked = readSharePreference();
      sharingEnabled = shareToggle.checked;

      shareToggle.addEventListener("change", () => {
        sharingEnabled = shareToggle.checked;
        writeSharePreference(sharingEnabled);

        if (!sharingEnabled) {
          // Withdraw immediately: pushes active=false if a cursor is live and
          // resets pointerDirty/lastSent, so nothing sends until re-opted-in.
          sendCursorInactive();
        }
      });
    }

    // -- Rendering --

    const cursorEls = new Map();

    function positionCursorEl(entry) {
      const rect = overlayRect;
      if (!rect) {
        return;
      }

      const x = entry.nx * rect.width;
      const y = entry.ny * rect.height;
      entry.el.style.transform = `translate(${x}px, ${y}px)`;
    }

    const svgNs = "http://www.w3.org/2000/svg";

    function buildCursorEl(userId, username) {
      const el = document.createElement("div");
      el.className = "cs-cursor";
      el.dataset.hue = String(((userId % cursorHueCount) + cursorHueCount) % cursorHueCount);

      const svg = document.createElementNS(svgNs, "svg");
      svg.setAttribute("width", "14");
      svg.setAttribute("height", "18");
      svg.setAttribute("viewBox", "0 0 14 18");
      svg.setAttribute("aria-hidden", "true");

      const path = document.createElementNS(svgNs, "path");
      path.setAttribute("d", "M1 1 L13 8.6 L7.2 9.9 L4.6 16.4 Z");
      path.setAttribute("fill", "currentColor");
      svg.appendChild(path);

      const label = document.createElement("span");
      label.className = "cs-cursor-label";
      label.textContent = username;

      el.appendChild(svg);
      el.appendChild(label);
      return el;
    }

    function removeCursorEl(userId) {
      const entry = cursorEls.get(userId);
      if (!entry) {
        return;
      }

      cursorEls.delete(userId);
      entry.el.remove();
    }

    function clearCursors() {
      cursorEls.forEach((entry) => entry.el.remove());
      cursorEls.clear();
    }

    function renderCursor(payload) {
      try {
        if (!payload || payload.v !== 1 || typeof payload.user_id !== "number") {
          return;
        }

        // Never render the viewer's own cursor.
        if (viewerId !== null && payload.user_id === viewerId) {
          return;
        }

        if (payload.active === false) {
          removeCursorEl(payload.user_id);
          return;
        }

        if (
          payload.active !== true ||
          typeof payload.x !== "number" ||
          typeof payload.y !== "number" ||
          typeof payload.username !== "string" ||
          payload.username === ""
        ) {
          return;
        }

        let entry = cursorEls.get(payload.user_id);

        if (!entry) {
          // Hard DOM cap: unknown users beyond it are ignored until capacity
          // frees up; existing cursors keep updating and removals still work.
          if (cursorEls.size >= maxRenderedCursors) {
            return;
          }

          entry = {
            el: buildCursorEl(payload.user_id, payload.username),
            nx: 0,
            ny: 0,
            lastSeen: 0,
          };
          cursorEls.set(payload.user_id, entry);
          cursorOverlay.appendChild(entry.el);
        }

        entry.nx = clamp01(payload.x);
        entry.ny = clamp01(payload.y);
        entry.lastSeen = Date.now();
        positionCursorEl(entry);
      } catch (err) {
        console.warn("[chat_live] cursor render failed", err);
      }
    }

    // Client-side safety net mirroring the gateway TTL: even if a removal
    // event is lost, a cursor that stops updating disappears and the DOM
    // stays bounded.
    const cursorStaleTimer = window.setInterval(() => {
      const cutoff = Date.now() - cursorStaleMs;
      cursorEls.forEach((entry, userId) => {
        if (entry.lastSeen < cutoff) {
          removeCursorEl(userId);
        }
      });
    }, cursorStaleSweepMs);

    function cursorTeardown() {
      sendCursorInactive();
      window.clearInterval(cursorSendTimer);
      window.clearInterval(cursorStaleTimer);
      root.removeEventListener("pointermove", onCursorPointerMove);
      root.removeEventListener("pointerleave", sendCursorInactive);
      document.removeEventListener("visibilitychange", onCursorVisibilityChange);
      clearCursors();
    }

    window.addEventListener("pagehide", cursorTeardown);

    channel.on("cursor", renderCursor);
    socket.onError(clearCursors);
    socket.onClose(clearCursors);

    cursorDebug = {
      count: () => cursorEls.size,
    };
  }

  window.eardeLiveChat = {
    socket,
    channel,
    topic,
    catchUp,
    getChatScroller,
    getLastMessageId: () => lastMessageId,
    getRecoveryCursor: () => recoveryCursor,
    getCursorCount: () => (cursorDebug ? cursorDebug.count() : 0),
  };
})();
