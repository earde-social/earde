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

  const composerForm = document.querySelector(".cs-composer form[action='/messages']");

  if (composerForm) {
    const textarea = composerForm.querySelector("textarea[name='content']");
    const sendButton = composerForm.querySelector("button[type='submit'], .cs-send");

    composerForm.addEventListener("submit", (event) => {
      if (!textarea || textarea.value.trim() === "") {
        event.preventDefault();
        return;
      }

      if (composerForm.dataset.submitting === "true") {
        event.preventDefault();
        return;
      }

      composerForm.dataset.submitting = "true";
      if (sendButton) {
        sendButton.disabled = true;
      }

      scrollToBottomSoon(chatScroller);
    });

    if (textarea) {
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
  const catchUpUrl = `${window.location.pathname.replace(/\/$/, "")}/messages.json`;

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

  const canStart = root.dataset.canStart === "true";
  const channelBasePath = window.location.pathname.replace(/\/$/, "");

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

  let lastMessageId = readLastMessageIdFromDom();
  let catchUpInFlight = false;
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

    if (payload.id !== undefined && payload.id !== null) {
      row.dataset.messageId = String(payload.id);

      const id = Number(payload.id);
      if (Number.isFinite(id) && id > lastMessageId) {
        lastMessageId = id;
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

    root.appendChild(row);

    if (shouldStayAtBottom) {
      scrollToBottomSoon(chatScroller);
    }
  }

  async function catchUp() {
    if (catchUpInFlight) {
      return;
    }

    catchUpInFlight = true;

    try {
      const url = `${catchUpUrl}?after_id=${encodeURIComponent(String(lastMessageId))}`;

      const response = await fetch(url, {
        method: "GET",
        headers: {
          Accept: "application/json",
        },
        credentials: "same-origin",
      });

      if (!response.ok) {
        console.warn("[chat_live] catch-up failed", response.status);
        return;
      }

      const data = await response.json();
      const messages = Array.isArray(data.messages) ? data.messages : [];
      const shouldStayAtBottom = isNearBottom(chatScroller);

      messages.forEach((message) => appendMessage(message, { wasNearBottom: shouldStayAtBottom }));

      if (shouldStayAtBottom) {
        scrollToBottomSoon(chatScroller);
      }
    } catch (err) {
      console.warn("[chat_live] catch-up exception", err);
    } finally {
      catchUpInFlight = false;
    }
  }

  channel.on("new_msg", (payload) => {
    console.log("[chat_live] new_msg", payload);
    const shouldStayAtBottom = isNearBottom(chatScroller);
    appendMessage(payload, { wasNearBottom: shouldStayAtBottom });
  });

  channel.on("presence_list", (payload) => {
    renderPresenceList(payload);
  });

  // --- Typing indicators ---------------------------------------------------
  // Client sends only { v: 1, active: bool }; identity and channel come from
  // the gateway's verified socket state. Advisory feature: every failure path
  // degrades to "no typing line" without touching the composer or chat.

  const typingLine = document.getElementById("chat-typing");
  const typingRefreshMs = 2500;
  const typingIdleMs = 4000;
  let typingActive = false;
  let lastTypingSentAt = 0;
  let typingIdleTimer = null;

  function clearTypingLine() {
    if (!typingLine) {
      return;
    }

    typingLine.textContent = "";
    typingLine.hidden = true;
  }

  function pushTyping(active) {
    if (!hasJoined || !socket.isConnected() || !channel.isJoined()) {
      return;
    }

    try {
      channel.push("typing", { v: 1, active });
    } catch (err) {
      console.warn("[chat_live] typing push failed", err);
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

  const typingTextarea = composerForm
    ? composerForm.querySelector("textarea[name='content']")
    : null;

  if (typingTextarea) {
    typingTextarea.addEventListener("input", () => {
      if (typingTextarea.value.trim() === "") {
        sendTypingStop();
        return;
      }

      sendTypingActive();
    });

    // Submitted messages are no longer "being typed"; push the stop while the
    // socket is still open (the form POST navigates, and disconnect cleanup
    // plus the gateway TTL cover anything that doesn't flush in time).
    composerForm.addEventListener("submit", () => {
      sendTypingStop();
    });
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

  window.eardeLiveChat = {
    socket,
    channel,
    topic,
    catchUp,
    getChatScroller,
    getLastMessageId: () => lastMessageId,
  };
})();
