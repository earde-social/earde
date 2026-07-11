(function () {
  const root = document.getElementById("chat-live-root");

  if (!root) {
    return;
  }

  const bottomThresholdPx = 100;

  // Connection-state chip rendered under the member composer (role=status, so
  // screen readers hear transitions politely). Absent for non-members and when
  // the websocket is not configured; every update must tolerate that.
  const statusChip = document.getElementById("cs-live-status");

  function setLiveStatus(state, text) {
    root.dataset.liveStatus = state;
    if (statusChip) {
      statusChip.dataset.liveState = state;
      statusChip.textContent = text;
    }
  }

  const STATUS_LIVE = ["live", "live"];
  const STATUS_CONNECTING = ["connecting", "live: connecting…"];
  const STATUS_RECONNECTING = ["reconnecting", "live: reconnecting — messages still save"];
  const STATUS_DEGRADED = ["degraded", "live: catch-up failed — retrying"];

  // Localize the SSR "YYYY-MM-DD HH:MM UTC" fallbacks: today's messages show a
  // bare local HH:MM, older ones keep a short date. Full local datetime in the
  // tooltip. Runs against a row or the whole stream.
  function formatMessageTimes(scope) {
    scope.querySelectorAll("time.cs-msg-time[datetime]").forEach((el) => {
      const date = new Date(el.getAttribute("datetime"));
      if (Number.isNaN(date.getTime())) {
        return;
      }
      const hm = date.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
      el.textContent =
        date.toDateString() === new Date().toDateString()
          ? hm
          : `${date.toLocaleDateString([], { month: "short", day: "numeric" })} ${hm}`;
      el.title = date.toLocaleString();
    });
  }

  formatMessageTimes(root);

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
  scrollToBottomSoon(chatScroller);

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

  const channelId = root.dataset.channelId;

  if (!channelId) {
    console.warn("[chat_live] missing data-channel-id");
    return;
  }

  if (!window.Phoenix || !window.Phoenix.Socket) {
    console.warn("[chat_live] phoenix.js not loaded");
    return;
  }

  const topic = `chan:${channelId}`;
  const socketUrl = root.dataset.socketUrl;
  const token = root.dataset.signedToken;
  const catchUpUrl = `${window.location.pathname.replace(/\/$/, "")}/messages.json`;

  if (!socketUrl) {
    console.warn("[chat_live] missing data-socket-url");
    return;
  }

  if (!token) {
    console.warn("[chat_live] missing data-signed-token");
    return;
  }

  const canStart = root.dataset.canStart === "true";
  const channelBasePath = window.location.pathname.replace(/\/$/, "");

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
  let catchUpRetryTimer = null;
  let hasJoined = false;

  const { Socket } = window.Phoenix;

  const socket = new Socket(socketUrl, {
    params: { token },
  });

  setLiveStatus(...STATUS_CONNECTING);

  socket.onOpen(() => {
    if (hasJoined) {
      setLiveStatus(...STATUS_LIVE);
      catchUp();
    }
  });

  socket.onError(() => {
    console.warn("[chat_live] socket error");
    setLiveStatus(...STATUS_RECONNECTING);
  });

  socket.onClose(() => {
    setLiveStatus(...STATUS_RECONNECTING);
  });

  socket.connect();

  const channel = socket.channel(topic, {});

  channel
    .join()
    .receive("ok", () => {
      hasJoined = true;
      setLiveStatus(...STATUS_LIVE);
      catchUp();
    })
    .receive("error", (resp) => {
      console.warn("[chat_live] join failed", resp);
      setLiveStatus(...STATUS_RECONNECTING);
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

    // Same shape as the SSR rows: <time datetime> localized by formatMessageTimes.
    const time = document.createElement("time");
    time.className = "cs-msg-time";
    time.textContent = payload.created_at || "";
    const rawCreatedAt = String(payload.created_at || "").slice(0, 19);
    if (/^\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}$/.test(rawCreatedAt)) {
      time.dateTime = rawCreatedAt.replace(" ", "T") + "Z";
    }

    const text = document.createElement("div");
    text.className = "cs-msg-text";
    text.textContent = payload.content || "";

    meta.appendChild(author);
    meta.appendChild(time);

    const hasId = payload.id !== undefined && payload.id !== null;
    const hasAuthor = payload.user_id !== undefined && payload.user_id !== null;
    const isDeleted = payload.deleted === true;
    const isPromoted = payload.thread_id !== undefined && payload.thread_id !== null;
    row.dataset.hasThread = isPromoted ? "true" : "false";

    body.appendChild(meta);
    body.appendChild(text);

    // "Start thread" lives in the persistent actions row, matching the SSR markup.
    if (canStart && hasId && hasAuthor && !isDeleted && !isPromoted) {
      const actions = document.createElement("div");
      actions.className = "cs-msg-actions";

      const startLink = document.createElement("a");
      startLink.className = "cs-msg-start";
      startLink.href = `${channelBasePath}/messages/${encodeURIComponent(
        String(payload.id)
      )}/start-thread`;
      startLink.dataset.promoteUrl = startLink.href;
      row.dataset.promoteUrl = startLink.href;
      startLink.textContent = "+ Start thread";

      actions.appendChild(startLink);
      body.appendChild(actions);
    }

    row.appendChild(avatar);
    row.appendChild(body);
    formatMessageTimes(row);

    root.appendChild(row);

    if (shouldStayAtBottom) {
      scrollToBottomSoon(chatScroller);
    }
  }

  // A dropped catch-up means missed history, not just missed fanout, so it is
  // surfaced on the status chip and retried on a timer (the socket's own
  // reconnect also re-triggers catchUp via onOpen).
  function scheduleCatchUpRetry() {
    if (catchUpRetryTimer) {
      return;
    }

    catchUpRetryTimer = window.setTimeout(() => {
      catchUpRetryTimer = null;
      catchUp();
    }, 8000);
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
        setLiveStatus(...STATUS_DEGRADED);
        scheduleCatchUpRetry();
        return;
      }

      const data = await response.json();
      const messages = Array.isArray(data.messages) ? data.messages : [];
      const shouldStayAtBottom = isNearBottom(chatScroller);

      messages.forEach((message) => appendMessage(message, { wasNearBottom: shouldStayAtBottom }));

      if (shouldStayAtBottom) {
        scrollToBottomSoon(chatScroller);
      }

      if (socket.isConnected()) {
        setLiveStatus(...STATUS_LIVE);
      }
    } catch (err) {
      console.warn("[chat_live] catch-up exception", err);
      setLiveStatus(...STATUS_DEGRADED);
      scheduleCatchUpRetry();
    } finally {
      catchUpInFlight = false;
    }
  }

  channel.on("new_msg", (payload) => {
    const shouldStayAtBottom = isNearBottom(chatScroller);
    appendMessage(payload, { wasNearBottom: shouldStayAtBottom });
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
