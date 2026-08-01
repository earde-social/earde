(** The authenticated top bar's unread-notification badge.

    One durable query and one renderer for every authenticated document, so
    no page can disagree with another about the same user's unread count.

    Read semantics live in [Db]: a notification becomes read only when
    [Db.mark_notifs_read] runs, and the only caller is the GET /notifications
    handler, which marks the whole mailbox read on load. Nothing else — no
    other page, no click on an individual notification, no visit to a
    notification's destination — changes read state. This module never
    writes; it only counts.

    The count is loaded once per request by {!middleware} and stashed on the
    request, because documents render synchronously and cannot query. A
    request with no stashed count renders no badge: that is the deliberate
    fail-soft path for an anonymous request, a request the middleware skipped,
    and a failed count alike. The absence of a count is never rendered as a
    zero. *)

(** Loads the current user's unread count once per request and stashes it for
    {!badge_html}. Best-effort in both directions: it never fails a request,
    and a failed count simply leaves nothing stashed. Only authenticated GETs
    for non-asset paths are counted.

    Counted requests are exactly the authenticated GETs that can render a
    document: assets and the two authenticated non-document GET routes (the
    live-chat catch-up JSON and the realtime-token refresh, both of which the
    chat page requests repeatedly while it is open) are excluded, so the count
    stays attached to page loads and never to a polling loop.

    Must run inside [Dream.sql_pool] and [Dream.sql_sessions]. *)
val middleware : Dream.middleware

(** The badge element, or the empty string.

    Empty whenever the count is zero or unknown — so a user with nothing
    unread gets a bare bell, with no badge element and no "0" anywhere in the
    document. A positive count renders as itself, capped at ["99+"] so a large
    mailbox cannot widen the top bar. *)
val badge_html : ?request:Dream.request -> unit -> string
