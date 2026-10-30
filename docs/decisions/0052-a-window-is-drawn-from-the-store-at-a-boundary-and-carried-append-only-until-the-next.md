# 0052. A window is drawn from the store at a boundary and carried, append-only, until the next

Status: accepted (2026-10-29)

Context: a provider caches a request from the front, and current models refuse a history that
carries thinking blocks once anything before them is edited. A window redrawn every turn misses
the cache on every turn and, with kept thinking, is refused as an edited history. A window that
slides at its budget, dropping its oldest turn to admit the newest, changes its prefix on every
turn once full. Turned down: redrawing each turn (the cache and the refusal); sliding at the
limit (the same, once full); folding a carried turn's messages into the next part's entries (its
dated line would change between turns); the assembler reading the carried window from the store
itself (what a turn was shown is the turn's record, read in its own step); compaction by a model
summary (a model call over the whole window at each boundary; later, measured against a clear).

Decision:

- **Boundaries.** A window is drawn afresh from the store at a start, a clear, a compaction and
  the limit. A compaction is, today, a period's close (ADR 0011): the window opens on the newest
  closing, so a close changes its opening.
- **Between boundaries the window is carried and only appended to.** The `ContextAssembler` is
  given the window carried into the turn (`CarriedWindow`: each part drawn since the boundary,
  each followed by its turn's own messages) and answers a `Projection`: appended after it, or a
  boundary that replaces it. ADR 0040's steps still run each turn; what they admit is appended
  after the carried window, never placed before it, and nothing carried is redrawn or ranked
  again.
- **A carried turn is shown as it was shown**, dated by its own first entry and the zones its
  `offer` recorded (ADR 0016), so its bytes are the same on every later turn of the window.
- **The limit is the deployment's `Assembly` budget**, read from the assembler, never from a
  turn's width (a width bounds only what the part drawn now may add). At the limit, when the
  carried window and the turns since it no longer fit, the window is cleared and re-projected,
  never slid. What a carried turn costs is counted as it is sent: its tool calls and results.
- **A boundary draws within half the limit.** The opening, the recent turns and recall fill to
  that share, and later turns' own messages and recall grow the window from there to the limit
  before the next boundary. A boundary drawn to the limit would leave no room for the next turn,
  and the limit would clear the window every other turn.
- **What a turn sends only grows within the turn**, gates included (ADR 0036), so a gated turn
  keeps its cache.

Consequences: on every turn after a window's first, the request begins with the last one's
prefix, so most of its input is read from the cache. Near the limit an append recalls less, and
recall never forces a boundary. A strand's and an asking thread's sections are as of the
boundary. A period's close clears the window, so the lifecycle's settle time is a chat's cache
lifetime: a placer that judges nobody waiting during a person's pause clears it. Until a
deployment setting names the share, a boundary draws half of what a fresh window drew before.
Enforced by the assembly suites, and by a test that a turn's request begins with the bytes of
the last one's prefix.
