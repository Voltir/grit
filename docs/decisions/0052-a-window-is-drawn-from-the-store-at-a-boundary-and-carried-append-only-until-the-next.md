# 0052. A window is drawn from the store at a boundary and carried, append-only, until the next

Status: accepted (2026-10-29); revised (2026-11-06)

Context: a provider caches a request from the front, and current models refuse a history that
carries thinking blocks once anything before them is edited. Turned down: redrawing the window
each turn (it misses the cache every turn and, with kept thinking, is refused as edited);
sliding it at its budget, dropping the oldest turn (its prefix changes every turn once full);
folding a carried turn's messages into the next part's entries (its dated line would change
between turns); the assembler reading the carried window itself (what a turn was shown is its
record, read in its own step); compaction by a model summary (a model call over the whole
window at each boundary).

Decision:

- **Boundaries.** A window is drawn afresh from the store at a start, a clear, a compaction and
  the limit. A compaction is, today, a period's close (ADR 0011): the window opens on the newest
  closing.
- **Between boundaries the window is carried and only appended to.** The `ContextAssembler` is
  given the window carried into the turn (`CarriedWindow`: each part drawn since the boundary,
  each followed by its turn's own messages) and answers a `Projection`: appended after it, or a
  boundary that replaces it. ADR 0040's steps still run each turn, appending what they admit;
  nothing carried is redrawn or ranked again.
- **A carried turn is shown as it was shown.** Its window record is a delta: what its turn
  appended, the record it follows (none at a boundary), its part's cost, and the dated line and
  topic note its request sent (ADR 0016). A later turn repeats those lines before the carried
  turn's own messages, so its bytes never change.
- **A turn carries a window only when it can send it unchanged.** Each answered turn keeps the
  digest of its last call's system prompt, tools and messages, and the model pair it went to: a
  pair caches only what was sent to it. A turn's `carried` step takes the conversation's newest
  earlier window record only when this turn's request of it goes to the same pair with the same
  digest, and this turn's first gate hides none of it; otherwise it draws a boundary. So a
  carried document gone, a changed tool set or prompt, or a turn whose last call or input limit
  added a note no record holds makes a boundary, never an edited prefix. A window drawn from
  the store replays no reasoning, which is bound to its request.
- **The limit is the deployment's `Assembly` budget** (`ContextAssembler.limit`), never a turn's
  width. At the limit, when the carried window and the turns since no longer fit, the window is
  cleared and re-projected, never slid; a carried turn costs what it sent, its tool calls and
  results included. **Within a turn, a turn at the limit ends**, its last calls told so, and the
  next turn draws a boundary: nothing is cleared inside a turn. Its requests are held to
  `TurnLoop.inputLimit`, read against each reply's input as the provider counted it, never an
  estimate, so a turn read back cuts where it cut live.
- **A boundary draws within half the limit.** The opening, the recent turns and recall fill to
  that share, and later turns grow the window to the limit. Drawn to the limit, it would leave
  the next turn no room and clear every other turn.
- **What a turn sends only grows within the turn**, gates included (ADR 0036). Every call of a
  turn sends the same system prompt, tools and tool choice, its last included: a provider binds
  a reply's thinking to the tools it was made under, and a changed tool list discards the
  cached prefix. What a call may not call (a tool its gate withholds, `task_status` before a
  turn is ending, any tool on its budget's last call) is refused when called, as a tool result,
  never withdrawn. A turn offers its tool set alone, whatever its gates and classifier decide
  (`topic` included, and `task_status` on a tasked turn, ADR 0058), so turns of one
  conversation send the same tools. A gate's notes and stubs are kept in no window record yet,
  so the turn after one whose gate hid an item or wrote a note draws a boundary.

Consequences: on every turn after a window's first, the request begins with the last one's
prefix, so most of its input is a cache read. Right after a boundary the model sees about half
what a redrawn window showed, the peak unchanged; a deployment sizes its budget for that. Near
the limit an append recalls less, and recall never forces a boundary. A strand's and an asking
thread's sections are as of the boundary. A period's close clears the window, so the
lifecycle's settle time is a chat's cache lifetime: a placer that judges nobody waiting during
a person's pause clears it. Enforced by the assembly suites, `TurnPrefixTests` (a carried
request begins with the last) and `TurnLoopTurnTests` (a cut turn's requests keep their tools;
each cause above draws a boundary).
