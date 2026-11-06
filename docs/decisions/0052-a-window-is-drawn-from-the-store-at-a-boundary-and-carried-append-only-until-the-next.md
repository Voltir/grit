# 0052. A window is drawn from the store at a boundary and carried, append-only, until the next

Status: accepted (2026-10-29); revised (2026-10-30, 2026-11-05)

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
- **A carried turn is shown as it was shown.** Its window record is a delta: what its turn
  appended, the record it follows (none at a boundary), its part's cost, and the dated line
  and topic note its request sent, as sent (ADR 0016). A later turn repeats those lines before
  the carried turn's own messages, so its bytes are the same on every later turn of the window.
- **A turn carries a window only when it can send it unchanged.** Each answered turn keeps the
  digest of its last call's system prompt, tools and messages, all of them, and the model pair
  the call went to: a pair caches only what was sent to it. A turn's
  `carried` step, before its first ask, takes the conversation's newest earlier window record
  only when the request this turn would send of that window goes to the same pair, has the
  same digest, and this
  turn's first gate, decided over what it carries, hides none of it; otherwise it draws a
  boundary. So a carried document gone, content this turn's route does not clear, a changed
  tool set or prompt, a turn that answered its budget's last call (its last-call message is in
  no record), or one that met its input limit (its last-tools note is in no record) makes a
  boundary, never an edited prefix. A window drawn from the store
  replays no reasoning: thinking is bound to the request that produced it.
- **The limit is the deployment's `Assembly` budget**, read from the assembler
  (`ContextAssembler.limit`), never from a turn's width (a width bounds only what the part
  drawn now may add). At the limit, when the carried window and the turns since it no longer
  fit, the window is cleared and re-projected, never slid. What a carried turn costs is
  counted as it is sent: its tool calls and results. **Within a turn, a turn at the limit
  ends**, its last calls told so, and the next turn draws a boundary: nothing is cleared inside
  a turn, whose request only grows. A turn's requests are held to the limit plus what its
  system prompt and tools cost, capped by the turn pin's context length less its output
  budget when the pin's profile gives one, less room for one clipped tool result; the loop
  reads it against each reply's input as the provider counted it, never an estimate, so a
  turn read back cuts where it cut live.
- **A boundary draws within half the limit.** The opening, the recent turns and recall fill to
  that share, and later turns' own messages and recall grow the window from there to the limit
  before the next boundary. A boundary drawn to the limit would leave no room for the next turn,
  and the limit would clear the window every other turn.
- **What a turn sends only grows within the turn**, gates included (ADR 0036), so a gated turn
  keeps its cache within the turn. Every call of a turn sends the same system prompt, tools and
  tool choice, its last included: a provider binds a reply's thinking to the tools it was made
  under, and a changed tool list discards the whole cached prefix. What a call may not call (a
  tool its gate withholds, `task_status` before a turn is ending, any tool on its budget's last
  call) is refused when called, as a tool result, never withdrawn from the request. What a
  turn offers is its tool set's alone, whatever its gates decide, `topic` on every turn,
  whatever its classifier decides, and `task_status` on every turn briefed for one task, so
  turns of one conversation send the same tools and system prompt. A gate's
  notes and stubs are kept in no window record yet, so the turn after one whose gate hid an
  item or wrote a note draws a boundary.

Consequences: on every turn after a window's first that carries it, the request begins with
the last one's prefix, so most of its input is read from the cache. Until a gate's notes and
stubs are carried, boundaries are more frequent than the limit alone makes them: every gated
turn costs one, as does a turn that met its input limit and a redeployed tool set or prompt. A window runs from half the limit,
just after a boundary, to the limit, where a window redrawn every turn ran at about the limit:
right after a boundary the model sees about half the history it did, the peak is unchanged,
and most input is a cache read; a deployment sizes its budget with that in mind. Near the limit an append recalls less, and
recall never forces a boundary. A strand's and an asking thread's sections are as of the
boundary. A period's close clears the window, so the lifecycle's settle time is a chat's cache
lifetime: a placer that judges nobody waiting during a person's pause clears it. Until a
deployment setting names the share, a boundary draws half of what a fresh window drew before.
A task longer than one window is a conversation of turns, each after the first rooted on grit's
own continuation note (ADR 0058). Enforced by the assembly suites, by a test that a turn's request begins with the last one's
whole on either gateway, by a test that every request of a cut turn carries the same tools and
tool choice, and by tests that a gated turn, a changed offer, an answered last call and a
carried document gone are each followed by a boundary.
