# 0036. Integrity is a Biba lattice beside confidentiality, a turn falls to the meet of what it read, and a gated tool takes only trusted arguments

Status: accepted (2026-10-13); revised (2026-10-16, 2026-10-29)

Context: ADRs 0030–0033 label what may be *read* (Bell–LaPadula). Nothing labelled what may be
*believed*: text a turn reads from a tool, a web page or a guest can steer a write out of
grit. Layout defences failed when measured: a forged record in grit's format was
believed however placed, on every model tried. Turned down:

- Strict Biba (no read down): an assistant must read untrusted text. The model as the judge of
  what to believe: the measurement above.
- One interned id per (confidentiality, integrity) pair: it couples the dimensions, and
  integrity hides no row.
- Always showing and lowering what reads below the turn, always hiding it, or re-offering a
  withheld tool as one asking a person; one level per job run: each costs every gated call.
- A tool result at one level per tool or repository (stand-ins for its authors), or at its
  edge's own claim.

Decision:

- **The lattice** (`Integrity`): a level and compartments, the compartments meaning *endorsed
  in*. Its levels: untrusted < outside < member < authority, each earning its place
  by what only it unlocks (outside may be kept and replied to; member may drive a write out of
  grit; authority may set standing policy, relabel rooms and set schedules); a level is added
  or removed by that test alone. Combining takes the meet.
- **Fixed at write (tranquility).** An inbound message takes its author's level (member if
  attested in a realm, else outside; authority for an administrator or steward, in their
  compartments), met with its room's cap in the entries insert, which no caller skips. A
  deployment caps (`Cap`) what is said at a place and every place within it no deeper cap
  covers: a room, service or repository. A tool result is believed per item: its service's
  extension reads each item's place and author (`Accounts`; a Drive file, its editors', ADR
  0039; a repository's file at a branch or tag, member, as only its writers push those), and
  the result takes the meet of each author's level and place's cap: untrusted if
  unaccountable, its service's cap if unread (untrusted when none, as for a relay). What grit
  writes takes its writer's current level.
- **A turn's low-water mark.** A turn starts at its trigger's level and falls each round to
  the meet of all its model read; its output takes that level. The level is a
  pure fold over its recorded gates (ADR 0038), so it survives a resume.
- **Gates.** Each hosted tool declares the integrity it requires: by default member for a tool
  writing out of grit, nothing for a read-only one. A turn below it, from its start or after
  a fall, keeps it offered and has its calls refused, told so in a note among its messages:
  what a turn offers is its tool set's alone, and what it sends only grows within the turn, so
  a provider's prompt cache and a model's recorded thinking stay valid, within a turn and
  across the turns that carry it (ADR 0052).
- **What a turn reads.** Before each model call, a recorded step's one pure rule decides what
  the model reads. An item the turn's level dominates is shown. One below is shown, and lowers
  the level, when lowering withholds no tool offered now; otherwise it is a stub naming its
  source, its level and the tools reading it would withdraw. A stub, once shown, is the same in
  every later request of the turn: the model reads it through `read_hidden`, whose answer is
  what it hides and lowers the level whatever it withdraws. The step records each item's fate,
  so a replay reads no row.
- **A job carries taint per value.** A move's inputs and results are `Tainted` at their own
  integrity: `call` checks its arguments against the tool's requirement, `ask` answers at the
  integrity of what it posed, a run's inputs arrive at its schedule's integrity, and `keep`
  refuses a value below outside.
- **Endorsement only raises, so soundness never depends on it.** An endorsement is a downgrade
  (`Downgrade`): a pure, described node a flow lists (`Flows.downgrades`), applied only through
  the `Raise` handed where it sits. There are three: the extraction, which endorses an answer of
  at most `Bounded.K` bits (`extract`: a quarantined model's typed answer) to `Trusted`
  at the turn's level met with member; the sanitiser, which raises free text from below
  outside to outside, still `Tainted`; and the declassification, which removes a compartment
  from a rewrite under its policy as applied to one document, checked by a judgment. A
  classifier labels features and code decides. Vetting reads the text the model reads, not the
  facts behind it (`docs/extending.md`).
- **In the types.** `Tainted[T]` has a pure `map` and no exit. `Trusted[T]` has a free `extract`
  and a pure `coflatMap`, and is made only by `Trusted.check` against a required level, a
  downgrade, or a constant. A gated tool takes `Trusted` arguments.

Consequences: a conversation's summaries carry its lowest level, so taint outlives the turn
that read untrusted text. Soundness rests on all untrusted content that steers an effect
arriving `Tainted`. Without Scala's safe mode, a pure closure can leak through mutation,
I/O or `throw`. Enforced by capture probes (`DowngradeCaptureTests`, `FlowCaptureTests`), the
compiler (`Tainted` and `Trusted` are made only in `grit.core.flow`), the store contract for
the insert's cap, and the law (`only-dbos-names-integrity-parts`).
