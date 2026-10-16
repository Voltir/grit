# 0036. Integrity is a Biba lattice beside confidentiality, a turn falls to the meet of what it read, and a gated tool takes only trusted arguments

Status: accepted (2026-10-13); revised (2026-10-16)

Context: ADRs 0030–0033 label what may be *read* (Bell–LaPadula). Nothing labelled what may be
*believed*: text a turn reads from a tool, a web page or a guest can steer a call that writes
out of grit. Layout defences failed when measured: a forged record in grit's own format was
believed whatever its position or framing, on every model tried. Turned down:

- Strict Biba (no read down): an assistant must read untrusted text. The model as the judge of
  what to believe: the measurement above.
- FIDES' flipped order (untrusted on top): ADR 0035 binds "higher integrity" to Biba's sense.
- One interned id per (confidentiality, integrity) pair: it couples the dimensions and
  multiplies every read filter's id set, though integrity never hides a row.
- For what a turn reads below its level: always showing and lowering (one relayed result
  costs every writing tool for the rest of the turn); always hiding (a read round per result);
  re-offering a withheld tool as one that asks a person (approval fatigue).
- For a job, one level per run: a planner that reads one untrusted value would lose every gated
  call after it.

Decision:

- **The lattice** (`Integrity`): a level and compartments, the compartments meaning *endorsed
  in*. The levels are grit's: untrusted < outside < member < authority, each earning its place
  by what only it unlocks (outside may be kept and replied to; member may drive a write out of
  grit; authority may set standing policy, relabel rooms and set schedules); a level is added
  or removed by that criterion. Combining takes the meet.
- **Fixed at write (tranquility).** An inbound message takes its author's level (member if
  attested in a realm, else outside; authority for an administrator or steward, in their
  compartments), capped by its room's integrity in the entries insert, so no caller can forget
  it. A tool result takes its service's declared output level; a relaying service (mail, web)
  declares untrusted. What grit writes takes its writer's current level.
- **A turn's low-water mark.** A turn starts at its trigger's level and falls, each round, to
  the meet of everything its model read; its output is stamped at that level. The level is the
  turn's loop state (ADR 0038), a pure fold over its recorded gates, so it only falls and a
  resumed turn finds it where it was.
- **Gates.** Each hosted tool declares a required integrity beside its destinations; a tool the
  turn's level does not meet is not offered. By default a tool that writes out of grit requires
  member, and a read-only tool nothing.
- **What a turn reads.** Before each model call a recorded step decides, by one pure rule over
  the round's items, what the model reads. An item the turn's level dominates is shown. One
  below is shown, and lowers the level, when lowering withholds no tool offered now or the
  model asked to read it (`read_hidden`); otherwise it is shown as a stub naming its source, its
  level and the tools reading it would withdraw. The step records each such item and its fate,
  so a replay reads no row.
- **A job carries taint per value.** A move's inputs and results are `Tainted` at their own
  integrity: `call` checks its arguments against the tool's requirement, `ask` answers at the
  integrity of what it posed, a run's inputs arrive at its schedule's integrity, and `keep`
  refuses a value below outside.
- **Endorsement only raises, so soundness never depends on it.** An endorsement is a downgrade
  (`Downgrade`): a pure, described node a flow lists (`Flows.downgrades`), applied only through
  the `Raise` handed where it sits. There are two: the extraction, which endorses a bounded
  answer (`extract`: a typed question of a hidden item, answered by a quarantined model call) to
  `Trusted` at the turn's level met with member; and the sanitiser, which raises free text from
  below outside to outside, still `Tainted`. Only a value of at most `Bounded.K` bits is
  endorsed to `Trusted`. A classifier labels features and code decides.
- **In SQL**, each labelled row has an `integrity_id` (`grit.integrity_labels`); it never hides
  a row.
- **In the types.** `Tainted[T]` has a pure `map` and no exit. `Trusted[T]` has a free `extract`
  and a pure `coflatMap`, and is made only by a check against a required level
  (`Trusted.check`), a downgrade, or a constant. A gated tool takes `Trusted` arguments.

Consequences: a conversation's summaries carry its lowest level, so taint outlives the turn
that read the untrusted text. Soundness rests on every untrusted content that steers an effect
arriving `Tainted`. grit is not compiled in Scala's safe mode, so a pure closure can still leak
through mutation, I/O or `throw`. Enforced by capture probes (`DowngradeCaptureTests`,
`FlowCaptureTests`: pure combinators, unforgeable capabilities, no `Bounded` for free text),
the compiler (`Tainted` and `Trusted` are made only in `grit.core.flow`), the store contract for
the insert's cap, and the law (`only-dbos-names-integrity-parts`).
