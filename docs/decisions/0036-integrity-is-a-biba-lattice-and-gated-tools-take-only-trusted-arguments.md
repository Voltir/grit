# 0036. Integrity is a Biba lattice beside confidentiality, a turn falls to the meet of what it read, and a gated tool takes only trusted arguments

Status: accepted (2026-10-13), revised (2026-10-14), revised (2026-10-15): the turn's level is loop state, by 0038;
revised (2026-10-15): an endorsement is a downgrade a flow lists, the sanitiser's raised to outside, still tainted
Context: ADRs 0030–0033 label what may be *read* (Bell–LaPadula). Nothing labels what may be
*believed*: text a turn reads from a tool, a web page or a guest can steer a call that writes
out of grit. Layout defences were measured and failed: a forged record placed in grit's own
format was believed whatever its position or framing, on every model tried.

Integrity is a second, independent dimension: a row has a confidentiality label and an
integrity label, each stored, combined and checked on its own. Alternatives turned down:

- Strict Biba (no read down): an assistant must read untrusted text.
- The model as the judge of what to believe: the measurements above.
- FIDES' flipped order (untrusted on top, so one join serves both dimensions): ADR 0035 binds
  "higher integrity" to Biba's sense.
- Storing each (confidentiality, integrity) pair as one interned id: it couples the dimensions
  in storage and multiplies every read filter's id set, though integrity never hides a row.
- Endorsement before gates: soundness does not need it.
- For what a turn reads below its level: showing everything and lowering (one read of a
  relaying service's result costs every writing tool for the rest of the turn, and through its
  summaries the conversation's); hiding every such item, as FIDES does (a read round per result
  in turns that lose nothing by reading); re-offering a withheld tool as one that asks a person
  (approval fatigue is measured high).
- For a job: one cell per run, as in a turn. A planner is code, and one that reads an untrusted
  value for one call would lose every gated call after it.

Decision:

- **The lattice.** An integrity label is a level and compartments. The levels are grit's, not
  a deployment's: untrusted < outside < member < authority. This scale is the starting one and
  may change while the design is first built and used, by the criterion below: a level is added
  when something needs an unlock no existing level gives, and removed when it unlocks nothing
  another does not. The compartments are the deployment's own set and mean *endorsed in*:
  `(authority, {finance})` is an authority on finance and nothing else, and the usual value is
  `{}`. Combining takes the meet. Each level earns its place by something only it unlocks. Outside content may be kept and replied to. Member content may
  drive a write out of grit within the realm. Authority content may set standing policy,
  relabel rooms and set schedules. Untrusted unlocks nothing: it is where content no source
  vouches for starts.
- **Entry, fixed at write (tranquility).** An inbound message takes its author's level: member
  if attested in a realm, else outside; authority for an administrator or steward, in their
  compartments. It is capped by its room's integrity, which the entries insert computes, so no
  caller can forget it. A room's integrity is nullable, meaning no cap. A tool result takes its
  service's declared output level: a service is a party (ADR 0031) and declares it, and a
  relaying service (mail, web) declares untrusted. What grit writes takes its writer's current
  level.
- **A turn's low-water mark.** A turn starts at its trigger's level and falls, each round, to
  the meet of everything its model read. The model's output is stamped at that level. The level
  is the turn's loop state (ADR 0038): its first round's recorded gate met with each later
  one's, a pure fold over what the history holds, so it only falls, a resumed turn finds it
  where it was, and no write signature takes an integrity.
- **Gates (FIDES' trusted-action policy).** Each hosted tool declares a required integrity
  beside its destinations (ADR 0031's send check is the permitted-flow policy). A tool the
  turn's current level does not meet is not offered. By default, a tool that writes out of grit
  requires member, and a read-only tool requires nothing.
- **What a turn reads.** Before each model call a recorded step decides, by one pure rule over
  the round's items in request order, what the model reads. An item whose integrity the turn's
  level dominates is shown. One below it is shown, and lowers the level, when lowering withholds
  no tool the turn is offered now, or when the model asked to read it (`read_hidden`);
  otherwise it is hidden: shown as a stub naming it, its source, its level, and the tools
  reading it would withdraw. The tools the level reached does not meet are withheld. The step
  records every item below the prior level with its integrity and fate, so the request and a
  replay read no row.
- **A job carries taint per value.** A move's inputs and results are tainted values at their own
  integrity: `call` checks its arguments against the tool's requirement, `ask` answers at the
  integrity of what it posed, and a run's inputs arrive trusted at its schedule's integrity.
  `keep` requires outside: a document or card is kept at the integrity of the value kept, and
  refused below outside.
- **Endorsement** only raises a level, so soundness never depends on it. An endorsement is a
  downgrade (ADR 0035): a pure, described node a flow lists, applied only through the `Raise`
  handed where it sits, on a step's recorded output, within a step's body, or within a program
  that declares it. There are two: the extraction, which endorses a bounded answer, or that
  there is none, to `Trusted`; and the integrity tree's, which raises free text to outside,
  still tainted. Only a value of
  bounded capacity may be endorsed, at most K bits, where K is a declared constant; free text is
  never endorsed. A classifier labels features and code decides. A classifier's verdict raises
  untrusted to outside at most. `extract` asks a typed, bounded question of a hidden item (yes
  or no, a number in a range, a day, one of a listed set, a short token), answered by a
  quarantined model call shown only the item and the question; the answer is endorsed at the
  turn's level met with member, so an extraction never reaches authority. The integrity tree returns `Either` and never throws on content,
  since a thrown step is recorded and replayed, and its presence leaks a bit.
- **In SQL.** A second interned table beside the confidentiality one, and an `integrity_id
  SMALLINT NOT NULL` column with no lasting default on each labelled row. In the same migration,
  `label_id` becomes `classification_id` and `grit.labels` becomes `grit.classifications`
  (ADR 0035). Integrity is never a row-hiding predicate. A read that requires a level matches
  an id set, as a clearance does: an `int2eq` test is leakproof, and array containment is not.
- **In the types.** `Classified[T]` has a pure `map` and `reveal` under a capability.
  `Tainted[T]` has a pure `map` and no exit. `Trusted[T]` has a free `extract` and a pure
  `coflatMap`, and is made only by a check against a required level, by a downgrade, or from a
  constant. A gated tool takes `Trusted` arguments. `Reveal` and `Raise` are minted in one
  package.

Consequences: a conversation's summaries carry its lowest level, so taint outlives the turn
that read the untrusted text. A turn that offers no writing tool, or reads only member text,
runs at the rounds and tokens it did before; hiding is paid only where it saves a tool. Lost tools are measured per conversation, and a bound on that
rate over real traffic is part of being done. Soundness rests on every piece of untrusted
content that steers an effect arriving `Tainted`. Integrity enters the types at three places:
a turn's model reply, stamped by its cell and checked against a tool's requirement when a call
is bound; a job's moves (`call`, `ask`, `keep`); and endorsement (the extraction, the
integrity tree's raise). Rows, and every reader of an entry's payload (assembly, the lifecycle,
the turn's request), are data: what they read is gated by the level each row was written at,
pinned by the store contract and the gate's recorded output. A law rule guards who may mint
`Tainted`, `Trusted` and the capabilities, not who reads rows. grit is not compiled in Scala's
safe mode, so a pure closure can still leak through mutation, I/O or `throw`; safe mode returns
for code agents write. K bounds each endorsement, not a turn. The turn's level is
monotone because it is a meet over its gates, not by a mutator's discipline. Each labelled table needs an `ALTER`. Enforced by capture
probes (pure combinators, unforgeable capabilities, no `Bounded` for free text, no gated call
with unchecked arguments), the store contract for the insert's cap, and the law rule.
