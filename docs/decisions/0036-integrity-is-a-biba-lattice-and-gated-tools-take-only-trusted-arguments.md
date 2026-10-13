# 0036. Integrity is a Biba lattice beside confidentiality, a turn falls to the meet of what it read, and a gated tool takes only trusted arguments

Status: accepted (2026-10-13)
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
  is a cell whose only mutator lowers it, carried on the acting value, so no write signature
  takes an integrity.
- **Gates (FIDES' trusted-action policy).** Each hosted tool declares a required integrity
  beside its destinations (ADR 0031's send check is the permitted-flow policy). A tool the
  turn's current level does not meet is not offered. By default, a tool that writes out of grit
  requires member, and a read-only tool requires nothing. A tool result below the turn's level
  is kept as a pointer: the model may pass it to a tool by name without reading it, and reading
  it lowers the turn.
- **Endorsement** only raises a level, so soundness never depends on it. Only a holder of the
  `Endorse` capability may endorse: the integrity tree's step and the ask flow. Only a value of
  bounded capacity may be endorsed, at most K bits, where K is a declared constant; free text is
  never endorsed. A classifier labels features and code decides. A classifier's verdict raises
  untrusted to outside at most. The integrity tree returns `Either` and never throws on content,
  since a thrown step is recorded and replayed, and its presence leaks a bit.
- **In SQL.** A second interned table beside the confidentiality one, and an `integrity_id
  SMALLINT NOT NULL` column with no lasting default on each labelled row. In the same migration,
  `label_id` becomes `classification_id` and `grit.labels` becomes `grit.classifications`
  (ADR 0035). Integrity is never a row-hiding predicate. A read that requires a level matches
  an id set, as a clearance does: an `int2eq` test is leakproof, and array containment is not.
- **In the types.** `Classified[T]` has a pure `map` and `reveal` under a capability.
  `Tainted[T]` has a pure `map` and no exit. `Trusted[T]` has a free `extract` and a pure
  `coflatMap`, and is made only by a check against a required level, by `endorse`, or from a
  constant. A gated tool takes `Trusted` arguments. `Reveal` and `Endorse` are minted in one
  package.

Consequences: a conversation's summaries carry its lowest level, so taint outlives the turn
that read the untrusted text. Lost tools are measured per conversation, and a bound on that
rate over real traffic is part of being done. Soundness rests on every reader of untrusted
content returning `Tainted`, so raw readers live only in the flow package, which a law rule
enforces. grit is not compiled in Scala's safe mode, so a pure closure can still leak through
mutation, I/O or `throw`; safe mode returns for code agents write. K bounds each endorsement,
not a turn. Separation checking gives the turn's cell exclusivity, not monotonicity; the cell's
single mutator gives monotonicity. Each labelled table needs an `ALTER`. Enforced by capture
probes (pure combinators, unforgeable capabilities, no `Bounded` for free text, no gated call
with unchecked arguments), the store contract for the insert's cap, and the law rule.
