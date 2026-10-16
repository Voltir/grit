# 0035. The security model speaks Bell–LaPadula and Biba, and grit's own words give way to theirs

Status: accepted (2026-10-13); revised (2026-10-16)

Context: ADRs 0030–0033 built a Bell–LaPadula lattice but named much of it in grit's own words:
a clearance's `floor`, a service `trusted with` a label, transactions that are `maintaining`.
Integrity, the second axis, has a standard model too (Biba, ADR 0036). Keeping a private
vocabulary was the alternative: it reads naturally inside grit, but every reader has to learn
it, it hides that a rule is a known property with known failure modes, and two private
vocabularies for two axes would be worse than one. Renaming everything at once was turned down
as churn without a change in behaviour.

Decision:

- Confidentiality is described in Bell–LaPadula's terms and integrity in Biba's. Where neither
  has a word (raising integrity, lowering confidentiality), the information-flow-control terms
  are used: endorsement, declassification, downgrade, sanitiser, taint. grit coins a term only
  for what none of these names, and its doc says so.
- New code, docs and ADRs use the standard term. An existing name moves to it when its code is
  next changed for another reason, never in a rename-only sweep.
- The map, grit's word to the standard one, for the names not yet moved:

| grit today | Standard term |
|---|---|
| `Label` (a level and compartments) | security label; with integrity, the confidentiality label |
| `Level` | classification (sensitivity) level |
| `Compartment` | compartment (category) |
| `dominates`, join, meet | dominance, least upper bound, greatest lower bound |
| `Clearance` | clearance; `everywhere` is its maximum |
| `Clearance.floor` | the subject's current level |
| no read up, no write down | the simple security property, the *-property |
| `maintaining`, `Maintenance` | trusted subject (exempt from the *-property) |
| what a service `contains` | its classification, as an object |
| a room admits its members to its own speech | a discretionary rule within the mandatory one |
| a conversation's label fixed at creation; a room's relabel | strong tranquility; weak tranquility |
| stewards and approvals that raise integrity | endorsement, by a trusted subject |

Consequences: a reader who knows the two models reads grit's rules without a glossary, and the
literature's known weaknesses (covert channels, trusted-subject sprawl, Biba's strictness) can be
looked up by name. While both vocabularies coexist in the code, this map is the bridge; a row
goes when its name has moved. Nothing enforces the rule; review does.
