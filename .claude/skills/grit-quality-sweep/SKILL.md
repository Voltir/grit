---
name: grit-quality-sweep
description: A periodic sweep of grit's production code through named lenses (parametric similarity, primitive obsession, capability over-reach, and others) — Haiku agents gather candidates per area, Opus agents judge and verify them, the manager spot-checks and files the offenders. Use only when asked to sweep for code-quality smells: it is not part of a milestone close, whose cost it would multiply.
argument-hint: "[lenses] [areas | git range]"
---

# The quality sweep

A smell is found by asking one precise question of the code, a **lens**. A sweep runs a few
lenses over a scope, in two tiers: cheap agents gather candidates, strong agents judge them.
It is read-only throughout; its product is a ranked list of offenders, picked off later one per PR.

## Arguments

- **Lenses**: names from the catalogue below. Default: `primitive`, `capability`, `parametric`.
- **Scope**: scout area names (`scripts/scout area` lists them), or a git range (the code added
  since a commit the request names, to `HEAD`). Default: every area, in groups of two to
  four related areas, one group per judge.

## The lenses

Each lens gives the census its mechanical query and the judge its question.

| Lens | Census looks for | The judge asks |
|---|---|---|
| `parametric` | defs and types with the same shape modulo type names; companions with the same member set; `@tailrec` and local recursive loops; write/read and print/parse pairs | Anti-unify the pair: is it one concept once one or two things are parameters? Class each hole: type, function, combine (Monoid, lattice), shape (fold over an ADT), step (unfold, state machine), codec or iso, capability. Three or more holes is a coincidence. |
| `primitive` | public signatures with `String`, `Boolean`, `Int`, `Long`, `Double` parameters or `Either[String, …]` results; the domain types that already exist (`find` on `opaque type`, the ids) | Which value here has a domain type it is not using, or should have one? A `Boolean` turned into words later is an enum; a `String` failure the caller matches on is an ADT; an error ADT says whether its failure is transient. |
| `capability` | signatures with `using` clauses, `^` captures or explicit capability parameters (`Tx`, `Durable`, `Provider`, `Clock`, a store) | Does the body use every capability the signature demands? Would a narrower one do (a read for a `Tx`, a pure function for a step body)? A signature is a promise (STYLE.md rule 2): over-reach makes it false. |
| `invariants` | Scaladoc lines saying *must*, *never*, *always*, *only if*, *only when*, *at least* | Is the stated invariant a missing type: a smart constructor, an opaque type, a non-empty collection? (rule 10) |
| `states` | case classes with two or more `Option` fields, a `Boolean` beside an `Option`, enum cases sharing most fields | Can the type hold a state the code never means? What sum would make it unrepresentable? |
| `ends` | writers without readers, adds without deletes, tables without a deletion path, cursor or retry loops without a cap | What is the missing inverse or the missing end, and what does its absence cost? |
| `order` | types with mutable state or docs saying *after*, *before*, *first*, *once* | Is there a call order nothing enforces? Would typestate or a phantom parameter enforce it? |

A sweep may add a lens; give it the same two columns in the brief.

## Run it

1. **Ground it.** `scripts/scout area` for the scope; for a git range, the production files it
   touched (`git diff --name-only <range> -- '*.scala' ':!*/test/*' ':!*/it/*' ':!*/probes/*'`).
   Split into groups of related areas.
2. **Census: one Haiku agent per (group, lens).** `subagent_type: Explore`, `model: haiku`, all in
   one message. The brief: the group's areas, the lens's census column, read-only, read through the
   scout MCP tools (`find`, `show`, `family`, `uses`) and `grep`, no Mill, no whole-file reads.
   Output: candidates only, each a `file:line`, the symbol, and one line on why it matched; no
   verdicts, no proposals; at most 40 per lens, the strongest first.
3. **Judge: one Opus agent per group.** `subagent_type: Plan`, `model: opus`, all in one message.
   The brief: the group's census output, each lens's judge column, the constraints below, and the
   report format. It verifies every instance it reports with scout before it states it, and it
   may add instances the census missed. Use `wisdom` instead for a group whose lens is mostly
   top-down (what a trait is an instance of): it is the stronger judge and costs more.
4. **Verify.** The manager reads the source behind each top finding before filing it, and marks it
   verified or the judge's claim.
5. **File** where parked work goes: the evidence in a sweep note, and
   each kept offender as one line on the pick-off list (the note's numbering in brackets, what the
   fix must not change). Findings that are flaws (a bug, a missing cap) go to the current round
   instead, under the no-small-flaws rule.
6. **Report** to Nick: the ranked offenders, the rules the judges converged on, and the tooling
   ideas, with the tokens each tier spent.

## Constraints every judge brief carries

- STYLE.md's rules; rule 8's quarantines: a generalisation never crosses a Java-library module.
- ADR 0004: a workflow's step names and output encodings, journal `Left` messages and every stored
  string are fixed within an epoch; a change to them is a cost the finding states.
- Capture and separation checking: a generic whose hole carries a capability is a risk the finding
  names (`docs/capture-checking.md`).
- Structure only where it is used: a Monoid needs a call site that combines two values, a fold over
  a sealed type its third interpreter.
- Same word, different concept is a coincidence; renames are Nick's call.

## The judge's report

Final message, about 2500 words at most:

1. **Offenders**, ranked, at most 8 per group: the instances (`file:line`, verified), the lens and
   the holes or the smell, the proposed type or signature with its Scaladoc as it would be written,
   whether a caller could use it from that alone (rule 10), its standard name and laws if any, its
   cost (replay, stored forms, capture), and a verdict: real concept, mere helper, or coincidence.
   Few strong over many weak; an area with none says so.
2. **Rules** grit should adopt so new code avoids the smell, each one sentence and its reason.
3. **Tooling**: what scout could compute from TASTy to find the smell mechanically, and
   which findings it would have caught.

## Cost

A judge spends about 100–200k tokens per group, a census agent far less. Every agent is read-only
and runs no Mill, so all of them may run in parallel; briefs point at the scout MCP tools, whose
server keeps what it has read between calls.
