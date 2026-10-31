# Architecture Decision Records

One short file per decision about grit's design that future code has to follow, numbered.

## Policy

- **A record states what is true now.** Its history is git's. The Status line is one line,
  `accepted (YYYY-MM-DD)` or `accepted (YYYY-MM-DD); revised (YYYY-MM-DD)`, never a list of
  amendments. A change that alters another record edits that record in the same commit, in
  place, rather than adding a note to either.
- **For 30 days from its acceptance** a record may be revised freely, while the design it
  records is still being built and run.
- **After that it is settled, not frozen.** It is amended in place for as long as the decision
  its title states stays true. A change that makes the title false is a new record: the old
  one is deleted in the same commit, and every reference to it retargeted. No stubs.
- **One record, one screen**: about 5 KB at most, or 8 KB for a record of the security model
  (who a principal is, labels, clearance, integrity), where a rule stated exactly is worth
  more than a short one. Evidence too long for that goes in the commit message. No paths into gitignored working notes, and no lists of suites: "Enforced
  by" names the kind of gate (the compiler, a probe, a law rule by its id, the replay gate)
  and at most two suites.
- **Reviewed at each milestone close**: every record over the cap, and every symbol or suite
  a record names that no longer exists, is fixed then.

## The threshold

A decision gets a record only if it passes **all five** tests:

1. **Someone could reasonably propose the alternative.** There was a real choice. A
   consequence of an earlier decision is not a new one.
2. **It is expensive to reverse.** It shapes a trait, the data model, or module boundaries.
   Judge by the code and data that exist now, not what will exist later.
3. **It binds code not yet written**, and the reason is not obvious from the code itself.
4. **It is about grit's design, not a tool's behaviour.** Version pins, tool quirks and
   probe results go in a comment beside the config or code they affect.
5. **Its title states the decision in one sentence.** A title that only names a topic is a
   decision not yet made.

A decision whose binding part is still open waits until that part is decided.

Everything else goes in a code comment next to what it explains, or in the commit
message. An idea that is not yet a decision is not recorded here.

## Template

`NNNN-title-stating-the-decision.md`, the slug following the title:

```markdown
# NNNN. Title stating the decision

Status: accepted (YYYY-MM-DD); revised (YYYY-MM-DD)

Context: the forces that made this a choice, and the alternatives turned down.

Decision: what grit does.

Consequences: what this makes easier, what it makes harder, and what enforces it (the
compiler, a probe, a law rule, the replay gate; at most two suites), if anything does.
```
