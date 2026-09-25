# Architecture Decision Records

One short file per decision about grit's design that future code has to follow.
Numbered, never edited once accepted. To reverse one, write a new record and mark the
old one `Superseded by NNNN`; changing only its Status line is allowed.

## The threshold

A decision gets a record only if it passes **all four** tests:

1. **Someone could reasonably propose the alternative.** There was a real choice. A
   consequence of an earlier decision is not a new one.
2. **It is expensive to reverse.** It shapes a seam, the data model, or module boundaries.
   Judge by the code and data that exist now, not what will exist later.
3. **It binds code not yet written**, and the reason is not obvious from the code itself.
4. **It is about grit's design, not a tool's behaviour.** Version pins, tool quirks and
   probe results go in a comment beside the config or code they affect.

A decision whose binding part is still open waits until that part is decided.

Everything else goes in a code comment next to what it explains, or in the commit
message. An idea that is not yet a decision is not recorded here.

## Template

`NNNN-short-title.md`:

```markdown
# NNNN. Title stating the decision

Status: accepted (YYYY-MM-DD) | superseded by NNNN
Context: the forces that made this a choice, and the alternative that was turned down.
Decision: what grit does.
Consequences: what this makes easier, what it makes harder, and what enforces it
(compiler, enola rule, test), if anything does.
```

Keep a record to one screen. If the evidence is long, link to it.
