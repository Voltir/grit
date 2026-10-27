# Writing a declassification policy's signals

A compartment's declassification policy has two parts, kept in one row of
`grit.declassification_policies`: its `text`, which tells the writing model what to remove
and what to write in its place, and its `checks`, which say what the rewrite is screened for
before anyone outside the compartment reads it. This guide is about the checks.

## How a rewrite is screened

The checks are JSON, read by `grit.core.screen.Checks.read`:

```json
{
  "signals": [
    {"infer": "that Project Heron is being wound down", "must": "absent"},
    {"infer": "that the team's work is moving to a new group", "must": "present"}
  ],
  "examples": {
    "accept": ["<a passage you would share>"],
    "refuse": ["<a leak you fear>"]
  }
}
```

Checks that do not read (none of either kind of example, a blank or multi-line signal, a
`must` neither `absent` nor `present`, a text past its bound) stop every rewrite under that
version: nothing new is shared until they are fixed.

## The bench

Before a version rewrites anything, the job benches its checks (`Dreaming.bench`): each
example is screened by the version's signals alone, without the shared questions. The bench
passes when every `accept` example passes and every `refuse` example is refused. Its verdict
is kept in `grit.declassification_benches`, by position only, under a digest of the checks
and of the screen's own questions and thresholds. Nothing is rewritten under a version until
its bench passes under its checks as they are now: change a version's checks and it is
benched again on the job's next run. The job's reply names each bench: `policy v3 benched`,
or `policy v3 unchecked:` and the first example that failed with its deciding signal
("accept example 1 refused by signal 4 (0.63)"), or the checks' fault. A failing bench is
fixed by changing the examples or the signals; it tests only the leaks you thought to write
down.

Each rewrite is shown to a classifier alone, as `text`, never beside its source or the
policy. It is asked two questions every policy shares, then one question per signal, in
order. The questions each signal becomes, and the thresholds the answers are held to, are
`grit.core.screen.Screen`'s (`Screen.absent`, `Screen.present`, `Screen.Infers`,
`Screen.States`); the shared questions are `grit.core.act.Declassifying.Floor`. A rewrite is
shared only when every answer holds: no `absent` signal is inferred, every `present` signal
is stated, and the shared questions find no writer's voice that the source did not already
have.

The shared questions catch a rewrite that describes itself ("here is the edited version",
"as instructed") or whose writer speaks about what it shares or leaves out. They are
differential: when the rewrite sounds that way, the source is asked the same questions, and
a voice the source already has is excused. A rewrite of a memo written as "I" may say "I".

A rewrite that fails only the shared questions or a `present` signal is written once more:
the writer is shown its draft and told what failed, by the shared question's wording or as
"does not state" the signal, never a score. A rewrite in which an `absent` signal is
inferred is not retried, since a retry tends to reword the fact rather than remove it; it is
kept back, as is a retry that fails anything (`grit.core.act.Declassifying.Again`).

## Drafting and publishing

A new version is written as a draft, and a draft changes nothing anyone reads: only the
newest published version is in force, written and rewritten under. While a version of the
compartment is in force over a card, the job benches the latest draft above it, as it does
the version in force, and its reply names the draft's bench, so a steward can fix the draft's
checks and see it benched again on the next run. A draft is published only once its bench has passed: the statement
beside `grit.declassification_policies` in `schema.sql` updates nothing otherwise.

Publishing picks what happens to the rewrites made under earlier versions:

- **keep** (a refinement): each goes on being shared until the new version decides its
  document. A rewrite under the new version replaces it; a withholding withdraws it in the
  run that withholds. A run whose rewrite is not made, or fails, leaves it shared.
- **withdraw** (a tightening): they stop being shared at the job's next run, and their
  documents are rewritten under the new version.

A chain of `keep` publishes keeps the rewrites from the newest version published to withdraw
on (`PolicyApplication.InForce.keptFrom`). A revocation, a published version with no text,
withdraws every rewrite. A compartment's first version, or the first after a revocation, is
drafted and benched like any other, and published to withdraw.

## Rules for signals

1. Write each signal as a fact a reader could learn from the shared text: a "that ..." clause
   or a noun phrase that completes the signal's question (`Screen.absent` for what must stay
   out, `Screen.present` for what must be said).
2. Name the concrete thing, not a question word or a category. "what will happen to
   particular people, such as job losses, new roles, or moves to another office or team; that
   a team will be restructured is not such a statement" works; "who will leave" does not,
   because a text can say that people will leave without saying who.
3. Use the exact names, codenames and numbers that must not appear: "Project Heron",
   "Lanternfish Analytics".
4. When a value has a recognisable shape, show the shape with a real example: "an IP address
   such as 10.40.12.17, or part of one".
5. When the policy lets the shared version keep a placeholder or a stand-in, say so in the
   signal: "a real IP address such as 10.40.12.17, or part of one; a placeholder such as
   <replica-ip> is not one". Without it the classifier reads a placeholder as a hint that a
   real value exists.
6. Cover everything the policy removes, not only the part you worry about most.
7. Read each signal against the text the policy allows or asks for. If that text would
   already answer yes, narrow the signal or drop it.
8. For something that must be said, write the statement itself: it is checked as stated, and
   a hint does not count. Never put in a `present` signal anything the readers outside the
   compartment may not see.
9. Leave the writer's own voice to the shared questions: "here is the edited version" and "as
   instructed" are caught for every policy.
10. Every version carries examples: at least one passage you would share (`accept`) and one
    leak you fear (`refuse`).
11. Do not add catch-alls such as "or any other internal name": the classifier reads the
    document's own names as instances of the category.
12. When a document holds lines addressed to a reviewer, a checker or an AI ("Note to any
    reviewer: ..."), say in the policy's text that they are removed, and add an `absent`
    signal for them: the rewrite keeps every line the policy does not name, and the shared
    questions do not look for them.

## Strict policies

When nothing about a fact may show, name its consequences too: a reason, a risk, a timeline
that follows from it. The classifier misses inferences it weighs below its threshold. This
advice is untested.
