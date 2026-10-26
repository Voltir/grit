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
