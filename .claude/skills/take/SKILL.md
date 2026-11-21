---
name: take
description: Take ready checkpoints from the project store and build each with a fresh coder, until none is ready.
---

# Taking checkpoints

Run it in grit's main checkout, in a session joined to the edge: the session's opening names its
claim holder. It builds one checkpoint at a time in the fixer worktree, `<top>/.claude/worktrees/fixer`
(`<top>` is `git rev-parse --show-toplevel`). The store's tools are the edge's `project_*` tools.

## Once, before the first take

1. Run `scripts/coder-needs`. If it exits non-zero, show its lines and stop: each names what a
   coder on this machine lacks, and what for.
2. Print the coder's claim holder with the command this session's opening gives, `coder-opus` in
   place of `TYPE`: `'<hook>' holder coder-opus --dir '<dir>' --session '<session>'`. It is this
   session's coder's alone; every claim below is for it.

## Each take

1. **The worktree.** If the fixer worktree exists, `git -C <fixer> status --porcelain` must print
   nothing and `git -C <fixer> symbolic-ref -q HEAD` must fail (it is detached). If either does
   not hold, say which and stop: what is there is someone's work.
2. **Go on, or claim.** `project_claims` with `project` `grit`: a claim in role `coder` whose
   `holder` is the coder's is one this session's coder already holds; take its key. Otherwise
   `project_claim` with `oldest` `{"project": "grit", "kind": "checkpoint", "state": "ready"}`,
   `role` `coder` and the coder's holder, and no `session`; its answer names the key. Refused
   saying there is nothing to take: say no checkpoint is ready, and stop. A checkpoint left in
   `building` with no claim is the manager's to make ready again, never this skill's to take.
3. **Building.** A checkpoint in `ready` is moved to `building` (`project_move`).
4. **The coder.** Start one `coder-opus` agent in the foreground, with this prompt:
   "Build checkpoint `<key>`, claimed for you as coder. Its brief is injected at your start; if
   it is not, read it with `project_show`. Work in the fixer worktree, on the branch and from
   the base the brief names; if that branch exists already, an earlier coder began it: go on
   from its last commit. Do not push."
   If the brief asks for a plan first and the coder reports one, show it to the person, or the
   manager's session when there is one, and wait for their reply, then send it to the same coder
   (`SendMessage`). Never give a go of your own.
5. **When the coder ends**, in this order:
   1. `project_claims` again. If no claim in role `coder` with the coder's holder is on the key,
      say the claim lapsed or was taken, naming who holds it now, and stop: nothing is pushed or
      moved.
   2. The branch is the one the coder's report names; `git -C <fixer> rev-list --count
      <base>..<branch>` must be at least 1.
   3. The gate: in the fixer worktree, `git checkout <branch>`, `scripts/check`, `./mill
      shutdown`, `git checkout --detach`. If it fails: `project_release` the key, leave the
      checkpoint in `building`, say that its gate did not pass, quoting the check's failing
      lines, and take the next.
   4. `git -C <fixer> push -u origin <branch>`.
   5. `project_move` the key to `review`.
   6. `project_release` the key.
   7. Say in one line the key, the branch and its head commit, and that it waits for review.
      Take the next.

## What it never does

- Merge, push anything but the branch its coder built, or open a pull request: a checkpoint in
  `review` is the manager's, and one whose change a person sees waits there for that person.
- Run two coders at once, or a Mill job of its own while a coder runs.
- Claim or build in any worktree but the fixer.
- Claim for any holder but its coder's, or claim by `key`.
- Work around a refusal. A refused claim, move or release, or a failed push, is said in its own
  words, and taking stops. The only exception is a failed gate, step 5.3.
