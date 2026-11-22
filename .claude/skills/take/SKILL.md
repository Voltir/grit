---
name: take
description: Take ready checkpoints from the project store and build each with a fresh coder, until none is ready.
---

# Taking checkpoints

Run it in grit's main checkout, in a session joined to the edge: the session's opening names its
claim holder. It builds one checkpoint at a time in the fixer worktree, `<top>/.claude/worktrees/fixer`
(`<top>` is `git rev-parse --show-toplevel`). The store's tools are the edge's `project_*` tools.

## Once, before the first take

The join to the edge is run by a person, in their own shell, before the session starts: a
session's permission check refuses a script that rewrites hooks and MCP configuration. If this
session's opening names no claim holder, say that the checkout is not joined, and stop.

1. Run `scripts/coder-needs`. If it exits non-zero, show its lines and stop: each names what a
   coder on this machine lacks, and what for. Its `tools/` lines are the exception: they name
   this checkout's fetched tools, and the gate runs in the fixer, which gets its own at each
   take's step 1; show them and go on.
2. Print the coder's claim holder with the command this session's opening gives, `coder-opus` in
   place of `TYPE`: `'<hook>' holder coder-opus --dir '<dir>' --session '<session>'`. It is this
   session's coder's alone; every claim below is for it.

## Each take

1. **The worktree.** If the fixer worktree exists, `git -C <fixer> status --porcelain` must print
   nothing and `git -C <fixer> symbolic-ref -q HEAD` must fail (it is detached). If either does
   not hold, say which and stop: what is there is someone's work. If it does not exist, make it:
   `git -C <top> fetch origin`, then `git -C <top> worktree add --detach <fixer> origin/main`.
   Either way, each of `tools/enola`, `tools/scalafmt` and `tools/shellcheck` the fixer lacks is
   fetched into it, `bash <fixer>/scripts/fetch-<tool>.sh`: they are per checkout, and the gate in
   step 5.3 runs there. A fetch that fails stops the take, in its own words.
2. **Go on, or claim.** `project_claims` with `project` `grit`: a claim in role `coder` whose
   `holder` is the coder's is one this session's coder already holds; take its key. Otherwise
   `project_claim` with `oldest` `{"project": "grit", "kind": "checkpoint", "state": "ready"}`,
   `role` `coder` and the coder's holder, and no `session`; its answer names the key. Refused
   saying there is nothing to take: say no checkpoint is ready, and stop. A checkpoint left in
   `building` with no claim is the manager's to make ready again, never this skill's to take.
3. **Building.** A checkpoint in `ready` is moved to `building` (`project_move`).
4. **The coder.** Start one `coder-opus` agent with this prompt, and wait for its report:
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
   4. `git -C <fixer> push -u origin <branch>`. Refused: say so in git's words, naming the
      branch and its head commit, and stop, with the claim held and the checkpoint in
      `building`. Never push it by another route (another remote, URL or credential).
   5. `project_move` the key to `review`.
   6. `project_release` the key.
   7. Say in one line the key, the branch and its head commit, and that it waits for review.
      Take the next.

## What it never does

- Merge, push anything but the branch its coder built, or open a pull request: a checkpoint in
  `review` is the manager's, and one whose change a person sees waits there for that person.
- Run two coders at once, or a Mill job of its own while a coder runs.
- Take a second checkpoint when the manager's message said to take only one.
- Claim or build in any worktree but the fixer.
- Claim for any holder but its coder's, or claim by `key`.
- Work around a refusal. A refused claim, move or release, or a failed push, is said in its own
  words, and taking stops. The only exception is a failed gate, step 5.3.
