---
name: land-prs
description: Land pull requests on main by fast-forward: stack them, gate and stamp the stack's head in the fixer worktree, and move main to it.
---

# Landing

`main` moves only by fast-forward to a commit the gate passed and stamped. What may land, and on
whose review, is CLAUDE.md's ("Landing" in its working agreements). Run this in grit's main
checkout, given the pull requests to land in the order they land. It gates in the fixer worktree,
`<fixer>` = `<top>/.claude/worktrees/fixer` (`<top>` is `git rev-parse --show-toplevel`). Every
`gh` call here is `env -u GITHUB_TOKEN -u GH_TOKEN gh …`, as gh is logged in.

The stamp, the commit status `grit/gate`, means gated, never reviewed. It guards against
mistakes, not against a session that posts the status with `gh api` itself. Only the stack's
head is stamped: a bisect may meet a commit that was never gated.

Never `gh pr merge`: a merge through GitHub writes commits the gate never saw.

## Before

Each pull request exists and is not a draft: `gh pr ready <n>` for one that is. A commit the lander
itself adds to a branch, such as a review's fix, is pushed to that branch first: step 4.2 stops on a
local branch holding commits origin lacks.

## Steps

1. **Ready.** Each must hold, or say which does not and stop:
   - `git -C <fixer> status --porcelain` prints nothing, and `git -C <fixer> symbolic-ref -q HEAD`
     fails (it is detached): a coder at work leaves it on a branch. This is the guard against
     landing under a coder.
   - `project_claims` with `project` `grit` shows no claim in role `coder` whose holder's first
     segment (up to the first `/`) is this machine's, as this session's own holder names it: a
     coder claimed here may be about to start. One exception: when this session is that coder's
     manager and the coder has reported and waits on it, say so and go on, leaving the coder's
     claim as it is.
   - `env -u GITHUB_TOKEN -u GH_TOKEN gh auth status --hostname github.com` exits 0.
2. **Claim.** Print the lander's holder with the hook this session's opening line names:
   `'<hook>' holder lander --dir <top> --session <this session's id>`. Then `project_claim` with
   `key` `grit/landing`, `role` `lander` and that holder, giving no `session`. Refused, naming a
   holder and when it lapses: another session is landing; stop, quoting it. What the claim is:
   [the landing mechanism](artifact:grit/628).
3. **Fetch.** `git -C <fixer> fetch origin`. Note each pull request's branch as fetched,
   `<fetched>` (`git -C <fixer> rev-parse origin/<branch>`): the lease its push in step 6 names.
4. **Stack**, in the fixer, each pull request in order:
   1. `gh pr view <n> --json headRefName,baseRefName,state`. It must read `OPEN`, or stop naming
      it. Where `baseRefName` is not `main`, `gh pr edit <n> --base main`: a pull request reads
      merged when its head reaches its own base.
   2. A local `<branch>` that is not an ancestor of `origin/<branch>` (`git merge-base
      --is-ancestor <branch> origin/<branch>` fails) holds commits origin lacks: stop, naming it.
   3. `git checkout -B <branch> origin/<branch>`, then `git rebase <onto>`, `<onto>` being
      `origin/main` for the first and the branch before it for each after. On a conflict, stop,
      naming the pull request.

   The last branch's head is `<head>`.
5. **Gate**, in the fixer. When `<head>` is `origin/<last branch>` (the rebase moved nothing) and
   `scripts/gate-stamp --check <head>` exits 0, it was gated and stamped as it stands: go to
   step 8. Otherwise `CHECK_BASE=$(git merge-base HEAD origin/main) scripts/check --it-reached`,
   then `./mill shutdown`. If the check fails, stop, quoting its failing lines.
6. **Push the branches.** For each branch whose head is not its `<fetched>`: `git -C <fixer> push
   --force-with-lease=<branch>:<fetched> origin <branch>`. These are feature branches; `main` is
   never pushed this way. Refused: stop, in git's words.
7. **Stamp.** `scripts/gate-stamp <head>` in the fixer. Refused by gh (a line `gh refused the
   post: …`): the gate's record is still there, so fix gh and rerun this step alone. Any other
   refusal: stop, in its words.
8. **Move main.** When the gate ran past the claim's `lapses` (`project_claims`), release
   `grit/landing` and claim it again as in step 2 first. Then `git -C <fixer> push origin
   <head>:main`, a fast-forward, never forced, and `project_release` `grit/landing` right after,
   whatever the push answered. Refused:
   - by the ruleset (a `remote:` line such as `Required status check "grit/gate" is expected`, or
     `push declined due to repository rule violations`): stop, quoting the remote's lines whole.
     Never retry it another way.
   - because main moved on (a non-fast-forward rejection): `git -C <fixer> fetch origin`, then
     stop, saying so. A later landing stacks again, from the new `main`.
9. **Detach.** `git -C <fixer> checkout --detach`.
10. **Merged.** Wait, at most 30 minutes, until each pull request's `gh pr view <n> --json state`
   reads `MERGED`. Then, for each branch, check its head is an ancestor of `origin/main`
   (`git -C <fixer> fetch origin`, then `git -C <fixer> merge-base --is-ancestor origin/<branch>
   origin/main`) and say so; delete it on origin (`git -C <fixer> push origin --delete <branch>`)
   and in each checkout here holding it (`git -C <checkout> branch -d <branch>`, never `-D`). A
   branch that is not an ancestor, and any still not merged when the wait ends, is named and not
   deleted.
11. **Tell** the managers of the other machines joined to the project which pull requests landed,
    and `main`'s new head.

**Every stop after step 2 made the claim** releases it (`project_release` `grit/landing`).
**Every stop after step 4 began** also leaves the fixer detached, and as it found it: `git rebase
--abort` when a rebase is open, `git checkout --detach`, and, before step 6 has pushed a branch,
`git branch -f <branch> origin/<branch>` for each branch step 4 checked out, which step 4.2 found
holding nothing origin lacks.

## When the ruleset blocks every landing

The ruleset on `main` requires `grit/gate`. When the stamp cannot be posted, its enforcement is
lifted and restored: `gh api -X PUT repos/<owner>/<repo>/rulesets/<id>` with `enforcement`
`disabled`, land, then `active` (`<owner>/<repo>` read from origin; `gh api
repos/<owner>/<repo>/rulesets` lists the id), or in the repository's Rules settings,
`https://github.com/<owner>/<repo>/settings/rules`. This is Nick's to do, never a session's.
