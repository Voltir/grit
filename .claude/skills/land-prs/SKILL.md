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

## Steps

1. **Ready.** Each must hold, or say which does not and stop:
   - `git -C <fixer> status --porcelain` prints nothing, and `git -C <fixer> symbolic-ref -q HEAD`
     fails (it is detached): a coder at work leaves it on a branch.
   - `project_claims` with `project` `grit` shows no claim in role `coder` whose holder's first
     segment (up to the first `/`) is this machine's, as this session's own holder names it: a
     coder claimed here may be about to start.
   - `env -u GITHUB_TOKEN -u GH_TOKEN gh auth status --hostname github.com` exits 0.
2. **Fetch.** `git -C <fixer> fetch origin`. Note each pull request's branch as fetched,
   `<fetched>` (`git -C <fixer> rev-parse origin/<branch>`): the lease its push in step 5 names.
3. **Stack**, in the fixer, each pull request in order:
   1. `gh pr view <n> --json headRefName,baseRefName,state`. It must read `OPEN`, or stop naming
      it. Where `baseRefName` is not `main`, `gh pr edit <n> --base main`: a pull request reads
      merged when its head reaches its own base.
   2. A local `<branch>` that is not an ancestor of `origin/<branch>` (`git merge-base
      --is-ancestor <branch> origin/<branch>` fails) holds commits origin lacks: stop, naming it.
   3. `git checkout -B <branch> origin/<branch>`, then `git rebase <onto>`, `<onto>` being
      `origin/main` for the first and the branch before it for each after. On a conflict, stop,
      naming the pull request.

   The last branch's head is `<head>`.
4. **Gate**, in the fixer. When `<head>` is `origin/<last branch>` (the rebase moved nothing) and
   `scripts/gate-stamp --check <head>` exits 0, it was gated and stamped as it stands: go to
   step 7. Otherwise `CHECK_BASE=$(git merge-base HEAD origin/main) scripts/check --it-reached`,
   then `./mill shutdown`. If the check fails, stop, quoting its failing lines.
5. **Push the branches.** For each branch whose head is not its `<fetched>`: `git -C <fixer> push
   --force-with-lease=<branch>:<fetched> origin <branch>`. These are feature branches; `main` is
   never pushed this way. Refused: stop, in git's words.
6. **Stamp.** `scripts/gate-stamp <head>` in the fixer. Refused by gh (a line `gh refused the
   post: …`): the gate's record is still there, so fix gh and rerun this step alone. Any other
   refusal: stop, in its words.
7. **Move main.** `git -C <fixer> push origin <head>:main`, a fast-forward, never forced.
   Refused (main moved on): `git -C <fixer> fetch origin`, then stop, saying so. A later landing
   stacks again, from the new `main`.
8. **Detach.** `git -C <fixer> checkout --detach`.
9. **Merged.** Wait, at most 30 minutes, until each pull request's `gh pr view <n> --json state`
   reads `MERGED`. Then delete each one's branch on origin (`git -C <fixer> push origin --delete
   <branch>`) and in each checkout here holding it (`git -C <checkout> branch -D <branch>`). When
   the wait ends with some not merged, name their branches and delete none of them.
10. **Tell** the managers of the other machines joined to the project which pull requests landed,
    and `main`'s new head.

**Every stop after step 3 began** leaves the fixer detached, and as it found it: `git rebase
--abort` when a rebase is open, `git checkout --detach`, and, before step 5 has pushed a branch,
`git branch -f <branch> origin/<branch>` for each branch step 3 checked out, which step 3.2 found
holding nothing origin lacks.

## When the ruleset blocks every landing

When a ruleset on `main` requires `grit/gate` and the stamp cannot be posted, its enforcement is
turned off and back on in the repository's Rules settings,
`https://github.com/<owner>/<repo>/settings/rules` (`<owner>/<repo>` read from origin):
Enforcement status Disabled, land, then Active. This is Nick's to do, never a session's.
