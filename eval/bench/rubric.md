# Reviewing a task's diffs

You are given one task's brief (`brief.md`) and two or more diffs that each try to do it
(`diff-A.patch`, `diff-B.patch`, …), all from the same base commit. Who or what wrote each diff
is not said, and nothing here depends on it. Review each diff against the brief, the repository's
`CLAUDE.md` and `STYLE.md` as they stood at that base, and the code the diff touches, then rank
them. Judge what each diff does, not how it reads as a story: read the code it adds, and the code
it calls.

## For each diff

List every finding as one line: its kind (below), the file and line or symbol, and what is wrong,
specifically enough that someone could check it. A finding you are not sure of says so. A diff
with no finding of a kind says "none".

### Must-findings

Each would stop the diff from landing as it stands.

1. **A wrong behaviour.** It does something the brief does not ask for, fails to do something
   the brief asks for, or does it wrongly: a bug, a lost or repeated effect, a missed case, a
   failure that is swallowed. A diff that would not compile, or whose tests would fail, is
   wrong here.
2. **A broken STYLE.md rule**, 1 to 11, named by its number.
3. **A test not seen red, or one that pins nothing.** A test that would pass against the code
   before the change, or against a plausible wrong one; an assertion too weak to fail (a
   non-empty check where the value is known); a name that claims more than the assertion
   checks; new behaviour with no test at all.
4. **A workflow step's name, order or recorded output encoding changed with no replay guard**
   (`Durable.patch`, or a new epoch; ADR 0004).
5. **A needless public surface.** A public type, member or parameter that no caller outside its
   own file needs, or that a narrower visibility or an existing type would serve; a definition
   nothing calls.

### Should-findings

Each is worth fixing before landing, but would not stop it alone.

1. **A doc that restates its type.** A comment or Scaladoc saying what the signature,
   visibility or a type already says, or saying how rather than what.
2. **Duplication of existing code.** Something that repeats, by hand, a type, function or step
   the repository already has, or a near-copy of code elsewhere in the diff.
3. **Scope beyond the brief.** A change the brief does not ask for and does not need,
   especially one a person would see.

### Size

The production lines each diff adds: lines added under a `src/` directory, not under a `test/`,
`it/` or `probes/` one, nor docs. Fewer is better, between diffs that do the same.

## Ranking

Rank the diffs within the task, best first, with no ties. A diff with fewer wrong behaviours
ranks above one with more, weighing how much each matters; between diffs alike there, fewer
other must-findings ranks higher; then fewer should-findings; then size. Give, for each place in
the ranking, one or two sentences saying what put it there.

## What to write

For each diff, by its letter: its must-findings, its should-findings, and its production lines
added. Then the ranking, as a line `Ranking: B, A, C`, and the sentences that justify it.
