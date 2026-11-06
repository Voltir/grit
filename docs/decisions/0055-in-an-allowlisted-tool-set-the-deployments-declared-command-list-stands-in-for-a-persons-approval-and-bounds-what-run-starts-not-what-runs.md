# 0055. In an allowlisted tool set the deployment's declared command list stands in for a person's approval, and bounds what `run` starts, not what runs

Status: accepted (2026-11-02); revised (2026-11-02)

Context: ADR 0017 has a person approve each `write`, `edit` and `run` before it runs, and a
deployment with no one to answer (a run with arguments, `grit serve`) offers only the tools that
read. A coder that works unattended needs to change files, build and commit with nobody
watching, as a Claude Code coder does with a list of allowed commands and no prompts.

Turned down:

- Asking per call, as today: nobody is there to answer in an unattended run.
- Standing approvals a person grants once per conversation: ADR 0017 turned these down, and they
  still ask someone.
- Predicates over a command's arguments: a predicate cannot be shown to the model or to a person,
  and filtering flags buys nothing while a listed build runs the checkout's code.
- A shell command line checked against the list: a shell reads what follows the first word.

Decision:

- **The list is the approval.** In an allowlisted set (`Offered.Allowlisted`, `Coding.allowlisted`)
  the deployment's declared `Allowlist` stands in for a person's approval of `write`, `edit` and
  `run`. It is given once, when the deployment is declared, per command shape (`Exactly` an argv,
  `Leading` with one, or `Following` with at least one more), never per call; none of the three asks.
- **What remains.** `write`, `edit` and `run` still require a call stamped at Member integrity, so
  only an attested asker's turn drives them. `run` takes a program and its arguments, with no
  shell, and `Allowlisted`, a sealed capability, starts only what the list permits, for at most
  its longest (never past `Runs.Longest`, 60 minutes); anything else is refused and nothing starts. The
  default offers are unchanged: the set is opt-in (`GRIT_TOOLS=allowlist`).
- **What it does not bound**, by name:
  - a file `write` puts in `.git` (a hook, `core.hooksPath` in `.git/config`) runs at the next
    listed `git commit`;
  - a listed build (`./mill`, `scripts/check`) runs the checkout's code, which `write` and `edit`
    change, so with a build listed, `run` can start any program;
  - the network, which a build reaches;
  - the OS user's other credentials (ADR 0054's adversaries).
  So the list names the programs `run` starts and removes the shell; it is not a boundary on what
  those programs do. What it buys is an accident bound: no one-line `rm -rf` or `curl … | sh`,
  and two deliberate calls before foreign code runs.
- **Why it is accepted:** it is the grant a Claude Code coder runs under, its allowed commands
  and no prompts, so a bench compares the two harnesses on like terms; and it holds only until an
  OS boundary contains what runs.
- **Under `grit serve`** the allowlisted set is offered too, but no conversation `serve`
  answers reaches it: the coding tools are served only by the local edge, which the chat and
  `grit work` start for their directory and `serve` does not, and only a chat conversation works
  in a directory. Were a conversation from a served edge to work in a checkout, anyone who can
  address the deployment there would drive `write`, `edit` and `run` on the operator's machine
  with no prompt; the bound is to come first, from the principal asking, as its integrity and
  clearance allow, which this record does not build.

- **Long runs.** A command still going after `run`'s wait outlives the call: its id comes back,
  and `tail` and `stop` take it (`Runs`). It belongs to the edge's process, not the turn's
  workflow, so no step changes and one call still waits at most `Shell.Longest`, on which
  `Calling.RunWithin` is built. Its handle lives in that process's memory and is forgotten on a
  restart, which answers `Gone`, never a throw: a table or a `Job` (ADR 0029) would outlive a
  process that no longer holds the program. Handles are per checkout, not per conversation:
  any conversation working there reaches them, as it reaches the checkout. The chat's end kills
  what is going; a signal leaves an orphan to its timeout or to the next start's sweep, which
  kills a run whose process left it. Its output is a file under `.grit/runs` in the checkout,
  killed past 16 MB, at most four going and eight ended kept: never a table.

Consequences:

- An unattended run can change files, build and commit. A deployment that wants a person in the
  loop keeps `Offered.All` in the chat.
- `Deployment.of` refuses only `Offered.All` while a served edge cannot answer an ask; an
  allowlisted set asks no one.
- The list is code a person reads once, and the model is told it whole in `run`'s description,
  from the same value that is enforced.
- Enforced by the sealed `Allowlisted` (probe `AllowlistedCaptureTests` in `grit.core`), the
  toolbox's capture set, which never names `Processes` (probe `AllowlistedCaptureTests` in
  `grit.tools`), and `AllowlistTests`.
