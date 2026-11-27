# 0069. A command an allowlist admits starts only once a machine guard clears it: code first, then Jev, failing closed

Status: accepted (2026-11-27)

Context: ADR 0055's allowlist names which programs `run` starts, not what one invocation does.
Once it lists the shell's common reads and git's stash (`find`, `sed`, `git stash pop`), one
listed command can delete the home directory, write through a link out of the checkout, pop
another coder's stash entry or push. A wrong edit inside the checkout is git's to undo; harm
to the machine beyond it is not. Turned down:

- Predicates per program, in code alone: no list of flags keeps up with `find -delete`,
  `sed 's/x/y/w /etc/x'` or `git diff --output=`, and a predicate cannot be shown to the model.
- Asking Jev of every command: a read or a build gives Jev nothing to judge beyond its paths,
  which code reads exactly (Jev's docs: deterministic rules belong in code), and each call is
  latency.
- A durable step in the turn: the turn cannot follow the checkout's links, and a new step
  needs a patch (ADR 0004). The verdict is the call's outcome, recorded as any result is.
- Failing open when Jev cannot be asked: the guard exists for the case nobody is watching.

Decision:

- **Where.** `Coding.allowlisted`'s `run` asks `MachineGuard.judge` after the allowlist admits
  a command and before it starts. A refusal is the call's result, starting with one fixed
  lead the bench's readout counts, and says what would work instead.
- **Code first.** Every argument is read as a path, as a shell would read it whether or not
  one does: `~` and `$HOME` as the home directory, an option's value as a word of its own,
  `..` and links followed on this machine (`Reach`). An argument naming any other variable
  cannot be read. A command of a cleared shape (git's reads, adds and commits, the stash's
  pushes and applies, the shell's reads that cannot write or run a program, and the rules an
  `Allowlist` marks cleared: its builds) starts unasked when every argument leads inside the
  checkout.
- **The stash, by code alone.** A clone's stash is shared by all its worktrees. In a checkout
  that is its own clone, `pop`, `drop` and `clear` start. In a shared one, `pop` and `drop`
  start only for an entry its `git stash list` label says was made on the checkout's own
  branch (a bare one means `stash@{0}`), and `clear` never; a refusal names the branch's own
  entries and `git stash apply`. The stash is read by git the guard runs itself
  (`Stashes`), never through the model's runs. Entries are never deleted for a coder: a
  shared clone's stash grows by each `push` its coders do not pop.
- **Then Jev.** Anything else is put to the deployment's classifier as four yes/no questions
  about one state (the command, the checkout, where its arguments lead outside it): whether
  it would change files outside the checkout, rewrite what other checkouts share, stop
  processes it did not start, or change another machine. Any answer at `MachineGuard.Refuses`
  or above refuses it, naming the harm. The threshold is set from a corpus of commands,
  catastrophic and benign, judged against Jev's recorded answers.
- **Failing closed.** No classifier, one not cleared for every label the deployment declares
  (an edge knows no classification of what it sends: `ClearedClassifier.everywhere`), or one
  that does not answer within 10 seconds: what code does not clear is refused, saying
  whether to try again. Under the stub placer the stub finds every harm likely.
- **Cost.** A judgment is one request of a few hundred tokens. It is not admitted against the
  day's spend, as a judgment move is: a refused admission would refuse every uncleared
  command mid-run, and the guard's spend is bounded by the commands a turn runs.

Consequences: an unattended coder may be given the shell's reads and the stash without one
command reaching past its checkout. What the guard does not bound is ADR 0055's: a listed
build runs the checkout's code, and reading outside the checkout (a credentials file) is not
refused. Each change to an allowlist reviews the corpus. Enforced by `MachineGuardTests` and
`MachineGuardCorpusTests`, and the probe `MachineGuardCaptureTests`.
