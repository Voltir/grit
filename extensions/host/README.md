# grit.host

Implements `Workspace`, `Edits`, `Shell`, `Processes`, `Runs`, `Instructions`, `Memory` and `Blobs` (`grit.core.host`; what a
deployment can supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

The local host: `grit.core.host`'s capabilities over this machine's files and processes. The
only module that starts a process or reads which process and machine this is (STYLE rule 8;
the law's `only-host-imports-*` rules and `scripts/enola-law.sh`'s process grep hold it there).

Its packages, in order, none importing another but `git` and `blobs`, which remove trees with
`checkout`'s `LocalTree`: `checkout`, `git`, `machine`, `blobs`.

- **`checkout`**: a checkout on this machine.
  - `LocalWorkspace` — reading a checkout: read, list and search.
  - `LocalEdits` — changing files in a checkout.
  - `LocalProcesses` — starting a program with arguments under a root, run to its end or held
    by its caller's scope; it sees only an allowlisted environment.
  - `LocalShell` — running a command in a checkout with `sh -c`, through `LocalProcesses`.
  - `LocalRuns` — commands that outlive the call starting them, through `LocalProcesses`'
    start, each output a file under the checkout's `.grit/runs`; its first start sweeps what a
    process that ended left there (ADR 0055).
  - `LocalHanding` — starting a program handed a secret, as a file only this user reads,
    through `LocalProcesses`' start.
  - `Redaction` — the deployment's secrets, replaced by core's `RedactionMark` in what
    a program shows (`run`'s output, a started program's lines, its errors) and in a file's
    text before it is read or searched. Each class above takes the deployment's one.
  - `LocalInstructions` — the instruction files around a directory.
  - `LocalMemory` — what the processes working in a directory of a checkout hold, and what the
    machine has available, read from `/proc`.
  - `LocalTree` (private to `grit.host`) — a directory readable by this user alone, a tree's
    removal that carries on past what it cannot delete, and a lock nobody holds freed with its
    directory (what `LocalBlobs` and `LocalRuns` sweep).
  - `Checkout` (private) — a checkout's root, and how a relative path is found under it.
- **`git`**: `WorkingCopies` — a run of work's own working copy of a repository's clone, a git
  worktree at `<root>/<repository>/<work>` on the branch `grit/<work>`, held while its run goes
  on; at most `Most` of one repository, and those unused for `Kept` swept at start. A copy
  holding changes never committed is removed only once unused for `Unpublished`. Its sweep deletes the branches of runs done
  with, on their remote repository first, by lease. `Branches` — a clone's run branches
  (`LocalBranch`), pushed and offered from the clone alone. `LocalStashes` — a checkout's git
  stash, read for the machine guard. `LocalUncommitted` — the files a commit in a checkout
  would leave out, read before a coding turn's `git commit`.
- **`machine`**: `LocalMachine` — this process's `ProcessIdentity`.
- **`blobs`**: `LocalBlobs` — bytes kept in a directory of this machine by their digest, and
  the removal of the scratch directories an earlier grit's Drive edge left.
