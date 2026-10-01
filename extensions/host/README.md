# grit.host

Implements `Workspace`, `Edits`, `Shell` and `Instructions` (`grit.core.host`; what a
deployment can supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

The local host: `grit.core.host`'s capabilities over this machine's files and processes, in
a checkout. The only module that starts a process or reads which process and machine this
is (STYLE rule 8; the law's `only-host-imports-*` rules and `scripts/enola-law.sh`'s process
grep hold it there).

One package, `grit.host`:

- `LocalWorkspace` — reading a checkout: read, list and search.
- `LocalEdits` — changing files in a checkout.
- `LocalShell` — running a command in a checkout; it sees only an allowlisted environment.
- `LocalInstructions` — the instruction files around a directory.
- `LocalMachine` — this process's `ProcessIdentity`.
- `Checkout` (private) — a checkout's root, and how a relative path is found under it.
