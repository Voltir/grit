# grit.host

Implements `Workspace`, `Edits`, `Shell`, `Processes`, `Instructions` and `Blobs` (`grit.core.host`; what a
deployment can supply: [`docs/extending.md`](../../docs/extending.md#extension-points)).

The local host: `grit.core.host`'s capabilities over this machine's files and processes. The
only module that starts a process or reads which process and machine this is (STYLE rule 8;
the law's `only-host-imports-*` rules and `scripts/enola-law.sh`'s process grep hold it there).

Its packages, none importing another:

- **`checkout`**: a checkout on this machine.
  - `LocalWorkspace` — reading a checkout: read, list and search.
  - `LocalEdits` — changing files in a checkout.
  - `LocalProcesses` — starting a program with arguments under a root, run to its end or held
    by its caller's scope; it sees only an allowlisted environment.
  - `LocalShell` — running a command in a checkout with `sh -c`, through `LocalProcesses`.
  - `LocalInstructions` — the instruction files around a directory.
  - `Checkout` (private) — a checkout's root, and how a relative path is found under it.
- **`machine`**: `LocalMachine` — this process's `ProcessIdentity`.
- **`blobs`**: `LocalBlobs` — bytes kept in a directory of this machine by their digest, and
  the removal of the scratch directories an earlier grit's Drive edge left.
