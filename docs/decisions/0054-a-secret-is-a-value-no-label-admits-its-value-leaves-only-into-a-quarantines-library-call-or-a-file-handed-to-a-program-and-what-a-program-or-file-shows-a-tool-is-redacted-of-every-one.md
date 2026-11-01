# 0054. A secret is a value no label admits: its value leaves only into a quarantine's library call or a file handed to a program, and what a program or file shows a tool is redacted of every one

Status: accepted (2026-11-01)

Context: grit holds model gateway keys, MCP and Slack tokens, a service account's private key and
the database password. Before this record they were guarded by a path rule (`.env`), a rule for
environment names, holders with redacted `toString`s beside holders with public fields, a raw map
of variables a caller could add to a program's environment, and one `replace` in the MCP client.
None of them touched the label lattice (ADRs 0030, 0035, 0036), and nothing stopped a key a
program printed from reaching a tool's result, the window, a row or a model provider.

The threat model is the one grit can meet today:

- **Accidents**: a command that prints a key, a file that holds one. The redaction guards these.
- **Reach by default**: grit's own keys are kept out of every child's environment, so a program
  a tool starts cannot read them without asking.
- **Adversaries**: code that sets out to read a key. Only an OS boundary guards these, and grit has
  none yet: until it does, code a build tool runs for a coder can read the OS user's other
  credentials.

Out of scope, by name: secrets people paste into a conversation, and secrets grit does not hold.

Turned down:

- A level above `Restricted`: maintenance reads every row, and a level admits storage.
- A third lattice: secrets are incomparable atoms, and nothing flows between them.
- An external secret manager: the value is still a `String` at the library call, and printable.
- Placeholders a model may name (`$KEY`): a model-chosen argument would select a secret.
- Redacting at the store's insert: by then facts are parsed and steps journaled, and an edge's
  secrets are its own.
- A secret in a child's environment: every grandchild inherits it unasked, and the one caller,
  the bench's `claude`, reads its key from a file.

Decision:

- **What a secret is.** A value whose disclosure grants authority outside grit, held as
  `grit.core.secret.Secret`: no public accessor, no JSON or `Journaled` form, equal only to
  itself, shown as `Secret(VARIABLE)`. `Secret.read` reads one from the environment, trimmed,
  only for a variable `Named.variable` holds of (KEY, TOKEN, SECRET or PASSWORD, in any case).
  `Named.file` is the one rule for a secrets file (`.env`, `.env.` and anything).
- **Confidentiality.** No clearance admits a secret. Rows, step outputs and tool results are built
  from `Journaled` and `ujson` values, which a `Secret` has none of. Its value leaves only through
  `Reveal.text`, which only these packages may name: `grit.dbos.sql` (JDBC), `grit.models` (an
  HTTP header), `grit.mcp.client`, `grit.slack.client`, and `grit.host.checkout`, which writes it
  to a file handed to a program.
- **Integrity.** Only deployment-declared code hands a secret on. It reaches a program only
  through `Handing`, a capability apart from `Processes`: the secret in a 0600 file in a 0700
  directory of its own outside the checkout, removed when the program's scope ends, its path given
  to a pure function that builds the argv. `Processes` takes no variables from its caller, and
  `Variables`, what `Handing` adds to an environment, refuses a name like a secret's. No API maps
  a runtime string to a secret, and the tool side (`grit.tools`, `grit.turn`, `grit.act`,
  `grit.core.tool`) names neither `Secret`, `Reveal` nor `Handing`, so a model-chosen argument
  never selects one.
- **Redaction.** `grit.host.checkout.Redaction` holds every value of `.env` over the environment
  whose name is named like a secret, one per deployment. It replaces each with
  `[redacted secret: VARIABLE]`: a one-line value whole, longest first; a multi-line value line by
  line, so a text keeps its line count; a value's suffix a text starts with and its prefix it ends
  with, as a cut leaves them; nothing under 8 characters, which would clear common words. The
  local host applies it by construction, before clipping: to `run`'s output and a timeout's, to a
  started program's lines and errors, and to a file's text before `read` and `search`, so a search
  pattern never matches a value. At start-up one log line names the variables redacted and those
  too short to be.
- **A write keeps the secret.** `write` refuses, and writes nothing, when the file holds a value
  the redaction changes and the new text holds the mark: written, the mark would take the
  secret's place. `edit` needs no guard, since its old text, copied from what the model saw,
  holds the mark and matches nothing.

Consequences:

- The mark reaches the model's window, the turn's stored entries, the chat TUI and a Slack reply
  that quotes it. It names the variable, never the value.
- Not redacted: an encoded, split or short value, a value a program prints in pieces, and
  libraries' own logs.
- A model may still write the mark over a secret through a file it never read whole, or through a
  command; the accident model accepts this.
- The bench's `claude` can read its key back: `/proc/$PPID/cmdline` names the key file's path,
  and the file is readable by the OS user until the trial ends. Accepted: the file is 0600,
  outside the checkout and deleted after each trial, and transcripts are redacted.
- Code holding only `Processes` cannot hand a secret on, whatever its arguments; a new caller that
  needs one takes a `Handing` in its signature, where review sees it.

Enforced by: the compiler (`Secret`'s private value and constructor, `Handing` a tracked
capability apart from `Processes`), the probes `SecretProbes` and `HandingCaptureTests`, and the law
rules `only-quarantines-reveal` and `no-tool-names-secret`.
