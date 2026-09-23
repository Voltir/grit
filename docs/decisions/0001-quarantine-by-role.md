# 0001. Java libraries are quarantined by role

Status: accepted (2026-09-23)

Context: grit's conventions (STYLE rules 1–7: pure by default, effects in the signature,
failure as values, no `null`) are broken by every Java library it needs: DBOS and JDBC
today, an HTTP client for the model providers and an LSP client for code mode soon. The
first quarantine was a single module, `grit.interop`, named for the mechanism rather than
the role. With more Java-facing libraries coming, there were two ways to grow it: one
module per library (a driver module each, logic elsewhere), or modules per mechanism,
each quarantining whatever Java its job needs. A single catch-all module was rejected
because it would gradually absorb the application.

Decision: a module owns a mechanism, and any Java library that mechanism needs lives only
inside it, translated into grit's conventions before anything leaves. Such *quarantine
modules* depend only on `grit.core` and meet only in `grit.app`, the composition root.
Logic that merely *uses* a mechanism (the turn, assembly) lives in its own module against
`grit.core`'s seams, never against a quarantine module. `grit.interop` is renamed
`grit.dbos` and becomes the DBOS quarantine module.

Consequences:
- Each mechanism's Java stays behind one boundary, and logic modules are testable with
  fakes of the seams.
- A quarantine module may need a grit-style API of its own (for `grit.dbos`: durable
  steps and workflows as capabilities) so that logic can be written against it.
- Enforcement: Mill makes an undeclared cross-module import a compile error, and rejects
  module cycles. `grit.app` depends on everything, so DBOS reaches it transitively;
  enola's `only-dbos-imports-dbos-*` rules are what keep it out. STYLE rules 6 and 8 carry
  the rule in prose.
