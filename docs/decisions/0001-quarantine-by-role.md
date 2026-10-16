# 0001. Java libraries are quarantined by role

Status: accepted (2026-09-23)

Context: grit's conventions (STYLE rules 1–7: pure by default, effects in the signature,
failure as values, no `null`) are broken by every Java library it needs: DBOS and JDBC, an
HTTP client for model providers and MCP servers, the Slack SDK, process and host APIs. The
alternatives were one catch-all interop module, which would gradually absorb the
application, and one module per library (a driver module each, its logic elsewhere).

Decision: a module owns a role, and any Java library that role needs lives only inside it,
translated into grit's conventions before anything leaves: DBOS and JDBC in `grit.dbos`, the
JDK HTTP client in `grit.models` and `grit.mcp`, the Slack SDK in `grit.slack`, processes in
`grit.host`. Such *quarantine modules* depend only on `grit.core` (an edge's also on
`grit.prose` and `grit.edge`), and meet only in `grit.kit` and `grit.app`. Logic that merely *uses* a role (the
turn, assembly, the lifecycle) is written against `grit.core`'s traits, never against a
quarantine module.

Consequences:
- Each role's Java stays behind one boundary, and logic modules are testable with fakes of
  the traits.
- A quarantine module may need a grit-style API of its own (for `grit.dbos`: durable steps
  and workflows as capabilities) so that logic can be written against it.
- Mill's `moduleDeps` are transitive, so `grit.kit` and `grit.app` see DBOS through
  `grit.dbos`; the law, not the compiler, keeps it out of them.
- Enforced by Mill (an undeclared cross-module import is a compile error, and module cycles
  are rejected) and by the law's `only-dbos-imports-*`, `only-models-imports-*`,
  `only-slack-imports-*` and `only-host-imports-*` rule families. STYLE rules 6 and 8 carry
  the rule in prose.
