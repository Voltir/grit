# grit.knowledge

Cards: one short document per unit of a source (a repository's files and directories), each a
summary with search terms and a pointer (ADR 0028) to the hosted call that reads the unit, so a
window can show what a source holds without holding it. Names `grit.core` alone: a source is
read through the MCP tools a deployment offers, never a client of its own.

Packages, in one-way order (each names only those before it):

- **`card`**: what a card is, whatever its source. `CardKey`, a unit's key (the SHA-256 of its
  corpus and path), `UnitVersion` (a git SHA), `CardSummary` and its reply (`CardSummary.Reply`,
  the schema a summary ask is sent), `Card` (a unit's path, kind, version, summary and pointer,
  as its document's key, text and data), and `CardMoves`, the names of a sync run's moves
  (`{phase}/{key}`, `held`, `drop`).

The test tree mirrors it.
