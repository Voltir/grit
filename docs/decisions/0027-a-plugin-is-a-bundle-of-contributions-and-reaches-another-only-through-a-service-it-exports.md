# 0027. A plugin is a bundle of contributions to core's points, and reaches another only through a service it exports

Status: accepted (2026-10-05); revised (2026-10-18)

Context: a plugin is posted each closed period and keeps cache documents (ADR 0011); anything
more was the kit's, which offered Digest's `recent_activity` because it recognised Digest by
type. Features that keep knowledge, track work or write a daily standup need more than
posting, and read one another. The close alternative was letting a plugin read another's
documents directly: their storage would become each other's interface, and neither could
change it. A runtime plugin registry was turned down as a framework of its own (ADR 0021).

Decision:

- **A plugin is a name, a version and a set of optional contributions**, each to a point core
  owns: posting from closed periods (cache documents); ledger documents (ADR 0028); tools the
  turn runs itself over grit's store, as `about` is, never hosted by an edge; prompt
  fragments for the windows of the places it scopes; and jobs (ADR 0029). A plugin whose tool
  must touch a workspace or a service contributes a `Hosted` description, and an edge runs it
  (ADR 0017). The kit wires every plugin's contributions the same way and names no plugin.
- **What a plugin derives is built only from closings** (ADR 0011), because a consumer of the
  raw log could hold back its deletion, and one stuck consumer would stop it all. A
  derivation that reads raw messages only while they are kept, holds nothing back and is
  withdrawn as they age out does not meet that reason, and is allowed through core alone: a
  dreaming job sees strands by id and count while core summarises their messages for it
  (ADR 0042), so its code never holds a message's text. Posting runs in a
  durable workflow of its own, triggered by a close, outside the turn's steps, so a plugin
  can be swapped, disabled or rebuilt without a patch or an epoch. Its documents are either
  a cache, made from one closing, rebuilt from closings and dropped with them, or a ledger's,
  with a window and bound of its own (ADR 0014), the only kind kept past the closings'
  window. A document merged from several closings (a rollup, an index) is a ledger's, never a
  cache's; a surface merges caches at read time.
- **A plugin depends on another by its module depending on the other's** (Mill's
  `moduleDeps`, which refuse a cycle), so a dependent is always built against its
  dependency's own version. It reaches the dependency at runtime only through a service value
  the dependency exports, a trait over its own documents, handed over when the deployment is
  built: to its tools, and to its dreaming jobs' scans, in the scan's transaction. No plugin
  reads or writes another's documents directly.
- **`Deployment.of` refuses a plugin whose dependency is not among the deployment's
  plugins** (`PluginUnmet`), naming both.
- **A plugin never reads a dependency while posting**: each plugin's cursor moves on its own,
  so a dependency read there may be half posted. Tools, fragments and jobs read it.
- **A plugin owns no table**: its rows are in grit's generic plugin tables
  (`grit.plugin_docs`, `grit.plugin_cursors`, `grit.documents`), keyed by its name. Plugin
  tables would need an owner for a deployment's schema, which no record gives.

Consequences:

- Digest offers `recent_activity` itself; a deployment's own plugin can contribute anything a
  shipped one can.
- A plugin needing a point core lacks is a change to core, decided as such.
- What was evicted, and kept by no ledger inside its window, is eventually forgotten.
- Enforced by Mill (no cycle), `Deployment.of` (no missing dependency) and the law
  (`plugins-name-core-and-plugins`).
