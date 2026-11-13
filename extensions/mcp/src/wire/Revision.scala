package grit.mcp.wire

/** A revision of MCP a server here answers in: 2026-07-28 statelessly, each request declaring
  * it; 2025-06-18 and 2025-11-25 after `initialize`. Older revisions are not listed: a server
  * of theirs must take batches, which this one refuses.
  */
private[wire] enum Revision(val text: String) {
  case Jun2025 extends Revision("2025-06-18")
  case Nov2025 extends Revision("2025-11-25")
  case Jul2026 extends Revision("2026-07-28")
}

private[wire] object Revision {

  /** Every revision, newest first, as `server/discover` and -32022 list them. */
  val Listed: Vector[Revision] = values.toVector.reverse

  /** The listed revision named `text`. */
  def named(text: String): Option[Revision] = values.find(_.text == text)
}
