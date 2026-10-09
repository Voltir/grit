package grit.outline.locate

/** A checkout's branch, None when detached, and its commit's first 8 characters or "unknown". */
final case class Revision(branch: Option[String], head: String)
