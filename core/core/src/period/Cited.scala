package grit.core.period

import grit.core.id.EntryId

/** What an added Standing line's citation named, at its close (ADR 0018): the `entries` of the
  * lines it cited, in the period's order, each once, whatever ground they gave (a heard,
  * untrusted or failed line's too); and the `tools` those entries ran, by the names their
  * stored calls give, as the model sent them. A reference outlives nothing: once an entry is
  * purged its id names no entry. [[Cited.Empty]] for an add that cites nothing, and for every
  * add of another section.
  */
final case class Cited private (entries: Vector[EntryId], tools: Set[String])

object Cited {

  /** Nothing cited. */
  val Empty: Cited = new Cited(Vector.empty, Set.empty)

  /** `entries`, each kept once where it first comes, and `tools`. */
  def of(entries: Vector[EntryId], tools: Set[String]): Cited = new Cited(entries.distinct, tools)
}
