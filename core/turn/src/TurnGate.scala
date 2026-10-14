package grit.turn

import grit.core.id.{ConversationId, DocumentVersion, EntrySeq, ToolCallId}
import grit.core.tool.{ToolName, ToolSet}
import grit.core.visibility.Integrity

/** One thing a round's request may show the model, by where it is kept: an entry of the
  * turn's own conversation, a nearby one, a document version, or a tool result of this turn.
  */
enum ItemRef derives CanEqual {
  case Own(seq: EntrySeq)
  case Nearby(conversation: ConversationId, seq: EntrySeq)
  case Document(version: DocumentVersion)
  case Result(call: ToolCallId)
}

/** An item as `gate:n` read it: where it is kept, how far it may be believed (its row's, fixed
  * at write), and its `source` as a stub names it (a tool's name, a document's label, "an
  * earlier reply").
  */
final case class Item(ref: ItemRef, integrity: Integrity, source: String)

/** A stub's name in the turn, `h1`, `h2`, …: numbered through the turn in the order items
  * were first hidden.
  */
opaque type HiddenName = String

object HiddenName {

  /** The name `text` writes: `h`, then a number from 1 with no leading zero; `Left` otherwise. */
  def read(text: String): Either[String, HiddenName] =
    Either.cond(Name.matches(text), text, s"not a hidden item's name: $text")

  private val Name = "h[1-9][0-9]*".r

  def value(name: HiddenName): String = name
}

/** What became of an item below the round's prior level: shown and lowering the turn
  * (`Lowered`), or shown only as its stub (`Hidden`), naming the tools reading it would
  * withdraw.
  */
enum Fate {
  case Lowered
  case Hidden(name: HiddenName, costs: Vector[ToolName])
}

/** An item `gate:n` decided, and what became of it. */
final case class Decided(item: Item, fate: Fate)

/** An offered tool the round's level withholds: its name and what it requires. */
final case class Withheld(tool: ToolName, requires: Integrity)

/** What one round shows the model and offers it, as its `gate:n` step decided (ADR 0036,
  * 0038): the turn's `level` after the round's reads; every item below the level before it,
  * `decided`, in request order (an item at or above that level is shown and is not listed);
  * and the offered tools the level withholds. Recorded: the round's request is rendered from
  * it and the recorded window, never from a row read again.
  */
final case class Gated(level: Integrity, decided: Vector[Decided], withheld: Vector[Withheld])

object TurnGate {

  /** The round's gate from `prior`, the turn's level before it, over `items` in the order the
    * request shows them: one whose integrity dominates `prior` is shown and not listed; every
    * other is shown and lowers the level to the meet of the two ([[Fate.Lowered]]). Then every
    * tool of `offered` whose `requires` the level reached does not dominate is withheld, in
    * `offered`'s order. Pure.
    */
  def decide(prior: Integrity, offered: ToolSet, items: Vector[Item]): Gated = {
    val below = items.filterNot(_.integrity.dominates(prior))
    val level = below.foldLeft(prior)((at, item) => at.meet(item.integrity))
    Gated(
      level,
      below.map(Decided(_, Fate.Lowered)),
      offered.tools
        .filterNot(t => level.dominates(t.requires))
        .map(t => Withheld(t.name, t.requires))
    )
  }

  /** The line a round's request adds to its system prompt when its gate, `now`, withholds a
    * tool the round before's, `before`, did not: each such tool, what it requires, and the
    * first item `now` lists whose integrity does not dominate that; `None` when `now` withdrew
    * nothing.
    */
  def withdrawn(before: Gated, now: Gated): Option[String] =
    now.withheld.filterNot(before.withheld.contains) match {
      case none if none.isEmpty => None
      case withdrawn =>
        val each = withdrawn.map { w =>
          val cost = now.decided
            .find(!_.item.integrity.dominates(w.requires))
            .fold("")(d =>
              s", since you read ${d.item.source} ${Integrity.shown(d.item.integrity)}"
            )
          s"${ToolName.value(w.tool)}, which requires ${Integrity.shown(w.requires)}$cost"
        }
        Some(s"Withdrawn for the rest of this turn: ${each.mkString("; ")}.")
    }
}
