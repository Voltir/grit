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
  * were first hidden; what `read_hidden` takes ([[ReadHidden]]).
  */
opaque type HiddenName = String

object HiddenName {

  /** The name `text` writes: `h`, then a number from 1 with no leading zero; `Left` otherwise. */
  def read(text: String): Either[String, HiddenName] =
    Either.cond(Name.matches(text), text, s"not a hidden item's name: $text")

  private val Name = "h[1-9][0-9]*".r

  def value(name: HiddenName): String = name

  /** The name after every one of `names`: `h1` when there are none. */
  private[turn] def after(names: Iterable[HiddenName]): HiddenName =
    s"h${names.flatMap(_.drop(1).toIntOption).maxOption.getOrElse(0) + 1}"
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

/** What one round shows the model and offers it, as its `gate:n` step decided (ADR
  * 0036): the turn's `level` after the round's reads; every item below the level before it,
  * `decided`, in request order (an item at or above that level is shown and is not listed);
  * and the offered tools the level withholds. Recorded: what the round's request offers is
  * read from it on a replay or a rebuild, never decided from rows again.
  */
final case class Gated(level: Integrity, decided: Vector[Decided], withheld: Vector[Withheld]) {

  /** The items shown only as stubs, in request order. */
  def hidden: Vector[(Item, Fate.Hidden)] =
    decided.collect { case Decided(item, hidden: Fate.Hidden) => item -> hidden }
}

object TurnGate {

  /** The round's gate from `prior`, the turn's level before it, over `items` in the order the
    * request shows them: one whose integrity dominates `prior` is shown and not listed. Every
    * other is listed: shown, lowering the level to the meet of the two ([[Fate.Lowered]]), when
    * that withdraws none of `offered` the level so far offers, or when `read` holds the name
    * `named` gave it in an earlier round; hidden otherwise ([[Fate.Hidden]]), under the name
    * `named` gave it, else the next after every name `named` holds, and naming the tools it
    * would withdraw, in `offered`'s order. Then every tool of `offered` whose `requires` the
    * level reached does not dominate is withheld, in `offered`'s order. Pure.
    */
  def decide(
      prior: Integrity,
      offered: ToolSet,
      items: Vector[Item],
      read: Set[HiddenName],
      named: Map[ItemRef, HiddenName]
  ): Gated = {
    def withheldAt(level: Integrity) = offered.tools.filterNot(t => level.dominates(t.requires))
    val below = items.filterNot(_.integrity.dominates(prior))
    val (level, decided, _) =
      below.foldLeft((prior, Vector.empty[Decided], HiddenName.after(named.values))) {
        case ((at, done, next), item) =>
          val lowered = at.meet(item.integrity)
          val costs = withheldAt(lowered).filter(t => at.dominates(t.requires)).map(_.name)
          val name = named.get(item.ref)
          if (costs.isEmpty || name.exists(read.contains))
            (lowered, done :+ Decided(item, Fate.Lowered), next)
          else
            name match {
              case Some(kept) => (at, done :+ Decided(item, Fate.Hidden(kept, costs)), next)
              case None =>
                (
                  at,
                  done :+ Decided(item, Fate.Hidden(next, costs)),
                  HiddenName.after(Vector(next))
                )
            }
      }
    Gated(level, decided, withheldAt(level).map(t => Withheld(t.name, t.requires)))
  }

  /** What the model is shown in `item`'s place when `hidden` hides it: its name, its source and
    * level, the tools reading it would withdraw, and that `read_hidden` reads it ([[ReadHidden]]).
    * Pure; not recorded, since a request is never replayed (only its reply is).
    */
  def stub(item: Item, hidden: Fate.Hidden): String = {
    val name = HiddenName.value(hidden.name)
    s"Hidden here as $name: something from ${item.source}, believed only at " +
      s"${Integrity.shown(item.integrity)}. Reading it would withdraw " +
      s"${hidden.costs.map(ToolName.value).mkString(", ")} for the rest of this turn; to read " +
      s"it, call ${ToolName.value(ReadHidden.Name)} with name $name."
  }

  /** The line a round's request adds to its system prompt when its gate, `now`, withholds a
    * tool the round before's, `before`, did not: each such tool, what it requires, and the
    * first item `now` lowered for whose integrity does not dominate that (never one it hid);
    * `None` when `now` withdrew nothing.
    */
  def withdrawn(before: Gated, now: Gated): Option[String] =
    now.withheld.filterNot(before.withheld.contains) match {
      case none if none.isEmpty => None
      case withdrawn =>
        val each = withdrawn.map { w =>
          val cost = now.decided
            .find {
              case Decided(item, Fate.Lowered) => !item.integrity.dominates(w.requires)
              case _ => false
            }
            .fold("")(d =>
              s", since you read ${d.item.source} ${Integrity.shown(d.item.integrity)}"
            )
          s"${ToolName.value(w.tool)}, which requires ${Integrity.shown(w.requires)}$cost"
        }
        Some(s"Withdrawn for the rest of this turn: ${each.mkString("; ")}.")
    }
}
