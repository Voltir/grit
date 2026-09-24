package grit.assembly

import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Window}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.store.{Db, Entry, EntryStore, Payload}

/** The window with no choosing: the most recent whole turns before the turn whose
  * messages fit in `budget` estimated tokens ([[TokenEstimate]]), oldest first. The
  * baseline every smarter assembler is measured against.
  *
  * Turns are kept or dropped whole, so a window never opens on a reply without its
  * question, or a tool result without its call. The first turn back that does not fit
  * ends the window, even if older turns would: the model never sees a history with holes.
  * A newest turn larger than the budget on its own leaves the window empty.
  */
final class LinearAssembler(entries: EntryStore, budget: Tokens) extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] =
    db.read(entries.list(request.turn.conversationId))
      .map { all =>
        val before =
          all.filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(request.turn.turnSeq))
        val turns = before.groupBy(e => TurnSeq.value(e.turnSeq)).toVector.sortBy(_._1).map(_._2)
        val kept = turns.reverseIterator
          .scanLeft((Tokens.Zero, Vector.empty[Entry])) { case ((spent, _), turn) =>
            (spent + LinearAssembler.cost(turn), turn)
          }
          .drop(1)
          .takeWhile { case (spent, _) => Tokens.value(spent) <= Tokens.value(budget) }
          .map(_._2)
          .toVector
          .reverse
        Window(kept.flatten.sortBy(_.seq).map(_.id))
      }
      .left
      .map(AssemblyError.Store(_))
}

object LinearAssembler {

  /** The window's default size: a small fraction of a current model's context, so a turn
    * stays cheap.
    */
  val DefaultBudget: Tokens = Tokens(24_000)

  private def cost(turn: Vector[Entry]): Tokens =
    turn
      .map(_.payload match { case Payload.Message(m) => TokenEstimate.of(m) })
      .foldLeft(Tokens.Zero)(_ + _)
}
