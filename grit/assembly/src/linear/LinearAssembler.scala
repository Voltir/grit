package grit.assembly.linear

import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Window}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.TokenEstimator
import grit.core.store.{Db, Entry, EntryStore, Payload}

/** The window with no choosing: the messages of the most recent whole turns before the
  * turn that fit in `budget` tokens by `estimator`, oldest first; never their summaries. The
  * baseline every smarter assembler is measured against.
  *
  * Turns are kept or dropped whole, so a window never opens on a reply without its
  * question, or a tool result without its call. The first turn back that does not fit
  * ends the window, even if older turns would: the model never sees a history with holes.
  * A newest turn larger than the budget on its own leaves the window empty.
  */
final class LinearAssembler(entries: EntryStore, estimator: TokenEstimator, budget: Tokens)
    extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] =
    db.read(entries.list(request.turn.conversationId))
      .map { all =>
        val kept = LinearAssembler.recent(
          LinearAssembler.turnsBefore(all, request.turn.turnSeq),
          estimator,
          budget
        )
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

  /** The message entries of the turns in `all` before `turn`, one vector per turn, oldest
    * turn first. Summaries and any other entries that are not messages are left out.
    */
  def turnsBefore(all: Vector[Entry], turn: TurnSeq): Vector[Vector[Entry]] =
    all
      .filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(turn) && isMessage(e))
      .groupBy(e => TurnSeq.value(e.turnSeq))
      .toVector
      .sortBy(_._1)
      .map(_._2.sortBy(_.seq))

  /** The most recent of `turns` that fit in `budget` together, oldest first. The first turn
    * back that does not fit ends them, even if older ones would.
    */
  def recent(
      turns: Vector[Vector[Entry]],
      estimator: TokenEstimator,
      budget: Tokens
  ): Vector[Vector[Entry]] =
    turns.reverseIterator
      .scanLeft((Tokens.Zero, Vector.empty[Entry])) { case ((spent, _), turn) =>
        (spent + cost(turn, estimator), turn)
      }
      .drop(1)
      .takeWhile { case (spent, _) => Tokens.value(spent) <= Tokens.value(budget) }
      .map(_._2)
      .toVector
      .reverse

  /** What `turn`'s messages cost by `estimator`. */
  def cost(turn: Vector[Entry], estimator: TokenEstimator): Tokens =
    turn
      .map(_.payload)
      .collect { case Payload.Message(m) => estimator.message(m) }
      .foldLeft(Tokens.Zero)(_ + _)

  private def isMessage(e: Entry): Boolean = e.payload match {
    case Payload.Message(_) => true
    case Payload.Summary(_) | Payload.Query(_) | Payload.Window(_, _) => false
  }
}
