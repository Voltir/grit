package grit.assembly.linear

import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Shown, Window}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.TokenEstimator
import grit.core.store.{Db, Entry, EntryStore, Payload, PeriodStore}

/** The window with no choosing: the closing entry of the conversation's newest closed
  * period, then the messages of the most recent whole turns of the turn's own period, before
  * the turn, that fit in what `budget` tokens by `estimator` leave; never their summaries.
  * The baseline every smarter assembler is measured against.
  *
  * The closing entry is paid for first; a budget too small for it leaves it out. Turns are
  * kept or dropped whole, so a window never opens on a reply without its question, or a tool
  * result without its call. The first turn back that does not fit ends the window, even if
  * older turns would: the model never sees a history with holes. A newest turn larger than
  * what is left on its own leaves no turns.
  */
final class LinearAssembler(
    entries: EntryStore,
    periods: PeriodStore,
    estimator: TokenEstimator,
    budget: Tokens
) extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] =
    db.read {
      for {
        opening <- periods.opening(request.turn)
        all <- entries.list(request.turn.conversationId)
      } yield {
        val (closings, left) =
          LinearAssembler.opened(opening.closing.map(_.entry).toVector, estimator, budget)
        val turns = LinearAssembler.turnsBefore(all, opening.first, request.turn.turnSeq)
        val kept = LinearAssembler.recent(turns, estimator, left)
        Window(closings.map(_.id) ++ kept.flatten.sortBy(_.seq).map(_.id))
      }
    }.left
      .map(AssemblyError.Store(_))
}

object LinearAssembler {

  /** The window's default size: a small fraction of a current model's context, so a turn
    * stays cheap.
    */
  val DefaultBudget: Tokens = Tokens(24_000)

  /** The message entries of the turns in `all` from `from` and before `turn`, one vector per
    * turn, oldest turn first. Summaries and any other entries that are not messages are left
    * out.
    */
  def turnsBefore(all: Vector[Entry], from: TurnSeq, turn: TurnSeq): Vector[Vector[Entry]] =
    all
      .filter { e =>
        val t = TurnSeq.value(e.turnSeq)
        t >= TurnSeq.value(from) && t < TurnSeq.value(turn) && isMessage(e)
      }
      .groupBy(e => TurnSeq.value(e.turnSeq))
      .toVector
      .sortBy(_._1)
      .map(_._2.sortBy(_.seq))

  /** The newest of `closings` (oldest first) whose shown messages fit in `budget` by
    * `estimator` together, oldest first, and what of `budget` they leave. The first one back
    * that does not fit ends them.
    */
  def opened(
      closings: Vector[Entry],
      estimator: TokenEstimator,
      budget: Tokens
  ): (Vector[Entry], Tokens) = {
    val kept = recent(closings.map(Vector(_)), estimator, budget).flatten
    val spent = kept.map(e => shownCost(e, estimator)).foldLeft(Tokens.Zero)(_ + _)
    (kept, Tokens(Tokens.value(budget) - Tokens.value(spent)))
  }

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

  /** What the model is shown of `entries` costs by `estimator` ([[Shown.of]]). */
  def cost(entries: Vector[Entry], estimator: TokenEstimator): Tokens =
    entries.map(shownCost(_, estimator)).foldLeft(Tokens.Zero)(_ + _)

  private def shownCost(e: Entry, estimator: TokenEstimator): Tokens =
    Shown.of(e).fold(Tokens.Zero)(estimator.message)

  private def isMessage(e: Entry): Boolean = e.payload match {
    case Payload.Message(_) => true
    // A turn's tool exchange is its own: a window never holds a call apart from its turn.
    case Payload.Summary(_) | Payload.Query(_) | Payload.Window(_, _) | Payload.Topic(_) |
        Payload.Exchange(_) | Payload.Result(_, _) | Payload.Attempt(_) | Payload.Ask(_, _) |
        Payload.Closed(_, _, _) =>
      false
  }
}
