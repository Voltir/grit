package grit.assembly.linear

import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Shown, Window}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.TokenEstimator
import grit.core.store.{Db, Entry, EntryStore, Payload, PeriodStore, Principals, Speakers}

/** The window with no choosing: the closing entry of the conversation's newest closed
  * period, then the messages of the most recent whole turns of the turn's own period, before
  * the turn, that fit in what `budget` tokens by `estimator` leave; never their summaries.
  * The baseline every smarter assembler is measured against. It never draws on other
  * conversations.
  *
  * The closing entry is paid for first; a budget too small for it leaves it out. Turns are
  * kept or dropped whole, so a window never opens on a reply without its question, or a tool
  * result without its call. The first turn back that does not fit ends the window, even if
  * older turns would: the model never sees a history with holes. A newest turn larger than
  * what is left on its own leaves no turns. A window that leaves turns out is charged one
  * gap line ([[Shown.Gap]]). A person's message is costed with its author's name line
  * ([[Shown.of]]), as it is sent.
  */
final class LinearAssembler(
    entries: EntryStore,
    periods: PeriodStore,
    principals: Principals,
    estimator: TokenEstimator,
    budget: Tokens
) extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] =
    db.read {
      for {
        opening <- periods.opening(request.turn)
        all <- entries.list(request.turn.conversationId)
        speakers <- principals.speakers(all.map(_.id))
      } yield {
        val (closings, left) =
          LinearAssembler.opened(opening.closing.map(_.entry).toVector, estimator, budget)
        val turns = LinearAssembler.turnsBefore(all, opening.first, request.turn.turnSeq)
        val kept = LinearAssembler.tail(turns, speakers, estimator, left)
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
    // A closing is grit's, never a person's: no name line is shown on it.
    val kept = recent(closings.map(Vector(_)), Speakers.none, estimator, budget).flatten
    val spent = kept.map(e => shownCost(e, Speakers.none, estimator)).foldLeft(Tokens.Zero)(_ + _)
    (kept, Tokens(Tokens.value(budget) - Tokens.value(spent)))
  }

  /** The most recent of `turns` that fit in `budget` together, each costed with `speakers`'
    * names, oldest first. The first turn back that does not fit ends them, even if older ones
    * would.
    */
  def recent(
      turns: Vector[Vector[Entry]],
      speakers: Speakers,
      estimator: TokenEstimator,
      budget: Tokens
  ): Vector[Vector[Entry]] =
    turns.reverseIterator
      .scanLeft((Tokens.Zero, Vector.empty[Entry])) { case ((spent, _), turn) =>
        (spent + cost(turn, speakers, estimator), turn)
      }
      .drop(1)
      .takeWhile { case (spent, _) => Tokens.value(spent) <= Tokens.value(budget) }
      .map(_._2)
      .toVector
      .reverse

  /** The most recent of `turns` that fit in `budget`, as [[recent]] chooses them, with one
    * gap line ([[Shown.Gap]]) paid for out of `budget` when they are not all of `turns`.
    */
  def tail(
      turns: Vector[Vector[Entry]],
      speakers: Speakers,
      estimator: TokenEstimator,
      budget: Tokens
  ): Vector[Vector[Entry]] = {
    val all = recent(turns, speakers, estimator, budget)
    if (all.size == turns.size) all
    else
      recent(
        turns,
        speakers,
        estimator,
        Tokens(Tokens.value(budget) - Tokens.value(gap(estimator)))
      )
  }

  /** What one gap line ([[Shown.Gap]]) costs by `estimator`. */
  def gap(estimator: TokenEstimator): Tokens = estimator.message(Shown.Gap)

  /** What the model is shown of `entries`, with `speakers`' names, costs by `estimator`
    * ([[Shown.of]]).
    */
  def cost(entries: Vector[Entry], speakers: Speakers, estimator: TokenEstimator): Tokens =
    entries.map(shownCost(_, speakers, estimator)).foldLeft(Tokens.Zero)(_ + _)

  private def shownCost(e: Entry, speakers: Speakers, estimator: TokenEstimator): Tokens =
    Shown.of(e, speakers).fold(Tokens.Zero)(estimator.message)

  private def isMessage(e: Entry): Boolean = e.payload match {
    case Payload.Message(_) => true
    // A turn's tool exchange is its own: a window never holds a call apart from its turn.
    case Payload.Heard(_) | Payload.Summary(_) | Payload.Query(_) | Payload.Window(_, _, _) |
        Payload.Topic(_) | Payload.Exchange(_) | Payload.Result(_, _) | Payload.Attempt(_) |
        Payload.Ask(_, _) | Payload.Closed(_, _, _) =>
      false
  }
}
