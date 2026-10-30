package grit.assembly.linear

import grit.core.context.{
  AssemblyError,
  AssemblyRequest,
  CarriedWindow,
  ContextAssembler,
  JudgeText,
  Packing,
  Prepared,
  Projection,
  Querying,
  Results,
  Retrieved,
  Shown,
  Width,
  Window
}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.TokenEstimator
import grit.core.store.{Db, Entry, EntryStore, Payload, PeriodStore, Principals, Speakers}
import grit.core.visibility.Subject

/** The window with no choosing: at a boundary, the closing entry of the conversation's newest
  * closed period, then the messages of the most recent whole turns of the turn's own period,
  * before the turn, that fit in what half of `budget` ([[LinearAssembler.share]]) leaves by
  * `estimator`; never their summaries. The baseline every smarter assembler is measured
  * against. It never draws on other conversations.
  *
  * Given a carried window it appends the turns since its last part's turn, every one of them,
  * with no closing and no gap line. Its limit, at which it draws afresh as with nothing
  * carried: the turn's period began after a carried part's turn (a close since), or the
  * carried parts' costs, their turns as sent ([[LinearAssembler.sent]]) and the turns since
  * spend more than `budget`. A request's width bounds only a boundary, never the limit.
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

  /** Always drawn: it never searches. */
  def prepare(request: AssemblyRequest)(using Db^): Either[AssemblyError, Prepared] =
    Right(Prepared.Drawn(Querying.Drawn(Vector.empty)))

  /** Nothing: it never searches. */
  def search(request: AssemblyRequest, written: Querying.Written)(using
      Db^
  ): Either[AssemblyError, Results] = Right(Results.empty)

  /** The turn's own messages, and nothing found: it never searches. */
  def judgeText(
      request: AssemblyRequest,
      retrieved: Vector[Retrieved],
      chars: Int
  )(using db: Db^): Either[AssemblyError, JudgeText] =
    db.read(Subject.Turn(request.turn)) {
      for {
        all <- entries.list(request.turn.conversationId)
        own = all.filter(_.turnSeq == request.turn.turnSeq)
        speakers <- principals.speakers(own.map(_.id))
      } yield JudgeText(JudgeText.text(Shown.turn(own, speakers), chars), Vector.empty)
    }.left
      .map(AssemblyError.Store(_))

  /** A boundary within [[LinearAssembler.share]] of `budget`, or the budget `request.width`
    * names; the drawn notes, or the query's, on it.
    */
  def assemble(request: AssemblyRequest, packing: Packing)(using
      db: Db^
  ): Either[AssemblyError, Projection] = {
    val budget = request.width match {
      case Width.Deployed => LinearAssembler.share(this.budget)
      case Width.Within(b, _) => b
    }
    val notes = packing match {
      case Packing.Drawn(drawn) => drawn.notes
      case Packing.Searched(written, _) => Vector(written.queried)
    }
    db.read(Subject.Turn(request.turn)) {
      for {
        opening <- periods.opening(request.turn)
        all <- entries.list(request.turn.conversationId)
        speakers <- principals.speakers(all.map(_.id))
      } yield {
        val turns = LinearAssembler.turnsBefore(all, opening.first, request.turn.turnSeq)
        LinearAssembler.carrying(
          request.carried,
          opening.first,
          all,
          turns,
          speakers,
          estimator,
          this.budget
        ) match {
          case Some(LinearAssembler.Carrying(since, _)) =>
            Projection.Appended(
              Window(since.flatten.sortBy(_.seq).map(_.seq), notes),
              LinearAssembler.cost(since.flatten, speakers, estimator)
            )
          case None =>
            val (closings, left) =
              LinearAssembler.opened(opening.closing.map(_.entry).toVector, estimator, budget)
            val kept = LinearAssembler.tail(turns, speakers, estimator, left)
            val gapped =
              if (kept.size != turns.size) LinearAssembler.gap(estimator) else Tokens.Zero
            Projection.Boundary(
              Window(closings.map(_.seq) ++ kept.flatten.sortBy(_.seq).map(_.seq), notes),
              Tokens(Tokens.value(budget) - Tokens.value(left)) +
                LinearAssembler.cost(kept.flatten, speakers, estimator) + gapped
            )
        }
      }
    }.left
      .map(AssemblyError.Store(_))
  }
}

object LinearAssembler {

  /** The window's default size: a small fraction of a current model's context, so a turn
    * stays cheap.
    */
  val DefaultBudget: Tokens = Tokens(24_000)

  /** What a boundary drawn at the deployed width fills: half of `limit`, so the turns after it
    * grow the window to the limit before the next boundary (ADR 0052).
    */
  def share(limit: Tokens): Tokens = Tokens(Tokens.value(limit) / 2)

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

  /** The own turns of `turns` (the period's, before the turn, oldest first) that a window
    * carrying `carried` appends, every one after its last part's turn, and what the carried
    * window spends with them: its parts' costs, their turns' own entries of `all` as sent, and
    * those turns as shown, each with `speakers`' names, by `estimator`. `None` at the limit:
    * nothing is carried, the turn's period began at `first`, after a carried part's turn (a
    * close since), or they spend more than `limit`.
    */
  def carrying(
      carried: CarriedWindow,
      first: TurnSeq,
      all: Vector[Entry],
      turns: Vector[Vector[Entry]],
      speakers: Speakers,
      estimator: TokenEstimator,
      limit: Tokens
  ): Option[Carrying] =
    carried.parts.lastOption
      .filterNot(_ => carried.parts.exists(p => TurnSeq.value(p.turn) < TurnSeq.value(first)))
      .flatMap { last =>
        val since =
          turns.filter(
            _.headOption.exists(e => TurnSeq.value(e.turnSeq) > TurnSeq.value(last.turn))
          )
        val spent = carried.parts
          .map(p => p.cost + sent(all.filter(_.turnSeq == p.turn), speakers, estimator))
          .foldLeft(Tokens.Zero)(_ + _) + cost(since.flatten, speakers, estimator)
        Option.when(Tokens.value(spent) <= Tokens.value(limit))(Carrying(since, spent))
      }

  /** What a window carrying a non-empty window appends: the own turns `since` its last part's
    * turn, oldest first, and what the carried window `spent` with them.
    */
  final case class Carrying(since: Vector[Vector[Entry]], spent: Tokens)

  /** What one gap line ([[Shown.Gap]]) costs by `estimator`. */
  def gap(estimator: TokenEstimator): Tokens = estimator.message(Shown.Gap)

  /** What the model is shown of `entries`, with `speakers`' names, costs by `estimator`
    * ([[Shown.of]]).
    */
  def cost(entries: Vector[Entry], speakers: Speakers, estimator: TokenEstimator): Tokens =
    entries.map(shownCost(_, speakers, estimator)).foldLeft(Tokens.Zero)(_ + _)

  /** What the model is sent of a turn's own `entries` ([[Shown.turn]]), with `speakers`'
    * names, costs by `estimator`: its tool calls and results as well as its messages.
    */
  def sent(entries: Vector[Entry], speakers: Speakers, estimator: TokenEstimator): Tokens =
    Shown.turn(entries, speakers).map(estimator.message).foldLeft(Tokens.Zero)(_ + _)

  private def shownCost(e: Entry, speakers: Speakers, estimator: TokenEstimator): Tokens =
    Shown.of(e, speakers).fold(Tokens.Zero)(estimator.message)

  private def isMessage(e: Entry): Boolean = e.payload match {
    case Payload.Message(_) | Payload.Heard(_) | Payload.Posted(_) => true
    // A turn's tool exchange is its own: a window never holds a call apart from its turn.
    case Payload.Summary(_) | Payload.Query(_) | (_: Payload.Window) | Payload.Prefix(_, _) |
        Payload.Agenda(_) | Payload.Exchange(_) | Payload.Result(_, _) | Payload.Declined(_, _) |
        Payload.Attempt(_) | Payload.Ask(_, _, _) | Payload.Closed(_, _, _) | Payload.Draft(_) =>
      false
  }
}
