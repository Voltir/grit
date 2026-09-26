package grit.assembly.retrieval

import grit.assembly.linear.LinearAssembler
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.store.{Db, Entry, EntrySearch, EntryStore, PeriodStore}

/** A window of the closing entry that opens the turn's period, the recent turns, and the
  * earlier turns of the period that match a written query. The closing entry is paid for
  * first, as [[LinearAssembler]] pays for them; then the most recent whole turns that fit in
  * `tail` (or what is left, when less); `writer` then writes a search query
  * ([[QueryWriter]]), `search` ranks the period's earlier entries against it (its `hits`
  * best), and the turns those entries belong to fill the rest of `budget`, best first, each
  * whole or not at all. The window holds the closing entry, then the turns in
  * conversation order. Search never reaches past the period: what came before it is its
  * closing entry.
  *
  * No query is written when the linear window of `budget` already holds every earlier turn
  * of the period. When the writer fails or writes nothing, the window is that linear one,
  * with a note saying why.
  */
final class RetrievalAssembler(
    entries: EntryStore,
    periods: PeriodStore,
    search: EntrySearch,
    writer: Provider^,
    estimator: TokenEstimator,
    budget: Tokens,
    tail: Tokens,
    hits: Int = RetrievalAssembler.DefaultHits
) extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] = {
    val turn = request.turn
    db.read {
      for {
        opening <- periods.opening(turn)
        all <- entries.list(turn.conversationId)
      } yield Read(opening.first, opening.closing.map(_.entry).toVector, all)
    }.left
      .map(AssemblyError.Store(_))
      .flatMap { read =>
        val (closings, left) = LinearAssembler.opened(read.closings, estimator, budget)
        val all = read.all
        val turns = LinearAssembler.turnsBefore(all, read.first, turn.turnSeq)
        val linear = LinearAssembler.recent(turns, estimator, left)
        if (linear.size == turns.size) Right(window(closings, linear, Vector.empty))
        else {
          val recent = LinearAssembler.recent(
            turns,
            estimator,
            Tokens(Tokens.value(tail) min Tokens.value(left))
          )
          val own = all.filter(_.turnSeq == turn.turnSeq)
          val asked = QueryWriter.request(own)
          writer.complete(asked) match {
            case Left(error) =>
              Right(
                window(closings, linear, Vector(AssemblyNote.FellBack(s"no query: ${error.cause}")))
              )
            case Right(reply) =>
              val query = QueryWriter.text(reply)
              val queried =
                AssemblyNote.Queried(query, reply.model, reply.usage, estimator.request(asked))
              if (query.isEmpty)
                Right(
                  window(
                    closings,
                    linear,
                    Vector(queried, AssemblyNote.FellBack("the query was blank"))
                  )
                )
              else {
                val from = recent.headOption.flatMap(_.headOption).fold(turn.turnSeq)(_.turnSeq)
                db.read(search.search(turn.conversationId, read.first, from, query, hits))
                  .left
                  .map(AssemblyError.Store(_))
                  .map { found =>
                    val older = turns.filter(t => t.headOption.exists(e => before(e.turnSeq, from)))
                    val used = Tokens(Tokens.value(budget) - Tokens.value(left)) + spent(recent)
                    val recalled = pack(found, older, used)
                    val seqs = recalled.flatMap(_.headOption.map(_.turnSeq)).sortBy(TurnSeq.value)
                    window(
                      closings,
                      recent ++ recalled,
                      Vector(queried, AssemblyNote.Recalled(seqs))
                    )
                  }
              }
          }
        }
      }
  }

  /** What one read of the store gave: the turn's period's first turn, the closings that
    * open it, and every entry of the conversation.
    */
  private final case class Read(first: TurnSeq, closings: Vector[Entry], all: Vector[Entry])

  /** The turns `found` points into, best first, that fit in what `budget` has left after
    * `used`; a turn that does not fit is passed over for the next.
    */
  private def pack(
      found: Vector[EntrySearch.Hit],
      older: Vector[Vector[Entry]],
      used: Tokens
  ): Vector[Vector[Entry]] = {
    val byTurn = older.flatMap(t => t.headOption.map(e => TurnSeq.value(e.turnSeq) -> t)).toMap
    val ranked = found.map(h => TurnSeq.value(h.turnSeq)).distinct.flatMap(byTurn.get)
    ranked
      .foldLeft((used, Vector.empty[Vector[Entry]])) { case ((spentSoFar, kept), t) =>
        val after = spentSoFar + LinearAssembler.cost(t, estimator)
        if (Tokens.value(after) <= Tokens.value(budget)) (after, kept :+ t)
        else (spentSoFar, kept)
      }
      ._2
  }

  private def spent(turns: Vector[Vector[Entry]]): Tokens =
    turns.map(LinearAssembler.cost(_, estimator)).foldLeft(Tokens.Zero)(_ + _)

  private def before(a: TurnSeq, b: TurnSeq): Boolean = TurnSeq.value(a) < TurnSeq.value(b)

  private def window(
      closings: Vector[Entry],
      turns: Vector[Vector[Entry]],
      notes: Vector[AssemblyNote]
  ): Window =
    Window(closings.map(_.id) ++ turns.flatten.sortBy(_.seq).map(_.id), notes)
}

object RetrievalAssembler {

  /** How many ranked entries a search returns. */
  val DefaultHits = 30

  /** The recent tail's default share of the window. */
  val DefaultTail: Tokens = Tokens(8_000)
}
