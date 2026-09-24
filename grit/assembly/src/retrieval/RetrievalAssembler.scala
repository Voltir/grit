package grit.assembly.retrieval

import grit.assembly.linear.LinearAssembler
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.{Provider, ProviderError, TokenEstimator}
import grit.core.store.{Db, Entry, EntrySearch, EntryStore}

/** A window of the recent turns plus the earlier turns that match a written query. The
  * most recent whole turns that fit in `tail` come first in the budget; `writer` then
  * writes a search query ([[QueryWriter]]), `search` ranks the earlier entries against it
  * (its `hits` best), and the turns those entries belong to fill the rest of `budget`,
  * best first, each whole or not at all. The window holds them in conversation order.
  *
  * No query is written when the linear window of `budget` already holds every earlier
  * turn. When the writer fails or writes nothing, the window is that linear one, with a
  * note saying why.
  */
final class RetrievalAssembler(
    entries: EntryStore,
    search: EntrySearch,
    writer: Provider^,
    estimator: TokenEstimator,
    budget: Tokens,
    tail: Tokens,
    hits: Int = RetrievalAssembler.DefaultHits
) extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] = {
    val turn = request.turn
    db.read(entries.list(turn.conversationId)).left.map(AssemblyError.Store(_)).flatMap { all =>
      val turns = LinearAssembler.turnsBefore(all, turn.turnSeq)
      val linear = LinearAssembler.recent(turns, estimator, budget)
      if (linear.size == turns.size) Right(window(linear, Vector.empty))
      else {
        val recent = LinearAssembler.recent(turns, estimator, tail)
        val own = all.filter(_.turnSeq == turn.turnSeq)
        val asked = QueryWriter.request(own)
        writer.complete(asked) match {
          case Left(ProviderError.Unavailable(cause)) =>
            Right(window(linear, Vector(AssemblyNote.FellBack(s"no query: $cause"))))
          case Right(reply) =>
            val query = QueryWriter.text(reply)
            val queried =
              AssemblyNote.Queried(query, reply.model, reply.usage, estimator.request(asked))
            if (query.isEmpty)
              Right(window(linear, Vector(queried, AssemblyNote.FellBack("the query was blank"))))
            else {
              val from = recent.headOption.flatMap(_.headOption).fold(turn.turnSeq)(_.turnSeq)
              db.read(search.search(turn.conversationId, from, query, hits))
                .left
                .map(AssemblyError.Store(_))
                .map { found =>
                  val older = turns.filter(t => t.headOption.exists(e => before(e.turnSeq, from)))
                  val recalled = pack(found, older, spent(recent))
                  val seqs = recalled.flatMap(_.headOption.map(_.turnSeq)).sortBy(TurnSeq.value)
                  window(recent ++ recalled, Vector(queried, AssemblyNote.Recalled(seqs)))
                }
            }
        }
      }
    }
  }

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

  private def window(turns: Vector[Vector[Entry]], notes: Vector[AssemblyNote]): Window =
    Window(turns.flatten.sortBy(_.seq).map(_.id), notes)
}

object RetrievalAssembler {

  /** How many ranked entries a search returns. */
  val DefaultHits = 30

  /** The recent tail's default share of the window. */
  val DefaultTail: Tokens = Tokens(8_000)
}
