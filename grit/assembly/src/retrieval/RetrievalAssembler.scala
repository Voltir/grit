package grit.assembly.retrieval

import grit.assembly.linear.LinearAssembler
import grit.core.context.{
  AssemblyError,
  AssemblyNote,
  AssemblyRequest,
  ContextAssembler,
  Shown,
  Window
}
import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.message.Tokens
import grit.core.place.{Locality, Place, Weight}
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.store.{
  Db,
  Entry,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  Nearby,
  OpenPeriod,
  PeriodStore,
  Principals,
  Speakers,
  StoreError
}

/** A window of the closing entry that opens the turn's period, the recent turns, and what a
  * written query finds: the period's earlier turns and, when the settings' scope holds other
  * conversations' places, the turns of their open periods. The closing is paid first and the
  * recent tail next, as in [[LinearAssembler]] (the tail is `tail` tokens, or what is left
  * when less). `writer` then writes one query ([[QueryWriter]]), and `search` ranks both
  * kinds of candidate against it, `hits` of each. The two rank in one pool, the turn's own
  * scores multiplied by the settings' weight (a tie goes to the turn's own), and whole turns
  * fill the rest of `budget`, best first, a turn that does not fit passed over for the next.
  * Own turns join the window in conversation order, each recalled one charged a gap line
  * ([[Shown.Gap]]) besides its own cost, and a tail that leaves turns out one more. Turns from elsewhere form one section
  * per conversation ([[Nearby]], shown as [[Shown.nearby]] and costed so), in the order of
  * their best turn, each section's turns in its conversation's order. No closing entry is a
  * candidate, here or elsewhere. A person's message is costed with its author's name line
  * ([[Shown.of]]), as it is sent.
  *
  * No query is written when there is nothing to search: the linear window of `budget` holds
  * every earlier turn of the period, and no open period elsewhere is in scope. When the
  * writer fails or writes nothing, the window is that linear one, with a note saying why,
  * and nothing from elsewhere.
  */
final class RetrievalAssembler(
    entries: EntryStore,
    periods: PeriodStore,
    principals: Principals,
    lifecycle: LifecycleStore,
    search: EntrySearch,
    writer: Provider^,
    estimator: TokenEstimator,
    budget: Tokens,
    tail: Tokens,
    hits: Int = RetrievalAssembler.DefaultHits
) extends ContextAssembler {
  import RetrievalAssembler.{Candidate, Found}

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] = {
    val turn = request.turn
    db.read {
      for {
        settings <- lifecycle.current()
        opening <- periods.opening(turn)
        all <- entries.list(turn.conversationId)
        open <- periods.openElsewhere(turn.conversationId)
        speakers <- principals.speakers(all.map(_.id))
      } yield {
        val locality = settings.locality
        Read(
          locality,
          opening.first,
          opening.closing.map(_.entry).toVector,
          all,
          speakers,
          open.filter(o => locality.scope.holds(o.place))
        )
      }
    }.left
      .map(AssemblyError.Store(_))
      .flatMap { read =>
        val (closings, left) = LinearAssembler.opened(read.closings, estimator, budget)
        val all = read.all
        val turns = LinearAssembler.turnsBefore(all, read.first, turn.turnSeq)
        val linear = LinearAssembler.tail(turns, read.speakers, estimator, left)
        val ownToFind = linear.size != turns.size
        if (!ownToFind && read.open.isEmpty) Right(window(closings, linear, Vector.empty))
        else {
          val recent =
            if (!ownToFind) linear
            else
              LinearAssembler.recent(
                turns,
                read.speakers,
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
                db.read(find(turn, read, ownToFind, from, query))
                  .left
                  .map(AssemblyError.Store(_))
                  .map { found =>
                    val older =
                      turns.filter(t => t.headOption.exists(e => before(e.turnSeq, from)))
                    // The tail leaves turns out, so the window holds a gap line before it.
                    val used = Tokens(Tokens.value(budget) - Tokens.value(left)) + spent(
                      recent,
                      read.speakers
                    ) +
                      (if (ownToFind) LinearAssembler.gap(estimator) else Tokens.Zero)
                    val places = read.open.map(o => o.conversation -> o.place).toMap
                    val packed =
                      pack(rank(found, older, places, read.locality.weight), used, read.speakers)
                    val recalled = packed.collect { case Candidate.Own(t) => t }
                    val seqs = recalled.flatMap(_.headOption.map(_.turnSeq)).sortBy(TurnSeq.value)
                    val notes =
                      if (ownToFind) Vector(queried, AssemblyNote.Recalled(seqs))
                      else Vector(queried)
                    window(closings, recent ++ recalled, notes, sections(packed))
                  }
              }
          }
        }
      }
  }

  /** What one read of the store gave: the locality in force, the turn's period's first turn,
    * the closings that open it, every entry of the conversation and who of them is named,
    * and the open periods elsewhere its scope holds.
    */
  private final case class Read(
      locality: Locality,
      first: TurnSeq,
      closings: Vector[Entry],
      all: Vector[Entry],
      speakers: Speakers,
      open: Vector[OpenPeriod]
  )

  /** Both searches for `query`, in one read: the period's own earlier turns before `from`
    * when `own`, and the open periods elsewhere; with the turns the nearby hits point into.
    */
  private def find(turn: TurnRef, read: Read, own: Boolean, from: TurnSeq, query: String)(using
      grit.core.store.Tx^
  ): Either[StoreError, Found] =
    for {
      mine <-
        if (own) search.search(turn.conversationId, read.first, from, query, hits)
        else Right(Vector.empty)
      near <-
        if (read.open.isEmpty) Right(Vector.empty)
        else search.nearby(read.open, query, hits)
      theirs <- near
        .map(_.turn.conversationId)
        .distinct
        .foldLeft[Either[StoreError, Map[ConversationId, Vector[Entry]]]](Right(Map.empty)) {
          (acc, c) => acc.flatMap(done => entries.list(c).map(es => done + (c -> es)))
        }
    } yield Found(mine, near, theirs)

  /** The candidate turns, best first: each hit scores its turn, own ones × `weight`; a turn
    * scores its best hit, and a tie keeps the order the hits came in, own ones first.
    */
  private def rank(
      found: Found,
      older: Vector[Vector[Entry]],
      places: Map[ConversationId, Place],
      weight: Weight
  ): Vector[Candidate] = {
    val w = Weight.value(weight)
    val byTurn = older.flatMap(t => t.headOption.map(e => TurnSeq.value(e.turnSeq) -> t)).toMap
    val own: Vector[(Double, Candidate)] = found.own.flatMap { h =>
      byTurn.get(TurnSeq.value(h.turn.turnSeq)).map(t => (h.score * w, Candidate.Own(t)))
    }
    val near: Vector[(Double, Candidate)] = found.near.flatMap { h =>
      val c = h.turn.conversationId
      val t = found.theirs
        .getOrElse(c, Vector.empty)
        .filter(e => e.turnSeq == h.turn.turnSeq)
      val messages = LinearAssembler.turnsBefore(t, h.turn.turnSeq, h.turn.turnSeq.next).flatten
      places
        .get(c)
        .filter(_ => messages.nonEmpty)
        .map(p => (h.score, Candidate.Near(c, p, messages)))
    }
    (own ++ near).zipWithIndex
      .sortBy { case ((score, _), i) => (-score, i) }
      .map(_._1._2)
      .distinctBy(_.key)
  }

  /** The candidates that fit in what `budget` has left after `used`, best first; one that
    * does not fit is passed over for the next. A turn from elsewhere costs what it adds to
    * its section, its label included with its first turn.
    */
  private def pack(ranked: Vector[Candidate], used: Tokens, speakers: Speakers): Vector[Candidate] =
    ranked
      .foldLeft((used, Vector.empty[Candidate])) { case ((spentSoFar, kept), c) =>
        val cost = c match {
          // A recalled turn splits a gap in two at most: one more gap line.
          case Candidate.Own(t) =>
            LinearAssembler.cost(t, speakers, estimator) + LinearAssembler.gap(estimator)
          case Candidate.Near(conversation, place, t) =>
            val before = kept.collect {
              case Candidate.Near(k, _, es) if k == conversation => es
            }.flatten
            val without = shown(place, before)
            Tokens(Tokens.value(shown(place, before ++ t)) - Tokens.value(without))
        }
        val after = spentSoFar + cost
        if (Tokens.value(after) <= Tokens.value(budget)) (after, kept :+ c)
        else (spentSoFar, kept)
      }
      ._2

  private def shown(place: Place, section: Vector[Entry]): Tokens =
    Shown.nearby(place, section.sortBy(_.seq)).fold(Tokens.Zero)(estimator.message)

  /** The packed turns from elsewhere as sections: one per conversation, in the order of its
    * first packed turn, its entries in its conversation's order.
    */
  private def sections(packed: Vector[Candidate]): Vector[Nearby] = {
    val near = packed.collect { case n: Candidate.Near => n }
    near.map(_.conversation).distinct.flatMap { c =>
      near.find(_.conversation == c).map { first =>
        Nearby(
          c,
          first.place,
          near.filter(_.conversation == c).flatMap(_.turn).sortBy(_.seq).map(_.id)
        )
      }
    }
  }

  private def spent(turns: Vector[Vector[Entry]], speakers: Speakers): Tokens =
    turns.map(LinearAssembler.cost(_, speakers, estimator)).foldLeft(Tokens.Zero)(_ + _)

  private def before(a: TurnSeq, b: TurnSeq): Boolean = TurnSeq.value(a) < TurnSeq.value(b)

  private def window(
      closings: Vector[Entry],
      turns: Vector[Vector[Entry]],
      notes: Vector[AssemblyNote],
      nearby: Vector[Nearby] = Vector.empty
  ): Window =
    Window(closings.map(_.id) ++ turns.flatten.sortBy(_.seq).map(_.id), notes, nearby)
}

object RetrievalAssembler {

  /** How many ranked entries each search returns. */
  val DefaultHits = 30

  /** The recent tail's default share of the window. */
  val DefaultTail: Tokens = Tokens(8_000)

  /** What both searches found: the conversation's own hits, the hits elsewhere, and the
    * entries of each conversation a hit elsewhere is in.
    */
  private final case class Found(
      own: Vector[EntrySearch.Hit],
      near: Vector[EntrySearch.Hit],
      theirs: Map[ConversationId, Vector[Entry]]
  )

  /** A turn the pool ranks: one of the conversation's own, or one from elsewhere. */
  private enum Candidate {
    case Own(turn: Vector[Entry])
    case Near(conversation: ConversationId, place: Place, turn: Vector[Entry])

    /** Which turn it is, so a turn found by several hits is ranked once. */
    def key: (String, Long) = this match {
      case Own(t) => ("", t.headOption.fold(-1L)(e => TurnSeq.value(e.turnSeq)))
      case Near(c, _, t) =>
        (ConversationId.value(c), t.headOption.fold(-1L)(e => TurnSeq.value(e.turnSeq)))
    }
  }
}
