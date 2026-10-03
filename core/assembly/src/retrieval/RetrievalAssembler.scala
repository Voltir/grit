package grit.assembly.retrieval

import grit.assembly.linear.LinearAssembler
import grit.core.context.{
  AssemblyError,
  AssemblyNote,
  AssemblyRequest,
  ContextAssembler,
  Shown,
  Width,
  Window
}
import grit.core.id.{CallSlot, ConversationId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.place.{Locality, Place, Weight}
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.stitch.{Along, Said, StitchStore, Strand, Tuning}
import grit.core.store.{
  Audience,
  ClosedElsewhere,
  ClosingEntry,
  Conversation,
  ConversationStore,
  Db,
  Entry,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  Nearby,
  OpenPeriod,
  Payload,
  PeriodStore,
  Principals,
  Speakers,
  StoreError
}

/** A window of the closing entry that opens the turn's period, the recent turns, and what a
  * written query finds: the period's earlier turns and, when the settings' scope holds other
  * conversations' places (the conversation's room read from its origin,
  * [[grit.core.store.Origin.room]]), the turns of their open periods and their kept
  * closings. The
  * closing is paid first and the recent tail next, as in [[LinearAssembler]] (the tail is
  * `tail` tokens, or what is left when less). `writer` then writes one query
  * ([[QueryWriter]]), and `search` ranks each kind of candidate against it, `hits` of each.
  * They rank in one pool, the turn's own scores multiplied by the settings' weight (a tie
  * goes to the turn's own), and fill the rest of `budget`, best first, one that does not
  * fit passed over for the next.
  * Own turns join the window in conversation order, each recalled one charged a gap line
  * ([[Shown.Gap]]) besides its own cost, and a tail that leaves turns out one more. Turns from elsewhere form one section
  * per conversation ([[Nearby]], shown as [[Shown.section]] and costed so), in the order of
  * their best candidate, each section's turns in its conversation's order. A conversation
  * elsewhere whose closings match is one candidate, shown as its newest kept closing's record
  * ([[Shown.recorded]]), unless any of its open turns is a candidate: then those alone are
  * its section. The conversation's own closing is paid first, never a candidate. A person's
  * message is costed with its author's name line ([[Shown.of]]), as it is sent.
  *
  * A conversation in a strand ([[grit.core.stitch.Strand]], read through `stitches` in the
  * scope in force, from `tuning.horizon` before its first message until before the turn) is
  * shown the strand as one [[Nearby.Along]] section per other member, after any sections from
  * afar: its root's opening message, then the messages nearest the turn, newest first while
  * they fit, at most `tuning.windowTokens` together, paid after the closing and before the
  * tail. A member whose first message is gone is shown by its newest kept closing's record.
  * Strand members are never also candidates.
  *
  * A conversation that begins with grit's post
  * ([[grit.core.store.ConversationStore.postedBy]]) is shown the turn that asked for it as
  * one [[Nearby.Asked]] section, first, while the turn's period is the conversation's first
  * and both conversations are colleagues' ([[Audience.Colleagues]]), whatever the scope: its
  * person's messages and grit's reply, from the first, as many as fit in the strand's
  * allowance, which it is paid from before the strand. That conversation is then never also
  * a candidate.
  *
  * No query is written when there is nothing to search: the linear window of `budget` holds
  * every earlier turn of the period, and no other conversation's open period or closing is
  * in scope. When the
  * writer fails or writes nothing, the window is that linear one, with a note saying why,
  * and nothing from elsewhere. `AssemblyError.Store` when a store fails or the turn's
  * conversation is gone.
  */
final class RetrievalAssembler(
    entries: EntryStore,
    conversations: ConversationStore,
    periods: PeriodStore,
    principals: Principals,
    lifecycle: LifecycleStore,
    search: EntrySearch,
    stitches: StitchStore,
    writer: Provider^,
    estimator: TokenEstimator,
    budget: Tokens,
    tail: Tokens,
    tuning: Tuning,
    hits: Int = RetrievalAssembler.DefaultHits
) extends ContextAssembler {
  import RetrievalAssembler.{Candidate, Found}

  /** Within `budget` and `hits`, or those `request.width` names. */
  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] = {
    val turn = request.turn
    val (budget, hits) = request.width match {
      case Width.Deployed => (this.budget, this.hits)
      case Width.Within(b, h) => (b, h)
    }
    db.read {
      for {
        settings <- lifecycle.current()
        found <- conversations.get(turn.conversationId)
        conversation <- found.toRight(
          StoreError.Invalid(s"conversation ${ConversationId.value(turn.conversationId)} is gone")
        )
        opening <- periods.opening(turn)
        all <- entries.list(turn.conversationId)
        open <- periods.openElsewhere(turn.conversationId)
        closed <- periods.closedElsewhere(turn.conversationId)
        until = all
          .filter(_.turnSeq == turn.turnSeq)
          .map(_.createdAt)
          .minOption
          .orElse(all.map(_.createdAt).maxOption.map(_.plusSeconds(1)))
          .getOrElse(java.time.Instant.EPOCH)
        began = all.minByOption(_.seq).fold(until)(_.createdAt)
        strand <- Along.read(
          stitches,
          conversation,
          settings.locality.scope,
          began.minusNanos(tuning.horizon.toNanos),
          until
        )
        posted <- conversations.postedBy(turn.conversationId)
        asked <- askedOf(conversation, posted, opening.closing.isEmpty)
        speakers <- principals.speakers(
          (all ++ strand.shown ++ asked.toVector.flatMap(_._3)).map(_.id)
        )
      } yield {
        val locality = settings.locality
        val members = strand.members.toSet ++ strand.gone ++ asked.map(_._1)
        val held = closed.filter(c => locality.scope.holds(conversation.origin.room, c.place))
        Read(
          locality,
          opening.first,
          opening.closing.map(_.entry).toVector,
          all,
          speakers,
          open.filter(o =>
            locality.scope.holds(conversation.origin.room, o.place) && !members(o.conversation)
          ),
          held.filterNot(c => members(c.conversation)),
          strand,
          held.filter(c => strand.gone.contains(c.conversation)),
          asked
        )
      }
    }.left
      .map(AssemblyError.Store(_))
      .flatMap { read =>
        val (closings, afterClosing) = LinearAssembler.opened(read.closings, estimator, budget)
        val allowance = Tokens(
          math.min(Tokens.value(tuning.windowTokens), Tokens.value(afterClosing))
        )
        val (askedShown, askedCost) = askedSection(read, allowance)
        val along = db
          .read(strandSections(read, Tokens(Tokens.value(allowance) - Tokens.value(askedCost))))
          .left
          .map(AssemblyError.Store(_))
        along.flatMap { (strandShown, strandCost) =>
          val left = Tokens(
            Tokens.value(afterClosing) - Tokens.value(askedCost) - Tokens.value(strandCost)
          )
          def window(
              closings: Vector[Entry],
              turns: Vector[Vector[Entry]],
              notes: Vector[AssemblyNote],
              nearby: Vector[Nearby] = Vector.empty
          ): Window = this.window(closings, turns, notes, askedShown ++ nearby ++ strandShown)
          val all = read.all
          val turns = LinearAssembler.turnsBefore(all, read.first, turn.turnSeq)
          val linear = LinearAssembler.tail(turns, read.speakers, estimator, left)
          val ownToFind = linear.size != turns.size
          if (!ownToFind && read.open.isEmpty && read.closed.isEmpty)
            Right(window(closings, linear, Vector.empty))
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
                  window(
                    closings,
                    linear,
                    Vector(AssemblyNote.FellBack(s"no query: ${error.cause}"))
                  )
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
                  db.read(find(turn, read, ownToFind, from, query, hits))
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
                        pack(
                          rank(found, older, places, read.closed, read.locality.weight),
                          used,
                          budget,
                          read.speakers
                        )
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
      open: Vector[OpenPeriod],
      closed: Vector[ClosedElsewhere],
      strand: Strand.Read,
      goneRecords: Vector[ClosedElsewhere],
      asked: Option[(ConversationId, Place, Vector[Entry])]
  )

  /** The turn that asked for the post `conversation` begins with, made by the call `posted`
    * names: its conversation, place, and the messages of that turn with text (a person's, and
    * grit's reply). `None` unless `first` (the turn's period is the conversation's first),
    * both conversations are colleagues' ([[Audience.Colleagues]]), and the asker is kept.
    */
  private def askedOf(
      conversation: Conversation,
      posted: Option[CallSlot],
      first: Boolean
  )(using
      grit.core.store.Tx^
  ): Either[StoreError, Option[(ConversationId, Place, Vector[Entry])]] =
    posted match {
      case Some(slot) if first && conversation.origin.audience == Audience.Colleagues =>
        val by = slot.turn.conversationId
        conversations.get(by).flatMap {
          case Some(asker) if asker.origin.audience == Audience.Colleagues =>
            entries.list(by).map { all =>
              val said = all.filter(e => e.turnSeq == slot.turn.turnSeq && spoken(e))
              Option.when(said.nonEmpty)((by, asker.origin.place, said))
            }
          case _ => Right(None)
        }
      case _ => Right(None)
    }

  /** Whether `e` is a message with words: a person's, or grit's reply with text. */
  private def spoken(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.User(_)) | Payload.Heard(_) => true
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      blocks.exists {
        case AssistantBlock.Text(t) => t.trim.nonEmpty
        case _ => false
      }
    case _ => false
  }

  /** The asked section as the window shows it, and what it costs: as many of its messages,
    * from the first, as fit in `allowance`; none when none does.
    */
  private def askedSection(read: Read, allowance: Tokens): (Vector[Nearby], Tokens) =
    read.asked match {
      case None => (Vector.empty, Tokens.Zero)
      case Some((c, place, said: Vector[Entry])) =>
        def cost(k: Int): Tokens =
          Shown.asked(place, said.take(k), read.speakers).fold(Tokens.Zero)(estimator.message)
        (said.size to 1 by -1).find(k => Tokens.value(cost(k)) <= Tokens.value(allowance)) match {
          case Some(k) => (Vector(Nearby.Asked(c, place, said.take(k).map(_.seq))), cost(k))
          case None => (Vector.empty, Tokens.Zero)
        }
    }

  /** The strand's sections as the window shows them, and what they cost: a gone member's
    * newest kept closing, then the root's opening and the messages nearest the turn, newest
    * first while they fit, within `cap`, one section per member in the order they were said.
    */
  private def strandSections(read: Read, cap: Tokens)(using
      grit.core.store.Tx^
  ): Either[StoreError, (Vector[Nearby], Tokens)] = {
    read.goneRecords
      .foldLeft[Either[StoreError, Vector[(ConversationId, Place, ClosingEntry)]]](
        Right(Vector.empty)
      ) { (acc, c) =>
        acc.flatMap(done =>
          entries
            .get(c.newest)
            .map(e => done ++ e.flatMap(ClosingEntry.of).map((c.conversation, c.place, _)))
        )
      }
      .map { records =>
        val recordCosts = records.map((_, place, r) => estimator.message(Shown.recorded(place, r)))
        val recordsKept = records
          .zip(recordCosts)
          .scanLeft(Tokens.Zero)((u, rc) => u + rc._2)
          .drop(1)
          .zip(records)
          .takeWhile((u, _) => Tokens.value(u) <= Tokens.value(cap))
          .map(_._2)
        val recordsCost = recordCosts.take(recordsKept.size).foldLeft(Tokens.Zero)(_ + _)
        val room = Tokens(Tokens.value(cap) - Tokens.value(recordsCost))
        val opening = read.strand.opening.toVector
        val rest = read.strand.said
          .filterNot(s => opening.exists(_.entry.id == s.entry.id))
          .sortBy(_.entry.createdAt)
        def sections(shown: Vector[Said]): Vector[Nearby] =
          shown
            .sortBy(_.entry.createdAt)
            .map(s => (s.conversation, s.place))
            .distinct
            .map((c, place) =>
              Nearby.Along(
                c,
                place,
                shown.filter(_.conversation == c).sortBy(_.entry.createdAt).map(_.entry.seq)
              )
            )
        def cost(shown: Vector[Said]): Tokens =
          sections(shown)
            .flatMap(n => Shown.section(n, shown.map(_.entry), read.speakers))
            .map(estimator.message)
            .foldLeft(Tokens.Zero)(_ + _)
        val fits = (rest.size to 0 by -1).iterator
          .map(k => opening ++ rest.takeRight(k))
          .find(shown => Tokens.value(cost(shown)) <= Tokens.value(room))
          .getOrElse(Vector.empty)
        (
          recordsKept.map((c, place, r) => Nearby.Closed(c, place, r.entry.seq)) ++ sections(fits),
          recordsCost + cost(fits)
        )
      }
  }

  /** Both searches for `query`, `hits` each, in one read: the period's own earlier turns
    * before `from` when `own`, and the open periods elsewhere; with the turns the nearby hits
    * point into.
    */
  private def find(
      turn: TurnRef,
      read: Read,
      own: Boolean,
      from: TurnSeq,
      query: String,
      hits: Int
  )(using
      grit.core.store.Tx^
  ): Either[StoreError, Found] =
    for {
      mine <-
        if (own) search.search(turn.conversationId, read.first, from, query, hits)
        else Right(Vector.empty)
      near <-
        if (read.open.isEmpty) Right(Vector.empty)
        else search.nearby(read.open, query, hits)
      records <-
        if (read.closed.isEmpty) Right(Vector.empty)
        else search.closings(read.closed.map(_.conversation), query, hits)
      // Each conversation a closing matched is shown by its newest kept closing.
      newest <- read.closed
        .filter(c => records.exists(_.turn.conversationId == c.conversation))
        .foldLeft[Either[StoreError, Map[ConversationId, ClosingEntry]]](Right(Map.empty)) {
          (acc, c) =>
            acc.flatMap(done =>
              entries
                .get(c.newest)
                .map(e => done ++ e.flatMap(ClosingEntry.of).map(c.conversation -> _))
            )
        }
      theirs <- near
        .map(_.turn.conversationId)
        .distinct
        .foldLeft[Either[StoreError, Map[ConversationId, Vector[Entry]]]](Right(Map.empty)) {
          (acc, c) => acc.flatMap(done => entries.list(c).map(es => done + (c -> es)))
        }
    } yield Found(mine, near, theirs, records, newest)

  /** The candidate turns, best first: each hit scores its turn, own ones × `weight`; a turn
    * scores its best hit, and a tie keeps the order the hits came in, own ones first.
    */
  private def rank(
      found: Found,
      older: Vector[Vector[Entry]],
      places: Map[ConversationId, Place],
      closed: Vector[ClosedElsewhere],
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
    val closedAt = closed.map(c => c.conversation -> c.place).toMap
    val records: Vector[(Double, Candidate.Record)] = found.records.flatMap { h =>
      val c = h.turn.conversationId
      for {
        place <- closedAt.get(c)
        record <- found.newest.get(c)
      } yield (h.score, Candidate.Record(c, place, record))
    }
    // One section per conversation: its open turns, when any is a candidate, whose period
    // is newer than any closing it has.
    val openHere = near.map(_._2).collect { case Candidate.Near(c, _, _) => c }.toSet
    (own ++ near ++ records.filterNot(r => openHere(r._2.conversation))).zipWithIndex
      .sortBy { case ((score, _), i) => (-score, i) }
      .map(_._1._2)
      .distinctBy(_.key)
  }

  /** The candidates that fit in what `budget` has left after `used`, best first; one that
    * does not fit is passed over for the next. A turn from elsewhere costs what it adds to
    * its section, its label included with its first turn.
    */
  private def pack(
      ranked: Vector[Candidate],
      used: Tokens,
      budget: Tokens,
      speakers: Speakers
  ): Vector[Candidate] =
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
          case Candidate.Record(_, place, record) =>
            estimator.message(Shown.recorded(place, record))
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
    packed
      .collect {
        case n: Candidate.Near => n.conversation
        case r: Candidate.Record => r.conversation
      }
      .distinct
      .flatMap { c =>
        near
          .find(_.conversation == c)
          .map { first =>
            Nearby.Open(
              c,
              first.place,
              near.filter(_.conversation == c).flatMap(_.turn).sortBy(_.seq).map(_.seq)
            )
          }
          .toVector ++ packed.collect {
          case Candidate.Record(k, place, record) if k == c =>
            Nearby.Closed(c, place, record.entry.seq)
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
      nearby: Vector[Nearby]
  ): Window =
    Window(closings.map(_.seq) ++ turns.flatten.sortBy(_.seq).map(_.seq), notes, nearby)
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
      theirs: Map[ConversationId, Vector[Entry]],
      records: Vector[EntrySearch.Hit],
      newest: Map[ConversationId, ClosingEntry]
  )

  /** A turn the pool ranks: one of the conversation's own, or one from elsewhere. */
  private enum Candidate {
    case Own(turn: Vector[Entry])
    case Near(conversation: ConversationId, place: Place, turn: Vector[Entry])
    case Record(conversation: ConversationId, place: Place, record: ClosingEntry)

    /** Which turn it is, so a turn found by several hits is ranked once. */
    def key: (String, Long) = this match {
      case Own(t) => ("", t.headOption.fold(-1L)(e => TurnSeq.value(e.turnSeq)))
      case Near(c, _, t) =>
        (ConversationId.value(c), t.headOption.fold(-1L)(e => TurnSeq.value(e.turnSeq)))
      // One record per conversation, whichever of its closings matched.
      case Record(c, _, _) => (s"record:${ConversationId.value(c)}", -1L)
    }
  }
}
