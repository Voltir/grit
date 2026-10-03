package grit.core.recipe

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.FiniteDuration

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Scope
import grit.core.stitch.{Placed, Said, StitchStore, Stitching, Strand}
import grit.core.store.{Conversation, Entry, Principals, StoreError, Tx}

/** What a call's input shows beyond its own thread: what `sources` find, in `sources` order,
  * as much as fits in `budget` characters.
  */
final case class Pool(sources: Vector[Source], budget: Int)

object Pool {

  /** No sources: it shows nothing. */
  val empty: Pool = Pool(Vector.empty, 0)

  /** The most of a message's words, or of a record's headline, a pool's line shows. */
  val LineChars = 160

  /** What `pool` shows for `heard`, said in `conversation` at t (its `createdAt`), from what
    * existed then in its room ([[grit.core.store.Origin.room]]), never from a conversation its
    * thread shows: its own, or one of `strand`'s ([[Strand.Read.conversations]]). Nothing,
    * reading nothing, when `scope` does not hold the room itself ([[Scope.holds]]; never under
    * [[Scope.Off]], nor under places narrower than the room). Each section that kept
    * anything, under its key, in the order its first source is listed.
    *
    *   - [[Source.Channel]]: messages anyone said there in [t − `within`, t), the latest `most`;
    *     [[Source.Author]]: those `heard`'s author wrote, none when it has none.
    *   - [[Source.Exchanges]]: the exchanges stitching offered `conversation`'s first message,
    *     as its kept placement shows them ([[grit.core.stitch.Seen.exchanges]]), in the order
    *     offered: each whose opening message, said before t, is still kept; one of `strand`'s
    *     shows its record alone. None when no placement is kept. What was offered depends on
    *     the strand links that stood when it was placed.
    *   - Kept in `sources` order, a message source's latest first: a message's line, or an
    *     exchange's lines together, is kept while every kept one's text, and one newline each,
    *     fits in `budget`; one that does not fit is skipped and the next tried.
    *   - An entry several sources find is kept once, under the first; an exchange whose opening
    *     an earlier source kept is kept as its record line alone, `Record of the exchange
    *     {speaker} opened {age} before: {headline}`, and not at all when it has no record.
    *   - Within a section, messages oldest first, each `{speaker}, {age} before: {words}`
    *     (`{speaker}` "Assistant" for grit's, the name `principals` enrolled, else "Someone";
    *     `{age}` before t in its largest whole unit: "1 minute", "3 hours"); exchanges as
    *     offered, each `Exchange opened {age} before by {speaker}: {words}`, then `  Record:
    *     {headline}` when it has one; one of `strand`'s only `Record of the exchange this
    *     thread continues: {headline}`, nothing when it has none. Words and headline at most
    *     [[LineChars]].
    *
    * A pool without sources reads nothing. Why not, when the store cannot be read.
    */
  def read(
      pool: Pool,
      scope: Scope,
      rooms: RoomReads,
      stitches: StitchStore,
      principals: Principals,
      conversation: Conversation,
      strand: Strand.Read,
      heard: Entry
  )(using Tx^): Either[StoreError, VectorMap[Section, String]] = {
    val room = conversation.origin.room
    if (scope.holds(room, room))
      shown(pool, rooms, stitches, principals, conversation, strand, heard)
    else Right(VectorMap.empty)
  }

  /** [[read]], in scope. */
  private def shown(
      pool: Pool,
      rooms: RoomReads,
      stitches: StitchStore,
      principals: Principals,
      conversation: Conversation,
      strand: Strand.Read,
      heard: Entry
  )(using Tx^): Either[StoreError, VectorMap[Section, String]] = {
    val at = new AsOf(rooms, stitches, conversation, strand, heard)
    val channel = pool.sources.collect { case Source.Channel(within, most) => (within, most) }
    val author = pool.sources.collect { case Source.Author(within, most) => (within, most) }
    val exchanges = pool.sources.contains(Source.Exchanges)
    // One read per kind of source, as wide as its widest: each window ends at t, so the latest
    // `most` of a narrower one are among the latest of the widest.
    // None when there is no such source, so nothing is read for it.
    def widest(of: Vector[(FiniteDuration, Int)]): Option[(FiniteDuration, Int)] =
      of.reduceOption((a, b) => (a._1.max(b._1), math.max(a._2, b._2)))
    val none: Either[StoreError, Vector[Said]] = Right(Vector.empty)
    for {
      said <- widest(channel).fold(none)(at.said)
      by <- widest(author).fold(none)(at.saidByAuthor)
      offered <- if (exchanges) at.offered() else Right(Vector.empty)
      names <- {
        val ids = (said ++ by).map(_.entry.id).distinct
        if (ids.isEmpty) Right(grit.core.store.Speakers.none) else principals.speakers(ids)
      }
    } yield {
      def spoken(s: Said) =
        Stitching
          .spoken(s)
          .map(Spoken(s.entry.id, Stitching.speaker(s, names), s.entry.createdAt, _))
      show(pool, Candidates(said.flatMap(spoken), by.flatMap(spoken), offered), at.t)
    }
  }

  /** The reads a pool makes for `heard`, said in `conversation` at t: each bounded before t, and
    * never of a conversation its thread shows. Made only by [[read]], from the heard entry
    * itself, so no source can name another time.
    */
  private final class AsOf(
      rooms: RoomReads,
      stitches: StitchStore,
      conversation: Conversation,
      strand: Strand.Read,
      heard: Entry
  ) {
    val t: Instant = heard.createdAt
    private val room = conversation.origin.room
    private val outside: Set[ConversationId] = strand.conversations + conversation.id

    /** What anyone said in [t − `within`, t), the latest `most`. */
    def said(window: (FiniteDuration, Int))(using Tx^): Either[StoreError, Vector[Said]] =
      rooms.said(room, from(window._1), t, outside, window._2)

    /** What `heard`'s author said in [t − `within`, t), the latest `most`. */
    def saidByAuthor(window: (FiniteDuration, Int))(using Tx^): Either[StoreError, Vector[Said]] =
      rooms
        .author(heard.id)
        .flatMap(
          _.fold[Either[StoreError, Vector[Said]]](Right(Vector.empty))(
            rooms.saidBy(room, _, from(window._1), t, outside, window._2)
          )
        )

    /** The exchanges `conversation`'s first message was offered, as its kept placement shows
      * them, each with its opening message said before t; none when no placement is kept.
      */
    def offered()(using Tx^): Either[StoreError, Vector[Offered]] =
      for {
        // The one read with no written-at bound: the placement is this conversation's own
        // stitch, written by its first message's triage before any of its messages is asked
        // about, and what it shows was read by stitching bounded at that message's time (t
        // itself when `heard` is that message, earlier for a reply). Its content is not fixed
        // by that time alone: the strand links stitching read then were read unbounded, so
        // which exchanges were offered depends on what other placements had been kept by
        // then, which is scheduling.
        first <- stitches.openings(Vector(conversation.id))
        placed <- first.headOption.fold[Either[StoreError, Option[Placed]]](Right(None))(f =>
          stitches.placed(f.entry.id)
        )
        exchanges = placed.fold(Vector.empty)(_.seen.exchanges)
        openings <- stitches.openings(exchanges.map(_.offer.root).distinct)
      } yield exchanges.flatMap { e =>
        openings
          .find(o => o.conversation == e.offer.root && o.entry.createdAt.isBefore(t))
          .map(o =>
            Offered(
              Spoken(o.entry.id, e.opening.from, o.entry.createdAt, e.opening.text),
              e.record,
              strand.conversations.contains(e.offer.root)
            )
          )
      }

    private def from(within: FiniteDuration): Instant = t.minusNanos(within.toNanos)
  }

  /** One thing a pool may keep: its section, the entry it shows (an exchange followed shows
    * none), its text, where it falls in its section, and what is kept `instead` when an
    * earlier source kept its entry.
    */
  private final case class Piece(
      section: Section,
      entry: Option[EntryId],
      text: String,
      rank: Rank,
      instead: Option[Piece] = None
  )

  /** A section's order: messages by when they were said, exchanges as offered. */
  private type Rank = (Instant, String, Int)

  /** What `pool` shows from `candidates`, already read, for a message said at `t`, as [[read]]
    * says: a message source keeps those said in [t − `within`, t), the latest `most`;
    * [[Source.Exchanges]] the offered, in order.
    */
  private[recipe] def show(
      pool: Pool,
      candidates: Candidates,
      t: Instant
  ): VectorMap[Section, String] = {
    def ago(at: Instant) = Stitching.ago(at, t)
    def messages(from: Vector[Spoken], within: FiniteDuration, most: Int) =
      from
        .filter(s => !s.at.isBefore(t.minusNanos(within.toNanos)) && s.at.isBefore(t))
        .sortBy(s => (s.at, EntryId.value(s.entry)))
        .reverse
        .take(math.max(0, most))
        .map(s =>
          Piece(
            Section.Nearby,
            Some(s.entry),
            s"${s.speaker}, ${ago(s.at)} before: ${s.text.take(LineChars)}",
            (s.at, EntryId.value(s.entry), 0)
          )
        )
    def exchanges = candidates.offered.zipWithIndex.flatMap { (o, i) =>
      val rank = (Instant.EPOCH, "", i)
      if (o.followed)
        o.record.map(h =>
          Piece(
            Section.Exchanges,
            None,
            s"Record of the exchange this thread continues: ${h.take(LineChars)}",
            rank
          )
        )
      else {
        val age = ago(o.opening.at)
        val opened =
          s"Exchange opened $age before by ${o.opening.speaker}: " + o.opening.text.take(LineChars)
        val record = o.record.fold("")(h => s"\n  Record: ${h.take(LineChars)}")
        val recordAlone = o.record.map(h =>
          Piece(
            Section.Exchanges,
            None,
            s"Record of the exchange ${o.opening.speaker} opened $age before: ${h.take(LineChars)}",
            rank
          )
        )
        Some(Piece(Section.Exchanges, Some(o.opening.entry), opened + record, rank, recordAlone))
      }
    }
    val found = pool.sources.flatMap {
      case Source.Channel(within, most) => messages(candidates.said, within, most)
      case Source.Author(within, most) => messages(candidates.byAuthor, within, most)
      case Source.Exchanges => exchanges
    }
    val (kept, _, _) = found.foldLeft((Vector.empty[Piece], Set.empty[EntryId], 0)) {
      case ((kept, seen, used), found) =>
        val piece = if (found.entry.exists(seen)) found.instead else Some(found)
        piece.filter(u => used + u.text.length + 1 <= pool.budget) match {
          case Some(u) => (kept :+ u, seen ++ u.entry, used + u.text.length + 1)
          case None => (kept, seen, used)
        }
    }
    VectorMap.from(pool.sources.map(_.section).distinct.flatMap { section =>
      val lines = kept.filter(_.section == section).sortBy(_.rank).map(_.text)
      Option.when(lines.nonEmpty)(section -> lines.mkString("\n"))
    })
  }
}
