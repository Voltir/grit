package grit.core.recipe

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.FiniteDuration

import grit.core.id.EntryId
import grit.core.stitch.Stitching

/** What a call's input shows beyond its own thread: what `sources` find, in `sources` order,
  * as much as fits in `budget` characters.
  */
final case class Pool(sources: Vector[Source], budget: Int)

object Pool {

  /** No sources: it shows nothing. */
  val empty: Pool = Pool(Vector.empty, 0)

  /** The most of a message's words, or of a record's headline, a pool's line shows. */
  val LineChars = 160

  /** One thing a pool may keep: its section, the entry it shows (an exchange followed shows
    * none), its text, and where it falls in its section.
    */
  private final case class Piece(section: Section, entry: Option[EntryId], text: String, rank: Rank)

  /** A section's order: messages by when they were said, exchanges as offered. */
  private type Rank = (Instant, String, Int)

  /** What `pool` shows from `candidates` for a message said at `t`: each section that kept
    * anything, under its key, in the order its first source is listed.
    *
    *   - A message source keeps those said in [t − `within`, t), the latest `most`;
    *     [[Source.Exchanges]] the offered, in order.
    *   - Kept in `sources` order, a message source's latest first: a message's line, or an
    *     exchange's lines together, is kept while every kept one's text, and one newline each,
    *     fits in `budget`; one that does not fit is skipped and the next tried.
    *   - An entry several sources find is kept once, under the first; an exchange whose opening
    *     an earlier source kept is not kept.
    *   - Within a section, messages oldest first, each `{speaker}, {age} before: {words}` (`{age}`
    *     in its largest whole unit: "1 minute", "3 hours");
    *     exchanges as offered, each `Exchange opened {age} before by {speaker}: {words}`, then
    *     `  Record: {headline}` when it has one; the one followed only `Record of the exchange
    *     this thread continues: {headline}`, nothing when it has none. Words and headline at
    *     most [[LineChars]].
    */
  def show(
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
        val opened =
          s"Exchange opened ${ago(o.opening.at)} before by ${o.opening.speaker}: " +
            o.opening.text.take(LineChars)
        val record = o.record.fold("")(h => s"\n  Record: ${h.take(LineChars)}")
        Some(Piece(Section.Exchanges, Some(o.opening.entry), opened + record, rank))
      }
    }
    val found = pool.sources.flatMap {
      case Source.Channel(within, most) => messages(candidates.said, within, most)
      case Source.Author(within, most) => messages(candidates.byAuthor, within, most)
      case Source.Exchanges => exchanges
    }
    val (kept, _, _) = found.foldLeft((Vector.empty[Piece], Set.empty[EntryId], 0)) {
      case ((kept, seen, used), u) =>
        val cost = used + u.text.length + 1
        if (u.entry.exists(seen) || cost > pool.budget) (kept, seen, used)
        else (kept :+ u, seen ++ u.entry, cost)
    }
    VectorMap.from(pool.sources.map(_.section).distinct.flatMap { section =>
      val lines = kept.filter(_.section == section).sortBy(_.rank).map(_.text)
      Option.when(lines.nonEmpty)(section -> lines.mkString("\n"))
    })
  }
}
