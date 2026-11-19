package grit.core.stitch

import java.time.{Duration, Instant}

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Place
import grit.core.store.Entry
import grit.core.visibility.{Integrity, Label}

/** `conversation`'s first message continues the exchange `root` began. */
final case class Link(conversation: ConversationId, root: ConversationId)

/** An entry of a conversation other than the reader's, with the place it was said at. */
final case class Said(conversation: ConversationId, place: Place, entry: Entry)

object Strand {

  /** The strand of `conversation` among `links`: the root it follows (itself when it follows
    * none) and every conversation that follows that root. One hop: a follower of a follower is
    * not in it.
    */
  def of(conversation: ConversationId, links: Vector[Link]): Set[ConversationId] = {
    val root = links.find(_.conversation == conversation).fold(conversation)(_.root)
    links.filter(_.root == root).map(_.conversation).toSet + root
  }

  /** What a reader is shown of its strand ([[Along.read]]): the root's `opening` message when
    * the root is another conversation and its first message is still kept; what the other
    * members `said` in the range read, oldest first (the opening among them when it was said
    * in range); the members whose first message is `gone` (their raw entries purged), shown by
    * their record instead; and every member but the reader (`conversations`), whether or not
    * anything of theirs is shown.
    */
  final case class Read(
      opening: Option[Said],
      said: Vector[Said],
      gone: Vector[ConversationId],
      conversations: Set[ConversationId]
  ) {

    /** Every entry it shows: the opening, then what was said, each once. */
    def shown: Vector[grit.core.store.Entry] =
      (opening.toVector ++ said).map(_.entry).distinctBy(_.id)

    /** Every conversation it shows messages of. */
    def members: Vector[ConversationId] =
      (opening.map(_.conversation).toVector ++ said.map(_.conversation)).distinct
  }

  object Read {
    val empty: Read = Read(None, Vector.empty, Vector.empty, Set.empty)
  }

  /** A strand as it stood `asOf`, by ids and counts alone ([[StrandShelf]]): its `room` and the
    * label in force there; its `root` and `members`, the root first then its followers by
    * first message, each holding a message said before `asOf` that is still kept and its
    * reader reads; how many messages they hold (`said`) and how many characters of them a
    * reader is shown, at most [[Stitching.MessageChars]] of each (`chars`); when the first and
    * last were said; and their [[StrandDigest]].
    */
  final case class Digested(
      room: Place,
      roomLabel: Label,
      root: ConversationId,
      members: Vector[ConversationId],
      said: Int,
      chars: Int,
      since: Instant,
      touched: Instant,
      digest: StrandDigest,
      asOf: Instant,
      span: Span = Span.Whole
  ) extends caps.Pure {
    def id: Id = Id(
      root,
      span match {
        case Span.Whole => None
        case Span.Run(first, _, _, _) => Some(first)
      }
    )
  }

  /** Which of its members' messages a strand places: every one, or its root's top-level
    * messages `first` to `last`, read with the `before` and `after` top-level neighbours its
    * [[Cut]] showed, as context it is not placed with.
    */
  enum Span extends caps.Pure {
    case Whole
    case Run(first: EntryId, last: EntryId, before: Int, after: Int)
  }

  /** What tells one strand from another: its root, and a run's first message. */
  final case class Id(root: ConversationId, first: Option[EntryId]) extends caps.Pure

  /** How a dreaming run cuts a channel's top level: messages said less than `gap` apart join one
    * run, of at most `most` (`gap` zero: each its own); each shown with up to `before` and
    * `after` top-level neighbours as context it is not placed with; offered once it has its
    * after neighbours (or, with `after` zero, once a message follows it) or `settle` has passed
    * since its last message; a run is the same whatever window reads it.
    */
  final case class Cut private (
      gap: Duration,
      most: Int,
      before: Int,
      after: Int,
      settle: Duration
  ) extends caps.Pure {

    /** The runs of `top`, one room's top-level messages oldest first from a run's true start,
      * that are offered at `until`, each with its context.
      */
    private[stitch] def pieces(top: Vector[Said], until: Instant): Vector[Cut.Piece] = {
      val starts = top.indices.filter(i =>
        i == 0 || Duration
          .between(top(i - 1).entry.createdAt, top(i).entry.createdAt)
          .compareTo(gap) >= 0
      )
      // Each gap-chain is cut into runs of at most `most`, counted from its start.
      val bounds = (starts :+ top.size).sliding(2).toVector.flatMap {
        case Seq(s, e) => (s until e by most).map(a => (a, (a + most).min(e)))
        case _ => Vector.empty
      }
      bounds.flatMap { (s, e) =>
        val run = top.slice(s, e)
        val following = top.drop(e)
        run.lastOption
          .filter(last =>
            following.size >= after.max(1) ||
              !until.isBefore(last.entry.createdAt.plus(settle))
          )
          .map(_ => Cut.Piece(top.slice((s - before).max(0), s), run, following.take(after)))
      }
    }
  }

  object Cut {

    /** A run with the neighbours shown as its context. */
    private[stitch] final case class Piece(
        before: Vector[Said],
        run: Vector[Said],
        after: Vector[Said]
    )

    /** A cut, or why not: `gap` negative, `most` outside 1–64, `before` or `after` outside
      * 0–8, or `settle` under `gap`.
      */
    def of(
        gap: Duration,
        most: Int,
        before: Int,
        after: Int,
        settle: Duration
    ): Either[String, Cut] =
      if (gap.isNegative) Left(s"a cut's gap is negative: $gap")
      else if (most < 1 || most > 64) Left(s"a cut's most is not within 1 to 64: $most")
      else if (before < 0 || before > 8) Left(s"a cut's before is not within 0 to 8: $before")
      else if (after < 0 || after > 8) Left(s"a cut's after is not within 0 to 8: $after")
      else if (settle.compareTo(gap) < 0) Left(s"a cut's settle $settle is under its gap $gap")
      else Right(Cut(gap, most, before, after, settle))

    /** 5 minutes apart, at most 16 a run, no neighbours shown, settled after 30 minutes: a
      * replay of two labelled rooms placed 5-minute runs best, and worse with neighbours shown.
      */
    val Default: Cut = Cut(Duration.ofMinutes(5), 16, 0, 0, Duration.ofMinutes(30))

    /** `c` as a job's params record it: `{"gap", "most", "before", "after", "settle"}`, the
      * durations in seconds.
      */
    def write(c: Cut): ujson.Value = ujson.Obj(
      "gap" -> c.gap.toSeconds.toDouble,
      "most" -> c.most,
      "before" -> c.before,
      "after" -> c.after,
      "settle" -> c.settle.toSeconds.toDouble
    )

    /** A cut [[write]] wrote, or why `v` is not one, or not a cut [[of]] takes. */
    def read(v: ujson.Value): Either[String, Cut] = {
      def count(key: String): Either[String, Long] =
        v.objOpt
          .flatMap(_.get(key))
          .flatMap(_.numOpt)
          .filter(n => n.isWhole)
          .map(_.toLong)
          .toRight(s"a cut's '$key' is not a whole number")
      for {
        gap <- count("gap")
        most <- count("most")
        before <- count("before")
        after <- count("after")
        settle <- count("settle")
        cut <- of(
          Duration.ofSeconds(gap),
          most.toInt,
          before.toInt,
          after.toInt,
          Duration.ofSeconds(settle)
        )
      } yield cut
    }
  }

  object Digested {

    /** `seen` as a step records it: `{"room", "roomLabel", "root", "members": [...], "said",
      * "chars", "since", "touched", "digest", "asOf"}`, a place and a label in their written
      * forms, instants ISO-8601, and for a run alone `"span": {"first", "last", "before",
      * "after"}`. Ids and counts alone: no message's text.
      */
    def write(seen: Digested): ujson.Value = {
      val o = ujson.Obj(
        "room" -> seen.room.written,
        "roomLabel" -> Label.written(seen.roomLabel),
        "root" -> ConversationId.value(seen.root),
        "members" -> ujson.Arr.from(seen.members.map(ConversationId.value)),
        "said" -> seen.said,
        "chars" -> seen.chars,
        "since" -> seen.since.toString,
        "touched" -> seen.touched.toString,
        "digest" -> StrandDigest.value(seen.digest),
        "asOf" -> seen.asOf.toString
      )
      seen.span match {
        case Span.Whole => o
        case Span.Run(first, last, before, after) =>
          ujson.Obj.from(
            o.value.toSeq :+ ("span" -> ujson.Obj(
              "first" -> EntryId.value(first),
              "last" -> EntryId.value(last),
              "before" -> before,
              "after" -> after
            ))
          )
      }
    }

    /** A strand [[write]] wrote, or why `v` is not one. */
    def read(v: ujson.Value): Either[String, Digested] = {
      def field(key: String): Either[String, ujson.Value] =
        v.objOpt.flatMap(_.get(key)).toRight(s"a strand has no '$key'")
      def text(key: String): Either[String, String] =
        field(key).flatMap(_.strOpt.toRight(s"a strand's '$key' is not text"))
      def count(key: String): Either[String, Int] =
        field(key).flatMap(_.numOpt.map(_.toInt).toRight(s"a strand's '$key' is not a number"))
      def instant(key: String): Either[String, Instant] =
        text(key).flatMap(t =>
          scala.util.Try(Instant.parse(t)).toOption.toRight(s"a strand's '$key' is no instant")
        )
      for {
        room <- text("room").flatMap(Place.read)
        label <- text("roomLabel").flatMap(Label.read)
        root <- text("root")
        members <- field("members").flatMap(
          _.arrOpt
            .flatMap(a => Option.when(a.forall(_.strOpt.nonEmpty))(a.toVector.flatMap(_.strOpt)))
            .toRight("a strand's 'members' are not text")
        )
        said <- count("said")
        chars <- count("chars")
        since <- instant("since")
        touched <- instant("touched")
        digest <- text("digest").flatMap(StrandDigest.read)
        asOf <- instant("asOf")
        span <- v.objOpt.flatMap(_.get("span")) match {
          case None => Right(Span.Whole)
          case Some(s) =>
            def part(key: String) = s.objOpt.flatMap(_.get(key))
            (for {
              first <- part("first").flatMap(_.strOpt)
              last <- part("last").flatMap(_.strOpt)
              before <- part("before").flatMap(_.numOpt)
              after <- part("after").flatMap(_.numOpt)
            } yield Span.Run(EntryId(first), EntryId(last), before.toInt, after.toInt))
              .toRight("a strand's 'span' is not a run's")
        }
      } yield Digested(
        room,
        label,
        ConversationId(root),
        members.map(ConversationId(_)),
        said,
        chars,
        since,
        touched,
        digest,
        asOf,
        span
      )
    }
  }

  /** A strand's messages as an excerpt shows them, and the integrity they are believed at
    * together ([[StrandExcerpts]]).
    */
  final case class Shown(excerpt: Stitching.Excerpt, integrity: Integrity)
}
