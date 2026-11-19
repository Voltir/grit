package grit.core.stitch

import java.time.Instant

import grit.core.id.{ConversationId, Href}
import grit.core.message.Cut
import grit.core.store.{Conversation, ConversationStore, StoreError, Tx}

/** The strands of the rooms a transaction reads ([[Strand.Digested]]). Each read takes each
  * strand's messages once, text and all, and keeps only their ids, count and lengths.
  */
trait StrandShelf {

  /** Each strand with at least one follower, a member of which had a message said from `from`
    * until before `until`, as it stood at `until`; oldest touched first.
    */
  def touched(from: Instant, until: Instant)(using Tx^): Either[StoreError, Vector[Strand.Digested]]

  /** [[touched]], only the strands whose room's label in force is the transaction's floor
    * ([[grit.core.store.Tx.floor]]): what a run kept at one label reads.
    */
  def atFloor(from: Instant, until: Instant)(using Tx^): Either[StoreError, Vector[Strand.Digested]]

  /** Each of `roots`' strands as it stood at `asOf`, in `roots`' order; a root none of whose
    * members holds a message kept and read is left out.
    */
  def of(roots: Vector[ConversationId], asOf: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Strand.Digested]]

  /** The links `seen`'s members' messages hold ([[Href.in]] over each, oldest first, each link
    * once), of those said before its `asOf` that the transaction reads, a thread's opening
    * among them and a run read by its span: the one part of their text a run's code is handed.
    */
  def links(seen: Strand.Digested)(using Tx^): Either[StoreError, Vector[Href]]

  /** The strands at the floor a run places, oldest touched first: [[atFloor]]'s; each thread
    * under a channel's top level spoken in within the range, whole, as it stood at `until`; and
    * each run `cut` makes of a channel's top level whose last message falls in the range and
    * that is offered by `until`, cut from its true start, whatever the range: a chain of
    * messages is read back at most [[StrandShelf.ReadBack]] days (or gaps, when longer), and
    * one longer is cut from the oldest message read.
    */
  def units(from: Instant, until: Instant, cut: Strand.Cut)(using
      Tx^
  ): Either[StoreError, Vector[Strand.Digested]]
}

object StrandShelf {

  /** The shelf over `stitches`, where strands are linked and said in, and `conversations`,
    * where each root's room is.
    */
  def over(stitches: StitchStore, conversations: ConversationStore): StrandShelf =
    new Over(stitches, conversations)

  /** What `root`'s strand said before `asOf`: the root's messages and its followers', oldest
    * first. The one read both a [[Strand.Digested]] and a [[StrandExcerpts]] read are made of.
    */
  private[stitch] def saidIn(stitches: StitchStore, root: ConversationId, asOf: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]] =
    stitches
      .links(Vector(root))
      .map(_.filter(_.root == root).map(_.conversation).distinct)
      .flatMap(followers => stitches.said(root +: followers, Instant.EPOCH, asOf))

  /** A day: how far back each step reads to find where a run's chain of messages begins. */
  private[stitch] val Reach: java.time.Duration = java.time.Duration.ofDays(1)

  /** 7: the most steps of [[Reach]] a run's chain is read back; past them, the oldest message
    * read starts it.
    */
  val ReadBack: Int = 7

  /** How many characters of `said` a reader is shown, at most [[Stitching.MessageChars]] of
    * each.
    */
  private[stitch] def chars(said: Vector[Said]): Int =
    said.flatMap(Stitching.spoken).map(Cut.head(_, Stitching.MessageChars).length).sum

  /** What a strand shows: the message it opens with (`opening`) and the messages it places
    * (`placed`).
    */
  private[stitch] final case class Placing(opening: Option[Said], placed: Vector[Said])

  /** What `seen` shows as it stood at its `asOf`, as its transaction reads it: a whole strand's
    * [[saidIn]], opening with a thread's asking message ([[grit.core.store.Origin.asking]],
    * `None` under grit's own post or one not read) or else its root's first message; a run's
    * messages `first` to `last` of its root, said from its `since` to its `touched`; nothing
    * placed when `first` or `last` is not read.
    */
  private[stitch] def placing(
      stitches: StitchStore,
      conversations: ConversationStore,
      seen: Strand.Digested
  )(using Tx^): Either[StoreError, Placing] = seen.span match {
    case Strand.Span.Whole =>
      for {
        said <- saidIn(stitches, seen.root, seen.asOf)
        root <- conversations.get(seen.root)
        asking <- root.flatMap(_.origin.asking) match {
          case None => Right(None)
          case Some((top, source)) =>
            conversations
              .find(top)
              .flatMap(
                _.fold[Either[StoreError, Option[Said]]](Right(None))(c =>
                  stitches.asked(c.id, source)
                )
              )
        }
      } yield Placing(
        // A thread opens with its asking message; a stitched strand with its root's first.
        if (root.exists(_.origin.asking.nonEmpty)) asking
        else said.find(_.conversation == seen.root),
        said
      )
    case Strand.Span.Run(first, last) =>
      // `said` reads until before its end: a millisecond past `touched` takes the last message.
      stitches.said(Vector(seen.root), seen.since, seen.touched.plusMillis(1)).map { all =>
        val i = all.indexWhere(_.entry.id == first)
        val j = all.indexWhere(_.entry.id == last)
        Placing(None, if (i < 0 || j < i) Vector.empty else all.slice(i, j + 1))
      }
  }

  private final class Over(stitches: StitchStore, conversations: ConversationStore)
      extends StrandShelf {

    def touched(from: Instant, until: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      stitches
        .touched(from, until)
        .flatMap(links => of(links.map(_.root).distinct, until))
        .map(_.sortBy(_.touched))

    def atFloor(from: Instant, until: Instant)(using
        tx: Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      touched(from, until).map(_.filter(_.roomLabel == Tx.floor(tx)))

    def of(roots: Vector[ConversationId], asOf: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      roots.foldLeft[Either[StoreError, Vector[Strand.Digested]]](Right(Vector.empty)) {
        (acc, root) =>
          acc.flatMap(done => seen(root, asOf).map(done ++ _))
      }

    def links(seen: Strand.Digested)(using Tx^): Either[StoreError, Vector[Href]] =
      placing(stitches, conversations, seen).map(p =>
        (p.opening.toVector ++ p.placed)
          .distinctBy(_.entry.id)
          .flatMap(Stitching.spoken)
          .flatMap(Href.in)
          .distinct
      )

    def units(from: Instant, until: Instant, cut: Strand.Cut)(using
        tx: Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      for {
        stitched <- atFloor(from, until)
        heard <- stitches.inRooms(from, until)
        spoken <- each(heard.map(_.conversation).distinct)(conversations.get)
        threads <- of(
          spoken.flatten.collect { case c if c.origin.asking.nonEmpty => c.id },
          until
        )
        runs <- each(spoken.flatten.filter(_.origin.rolls))(c => runsOf(c, from, until, cut))
      } yield (stitched ++ (threads ++ runs.flatten).filter(_.roomLabel == Tx.floor(tx)))
        .sortBy(_.touched)

    /** The runs `cut` makes of `c`'s messages, whose last is said from `from` and that are
      * offered at `until`.
      */
    private def runsOf(
        c: Conversation,
        from: Instant,
        until: Instant,
        cut: Strand.Cut
    )(using Tx^): Either[StoreError, Vector[Strand.Digested]] = {
      val reach = if (Reach.compareTo(cut.gap) >= 0) Reach else cut.gap
      // The chain the first message said from `from` is in begins after a gap of cut.gap or
      // more; read back a step of `reach` at a time, at most ReadBack steps, until that gap is
      // read. A chain begun at the oldest message read may reach back past `lo`.
      def back(lo: Instant, got: Vector[Said], steps: Int): Either[StoreError, Vector[Said]] = {
        val first = got.indexWhere(!_.entry.createdAt.isBefore(from))
        val open = first >= 0 &&
          cut.starts(got).takeWhile(_ <= first).lastOption.contains(0) &&
          got.headOption.exists(h => lo.isAfter(h.entry.createdAt.minus(cut.gap)))
        if (!open || steps >= ReadBack) Right(got)
        else
          stitches
            .said(Vector(c.id), lo.minus(reach), lo)
            .flatMap(older =>
              if (older.isEmpty) Right(got) else back(lo.minus(reach), older ++ got, steps + 1)
            )
      }
      stitches.said(Vector(c.id), from, until).flatMap(back(from, _, 0)).map { top =>
        val room = c.origin.room
        cut
          .pieces(top, until)
          .filterNot(_.lastOption.exists(_.entry.createdAt.isBefore(from)))
          .flatMap { run =>
            for {
              first <- run.headOption
              last <- run.lastOption
            } yield Strand.Digested(
              room,
              Tx.roomLabel(room),
              c.id,
              Vector(c.id),
              run.size,
              chars(run),
              first.entry.createdAt,
              last.entry.createdAt,
              StrandDigest.of(run.map(_.entry.id)),
              until,
              Strand.Span.Run(first.entry.id, last.entry.id)
            )
          }
      }
    }

    private def each[A, B](as: Vector[A])(
        f: A => Either[StoreError, B]
    ): Either[StoreError, Vector[B]] =
      as.foldLeft[Either[StoreError, Vector[B]]](Right(Vector.empty))((acc, a) =>
        acc.flatMap(done => f(a).map(done :+ _))
      )

    private def seen(root: ConversationId, asOf: Instant)(using
        Tx^
    ): Either[StoreError, Option[Strand.Digested]] =
      for {
        conversation <- conversations.get(root)
        said <- saidIn(stitches, root, asOf)
      } yield
        for {
          c <- conversation
          first <- said.headOption
          last <- said.lastOption
        } yield {
          val room = c.origin.room
          val heard = said.map(_.conversation).distinct
          Strand.Digested(
            room,
            Tx.roomLabel(room),
            root,
            heard.filter(_ == root) ++ heard.filterNot(_ == root),
            said.size,
            chars(said),
            first.entry.createdAt,
            last.entry.createdAt,
            StrandDigest.of(said.map(_.entry.id)),
            asOf
          )
        }
  }
}
