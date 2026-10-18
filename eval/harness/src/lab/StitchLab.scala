package grit.eval.harness.lab

import java.time.Instant

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.id.{ConversationId, EntryId, SourceId, TurnRef}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.period.Probability
import grit.core.speech.Reach
import grit.core.stitch.{Offered, Placed, StitchReads, Stitching, StrandShelf, Tuning}
import grit.core.store.{Origin, StoreError}
import grit.core.visibility.Subject
import grit.dbos.engine.Engine
import grit.eval.Room
import grit.eval.harness.capture.CaseId
import grit.eval.harness.jev.Budget
import grit.eval.harness.log.{Cache, Outcome, Row, Suite, Weights}
import grit.eval.harness.run.Run
import grit.eval.harness.score.RoomPlacing

/** A room placed through the shipped stitch ask: what each opening made, in the order said; the
  * strands they ended in, each by its openings' ids, root first; the rows of the calls asked;
  * and the budget left.
  */
final case class Lab(
    placings: Vector[RoomPlacing],
    strands: Vector[Vector[String]],
    rows: Vector[Row[Vector[Weights]]],
    budget: Budget
)

/** The stitch lab: a [[Room]] heard through an engine's inbox, then every opening placed in the
  * order said, as the stitch workflow places it ([[Stitching.placing]]), and each placement
  * kept, so a later opening sees the links an earlier one made.
  */
object StitchLab {

  /** `room` heard into `engine`'s database, which must hold none of it, each message through the
    * inbox as an edge hears it, its author named; then each opening placed under `tuning`, Jev
    * asked as `model` through `cache` within `budget` ([[Run.asked]]), each call timed by
    * `clock`, and the placement kept, dated when the opening was said. Once a call is skipped at
    * the cap, it and every opening after it that would ask are `Skipped`, keeping nothing. The
    * strands are read as they stand after the last placement. `Left` when the room is seeded
    * already, a message is refused, or a store fails.
    */
  def run(
      room: Room,
      engine: Engine^,
      jev: Classifier^,
      model: String,
      cache: Cache[Vector[Weights]]^,
      budget: Budget,
      tuning: Tuning,
      clock: Clock^
  ): Either[String, Lab] = {
    val heard = Heard(room)
    val reads = StitchReads(
      engine.entries,
      engine.conversations,
      engine.lifecycle,
      engine.stitches,
      engine.search,
      engine.principals
    )
    for {
      _ <- seed(heard, engine)
      convs <- each(room.openings)(m =>
        engine.db
          .read(Subject.Public)(engine.conversations.find(heard.origin(m)))
          .left
          .map(said)
          .flatMap(_.map(c => m -> c.id).toRight(s"${m.id}: no conversation was made"))
      )
      idOf = convs.map((m, c) => c -> m.id).toMap
      placed <- convs.foldLeft[Either[String, Placings]](
        Right(Placings(Vector.empty, Vector.empty, budget, stopped = false))
      ) { case (acc, (m, c)) =>
        acc.flatMap(done =>
          place(Opening(m, c, heard.origin(m)), done, reads, engine, idOf)(
            jev,
            model,
            cache,
            tuning,
            clock
          )
        )
      }
      strands <- engine.db
        .read(Subject.Public)(
          StrandShelf
            .over(engine.stitches, engine.conversations)
            .touched(
              Instant.EPOCH,
              room.messages.lastOption.fold(Instant.EPOCH)(_.at).plusSeconds(1)
            )
        )
        .left
        .map(said)
    } yield Lab(
      placed.placings,
      strands.map(_.members.map(c => idOf.getOrElse(c, ConversationId.value(c)))),
      placed.rows,
      placed.budget
    )
  }

  /** Each message of `room` as Slack would carry it: its ts (its second, then its place in the
    * room for uniqueness), and its conversation's origin, the thread it is in.
    */
  private final case class Heard(room: Room) {
    private val ts: Map[String, String] =
      room.messages.zipWithIndex
        .map((m, i) => m.id -> f"${m.at.getEpochSecond}%d.${i + 1}%06d")
        .toMap
    def tsOf(id: String): String = ts.getOrElse(id, id)
    def origin(m: Room.Message): Origin =
      Origin.Slack(room.team, room.channel, tsOf(m.thread.getOrElse(m.id)))
  }

  /** An opening to place: its message, conversation and origin. */
  private final case class Opening(m: Room.Message, c: ConversationId, origin: Origin)

  /** What the placings so far made, and whether a call was skipped at the cap. */
  private final case class Placings(
      placings: Vector[RoomPlacing],
      rows: Vector[Row[Vector[Weights]]],
      budget: Budget,
      stopped: Boolean
  )

  /** What one opening's placing gave the lab: made asking nothing, asked, or not asked. */
  private enum Step {
    case Made(root: EntryId, placed: Placed)
    case Asked(root: EntryId, placed: Placed, row: Option[Row[Vector[Weights]]], budget: Budget)
    case Stopped
  }

  private def seed(heard: Heard, engine: Engine^): Either[String, Unit] = {
    val room = heard.room
    for {
      already <- room.messages.headOption.fold[Either[String, Boolean]](Right(false))(m =>
        engine.db
          .read(Subject.Public)(engine.conversations.find(heard.origin(m)))
          .left
          .map(said)
          .map(_.isDefined)
      )
      _ <- Either.cond(!already, (), "the room is in the database already: build it afresh")
      heardAll <- every(room.messages) { m =>
        for {
          by <- Account.read(s"slack:${room.team}/U${m.who.toUpperCase}")
          _ <- engine.jot.write(Subject.Public)(engine.principals.name(by, m.who)).left.map(said)
          root = heard.tsOf(m.thread.getOrElse(m.id))
          _ <- engine.inbox
            .hear(
              heard.origin(m),
              SourceId(heard.tsOf(m.id)),
              m.text,
              by,
              m.at,
              Reach(Some(s"${room.channel}/$root/${heard.tsOf(m.id)}"), Set.empty)
            )
            .left
            .map((e: InboxError) => s"${m.id}: not heard: $e")
        } yield ()
      }
    } yield ()
  }

  private def place(
      o: Opening,
      done: Placings,
      reads: StitchReads,
      engine: Engine^,
      idOf: Map[ConversationId, String]
  )(
      jev: Classifier^,
      model: String,
      cache: Cache[Vector[Weights]]^,
      tuning: Tuning,
      clock: Clock^
  ): Either[String, Placings] = {
    def id(of: ConversationId): String = idOf.getOrElse(of, ConversationId.value(of))
    def placing(made: RoomPlacing.Made, p: Option[Placed]): RoomPlacing = RoomPlacing(
      o.m.id,
      made,
      p.toVector
        .flatMap(_.seen.offered)
        .map(x =>
          RoomPlacing.Offer(
            id(x.root),
            x.why match {
              case Offered.Recent(_) => true
              case Offered.Lexical(_) => false
            },
            x.p.map(Probability.value)
          )
        )
    )
    def keep(root: EntryId, p: Placed): Either[String, RoomPlacing] =
      engine.jot
        .write(Subject.Conversation(o.c))(engine.stitches.record(root, p, o.m.at))
        .left
        .map(said)
        .map(_ =>
          placing(
            p match {
              case f: Placed.Follows => RoomPlacing.Made.Follows(id(f.root), Probability.value(f.p))
              case b: Placed.Begins => RoomPlacing.Made.Begins(Probability.value(b.p))
              case u: Placed.Unread => RoomPlacing.Made.Unread(u.why)
            },
            Some(p)
          )
        )
    val skipped = done.copy(placings = done.placings :+ placing(RoomPlacing.Made.Skipped, None))
    for {
      first <- engine.db
        .read(Subject.Conversation(o.c))(engine.entries.list(o.c))
        .left
        .map(said)
        .flatMap(_.minByOption(_.seq).toRight(s"${o.m.id}: no entry"))
      caseId <- CaseId.opening(o.origin).toRight(s"${o.m.id}: no case id")
      step <- Stitching
        .placing(
          reads,
          engine.db.as(Subject.Conversation(o.c)),
          TurnRef(o.c, first.turnSeq),
          tuning
        )(
          (root, p) => Step.Made(root, p),
          (root, q) =>
            if (done.stopped) Step.Stopped
            else {
              val asked = Run.asked(
                Suite.Stitch,
                caseId,
                0,
                None,
                jev,
                model,
                cache,
                done.budget,
                clock
              )(classifier => q.placed(classifier.ask(q.state, q.choice)))
              Step.Asked(root, asked.made, asked.row, asked.budget)
            }
        )
        .left
        .map(said)
      next <- step match {
        case None =>
          Right(done.copy(placings = done.placings :+ placing(RoomPlacing.Made.Unoffered, None)))
        case Some(Step.Stopped) => Right(skipped)
        case Some(Step.Made(root, p)) =>
          keep(root, p).map(x => done.copy(placings = done.placings :+ x))
        case Some(Step.Asked(_, _, Some(r), left)) if r.outcome == Outcome.Skipped =>
          Right(skipped.copy(rows = done.rows :+ r, budget = left, stopped = true))
        case Some(Step.Asked(root, p, row, left)) =>
          keep(root, p).map(x =>
            done.copy(placings = done.placings :+ x, rows = done.rows ++ row, budget = left)
          )
      }
    } yield next
  }

  private def every[A](as: Vector[A])(f: A => Either[String, Unit]): Either[String, Unit] =
    as.foldLeft[Either[String, Unit]](Right(()))((acc, a) => acc.flatMap(_ => f(a)))

  private def each[A, B](as: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    as.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => f(a).map(done :+ _))
    )

  private def said(e: StoreError): String = e match {
    case StoreError.DuplicateId(id) => s"duplicate id ${EntryId.value(id)}"
    case StoreError.DatabaseError(cause) => s"database error: $cause"
    case StoreError.Invalid(cause) => s"invalid: $cause"
  }
}
