package grit.act.phase

import java.time.Duration as JDuration

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.durable.{Flow, Journaled, Step, StepName}
import grit.core.id.TurnRef
import grit.core.stitch.{Opening, Placements}
import grit.core.store.{ConversationStore, Db, EntryStore, StoreError}
import grit.core.visibility.Subject

/** The wait for a message's placement, and then for a quiet after it, which the turn and triage
  * share: what the placement of a turn's message did, once it has ended, when the message is an
  * [[Opening]].
  */
object Placing {

  /** What the wait reads the message through (`entries` and `conversations`, in one read
    * through `db`), waits on (`placements`) and keeps quiet by (`clock`).
    */
  final case class Env(
      entries: EntryStore,
      conversations: ConversationStore,
      db: Db^,
      placements: Placements^,
      clock: Clock^
  )

  /** Why there is no placement to report. */
  enum Unplaced {

    /** The conversation could not be read. */
    case Unread(error: StoreError)

    /** The placement failed, `why`. */
    case Failed(why: String)
  }

  /** The step `name`: what the placement of `turn`'s message did, read for `subject`, once it
    * has ended, when the message is an [[Opening]]; `None` when it is not, or is gone; why not,
    * as `unplaced` writes it, when the conversation cannot be read or the placement failed.
    * When the message was read, it then returns no sooner than `quiet` after the message was
    * said ([[grit.core.clock.Clock.sleep]] on the env's clock): a step run again after a crash
    * waits only what is left.
    */
  def placed[I, F](
      name: StepName[I],
      turn: I -> TurnRef,
      subject: I -> Subject,
      unplaced: Unplaced -> F,
      quiet: FiniteDuration
  )(using Journaled[Either[F, Option[String]]]): Flow[Env, I, Nothing, Either[F, Option[String]]] =
    Flow.step(
      Step[Env, I, Either[F, Option[String]]](name)(env =>
        in => {
          val t = turn(in)
          val c = t.conversationId
          env.db
            .read(subject(in)) {
              for {
                all <- env.entries.list(c)
                conversation <- env.conversations.get(c)
              } yield (
                conversation.flatMap(Opening.of(_, all, t)),
                all.filter(_.turnSeq == t.turnSeq).minByOption(_.seq).map(_.createdAt)
              )
            }
            .left
            .map(e => unplaced(Unplaced.Unread(e)))
            .flatMap { (opening, said) =>
              val placed = opening match {
                case None => Right(None)
                case Some(o) =>
                  env.placements
                    .awaited(o)
                    .left
                    .map(why => unplaced(Unplaced.Failed(why)))
                    .map(Some(_))
              }
              said.foreach(at =>
                env.clock.sleep(
                  JDuration.between(env.clock.now(), at.plusNanos(quiet.toNanos)).toNanos.nanos
                )
              )
              placed
            }
        }
      )
    )
}
