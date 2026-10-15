package grit.act.phase

import grit.core.durable.{Flow, Journaled, Step, StepName}
import grit.core.id.TurnRef
import grit.core.stitch.{Opening, Placements}
import grit.core.store.{ConversationStore, Db, EntryStore, StoreError}
import grit.core.visibility.Subject

/** The wait for a message's placement, which the turn and triage share: what the placement of a
  * turn's message did, once it has ended, when the message is an [[Opening]].
  */
object Placing {

  /** What the wait reads the message through (`entries` and `conversations`, in one read
    * through `db`) and waits on (`placements`).
    */
  final case class Env(
      entries: EntryStore,
      conversations: ConversationStore,
      db: Db^,
      placements: Placements^
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
    */
  def placed[I, F](
      name: StepName[I],
      turn: I -> TurnRef,
      subject: I -> Subject,
      unplaced: Unplaced -> F
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
              } yield conversation.flatMap(Opening.of(_, all, t))
            }
            .left
            .map(e => unplaced(Unplaced.Unread(e)))
            .flatMap {
              case None => Right(None)
              case Some(opening) =>
                env.placements
                  .awaited(opening)
                  .left
                  .map(why => unplaced(Unplaced.Failed(why)))
                  .map(Some(_))
            }
        }
      )
    )
}
