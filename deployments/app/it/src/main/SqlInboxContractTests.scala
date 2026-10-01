package grit.app.main

import java.time.Instant
import java.util.UUID

import grit.core.id.{CallSlot, CloseRef, ConversationId, EntryId, PrincipalId, TurnRef, TurnSeq}
import grit.core.inbox.{Inbox, InboxContract}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Period, TestClosings}
import grit.core.speech.Reach
import grit.core.spend.Budget
import grit.core.store.{Entry, Origin, Payload, StoreError}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

/** The inbox contract, kept by SqlInbox against a real Postgres, under a launched engine (a
  * turn's progress is read from DBOS's tables, which launching creates).
  */
object SqlInboxContractTests extends InboxContract {
  import LiveTurn.*

  private lazy val config = TestPostgres.freshDatabase("sql_inbox_contract")

  protected def withInbox[A](budget: Budget)(body: (Inbox, InboxContract.Store^) => A): A = {
    val engine = LiveEngine.open(config, Turn.Epoch, budget)
    try {
      launch(engine, engine.entries, new CountingProvider)
      def spend(usd: BigDecimal): Unit = {
        val turn = TurnRef(ConversationId(UUID.randomUUID().toString), TurnSeq.First)
        val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(usd))
        engine.jot
          .write(
            engine.ledger
              .record(
                EntryId(s"spent:${UUID.randomUUID()}"),
                turn,
                turn.workflowId,
                "m",
                usage,
                Tokens(1)
              )
          )
          .fold(e => sys.error(e.toString), identity)
      }
      def exists(origin: Origin): Boolean =
        engine.db
          .read(engine.conversations.find(origin))
          .fold(e => sys.error(e.toString), _.nonEmpty)
      def written(origin: Origin): Vector[(Payload, Option[String])] =
        engine.db
          .read(for {
            found <- engine.conversations.find(origin)
            all <- found.fold(Right(Vector.empty): Either[StoreError, Vector[Entry]])(c =>
              engine.entries.list(c.id)
            )
            names <- engine.principals.speakers(all.map(_.id))
          } yield all.map(e => (e.payload, names.of(e.id))))
          .fold(e => sys.error(e.toString), identity)
      def dated(origin: Origin): Vector[java.time.Instant] =
        engine.db
          .read(
            engine.conversations
              .find(origin)
              .flatMap(
                _.fold(Right(Vector.empty): Either[StoreError, Vector[Entry]])(c =>
                  engine.entries.list(c.id)
                )
              )
          )
          .fold(e => sys.error(e.toString), _.map(_.createdAt))
      def periods(origin: Origin): Vector[Period] =
        engine.db
          .read(
            engine.conversations
              .find(origin)
              .flatMap(
                _.fold(Right(Vector.empty): Either[StoreError, Vector[Period]])(c =>
                  engine.periods.all(c.id)
                )
              )
          )
          .fold(e => sys.error(e.toString), identity)
      def close(turn: TurnRef): Unit =
        engine.jot
          .write(
            engine.periods
              .of(turn)
              .flatMap(
                _.toRight(StoreError.Invalid(s"no period holds $turn")).flatMap(p =>
                  engine.periods.seal(
                    CloseRef(p.ref, turn.turnSeq, Instant.EPOCH),
                    CloseReason.Lapsed,
                    TestClosings.prose("closed"),
                    Instant.EPOCH
                  )
                )
              )
          )
          .fold(e => sys.error(e.toString), _ => ())
      def enroll(id: PrincipalId, name: String): Unit =
        engine.jot
          .write(engine.principals.enroll(id, name))
          .fold(e => sys.error(e.toString), identity)
      def reached(origin: Origin): Vector[Option[Reach]] =
        engine.db
          .read(for {
            found <- engine.conversations.find(origin)
            all <- found.fold(Right(Vector.empty): Either[StoreError, Vector[Entry]])(c =>
              engine.entries.list(c.id)
            )
            reaches <- all.foldLeft[Either[StoreError, Vector[Option[Reach]]]](
              Right(Vector.empty)
            ) { (acc, e) =>
              acc.flatMap(done =>
                engine.speech.reach(TurnRef(e.conversationId, e.turnSeq)).map(done :+ _)
              )
            }
          } yield reaches)
          .fold(e => sys.error(e.toString), identity)
      def postedBy(origin: Origin): Option[CallSlot] =
        engine.db
          .read(
            engine.conversations
              .find(origin)
              .flatMap(
                _.fold(Right(None): Either[StoreError, Option[CallSlot]])(c =>
                  engine.conversations.postedBy(c.id)
                )
              )
          )
          .fold(e => sys.error(e.toString), identity)
      body(
        engine.inbox,
        InboxContract.Store(
          spend,
          exists,
          written,
          dated,
          periods,
          close,
          enroll,
          reached,
          postedBy
        )
      )
    } finally engine.close()
  }
}
