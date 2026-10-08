package grit.app.main

import java.time.Instant
import java.util.UUID

import scala.util.Using

import grit.core.id.{CallSlot, CloseRef, ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.admin.{Answer, Command}
import grit.core.identity.{Account, Standing, TestAccounts, Vouched}
import grit.core.inbox.{Inbox, InboxContract}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Period, TestClosings}
import grit.core.place.Place
import grit.core.speech.Reach
import grit.core.spend.Budget
import grit.core.store.{Entry, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.turn.Turn

/** The inbox contract, kept by SqlInbox against a real Postgres, under a launched engine (a
  * turn's progress is read from DBOS's tables, which launching creates).
  */
object SqlInboxContractTests extends InboxContract {
  import LiveTurn.*

  private lazy val config = TestPostgres.freshDatabase("sql_inbox_contract")

  /** `origin`'s conversation, in the database `config` names, made to name as its creator an
    * account no account could be, which the store reads as `Invalid`.
    */
  private def unreadable(engine: Engine^, config: DbConfig, origin: Origin): Unit = {
    val id = engine.db
      .read(Subject.Public)(engine.conversations.find(origin))
      .fold(e => sys.error(e.toString), _.map(_.id))
      .getOrElse(sys.error(s"no conversation of $origin"))
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Vector(
        "INSERT INTO grit.principals (id, kind) VALUES ('unreadable', 'person') ON CONFLICT DO NOTHING",
        "INSERT INTO grit.identities (account, home) VALUES ('no one', 'unreadable') ON CONFLICT DO NOTHING",
        s"UPDATE grit.conversations SET created_by = 'no one' WHERE id = '${ConversationId.value(id)}'::uuid"
      ).foreach(sql => Using.resource(conn.prepareStatement(sql))(ps => ps.executeUpdate()))
    }
  }

  /** A full member of [[InboxContract.T1]], who makes a room quiet in [[quieted]]. */
  private val Quieter: Account = TestAccounts.account("slack:T1/U-quieter")

  /** `room` made quiet through `engine`'s administration by [[Quieter]], attested a full member
    * first, as a person's command is.
    */
  private def quieted(engine: Engine^, room: Place): Unit = {
    engine.jot
      .write(Subject.Public)(
        engine
          .voucher(Set(InboxContract.T1, InboxContract.T2), InboxContract.Claimed)
          .vouch(Vouched(Quieter, Standing.Full(None)))
      )
      .fold(e => sys.error(e.toString), _ => ())
    engine.administration.run(Quieter, room, Command.Quiet(true), Instant.EPOCH) match {
      case Right(Answer.Refused(why)) => sys.error(s"quieting refused: $why")
      case Right(_) => ()
      case Left(e) => sys.error(e.toString)
    }
  }

  /** Over the suite's shared database under the shipped visibility, and over a database of its
    * own under any other: an engine refuses to start under compartments that drop one its
    * database ran under.
    */
  protected def withInbox[A](budget: Budget, visibility: Visibility)(
      body: (Inbox, InboxContract.Store^) => A
  ): A =
    over(
      if (visibility == Visibility.Shipped) config
      else TestPostgres.freshDatabase("sql_inbox_contract_labelled"),
      budget,
      visibility
    )(body)

  /** Over a database of its own, its engine closed after `before` and launched again under
    * `now`.
    */
  protected def reopening[A, B](budget: Budget, was: Visibility, now: Visibility)(
      before: (Inbox, InboxContract.Store^) => A
  )(after: (A, Inbox, InboxContract.Store^) => B): B = {
    val db = TestPostgres.freshDatabase("sql_inbox_contract_reopened")
    val a = over(db, budget, was)(before)
    over(db, budget, now)((inbox, store) => after(a, inbox, store))
  }

  /** Runs `body` with the inbox of an engine launched on `db` under `visibility`, taking new
    * messages as `budget` allows, and the store under it; the engine closed after.
    */
  private def over[A](db: DbConfig, budget: Budget, visibility: Visibility)(
      body: (Inbox, InboxContract.Store^) => A
  ): A = {
    val engine = LiveEngine.open(
      db,
      Turn.Epoch,
      budget,
      visibility
    )
    try {
      launch(engine, engine.entries, new CountingProvider)
      def spend(usd: BigDecimal): Unit = {
        val turn = TurnRef(ConversationId(UUID.randomUUID().toString), TurnSeq.First)
        val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(usd))
        engine.jot
          .write(Subject.Public)(
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
          .read(Subject.Public)(engine.conversations.find(origin))
          .fold(e => sys.error(e.toString), _.nonEmpty)
      def written(origin: Origin): Vector[(Payload, Option[String])] =
        engine.db
          .read(Subject.Public)(for {
            found <- engine.conversations.find(origin)
            all <- found.fold(Right(Vector.empty): Either[StoreError, Vector[Entry]])(c =>
              engine.entries.list(c.id)
            )
            names <- engine.principals.speakers(all.map(_.id))
          } yield all.map(e => (e.payload, names.of(e.id))))
          .fold(e => sys.error(e.toString), identity)
      def dated(origin: Origin): Vector[java.time.Instant] =
        engine.db
          .read(Subject.Public)(
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
          .read(Subject.Public)(
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
          .write(Subject.Public)(
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
      def name(account: Account, name: String): Unit =
        engine.jot
          .write(Subject.Public)(engine.principals.name(account, name))
          .fold(e => sys.error(e.toString), identity)
      def reached(origin: Origin): Vector[Option[Reach]] =
        engine.db
          .read(Subject.Public)(for {
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
          .read(Subject.Public)(
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
          name,
          reached,
          postedBy,
          (declared, now) =>
            engine.jot
              .write(Subject.Public)(engine.schedules.declare(declared, now))
              .fold(e => sys.error(e.toString), identity),
          id =>
            engine.db
              .read(Subject.Public)(engine.schedules.read(id))
              .fold(e => sys.error(e.toString), identity),
          now =>
            engine.db
              .read(Subject.Public) {
                for {
                  flying <- engine.schedules.inFlight(1000)
                  due <- engine.schedules.due(now, 1000)
                } yield flying ++ due
              }
              .fold(e => sys.error(e.toString), _.map(_._1)),
          turn => {
            val _ = engine.awaitTurn(turn)
          },
          (slot, version, at) =>
            engine.jot
              .write(Subject.Public)(engine.schedules.replied(slot, version, at))
              .fold(e => sys.error(e.toString), identity),
          origin =>
            engine.db
              .read(Subject.Public)(engine.conversations.find(origin))
              .fold(e => sys.error(e.toString), _.map(_.label)),
          origin => unreadable(engine, db, origin),
          vouched =>
            engine.jot
              .write(Subject.Public)(
                engine
                  .voucher(Set(InboxContract.T1, InboxContract.T2), InboxContract.Claimed)
                  .vouch(vouched)
              )
              .fold(e => sys.error(e.toString), _ => ()),
          room => quieted(engine, room)
        )
      )
    } finally engine.close()
  }
}
