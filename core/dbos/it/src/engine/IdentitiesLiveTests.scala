package grit.dbos.engine

import java.util.UUID
import java.util.concurrent.{CountDownLatch, Executors}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Try, Using}

import grit.core.id.{PrincipalId, SourceId, TurnRef}
import grit.core.identity.{Account, TestAccounts}
import grit.core.inbox.{InboundId, InboxError}
import grit.core.message.Message
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{LiveDb, TestPostgres}

import utest.*

/** Each account an action comes through, as `grit.identities` keeps it, against a real
  * Postgres.
  */
object IdentitiesLiveTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("identities")
    LiveEngine.open(c, "test").close()
    c
  }

  private val ana = TestAccounts.account("slack:T1/UA")
  private val ben = TestAccounts.account("slack:T1/UB")

  private def ingest(engine: Engine^, thread: String, by: Account): Either[InboxError, TurnRef] =
    engine.inbox.ingest(
      Origin.Slack("T1", "C1", thread),
      SourceId(thread),
      Message.User("hello"),
      by
    )

  /** Who wrote the message `turn` began with, as a room's reads name them. */
  private def author(engine: Engine^, turn: TurnRef, thread: String): Option[PrincipalId] =
    LiveDb
      .transaction(config)(
        engine.rooms.author(InboundId.of(turn.conversationId, SourceId(thread)))
      )
      .fold(e => throw new java.lang.AssertionError(s"reading an author: $e"), identity)

  /** The version of the UUID `id` spells; `None` when it spells none. */
  private def uuidVersion(id: PrincipalId): Option[Int] =
    Try(UUID.fromString(PrincipalId.value(id))).toOption.map(_.version)

  /** `account`'s row: its principal, evidence and home. */
  private def identityOf(account: Account): Option[(String, String, Option[String])] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT principal_id, evidence, home FROM grit.identities WHERE account = ?"
        )
      ) { ps =>
        ps.setString(1, Account.written(account))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next())((rs.getString(1), rs.getString(2), Option(rs.getString(3))))
        }
      }
    }

  /** How many people `grit.principals` holds. */
  private def people(): Long =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("SELECT count(*) FROM grit.principals WHERE kind = 'person'")
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          rs.getLong(1)
        }
      }
    }

  val tests = Tests {
    test(
      "an account first seen is a new person's one account, enrolled, under an id minted for them, never its spelling"
    ) {
      val engine = LiveEngine.open(config, "test")
      try {
        def said(thread: String, by: Account): Option[PrincipalId] =
          ingest(engine, thread, by).toOption.flatMap(author(engine, _, thread))
        val hers = said("1.0", ana)
        hers.flatMap(uuidVersion) ==> Some(7)
        said("2.0", ana) ==> hers
        val his = said("3.0", ben)
        assert(his.flatMap(uuidVersion) == Some(7), his != hers)
        identityOf(ana) ==> hers.map(p =>
          (PrincipalId.value(p), "enrolled", Some(PrincipalId.value(p)))
        )
      } finally engine.close()
    }

    test("first sightings of one account at once make one person, and every one records") {
      val engine = LiveEngine.open(config, "test")
      val pool = Executors.newFixedThreadPool(8)
      given ExecutionContext = ExecutionContext.fromExecutorService(pool)
      try {
        val before = people()
        val accounts = (1 to 5).map(n => TestAccounts.account(s"slack:T1/URACE$n")).toVector
        val authors = accounts.map { account =>
          val go = new CountDownLatch(1)
          val threads = (1 to 8).map(n => s"race-${Account.written(account)}-$n").toVector
          val pending = Future.sequence(threads.map { thread =>
            Future {
              go.await()
              ingest(engine, thread, account).map(author(engine, _, thread))
            }
          })
          go.countDown()
          Await.result(pending, 60.seconds)
        }
        authors.flatten.collect { case Left(e) => e } ==> Vector()
        authors.map(_.distinct.size) ==> accounts.map(_ => 1)
        people() - before ==> accounts.size.toLong
      } finally {
        pool.shutdownNow()
        engine.close()
      }
    }
  }
}
