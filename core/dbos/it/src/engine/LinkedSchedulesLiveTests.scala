package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.SetClock
import grit.core.id.{PluginName, ScheduleId, TestCallSlots, TurnRef}
import grit.core.identity.{Account, TestAccounts}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.{booking, hour}
import grit.core.job.{DeskRefusal, ScheduleContract, ScheduleDesk, When}
import grit.core.store.Tx
import grit.dbos.sql.{DbConfig, LiveDb}

import utest.*

/** An account's asked schedules while a trusted realm links it to an email's person, against a
  * real Postgres: those asked before the link stay its home's, those asked while linked are the
  * person's, and through the account both are listed, cancelled and capped together.
  */
object LinkedSchedulesLiveTests extends TestSuite {

  private val remind = new Counting("remind")

  private def desk(u: ScheduleContract.Under): ScheduleDesk^ =
    u.desk(
      PluginName.of("reminders").fold(sys.error, identity),
      Vector(remind.name),
      new SetClock(Instant.parse("2026-10-07T09:00:00Z"))
    )

  /** Runs `sql` with `params`, arranging rows a vouching would write. */
  private def execute(config: DbConfig, sql: String, params: String*): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        val _ = ps.executeUpdate()
      }
    }

  /** `accounts` attested `email`, its person minted when new: each now that email's person. */
  private def linked(config: DbConfig, email: String, accounts: Account*): Unit = {
    execute(
      config,
      """WITH person AS (INSERT INTO grit.principals (id, kind)
        |                SELECT uuidv7()::text, 'person'
        |                 WHERE NOT EXISTS (SELECT 1 FROM grit.emails WHERE email = ?)
        |                RETURNING id)
        |INSERT INTO grit.emails (email, principal_id) SELECT ?, id FROM person""".stripMargin,
      email,
      email
    )
    accounts.foreach(a =>
      execute(
        config,
        """INSERT INTO grit.attestations (account, email, member, seen_at)
          |VALUES (?, ?, true, now())
          |ON CONFLICT (account) DO UPDATE SET email = EXCLUDED.email""".stripMargin,
        Account.written(a),
        email
      )
    )
  }

  /** `account`'s attestation holding no email: it is its home again. */
  private def lapsed(config: DbConfig, account: Account): Unit =
    execute(
      config,
      "UPDATE grit.attestations SET email = NULL WHERE account = ?",
      Account.written(account)
    )

  /** A turn in its own thread asked through `by`, its reply posted there. */
  private def askedThrough(u: ScheduleContract.Under, thread: String, by: Account): TurnRef = {
    val turn = u.turn(thread)
    u.asking(turn, by, Some(s"C1/$thread"))
    turn
  }

  private def ask(d: ScheduleDesk^, from: TurnRef, index: Int): Either[DeskRefusal, ScheduleId] =
    d.ask(
      TestCallSlots.at(from, index = index),
      booking(remind),
      When.In(1.hour),
      hour,
      Count(index)
    ).map(_.id)

  private def listed(d: ScheduleDesk^, from: TurnRef): Either[DeskRefusal, Set[ScheduleId]] =
    d.pending(TestCallSlots.at(from, index = 999), booking(remind)).map(_.schedules.map(_.id).toSet)

  val tests = Tests {
    test(
      "a linked account lists what was asked through it before its link and since; after a lapse, its home's alone"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("linked_listed")
      val a = TestAccounts.account("slack:T1/U-linked-a")
      val d = desk(u)
      val before = ask(d, askedThrough(u, "1.0", a), 1).fold(r => sys.error(s"$r"), identity)
      linked(config, "linked@example.com", a)
      val since = ask(d, askedThrough(u, "1.1", a), 2).fold(r => sys.error(s"$r"), identity)
      val whileLinked = listed(d, askedThrough(u, "1.2", a))
      lapsed(config, a)
      (whileLinked, listed(d, askedThrough(u, "1.3", a))) ==> (
        Right(Set(before, since)),
        Right(Set(before))
      )
    }

    test("a linked account cancels what was asked through it before its link") {
      val (u, config) = SqlSchedulesUnder.withConfig("linked_cancel")
      val a = TestAccounts.account("slack:T1/U-cancel-a")
      val d = desk(u)
      val before = ask(d, askedThrough(u, "2.0", a), 1).fold(r => sys.error(s"$r"), identity)
      linked(config, "cancel@example.com", a)
      val through = askedThrough(u, "2.1", a)
      (
        d.cancel(TestCallSlots.at(through, index = 3), booking(remind), before),
        listed(d, through)
      ) ==> (Right(()), Right(Set()))
    }

    test(
      "a linked account's cap counts its home's pending schedules and its person's together"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("linked_cap")
      val a = TestAccounts.account("slack:T1/U-cap-a")
      val b = TestAccounts.account("slack:T1/U-cap-b")
      val d = desk(u)
      val half = ScheduleDesk.PendingCap / 2
      val home = askedThrough(u, "3.0", a)
      val atHome = (0 until half).map(ask(d, home, _)).collect { case Left(r) => r }
      val person = askedThrough(u, "3.1", b)
      linked(config, "cap@example.com", a, b)
      val onPerson =
        (0 until ScheduleDesk.PendingCap - half).map(ask(d, person, _)).collect { case Left(r) =>
          r
        }
      (atHome, onPerson, ask(d, askedThrough(u, "3.2", a), 0)) ==> (
        Vector(),
        Vector(),
        Left(DeskRefusal.TooMany(ScheduleDesk.PendingCap))
      )
    }

    test("another account of the person lists the person's schedules and never a's home's") {
      val (u, config) = SqlSchedulesUnder.withConfig("linked_other")
      val a = TestAccounts.account("slack:T1/U-other-a")
      val b = TestAccounts.account("slack:T1/U-other-b")
      val d = desk(u)
      val _ = ask(d, askedThrough(u, "4.0", a), 1).fold(r => sys.error(s"$r"), identity)
      val theirs = askedThrough(u, "4.2", b)
      linked(config, "other@example.com", a, b)
      val since = ask(d, askedThrough(u, "4.1", a), 2).fold(r => sys.error(s"$r"), identity)
      listed(d, theirs) ==> Right(Set(since))
    }
  }
}
