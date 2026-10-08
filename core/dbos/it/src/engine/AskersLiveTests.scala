package grit.dbos.engine

import grit.core.id.{ConversationId, SourceId, TestCallSlots, TurnRef, TurnSeq}
import grit.core.identity.{Account, Evidence, Held, Principal, TestAccounts, Vouched}
import grit.core.message.Message
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Clearance, Subject}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** Who a turn answers, against a real Postgres: the person its transactions are cleared for, by
  * the one asker rule the opener calls.
  */
object AskersLiveTests extends TestSuite {

  private val ana = TestAccounts.sourced("slack:T1/U-ana-asks")
  private val dana = TestAccounts.sourced("slack:T1/U-dana-asks")

  /** Runs `body` with an engine on a fresh database, under [[Vouchings.Seen]], closed after. */
  private def engine[A](suite: String)(body: (Engine, DbConfig) => A): A = {
    val config = TestPostgres.freshDatabase(suite)
    val e = LiveEngine.open(config, "test", visibility = Vouchings.Seen)
    try body(e, config)
    finally e.close()
  }

  /** `by`'s message `ts` from `origin`, as its turn. */
  private def told(e: Engine, origin: Origin, by: Account, ts: String): TurnRef =
    e.inbox
      .ingest(origin, SourceId(ts), Message.User(s"message $ts"), by)
      .fold(err => throw new java.lang.AssertionError(s"ingest: $err"), identity)

  /** `turn`'s asker, and the clearance a transaction opened for it reads at. */
  private def asked(e: Engine, turn: TurnRef): (Option[Principal], Clearance) =
    e.jot
      .write(Subject.Turn(turn)) { (tx: Tx^) ?=>
        e.askers.of(turn).map(a => (a, Tx.clearance(tx)))
      }
      .fold(err => throw new java.lang.AssertionError(s"asking: $err"), identity)

  val tests = Tests {
    test(
      "a turn asked through a vouched account answers that person, with the accounts and evidence its transactions are cleared by"
    ) {
      engine("askers_vouched") { (e, config) =>
        LiveDb
          .transaction(config)(
            e.voucher(Set(Vouchings.T1), Vouchings.Claimed)
              .vouch(Vouched(ana, Vouchings.full("ana@example.com")))
          )
          .isRight ==> true
        // A room above what ana is cleared for, so what the turn reads beyond it is hers.
        val origin = Origin.Slack("T1", "C-asks", "1.0")
        val _ = LiveDb.conversation(config, origin, Vouchings.Confidential)
        val turn = told(e, origin, ana, "1.0")
        val (asker, clearance) = asked(e, turn)
        val person = LiveDb.principal(config, ana)
        (asker, clearance) ==> (
          Some(Principal.Person(person, Set(Held(ana, Evidence.Vouched, member = true)))),
          Clearance.inRoom(origin.room, Vouchings.Confidential, Vouchings.Internal)
        )
      }
    }

    test(
      "in a direct message whose turn grit's own entry begins, the asker is the room's person, not grit"
    ) {
      engine("askers_direct") { (e, config) =>
        val origin = Origin.Direct(dana, "1.0")
        val first = told(e, origin, dana, "1.0")
        val opened = TurnRef(first.conversationId, TurnSeq.First.next)
        LiveDb.asking(config, opened, Account.Grit, None)
        asked(e, opened)._1 ==> Some(
          Principal.Person(
            LiveDb.principal(config, dana),
            Set(Held(dana, Evidence.Home, member = false))
          )
        )
      }
    }

    test(
      "a channel's turn grit's own post begins is grit's; a direct message whose account was never seen, and a conversation gone, have no asker"
    ) {
      engine("askers_edges") { (e, config) =>
        val posted = Origin.Slack("T1", "C-grit", "1.0")
        e.inbox.posted(
          posted,
          SourceId("1.0"),
          "a summary",
          java.time.Instant.EPOCH,
          TestCallSlots.First,
          Account.Grit
        ) ==> Right(true)
        val begun = TurnRef(LiveDb.conversation(config, posted).id, TurnSeq.First)
        val unseen = Origin.Direct(TestAccounts.sourced("slack:T1/U-never-seen"), "1.0")
        val dm = TurnRef(LiveDb.conversation(config, unseen).id, TurnSeq.First)
        val gone = TurnRef(ConversationId(java.util.UUID.randomUUID().toString), TurnSeq.First)
        (asked(e, begun)._1, asked(e, dm)._1, asked(e, gone)._1) ==>
          (Some(Principal.Grit), None, None)
      }
    }

    test("a turn with no first entry has no asker, and a task's run is grit's") {
      engine("askers_none") { (e, _) =>
        val channel = told(e, Origin.Slack("T1", "C-none", "1.0"), ana, "1.0")
        val task = told(e, Origin.Task("asks", "run"), Account.Local, "1.0")
        (
          asked(e, TurnRef(channel.conversationId, TurnSeq.First.next))._1,
          asked(e, task)._1
        ) ==> (None, Some(Principal.Grit))
      }
    }
  }
}
