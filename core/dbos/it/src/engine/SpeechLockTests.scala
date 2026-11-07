package grit.dbos.engine

import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.util.Using

import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{DbConfig, LiveDb, SqlSpeechStore, TestPostgres}
import utest.*

/** [[SqlSpeechStore.lockRoom]] against a real Postgres: what two transactions at once see. */
object SpeechLockTests extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("speech_lock")
    // Opening an engine applies schema.sql; nothing here launches DBOS.
    LiveEngine.open(c, "test").close()
    c
  }

  private val speech = new SqlSpeechStore()

  /** A drafting decision on `conversation`'s turn `seq`, in `room`, kept as the engine keeps one. */
  private def decided(conversation: ConversationId, seq: Long, room: String): TurnRef = {
    val turn = TurnRef(conversation, TurnSeq(seq))
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.speech (workflow, conversation_id, turn_seq, room, decided_at, drafting)
            |VALUES (?, ?::uuid, ?, ?, now(), true)""".stripMargin
        )
      ) { ps =>
        ps.setString(1, WorkflowId.value(turn.workflowId))
        ps.setString(2, ConversationId.value(conversation))
        ps.setLong(3, seq)
        ps.setString(4, room)
        val _ = ps.executeUpdate()
      }
    }
    turn
  }

  /** Whether a transaction taking `turn`'s room lock gets it within `ms` while another holds
    * `held`'s, and whether it then gets it once that one has committed.
    */
  private def waits(held: TurnRef, turn: TurnRef, ms: Long): (Boolean, Boolean) = {
    val taken = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val holder = new Thread(() =>
      LiveDb.transaction(config) { (tx: Tx^) ?=>
        val _ = speech.lockRoom(held)
        taken.countDown()
        val _ = release.await(30, TimeUnit.SECONDS)
      }
    )
    holder.start()
    val _ = taken.await(30, TimeUnit.SECONDS)
    val got = new CountDownLatch(1)
    val second = new Thread(() =>
      LiveDb.transaction(config) { (tx: Tx^) ?=>
        val _ = speech.lockRoom(turn)
        got.countDown()
      }
    )
    second.start()
    val early = got.await(ms, TimeUnit.MILLISECONDS)
    release.countDown()
    val late = got.await(30, TimeUnit.SECONDS)
    holder.join()
    second.join()
    (early, late)
  }

  val tests = Tests {
    test(
      "a placement re-check in a room waits for another holding that room's lock to commit; one in another room does not"
    ) {
      val c = LiveDb.conversation(config, Origin.Room("T1", "C1")).id
      val first = decided(c, 1, "slack/T1/C1")
      val same = decided(c, 2, "slack/T1/C1")
      val other = decided(c, 3, "slack/T1/C2")
      (waits(first, same, 300), waits(first, other, 5000)) ==> ((false, true), (true, true))
    }
  }
}
