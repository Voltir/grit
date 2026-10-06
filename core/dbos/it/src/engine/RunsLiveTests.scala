package grit.dbos.engine

import java.util.concurrent.ConcurrentLinkedQueue

import scala.annotation.unused
import scala.jdk.CollectionConverters.*
import scala.util.Using

import grit.core.durable.Durable
import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{LiveDb, TestPostgres}
import grit.dbos.workflow.Runs

import dev.dbos.transact.DBOSClient
import org.postgresql.ds.PGSimpleDataSource
import utest.*

/** A job's run as DBOS knows it, against a real Postgres under a launched engine. */
object RunsLiveTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  /** The workflow `id`'s name, queue and partition key, as DBOS recorded them. */
  private def recorded(id: WorkflowId)(using tx: Tx^): Option[(String, String, String)] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(
      conn.prepareStatement(
        "SELECT name, queue_name, queue_partition_key FROM dbos.workflow_status WHERE workflow_uuid = ?"
      )
    ) { ps =>
      ps.setString(1, WorkflowId.value(id))
      Using.resource(ps.executeQuery()) { rs =>
        Option.when(rs.next())((rs.getString(1), rs.getString(2), rs.getString(3)))
      }
    }
  }

  val tests = Tests {
    test(
      "a run enqueued as one runs the run body, as a workflow named run on the turns queue under its conversation"
    ) {
      val config = TestPostgres.freshDatabase("runs_live")
      val ran = new ConcurrentLinkedQueue[WorkflowId]()
      def run(id: WorkflowId)(using @unused d: Durable^): String = {
        ran.add(id)
        "ran"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, nothing, nothing, Vector.empty, run = run)
        val conversation =
          LiveDb.conversation(config, Origin.Task("remind", "s@2026-10-07T09:00:00Z")).id
        val turn = TurnRef(conversation, TurnSeq.First)
        val ds = new PGSimpleDataSource()
        ds.setURL(config.jdbcUrl)
        ds.setUser(config.user)
        ds.setPassword(config.password)
        Using.resource(new DBOSClient(ds)) { client =>
          val _ = client.enqueueWorkflow[String, Exception](
            Runs.enqueueOptions(turn),
            // As the inbox's: the array is empty and DBOS only reads it.
            caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
          )
        }
        engine.awaitTurn(turn) ==> "ran"
        (ran.asScala.toVector, LiveDb.transaction(config)(recorded(turn.workflowId))) ==> (
          Vector(turn.workflowId),
          Some(("run", "turns", ConversationId.value(conversation)))
        )
      } finally engine.close()
    }
  }
}
