package grit.app.main

import java.time.Instant

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{CloseRef, PeriodRef, PeriodSeq, PluginName, SourceId, ToolCallId, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{CloseOrdinal, CloseReason, Probability, TestClosings}
import grit.core.plugin.{CacheDocs, Plugin, PostRef}
import grit.core.store.{ClosedPeriod, Db, Origin, Sealed, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.digest.Digest
import grit.lifecycle.post.{PostEnv, Posting}

import utest.*

/** Plugins over DBOS against a real Postgres: posting from closing entries alone, the
  * cursor and the plugin's writes committing together, and Digest's backfill and surface.
  */
object PluginLiveTests extends TestSuite {

  private def name(s: String): PluginName = PluginName.of(s).getOrElse(sys.error(s))

  private def nothing(id: WorkflowId)(using @unused d: Durable^): String = WorkflowId.value(id)

  /** Writes a document, then refuses: nothing it wrote may outlive the refusal. */
  private final class Refusing(val name: PluginName) extends Plugin {
    val version = 1
    def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
      docs.put("half", ujson.Str("written before the refusal")).flatMap(_ => Left(StoreError.Invalid("refused")))
  }

  /** `n` periods of one conversation, each one message, closed in turn: as a database
    * holds them before any plugin is enabled.
    */
  private def closed(engine: Engine^, n: Int): Unit =
    for (i <- 1 to n) {
      val t = engine.inbox
        .ingest(Origin.Tui(grit.core.place.Directory.of("/plugins").fold(e => sys.error(e), identity), "plugins"), SourceId(s"m$i"), Message.User(s"message $i"), grit.core.id.PrincipalId.Local)
        .fold(e => sys.error(s"$e"), identity)
      val closing = TestClosings.prose(s"Period $i. More.")
      val ref = PeriodRef(t.conversationId, PeriodSeq.of(i.toLong).getOrElse(sys.error("seq")))
      engine.jot.write(engine.periods.seal(CloseRef(ref, t.turnSeq, Instant.EPOCH), CloseReason.Resolved(Probability.One), closing, Instant.parse(s"2026-09-2${i}T10:00:00Z"))) ==>
        Right(Sealed.Closed(ref.closingId))
    }

  /** Waits up to 30 s for workflow `id` to finish, and says whether it did. */
  private def finished(config: DbConfig, id: WorkflowId): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    def done = LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SELECT status FROM dbos.workflow_status WHERE workflow_uuid = ?")) { ps =>
        ps.setString(1, WorkflowId.value(id))
        Using.resource(ps.executeQuery())(rs => rs.next() && rs.getString(1) == "SUCCESS")
      }
    }
    var held = done
    while (!held && System.nanoTime() < until) { Thread.sleep(50); held = done }
    held
  }

  private def launch(engine: Engine^, plugins: Vector[Plugin]): Unit =
    engine.launch(
      nothing,
      nothing,
      nothing,
      Posting.body(plugins, PostEnv(
            engine.periods,
            engine.cursors,
            engine.cache,
            engine.tombstones,
            engine.jot,
            Clock.system()
          )),
      nothing,
      plugins
    )

  val tests = Tests {
    test("Digest enabled on a database with closed periods posts them all, and lists them") {
      val config = TestPostgres.freshDatabase("plugin_backfill")
      val engine = LiveEngine.open(config, "test")
      try {
        val digest = new Digest(name("digest"))
        launch(engine, Vector(digest))
        closed(engine, 2)
        val run = PostRef(digest.name, 1, CloseOrdinal.Start, 0)
        engine.sweep(Instant.now()).map(_.posted) ==> Right(Vector(run))
        assert(finished(config, run.workflowId))
        engine.jot.write(engine.cursors.start(digest.name, 1, java.time.Instant.now())) ==> Right(CloseOrdinal.of(2).getOrElse(sys.error("o")))
        engine.sweep(Instant.now()).map(_.posted) ==> Right(Vector())
        val store: Db^ = engine.db
        val tools = Toolbox.of[{store}](Digest.recentActivity(store, engine.docs(digest.name))).fold(d => sys.error(s"$d"), identity)
        tools.bind(AssistantBlock.ToolCall(ToolCallId("c"), "recent_activity", ujson.Obj()), Repairs.All) match {
          case Right(free: Bound.Free) =>
            free() ==> Outcome.Done(
              "2026-09-22 10:00 · tui plugins · resolved · Period 2.\n" +
                "2026-09-21 10:00 · tui plugins · resolved · Period 1."
            )
          case other => sys.error(s"not free: $other")
        }
      } finally engine.close()
    }

    test("a plugin that refuses leaves its cursor and nothing it wrote, and is run again a bounded number of times") {
      val config = TestPostgres.freshDatabase("plugin_refused")
      val engine = LiveEngine.open(config, "test")
      try {
        val refusing = new Refusing(name("refusing"))
        launch(engine, Vector(refusing))
        closed(engine, 1)
        val runs = (0 until PostRef.Attempts).toVector.map(PostRef(refusing.name, 1, CloseOrdinal.Start, _))
        // Still behind after each: the next run from the same cursor, under the next id.
        runs.map { run =>
          val posted = engine.sweep(Instant.now()).map(_.posted)
          assert(finished(config, run.workflowId))
          posted
        } ==> runs.map(run => Right(Vector(run)))
        engine.db.read(engine.docs(refusing.name).get("half")) ==> Right(None)
        engine.jot.write(engine.cursors.start(refusing.name, 1, java.time.Instant.now())) ==> Right(CloseOrdinal.Start)
        // Then the cursor is left for a person, its last run named.
        engine.sweep(Instant.now()).map(s => (s.posted, s.stuck)) ==>
          Right((Vector(), runs.lastOption.map(_.workflowId).toVector))
      } finally engine.close()
    }
  }
}
