package grit.dbos.engine

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.*

import grit.core.edge.{Permit, ToolRequest}
import grit.core.id.{CallSlot, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.store.Origin
import grit.core.tool.{Retry, ToolName}
import grit.dbos.sql.{LiveDb, SqlToolRequests, TestPostgres}

import dev.dbos.transact.DBOSClient
import org.postgresql.ds.PGSimpleDataSource
import utest.*

/** A desk against a real Postgres: what only a live connection shows. */
object EdgeLiveTests extends TestSuite {

  val tests = Tests {
    test("an idle desk waits out its wait, and a dispatch wakes it at once, well inside it") {
      val config = TestPostgres.freshDatabase("edge_wake")
      LiveEngine.open(config, "test").close()
      val ds = new PGSimpleDataSource()
      ds.setURL(config.jdbcUrl)
      ds.setUser(config.user)
      ds.setPassword(config.password)
      val client = new DBOSClient(ds)
      val place = Place.of(Directory.of("/wake").getOrElse(throw new java.lang.AssertionError()))
      val desk =
        SqlDesk.open(config, ds, client, PrincipalId.Local, Set(place), LiveEngine.Identity) match {
          case Right(d) => d
          case Left(e) => sys.error(s"$e")
        }
      try {
        val idle = System.nanoTime()
        desk.await(300.millis) ==> false
        assert((System.nanoTime() - idle) / 1000000 >= 300)
        val woke = new AtomicLong(-1)
        val waiting = new Thread(() => {
          val started = System.nanoTime()
          if (desk.await(10.seconds)) woke.set((System.nanoTime() - started) / 1000000)
        })
        waiting.start()
        Thread.sleep(500)
        val turn =
          TurnRef(LiveDb.conversation(config, Origin.Task("edge", "wake")).id, TurnSeq.First)
        val request = ToolRequest(
          CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
          ToolRequest.Protocol,
          turn.conversationId,
          place,
          PrincipalId.Local,
          ToolName("read"),
          Permit.Free,
          Retry.Rerun,
          ujson.Obj(),
          Set.empty
        )
        LiveDb.transaction(config)(new SqlToolRequests().dispatch(Vector(request))) ==> Right(())
        waiting.join()
        // Woken by the NOTIFY, half a second into a ten-second wait.
        assert(woke.get() >= 400, woke.get() < 2000)
      } finally {
        desk.close()
        client.close()
      }
    }
  }
}
