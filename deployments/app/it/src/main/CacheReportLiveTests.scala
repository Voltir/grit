package grit.app.main

import grit.core.clock.Clock
import grit.core.id.SourceId
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.store.Origin
import grit.core.visibility.{Label, Level, Subject}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.kit.deployment.Offered
import grit.kit.environment.Secrets
import grit.kit.run.{Kit, Launch}

import utest.*

/** Against Postgres and DBOS: a conversation's cache report ([[Kit.cacheReport]]), read
  * after the engine that ran its turns has closed.
  */
object CacheReportLiveTests extends TestSuite {

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  val tests = Tests {
    test(
      "with no engine running, the report reads each turn back: the first drew afresh with no window, the second carried it"
    ) {
      val config = TestPostgres.freshDatabase("app_cache_report")
      val deployment = right(
        Main.deployment(
          Map.empty,
          Offered.Read,
          Vector.empty,
          Vector.empty,
          java.time.ZoneOffset.UTC
        )
      )
      val env = Map(
        DbConfig.UrlVar -> config.jdbcUrl,
        DbConfig.UserVar -> config.user,
        DbConfig.PasswordVar -> config.password
      )
      val secrets = Secrets.of(env, deployment).fold(r => sys.error(r.message), identity)
      val origin = Origin.Task("cache", "report")
      val engine = LiveEngine.open(config, deployment.runsUnder, visibility = deployment.visibility)
      val conversation =
        try {
          val launched = Main.ownEngine(engine, deployment, secrets, Launch.Run.Served)
          val c = right(
            launched.jot.write(Subject.Public)(
              launched.conversations.findOrCreate(origin, Account.Local, Label.at(Level.Public))
            )
          )
          for (said <- Vector("one", "two")) {
            val turn = right(
              launched.inbox.ingest(origin, SourceId(said), Message.User(said), Account.Local, None)
            )
            right(launched.inbox.startTurn(turn))
            val _ = launched.awaitTurn(turn)
          }
          c.id
        } finally engine.close()
      val lines = right(Kit.cacheReport(deployment, secrets, conversation, Clock.system()))
      lines.filter(_.startsWith("turn ")).map(_.split(" · ").toVector.take(2)) ==> Vector(
        Vector("turn 0", "afresh: no-window"),
        Vector("turn 1", "carried")
      )
    }
  }
}
