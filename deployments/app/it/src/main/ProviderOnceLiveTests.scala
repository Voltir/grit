package grit.app.main

import grit.core.id.EntryId
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{LiveDb, TestPostgres}
import grit.models.StubProvider
import grit.turn.{Turn, TurnSummary}

import utest.*

/** Against Postgres and DBOS: a turn's provider call happens once,
  * however often the turn is started, restarted or recovered.
  */
object ProviderOnceLiveTests extends TestSuite {
  import LiveTurn.*

  val tests = Tests {
    test("the same source twice is one turn; started twice and rerun, the provider runs once") {
      val config = TestPostgres.freshDatabase("provider_once")
      val first = new CountingProvider
      val engine = LiveEngine.open(config, Turn.Epoch)
      val (turn, again, output) =
        try {
          launch(engine, engine.entries, first)
          val turn = say(engine, "a")
          val again = say(engine, "a")
          (turn, again, engine.awaitTurn(turn))
        } finally engine.close()
      again ==> turn
      assert(output.startsWith("replied: "))
      first.calls ==> 1

      // A new engine on the same database: the finished turn is not run again.
      val second = new CountingProvider
      val restarted = LiveEngine.open(config, Turn.Epoch)
      val (rerun, text) =
        try {
          launch(restarted, restarted.entries, second)
          restarted.inbox.startTurn(turn)
          (restarted.awaitTurn(turn), reply(restarted, turn))
        } finally restarted.close()
      rerun ==> output
      second.calls ==> 0
      text ==> Some("stub reply to: message a")
      LiveDb.ledger(config).map(r => (r._1, r._2, r._3)) ==>
        Vector(turn.replyId, TurnSummary.id(turn))
          .map(id => (EntryId.value(id), StubProvider.Model, Some(BigDecimal(0))))
    }

    test("a crash after the model call resumes without calling the model again") {
      val config = TestPostgres.freshDatabase("m0_crash")
      val child = os
        .proc(
          sys.env("GRIT_TEST_JAVA"),
          "--sun-misc-unsafe-memory-access=allow",
          "-cp",
          sys.env("GRIT_TEST_CLASSPATH"),
          "grit.app.main.CrashingTurn",
          "crash"
        )
        .call(
          env = Map(
            "GRIT_DATABASE_URL" -> config.jdbcUrl,
            "GRIT_DATABASE_USER" -> config.user,
            "GRIT_DATABASE_PASSWORD" -> config.password
          ),
          check = false,
          mergeErrIntoOut = true
        )
      assert(child.exitCode == CrashingTurn.Halted)

      val provider = new CountingProvider
      val engine = LiveEngine.reopen(config, Turn.Epoch)
      val (output, text) =
        try {
          launch(engine, engine.entries, provider)
          // Idempotent ingest hands back the crashed turn; recovery is already running it.
          val turn = say(engine, "crash")
          (engine.awaitTurn(turn), reply(engine, turn))
        } finally engine.close()
      assert(output.startsWith("replied: "))
      provider.calls ==> 0
      text ==> Some("stub reply to: message crash")
    }
  }
}
