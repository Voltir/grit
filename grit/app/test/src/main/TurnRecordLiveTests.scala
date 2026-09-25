package grit.app.main

import grit.core.store.Payload
import grit.dbos.engine.{Engine, TurnStatus}
import grit.dbos.sql.TestPostgres
import grit.models.StubProvider
import grit.turn.{Turn, TurnStream}

import utest.*

/** What a finished turn leaves for an edge to read, against Postgres and DBOS: the steps
  * its workflow recorded, and the record of its window.
  */
object TurnRecordLiveTests extends TestSuite {
  import LiveTurn.*

  val tests = Tests {
    test("a finished turn's steps read back in order, each started before it completed") {
      val config = TestPostgres.freshDatabase("turn_record")
      val engine = Engine.open(config, Turn.Epoch)
      try {
        launch(engine, engine.entries, new CountingProvider)
        val first = say(engine, "one")
        val _ = engine.awaitTurn(first)
        val turn = say(engine, "two")
        val _ = engine.awaitTurn(turn)
        val steps = engine.steps(turn)
        // DBOS records each patch's marker where the turn took it, as the replay histories do.
        steps.map(_.name) ==>
          (s"DBOS.patch-${Turn.Patches.Topics}" +: Turn.Step.all)
            .patch(4, Vector(s"DBOS.patch-${Turn.Patches.RecordWindow}"), 0)
        val own = steps.filter(s => Turn.Step.all.contains(s.name))
        assert(own.forall(s => s.started.zip(s.completed).exists((a, b) => !b.isBefore(a))))
        assert(!engine.status(turn).isInstanceOf[TurnStatus.Running])

        val window = engine.db.read(engine.entries.get(Turn.windowId(turn))).toOption.flatten
        window.map(_.payload) ==>
          Some(Payload.Window(Vector(sayId(engine, first), Turn.replyId(first)), Vector.empty))
        // Where each message went, written to Postgres and read back as topic events.
        val placed = Vector(first, turn).flatMap { t =>
          engine.db.read(engine.entries.get(grit.turn.TurnTopics.placedId(t))).toOption.flatten
        }
        placed.map(_.payload match {
          case Payload.Topic(events) => events.size
          case _ => 0
        }) ==> Vector(2, 1)
      } finally engine.close()
    }

    test("the reply streams to an edge while the turn runs, and joins back to it") {
      val config = TestPostgres.freshDatabase("turn_stream")
      val engine = Engine.open(config, Turn.Epoch)
      try {
        launch(engine, engine.entries, new StubProvider(2000))
        val turn = say(engine, Vector.tabulate(40)(i => s"w$i").mkString(" "))
        val pieces = engine.stream(turn, TurnStream.Key)
        val first = pieces.next()
        // The first piece is read while the model is still answering.
        assert(engine.status(turn).isInstanceOf[TurnStatus.Running])
        val all = (first +: pieces.toVector).flatMap(TurnStream.decode(_).toOption)
        assert(all.size > 1)
        Some(all.foldLeft(TurnStream.Heard.nothing)(_ + _).text) ==> reply(engine, turn)
      } finally engine.close()
    }

    test("a crash mid-stream: the rerun's pieces follow the dead run's, and the edge hears one") {
      val config = TestPostgres.freshDatabase("turn_stream_crash")
      val source = Vector.tabulate(40)(i => s"w$i").mkString(" ")
      val child = os
        .proc(
          sys.env("GRIT_TEST_JAVA"),
          "--sun-misc-unsafe-memory-access=allow",
          "-cp",
          sys.env("GRIT_TEST_CLASSPATH"),
          "grit.app.main.CrashingTurn",
          source,
          "mid-stream"
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

      val engine = Engine.open(config, Turn.Epoch)
      try {
        launch(engine, engine.entries, new CountingProvider)
        val turn = say(engine, source)
        val _ = engine.awaitTurn(turn)
        val all =
          engine.stream(turn, TurnStream.Key).toVector.flatMap(TurnStream.decode(_).toOption)
        all.map(_.attempt).distinct.size ==> 2
        Some(all.foldLeft(TurnStream.Heard.nothing)(_ + _).text) ==> reply(engine, turn)
      } finally engine.close()
    }
  }

  /** The id of the user message that started `turn`. */
  private def sayId(engine: Engine^, turn: grit.core.id.TurnRef): grit.core.id.EntryId =
    engine.db
      .read(engine.entries.list(turn.conversationId))
      .toOption
      .flatMap(_.find(e => e.turnSeq == turn.turnSeq).map(_.id))
      .getOrElse(sys.error("no message for the turn"))
}
