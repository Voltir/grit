package grit.app.main

import grit.core.store.Payload
import grit.dbos.engine.{Engine, TurnStatus}
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

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
        // DBOS records the patch's marker where the turn took it, as the replay histories do.
        steps.map(_.name) ==>
          Turn.Step.all.patch(1, Vector(s"DBOS.patch-${Turn.Patches.RecordWindow}"), 0)
        val own = steps.filter(s => Turn.Step.all.contains(s.name))
        assert(own.forall(s => s.started.zip(s.completed).exists((a, b) => !b.isBefore(a))))
        assert(!engine.status(turn).isInstanceOf[TurnStatus.Running])

        val window = engine.db.read(engine.entries.get(Turn.windowId(turn))).toOption.flatten
        window.map(_.payload) ==>
          Some(Payload.Window(Vector(sayId(engine, first), Turn.replyId(first)), Vector.empty))
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
