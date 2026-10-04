package grit.app.main

import grit.core.inbox.Progress
import grit.core.message.AssistantBlock
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

import utest.*

/** Against Postgres and DBOS: a finished turn's progress, as an edge reads it through the inbox. */
object InboxProgressLiveTests extends TestSuite {
  import LiveTurn.*

  val tests = Tests {
    test("a turn DBOS has never seen is Open: not yet started, as an edge may ask of one") {
      val config = TestPostgres.freshDatabase("inbox_progress_unseen")
      val engine = LiveEngine.open(config, Turn.Epoch)
      val progress =
        try {
          launch(engine, engine.entries, new CountingProvider)
          val conversation =
            engine.conversation(
              grit.core.store.Origin.Task("unseen", "c"),
              grit.core.id.PrincipalId.Grit
            )
          conversation.map(c =>
            engine.inbox.progress(grit.core.id.TurnRef(c, grit.core.id.TurnSeq(7)))
          )
        } finally engine.close()
      progress ==> Right(Right(Progress.Open))
    }

    test("a finished turn is Done, with its reply entry and its workflow's output") {
      val config = TestPostgres.freshDatabase("inbox_progress")
      val engine = LiveEngine.open(config, Turn.Epoch)
      val (output, progress) =
        try {
          launch(engine, engine.entries, new CountingProvider)
          val turn = say(engine, "a")
          val output = engine.awaitTurn(turn)
          (output, engine.inbox.progress(turn))
        } finally engine.close()
      progress.map {
        case Progress.Done(reply, outcome) =>
          (reply.map(_.blocks.collect { case AssistantBlock.Text(t) => t }), outcome)
        case Progress.Open => (None, "open")
      } ==> Right((Some(Vector("stub reply to: message a")), output))
    }
  }
}
