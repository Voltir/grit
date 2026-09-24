package grit.turn

import grit.core.durable.{History, InMemoryDurable}
import grit.core.id.TurnRef
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger, Payload}

/** Writes this epoch's recorded turn histories, one per shape a turn can leave behind,
  * into `GRIT_HISTORIES/{Turn.Epoch}`. Never overwrites: a history, once written, is what
  * builds of this epoch must keep replaying. Run it when an epoch starts or a new shape
  * appears:
  *
  * {{{./mill grit.turn.test.runMain grit.turn.RecordTurnHistories}}}
  */
object RecordTurnHistories {
  import TurnFixtures.*

  def main(args: Array[String]): Unit = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch
    os.makeDir.all(dir)
    for ((name, history) <- shapes) {
      val file = dir / s"$name.json"
      if (os.exists(file)) println(s"kept    $file")
      else {
        os.write(file, ujson.write(History.write(history), indent = 2) + "\n")
        println(s"wrote   $file")
      }
    }
  }

  private def recorded(durable: InMemoryDurable, turn: TurnRef): History =
    History("turn", turn.workflowId, Turn.Epoch, "recorded", durable.history(turn.workflowId))

  /** Each shape, by name. A name whose file was written before a later step existed keeps
    * that shorter history, so a new step that changes a shape gets a new name.
    */
  private def shapes: Vector[(String, History)] = {
    val replied = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val laterTurn = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      durable.run(say(entries, "one").workflowId)(turnBody(entries, new RecordingProvider))
      val turn = say(entries, "two")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val modelFailed = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider(fail = true)))
      recorded(durable, turn)
    }
    val crashedBeforeAppend = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      try durable.run(turn.workflowId)(turnBody(new CrashOnInsert(store), new RecordingProvider))
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }
    val summarised = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      recorded(durable, turn)
    }
    val summaryFailed = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        turnBody(entries, new RecordingProvider, summarizer = new RecordingProvider(fail = true))
      )
      recorded(durable, turn)
    }
    val crashedBeforeSummaryAppend = {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Summary])
      try durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider))
      catch { case _: InMemoryDurable.Crash => "" }
      recorded(durable, turn)
    }
    val queried = {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          new RecordingProvider,
          new Noting(entries, TurnFixtures.queried),
          new InMemoryUsageLedger
        )
      )
      recorded(durable, turn)
    }
    Vector(
      "queried" -> queried,
      "summarised" -> summarised,
      "summary-failed" -> summaryFailed,
      "crashed-before-summary-append" -> crashedBeforeSummaryAppend,
      "replied" -> replied,
      "later-turn" -> laterTurn,
      "model-failed" -> modelFailed,
      "crashed-before-append" -> crashedBeforeAppend
    )
  }
}
