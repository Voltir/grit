package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, TurnRef}
import grit.core.message.Message
import grit.core.speech.Speaking
import grit.core.store.{Entry, InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.core.visibility.Integrity
import grit.dbos.sql.TestTx
import grit.models.StubProvider

import utest.*

/** The turn's integrity inside the durable turn ([[TurnGate]]): a `gate:n` step before each
  * model call, the tools its level withholds out of the request and refused when called, and
  * the rows the turn writes after believed at its level.
  */
object TurnGateTurnTests extends TestSuite {
  import TurnFixtures.*

  private val files = Map("a.txt" -> "alpha")

  private def kept(entries: InMemoryEntryStore, id: String): Option[Entry] =
    entries
      .list(conversation)(using TestTx.fake)
      .getOrElse(Vector.empty)
      .find(e => EntryId.value(e.id) == id)

  /** `text`, said by a guest, believed at [[Integrity.Outside]], as a turn of its own. */
  private def guest(entries: InMemoryEntryStore, text: String): Unit = {
    given grit.core.store.Tx = TestTx.fake
    val next = entries.lockNext(conversation).getOrElse(sys.error("in-memory store"))
    val _ = entries.insert(
      Entry(
        EntryId(s"guest:$text"),
        conversation,
        next.turnSeq,
        None,
        next.seq,
        Payload.Message(Message.User(text)),
        java.time.Instant.EPOCH,
        Integrity.Outside
      )
    )
  }

  /** `turn` answered by [[gatedScript]] with [[gatedTools]] offered over `files`. */
  private def gatedRun(durable: InMemoryDurable, entries: InMemoryEntryStore, turn: TurnRef)(
      provider: Scripted
  ): String = {
    val ws = new Files(files)
    durable.run(turn.workflowId)(
      tooledBody(
        entries,
        provider,
        new InMemoryUsageLedger,
        new StubProvider(),
        NoClassifier,
        ws,
        gatedTools(ws),
        5
      )
    )
  }

  val tests = Tests {
    test(
      "a result below a tool's requirement withholds it from the next round, which is told why"
    ) {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = gatedScript
      val turn = say(entries, "read and post")
      gatedRun(durable, entries, turn)(provider) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "record-window").drop(1) ==> Vector(
        "DBOS.patch-integrity",
        "gate:0",
        "call-model",
        "record-call:0",
        "tool:0:0",
        "gate:1",
        "call-model:1",
        "record-call:1",
        "tool:1:0",
        "gate:2",
        "call-model:2",
        "append",
        "summarise",
        "append-summary"
      )
      provider.requests.map(_.tools.map(_.name)) ==>
        Vector(Vector("peek", "post"), Vector("peek"), Vector("peek"))
      val withdrawn = "Withdrawn for the rest of this turn: post, which requires " +
        "[integrity: member], since you read peek [integrity: untrusted]."
      provider.requests.map(_.system.endsWith(s"\n\n$withdrawn")) ==> Vector(false, true, false)
    }

    test("a call of a withheld tool is answered that it is withheld, and nothing runs") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "read and post")
      gatedRun(new InMemoryDurable, entries, turn)(gatedScript)
      kept(entries, "result:c1:0:1:0").map(_.payload) ==> Some(
        Payload.Result(
          Message.ToolResult(
            grit.core.id.ToolCallId("t2"),
            "The call to `post` was not run: it needs what this turn has read believed at " +
              "[integrity: member] or above, and this turn has read something believed only " +
              "at [integrity: untrusted].",
            isError = true
          ),
          "post"
        )
      )
    }

    test("the turn's rows after a round are believed at its level then, its window read") {
      val entries = new InMemoryEntryStore
      guest(entries, "hello from outside")
      val turn = say(entries, "read and post")
      gatedRun(new InMemoryDurable, entries, turn)(gatedScript)
      Vector("call:c1:1:0", "call:c1:1:1", "reply:c1:1", "summary:c1:1")
        .map(kept(entries, _).map(_.integrity)) ==>
        Vector(
          Some(Integrity.Outside),
          Some(Integrity.Untrusted),
          Some(Integrity.Untrusted),
          Some(Integrity.Untrusted)
        )
    }

    test("a heard root's turn is believed at most Outside, however its author is") {
      val w = speechWorld()
      new InMemoryDurable().run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new RecordingProvider,
          new Before(w.entries),
          w.ledger,
          new Judge(Some((0.9, 0.9))),
          TurnSpeech(Speaking.Within(speechLimits), w.store, w.deliveries)
        )
      )
      kept(w.entries, EntryId.value(w.turn.draftId)).map(_.integrity) ==> Some(Integrity.Outside)
    }

    test("a turn that passed the gate's point before it shipped finishes ungated") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable(unpatched = Set(Turn.Patches.Integrity))
      val provider = gatedScript
      val turn = say(entries, "read and post")
      gatedRun(durable, entries, turn)(provider)
      durable.recordedSteps(turn.workflowId).filter(_.startsWith("gate")) ==> Vector.empty
      provider.requests.map(_.tools.map(_.name)) ==> Vector.fill(3)(Vector("peek", "post"))
      kept(entries, "result:c1:0:1:0").map(_.payload) ==> Some(
        Payload.Result(
          Message.ToolResult(grit.core.id.ToolCallId("t2"), "posted", false),
          "post a.txt"
        )
      )
    }

    test("a running turn after a gate is in the model call it gates") {
      Turn.running(Vector("record-window", "DBOS.patch-integrity", "gate:0")) ==> "call-model"
      Turn.running(Vector("call-model", "record-call:0", "tool:0:0", "gate:1")) ==> "call-model:1"
      Turn.Step.family("gate:3") ==> Some(Turn.Step.Gate)
    }
  }
}
