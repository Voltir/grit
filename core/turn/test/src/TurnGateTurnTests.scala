package grit.turn

import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, EntryId, EntrySeq, PeriodSeq, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.Place
import grit.core.provider.ModelRequest
import grit.core.speech.Speaking
import grit.core.store.{Db, Entry, InMemoryEntryStore, InMemoryUsageLedger, Nearby, Payload}
import grit.core.tool.Toolbox
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

  /** What `request` shows as the result of the call `id`; `None` when it shows none. */
  private def resultOf(request: ModelRequest, id: String): Option[String] =
    request.messages.collectFirst {
      case Message.ToolResult(call, content, _) if ToolCallId.value(call) == id => content
    }

  /** The stub peek's result `t1` is shown as while hidden. */
  private val peekStub = "Hidden here as h1: something from peek, believed only at " +
    "[integrity: untrusted]. Reading it would withdraw post for the rest of this turn; to read " +
    "it, call read_hidden with name h1."

  /** `turn` answered by `provider` with [[gatedTools]] offered over `files`. */
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
      "a result whose reading would withdraw a tool is shown as its stub, the tool still offered"
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
      provider.requests.map(_.tools.map(_.name)) ==> Vector(
        Vector("peek", "post"),
        Vector("read_hidden", "peek", "post"),
        Vector("read_hidden", "peek", "post")
      )
      provider.requests.map(resultOf(_, "t1")) ==> Vector(None, Some(peekStub), Some(peekStub))
      // Never having read the result, the turn may post.
      kept(entries, "result:c1:0:1:0").map(_.payload) ==> Some(
        Payload.Result(Message.ToolResult(ToolCallId("t2"), "posted", false), "post a.txt")
      )
    }

    test(
      "a stub read through read_hidden is shown in full next round, which withdraws what it named"
    ) {
      val entries = new InMemoryEntryStore
      val provider = hidingScript
      val turn = say(entries, "read and post")
      gatedRun(new InMemoryDurable, entries, turn)(provider) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      provider.requests.map(_.tools.map(_.name)) ==> Vector(
        Vector("peek", "post"),
        Vector("read_hidden", "peek", "post"),
        Vector("read_hidden", "peek"),
        Vector("read_hidden", "peek")
      )
      provider.requests.map(resultOf(_, "t1")) ==>
        Vector(None, Some(peekStub), Some("alpha"), Some("alpha"))
      provider.requests.lift(2).flatMap(resultOf(_, "t2")) ==>
        Some("h1 is shown in full in its place from your next request on.")
      val withdrawn = "Withdrawn for the rest of this turn: post, which requires " +
        "[integrity: member], since you read peek [integrity: untrusted]."
      provider.requests.map(_.system.endsWith(s"\n\n$withdrawn")) ==>
        Vector(false, false, true, false)
    }

    test("a call refused before it runs is answered at the turn's level, and hides nothing") {
      val entries = new InMemoryEntryStore
      val provider = new Scripted((_, n) =>
        Right(
          if (n == 0) said("", AssistantBlock.ToolCall(ToolCallId("t1"), "grep", ujson.Obj()))
          else said("done")
        )
      )
      val turn = say(entries, "grep")
      gatedRun(new InMemoryDurable, entries, turn)(provider)
      kept(entries, "result:c1:0:0:0").map(_.integrity) ==> Some(Integrity.Member)
      provider.requests.lift(1).flatMap(resultOf(_, "t1")) ==>
        Some("There is no tool named `grep`; the tools are `peek`, `post`.")
    }

    test("a call of a withheld tool is answered that it is withheld, and nothing runs") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "read and post")
      gatedRun(new InMemoryDurable, entries, turn)(hidingScript)
      kept(entries, "result:c1:0:2:0").map(_.payload) ==> Some(
        Payload.Result(
          Message.ToolResult(
            ToolCallId("t3"),
            "The call to `post` was not run: it needs what this turn has read believed at " +
              "[integrity: member] or above, and this turn has read something believed only " +
              "at [integrity: untrusted].",
            isError = true
          ),
          "post"
        )
      )
    }

    test(
      "the turn's rows after a round are believed at its level then, a window's item once read"
    ) {
      val entries = new InMemoryEntryStore
      guest(entries, "hello from outside")
      val turn = say(entries, "read and post")
      // The guest's message is hidden as h1 and read in the second round; peek's result, hidden
      // as h2, costs nothing once post is withheld, and is shown in the third.
      val provider = hidingScript
      gatedRun(new InMemoryDurable, entries, turn)(provider)
      val guestStub = "Hidden here as h1: something from someone, believed only at " +
        "[integrity: outside]. Reading it would withdraw post for the rest of this turn; to " +
        "read it, call read_hidden with name h1."
      provider.requests.map(_.messages.headOption) ==> Vector(
        Some(Message.User(guestStub)),
        Some(Message.User(guestStub)),
        Some(Message.User("hello from outside")),
        Some(Message.User("hello from outside"))
      )
      Vector("call:c1:1:0", "call:c1:1:1", "call:c1:1:2", "reply:c1:1", "summary:c1:1")
        .map(kept(entries, _).map(_.integrity)) ==>
        Vector(
          Some(Integrity.Member),
          Some(Integrity.Member),
          Some(Integrity.Untrusted),
          Some(Integrity.Untrusted),
          Some(Integrity.Untrusted)
        )
    }

    test(
      "a nearby message and a closed conversation's record are hidden as stubs in their places"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      given grit.core.store.Tx = TestTx.fake
      val (api, ops) = (ConversationId("api"), ConversationId("ops"))
      val _ = entries.insert(
        Entry(
          EntryId("api:u"),
          api,
          TurnSeq(0),
          None,
          EntrySeq(0),
          Payload.Message(Message.User("the invoice test is flaky")),
          java.time.Instant.EPOCH,
          Integrity.Outside
        )
      )
      val _ = entries.insert(
        Entry(
          EntryId("ops:closing"),
          ops,
          TurnSeq(4),
          None,
          EntrySeq(5),
          Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("Froze deploys.")),
          java.time.Instant.EPOCH,
          Integrity.Outside
        )
      )
      val turn = say(entries, "two")
      val at = (p: String) => Place.read(p).fold(e => sys.error(e), identity)
      val nearby = Vector(
        Nearby.Open(api, at("fs:/home/nick/api"), Vector(EntrySeq(0))),
        Nearby.Closed(ops, at("slack:T1/C1/2.0"), EntrySeq(5))
      )
      val opening = new ContextAssembler {
        def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
          Right(Window(Vector.empty, Vector.empty, nearby))
      }
      val posting = Toolbox
        .of[caps.CapSet^{NoCheckout}](hostedPost.over(_ => grit.core.tool.Outcome.Done("posted")))
        .fold(d => throw new java.lang.AssertionError(d), identity)
      new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(entries, provider, opening, new InMemoryUsageLedger, tools = posting)
      )
      def stub(name: String, source: String) =
        s"Hidden here as $name: something from $source, believed only at " +
          "[integrity: outside]. Reading it would withdraw post for the rest of this turn; to " +
          s"read it, call read_hidden with name $name."
      provider.requests.headOption.map(_.messages.take(2)) ==> Some(
        Vector(
          Message.User(
            "[afar] another conversation, shown by grit, still open, at fs:/home/nick/api:\n" +
              s"User: ${stub("h1", "someone")}"
          ),
          Message.User(stub("h2", "a closing record"))
        )
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
          Message.ToolResult(ToolCallId("t2"), "posted", false),
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
