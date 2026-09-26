package grit.app.chat

import java.time.{Instant, LocalDate}

import grit.assembly.estimate.CharEstimate
import grit.core.context.Shown
import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.model.{
  Assignment,
  Catalog,
  Known,
  ModelId,
  ModelRef,
  Policy,
  Profile,
  Source,
  StrictSchemas,
  Upstream
}
import grit.core.period.{CloseReason, Closing, Probability}
import grit.core.store.{Entry, Payload, UsageLedger}
import grit.dbos.engine.RecordedStep
import grit.turn.Turn

import utest.*

/** A turn as the panel describes it, from recorded entries, steps and costs alone. */
object TurnViewTests extends TestSuite {

  private val c = ConversationId("c")

  private def entry(seq: Long, turn: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$seq"), c, TurnSeq(turn), None, seq, payload, Instant.EPOCH)

  private def user(seq: Long, turn: Long, text: String) =
    entry(seq, turn, Payload.Message(Message.User(text)))

  private def reply(seq: Long, turn: Long, text: String, input: Long) = entry(
    seq,
    turn,
    Payload.Message(
      Message.Assistant(
        Vector(AssistantBlock.Text(text)),
        StopReason.EndTurn,
        Usage(Tokens(input), Tokens(3), Tokens.Zero, Some(BigDecimal("0.0002"))),
        "m"
      )
    )
  )

  private def step(name: String, from: Long, to: Long) =
    RecordedStep(name, Some(Instant.ofEpochMilli(from)), Some(Instant.ofEpochMilli(to)))

  /** Turn 0 and turn 1 answered, then turn 2 asked with a window of turn 1 (recent) and
    * turn 0 (recalled).
    */
  private val entries: Vector[Entry] = Vector(
    user(0, 0, "first question"),
    reply(1, 0, "first answer", 10),
    user(2, 1, "second"),
    reply(3, 1, "second answer", 20),
    user(4, 2, "third, about the first"),
    entry(5, 2, Payload.Query("first question")),
    entry(
      6,
      2,
      Payload.Window(
        Vector(EntryId("e0"), EntryId("e1"), EntryId("e2"), EntryId("e3")),
        Vector(TurnSeq(0))
      )
    ),
    reply(7, 2, "the answer", 99)
  )

  private val turn2 = TurnRef(c, TurnSeq(2))

  val tests = Tests {
    test("the latest turn is the one the last user message started") {
      TurnView.latest(entries) ==> Some(turn2)
      TurnView.latest(Vector.empty) ==> None
    }

    test("a finished turn: its steps timed, its query, its window split, its cost") {
      val steps = Vector(step("assemble", 0, 400), step("call-model", 400, 3500))
      val costs = Vector(
        UsageLedger.Row(
          EntryId("q"),
          "w",
          Usage(Tokens(5), Tokens(1), Tokens.Zero, Some(BigDecimal("0.0001"))),
          Tokens(4)
        ),
        UsageLedger.Row(
          EntryId("e7"),
          "m",
          Usage(Tokens(99), Tokens(3), Tokens.Zero, Some(BigDecimal("0.0002"))),
          Tokens(90)
        )
      )
      val v =
        TurnView.of(turn2, entries, steps, running = false, costs, "You are grit.", CharEstimate)
      v.turn ==> TurnSeq(2)
      v.steps ==> Vector(
        TurnView.Step("assemble", Some(400)),
        TurnView.Step("call-model", Some(3100))
      )
      v.query ==> Some("first question")
      v.spent ==> Some(Cost.Exact(BigDecimal("0.0003")))
      val unpriced =
        costs.map(r => r.copy(usage = r.usage.copy(costUsd = None))).take(1) ++ costs.drop(1)
      TurnView.of(turn2, entries, steps, running = false, unpriced, "s", CharEstimate).spent ==>
        Some(Cost.AtLeast(BigDecimal("0.0002")))
      v.billed ==> Some(Tokens(99))
      val w = v.window.getOrElse(sys.error("no window"))
      w.recalledTurns ==> Vector(TurnSeq(0))
      w.system ==> CharEstimate.system("You are grit.")
      w.recalled ==> Seq("first question", "first answer").map(msg).reduce(_ + _)
      w.recent ==> Seq("second", "second answer").map(msg).reduce(_ + _)
      w.message ==> CharEstimate.message(Message.User("third, about the first"))
    }

    test("a window opened by a closing entry counts it as the model was shown it") {
      val closing = Closing
        .of(
          "We chose exiftool.",
          Some("exiftool"),
          Vector("exiftool"),
          Vector(),
          Vector(),
          Vector()
        )
        .getOrElse(sys.error("closing"))
      val closed = entry(
        0,
        0,
        Payload.Closed(PeriodSeq.First, CloseReason.Resolved(Probability.One), closing)
      )
      val asked = Vector(
        closed,
        user(1, 1, "what did we decide?"),
        entry(2, 1, Payload.Window(Vector(EntryId("e0")), Vector.empty)),
        reply(3, 1, "exiftool", 50)
      )
      val w = TurnView
        .of(
          TurnRef(c, TurnSeq(1)),
          asked,
          Vector.empty,
          running = false,
          Vector.empty,
          "s",
          CharEstimate
        )
        .window
        .getOrElse(sys.error("no window"))
      val shown = Shown.of(closed).map(CharEstimate.message).getOrElse(sys.error("not shown"))
      w.closing ==> shown
      w.recent ==> Tokens.Zero
      w.total ==> CharEstimate.system("s") + shown + CharEstimate.message(
        Message.User("what did we decide?")
      )
    }

    test("what its calls were made under: the turn's pair, a role on another, and who served it") {
      def ref(model: String, upstream: Option[String]) =
        ModelRef(
          ModelId.of(model).getOrElse(sys.error(model)),
          upstream.map(u => Upstream.of(u).getOrElse(sys.error(u)))
        )
      val oss = ref("openai/gpt-oss-120b", Some("cerebras/fp16"))
      val flash = ref("deepseek/deepseek-v4.1-flash-20260910", Some("fireworks"))
      val profiled = Profile(
        oss,
        strict = Known.Of(StrictSchemas.Ignored, Source.Declared("nick", LocalDate.of(2026, 9, 25)))
      )
      val pinned = Catalog
        .of(
          Policy(
            Assignment(oss, 4096, None),
            Assignment(oss, 1024, None),
            Assignment(flash, 1024, None)
          ),
          Vector(profiled)
        )
        .pin
      val served = entries.map {
        case e @ Entry(_, _, _, _, _, Payload.Message(m: Message.Assistant), _)
            if e.turnSeq == TurnSeq(2) =>
          e.copy(payload = Payload.Message(m.copy(upstream = Some("Cerebras"))))
        case e => e
      }
      val v = TurnView.of(
        turn2,
        served,
        Vector.empty,
        running = false,
        Vector.empty,
        "s",
        CharEstimate,
        Some(pinned)
      )
      v.models ==> Some(
        TurnView.Models(Vector("model" -> oss, "query" -> flash), profiled = true, Some("Cerebras"))
      )
      // No profile for the pair, and no upstream named: unprofiled, served by none said.
      val bare = Catalog
        .of(
          Policy(
            Assignment(oss, 4096, None),
            Assignment(oss, 1024, None),
            Assignment(oss, 1024, None)
          ),
          Vector.empty
        )
        .pin
      TurnView
        .of(
          turn2,
          entries,
          Vector.empty,
          running = false,
          Vector.empty,
          "s",
          CharEstimate,
          Some(bare)
        )
        .models ==>
        Some(TurnView.Models(Vector("model" -> oss), profiled = false, None))
      TurnView
        .of(turn2, entries, Vector.empty, running = false, Vector.empty, "s", CharEstimate)
        .models ==> None
    }

    test("settled once it has stopped with its summary recorded; not while either is missing") {
      def of(steps: Vector[String], running: Boolean) = TurnView.of(
        turn2,
        entries,
        steps.map(step(_, 0, 1)),
        running,
        Vector.empty,
        "s",
        CharEstimate
      )
      val summarised = Vector("assemble", "call-model", Turn.Step.AppendSummary)
      of(summarised, running = false).settled ==> true
      // Still running, though its last step is recorded.
      of(summarised, running = true).settled ==> false
      // Stopped short of the summary, as a failed turn does.
      of(summarised.dropRight(1), running = false).settled ==> false
    }

    test("a person's wait is its own step, timed from the ask to the answer") {
      val asked = Vector(step("record-call:0", 0, 10), step("ask:0:0", 10, 20))
      def of(steps: Vector[RecordedStep]) =
        TurnView.of(turn2, entries.take(5), steps, running = true, Vector.empty, "s", CharEstimate)
      // DBOS records the wait's end as it begins; the wait itself only when it ends.
      val waiting = of(asked :+ step("DBOS.sleep", 21, 21))
      waiting.running ==> Some("wait:0:0")
      val answered = of(asked ++ Vector(step("DBOS.recv", 21, 234020), step("DBOS.sleep", 21, 21)))
      answered.running ==> Some("tool:0:0")
      answered.steps.map(s => (s.name, s.ms)) ==> Vector(
        ("record-call:0", Some(10L)),
        ("ask:0:0", Some(10L)),
        ("wait:0:0", Some(234000L)),
        ("DBOS.sleep", Some(0L))
      )
    }

    test("a running turn is in the step after its last recorded one, before any window") {
      val asked = entries.take(5)
      val v = TurnView.of(
        turn2,
        asked,
        Vector(step("assemble", 0, 10)),
        running = true,
        Vector.empty,
        "s",
        CharEstimate
      )
      v.running ==> Some("record-window")
      v.window ==> None
      v.spent ==> None
      v.billed ==> None
    }
  }

  /** The estimate of a message whose text is `text`, as the fixtures above write it. */
  private def msg(text: String): Tokens =
    entries
      .collectFirst {
        case Entry(_, _, _, _, _, Payload.Message(m @ Message.User(`text`)), _) => m: Message
        case Entry(_, _, _, _, _, Payload.Message(m: Message.Assistant), _)
            if m.blocks == Vector(AssistantBlock.Text(text)) =>
          m: Message
      }
      .map(CharEstimate.message)
      .getOrElse(sys.error(s"no message $text"))
}
