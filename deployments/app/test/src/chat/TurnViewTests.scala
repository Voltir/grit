package grit.app.chat

import java.time.{Instant, LocalDate}

import grit.assembly.estimate.CharEstimate
import grit.core.context.Shown
import grit.core.id.{ConversationId, EntryId, EntrySeq, PeriodSeq, TurnRef, TurnSeq}
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
import grit.core.period.{Balance, CloseReason, Closing, Flows, Probability}
import grit.core.place.Place
import grit.core.prompt.{Fragment, Layer, SystemPrompt}
import grit.core.store.{Entry, Nearby, Payload, Speakers, UsageLedger}
import grit.dbos.engine.RecordedStep
import grit.turn.Turn

import utest.*

/** A turn as the panel describes it, from recorded entries, steps and costs alone. */
object TurnViewTests extends TestSuite {

  private val c = ConversationId("c")

  /** A prompt of one base fragment, `text`. */
  private def prompt(text: String): Option[SystemPrompt] =
    Some(SystemPrompt.of(Vector(Fragment(Layer.Base, Fragment.Grit, text))))

  private def entry(seq: Long, turn: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$seq"), c, TurnSeq(turn), None, EntrySeq(seq), payload, Instant.EPOCH)

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
        Vector(EntrySeq(0), EntrySeq(1), EntrySeq(2), EntrySeq(3)),
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
        TurnView.of(
          turn2,
          entries,
          Speakers.none,
          steps,
          running = false,
          costs,
          prompt("You are grit."),
          CharEstimate
        )
      v.turn ==> TurnSeq(2)
      v.steps ==> Vector(
        TurnView.Step("assemble", Some(400)),
        TurnView.Step("call-model", Some(3100))
      )
      v.query ==> Some("first question")
      v.spent ==> Some(Cost.Exact(BigDecimal("0.0003")))
      val unpriced =
        costs.map(r => r.copy(usage = r.usage.copy(costUsd = None))).take(1) ++ costs.drop(1)
      TurnView
        .of(
          turn2,
          entries,
          Speakers.none,
          steps,
          running = false,
          unpriced,
          prompt("s"),
          CharEstimate
        )
        .spent ==>
        Some(Cost.AtLeast(BigDecimal("0.0002")))
      v.billed ==> Some(Tokens(99))
      val w = v.window.getOrElse(sys.error("no window"))
      w.recalledTurns ==> Vector(TurnSeq(0))
      w.system ==> CharEstimate.system("You are grit.")
      w.recalled ==> Seq("first question", "first answer").map(msg).reduce(_ + _)
      w.recent ==> Seq("second", "second answer").map(msg).reduce(_ + _)
      w.message ==> CharEstimate.message(Message.User("third, about the first"))
    }

    test("a window's messages count the name lines their speakers were shown with") {
      val named =
        Speakers(Map(EntryId("e0") -> "Ana", EntryId("e2") -> "Bo", EntryId("e4") -> "Ana"))
      val w = TurnView
        .of(turn2, entries, named, Vector.empty, running = false, Vector.empty, None, CharEstimate)
        .window
        .getOrElse(sys.error("no window"))
      def said(text: String) = CharEstimate.message(Message.User(text))
      w.recalled ==> said("Ana wrote:\nfirst question") + msg("first answer")
      w.recent ==> said("Bo wrote:\nsecond") + msg("second answer")
      w.message ==> said("Ana wrote:\nthird, about the first")
    }

    test("the gap lines a window was shown with count with its recent turns") {
      // Turn 3's window: turn 0 recalled, turn 2 recent; turn 1 left out between them.
      val asked = Vector(
        user(0, 0, "first question"),
        reply(1, 0, "first answer", 10),
        user(2, 1, "second"),
        reply(3, 1, "second answer", 20),
        user(4, 2, "third"),
        reply(5, 2, "third answer", 30),
        user(6, 3, "fourth, about the first"),
        entry(
          7,
          3,
          Payload.Window(
            Vector(EntrySeq(0), EntrySeq(1), EntrySeq(4), EntrySeq(5)),
            Vector(TurnSeq(0))
          )
        )
      )
      val w = TurnView
        .of(
          TurnRef(c, TurnSeq(3)),
          asked,
          Speakers.none,
          Vector.empty,
          false,
          Vector.empty,
          prompt("s"),
          CharEstimate
        )
        .window
        .getOrElse(sys.error("no window"))
      val third =
        asked
          .slice(4, 6)
          .flatMap(e => Shown.of(e, Speakers.none))
          .map(CharEstimate.message)
          .reduce(_ + _)
      w.recent ==> third + CharEstimate.message(Shown.Gap)
    }

    test("a window opened by a closing entry counts it as the model was shown it") {
      val closing = Closing(
        Flows.of("We chose exiftool.", Some("exiftool"), Vector()).getOrElse(sys.error("flows")),
        Balance.empty
      )
      val closed = entry(
        0,
        0,
        Payload.Closed(PeriodSeq.First, CloseReason.Resolved(Probability.One), closing)
      )
      val asked = Vector(
        closed,
        user(1, 1, "what did we decide?"),
        entry(2, 1, Payload.Window(Vector(EntrySeq(0)), Vector.empty)),
        reply(3, 1, "exiftool", 50)
      )
      val w = TurnView
        .of(
          TurnRef(c, TurnSeq(1)),
          asked,
          Speakers.none,
          Vector.empty,
          running = false,
          Vector.empty,
          prompt("s"),
          CharEstimate
        )
        .window
        .getOrElse(sys.error("no window"))
      val shown =
        Shown.of(closed, Speakers.none).map(CharEstimate.message).getOrElse(sys.error("not shown"))
      w.closing ==> shown
      w.recent ==> Tokens.Zero
      w.total ==> CharEstimate.system("s") + shown + CharEstimate.message(
        Message.User("what did we decide?")
      )
    }

    test("a window's nearby sections are counted as the model was shown them, listed by place") {
      val api = ConversationId("api")
      val web = ConversationId("web")
      def place(written: String) = Place.read(written).fold(e => sys.error(e), identity)
      val apiAt = place("fs:/home/nick/api")
      val webAt = place("fs:/home/nick/web")
      def their(id: String, conversation: ConversationId, turn: Long, seq: Long, text: String) =
        Entry(
          EntryId(id),
          conversation,
          TurnSeq(turn),
          None,
          EntrySeq(seq),
          Payload.Message(Message.User(text)),
          Instant.EPOCH
        )
      val fromApi = Vector(their("a0", api, 3, 0, "flaky?"), their("a1", api, 3, 1, "TZ"))
      val fromWeb = Vector(their("w0", web, 5, 0, "the login page is blank"))
      // The window lists web before api, so neither conversation nor place order agrees with
      // it; web's seq 9 is one the window named whose entry was not read back, and both
      // conversations have an entry at seq 0.
      val asked = Vector(
        user(0, 0, "which fix?"),
        entry(
          1,
          0,
          Payload.Window(
            Vector.empty,
            Vector.empty,
            Vector(
              Nearby.Open(web, webAt, Vector(EntrySeq(0), EntrySeq(9))),
              Nearby.Open(api, apiAt, Vector(EntrySeq(0), EntrySeq(1)))
            )
          )
        ),
        reply(2, 0, "TZ=UTC", 50)
      )
      val w = TurnView
        .of(
          TurnRef(c, TurnSeq(0)),
          asked,
          Speakers.none,
          Vector.empty,
          running = false,
          Vector.empty,
          prompt("s"),
          CharEstimate,
          None,
          fromApi ++ fromWeb
        )
        .window
        .getOrElse(sys.error("no window"))
      def shown(at: Place, es: Vector[Entry]) =
        Shown.nearby(at, es).map(CharEstimate.message).getOrElse(sys.error("not shown"))
      val sections = shown(webAt, fromWeb) + shown(apiAt, fromApi)
      w.nearby ==> sections
      w.total ==> CharEstimate.system("s") + sections + CharEstimate.message(
        Message.User("which fix?")
      )
      w.nearbyTurns ==> Vector(
        TurnView.Near.Turns(webAt, Vector(TurnSeq(5))),
        TurnView.Near.Turns(apiAt, Vector(TurnSeq(3)))
      )
      TurnView.Near.shown(w.nearbyTurns) ==> "web turn 5 · api turn 3"
    }

    test("a closed section is counted as its record, and listed as a record") {
      val ops = ConversationId("ops")
      val opsAt = Place.read("slack:T1/C1/2.0").fold(e => sys.error(e), identity)
      val kept = Entry(
        EntryId("k"),
        ops,
        TurnSeq(4),
        None,
        EntrySeq(5),
        Payload.Closed(
          grit.core.id.PeriodSeq.First,
          grit.core.period.CloseReason.Lapsed,
          Closing(
            Flows.of("Froze deploys.", None, Vector.empty).getOrElse(sys.error("flows")),
            Balance.empty
          )
        ),
        Instant.EPOCH
      )
      val asked = Vector(
        user(0, 0, "when is the freeze?"),
        entry(
          1,
          0,
          Payload
            .Window(Vector.empty, Vector.empty, Vector(Nearby.Closed(ops, opsAt, EntrySeq(5))))
        ),
        reply(2, 0, "Friday", 50)
      )
      val w = TurnView
        .of(
          TurnRef(c, TurnSeq(0)),
          asked,
          Speakers.none,
          Vector.empty,
          running = false,
          Vector.empty,
          prompt("s"),
          CharEstimate,
          None,
          Vector(kept)
        )
        .window
        .getOrElse(sys.error("no window"))
      val record = grit.core.store.ClosingEntry.of(kept).getOrElse(sys.error("closing"))
      w.nearby ==> CharEstimate.message(Shown.recorded(opsAt, record))
      TurnView.Near.shown(w.nearbyTurns) ==> "2.0 record"
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
            Assignment(flash, 1024, None),
            Assignment(oss, 1024, None)
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
        Speakers.none,
        Vector.empty,
        running = false,
        Vector.empty,
        prompt("s"),
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
          Speakers.none,
          Vector.empty,
          running = false,
          Vector.empty,
          prompt("s"),
          CharEstimate,
          Some(bare)
        )
        .models ==>
        Some(TurnView.Models(Vector("model" -> oss), profiled = false, None))
      TurnView
        .of(
          turn2,
          entries,
          Speakers.none,
          Vector.empty,
          running = false,
          Vector.empty,
          prompt("s"),
          CharEstimate
        )
        .models ==> None
    }

    test("settled once it has stopped with its summary recorded; not while either is missing") {
      def of(steps: Vector[String], running: Boolean) = TurnView.of(
        turn2,
        entries,
        Speakers.none,
        steps.map(step(_, 0, 1)),
        running,
        Vector.empty,
        prompt("s"),
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
        TurnView.of(
          turn2,
          entries.take(5),
          Speakers.none,
          steps,
          running = true,
          Vector.empty,
          prompt("s"),
          CharEstimate
        )
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
        Speakers.none,
        Vector(step("assemble", 0, 10)),
        running = true,
        Vector.empty,
        prompt("s"),
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
