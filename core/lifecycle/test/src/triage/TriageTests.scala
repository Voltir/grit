package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Classifier, Question}
import grit.core.durable.InMemoryDurable
import grit.core.edge.Acknowledgement
import grit.core.id.{ConversationId, EntryId, QuestionName, TriageRef, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{Judgement, Probability, Verdict}
import grit.core.speech.{Decision, Limits, Outcome, Silence, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{Opening, Stitching, Tuning}
import grit.core.store.Tx
import grit.core.triage.{Bound, Corpora, Cues, Earning, Gate, Reading, Tags}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

object TriageTests extends TestSuite {
  import grit.lifecycle.shadow.ShadowFixtures.{Catalog, Github}

  import TriageFixtures.*

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  private def name(text: String) = QuestionName.read(text).getOrElse(sys.error(text))

  /** v5 answered: gap closes at 0.75; open 0.125, to 0.25, to-grit 0.125, durable 0.875,
    * anchor-record 0.375; any source 0.5.
    */
  private def decided =
    new Scripted(
      Vector(0.0, 0.125, 0.75, 0.0, 0.125),
      Vector(0.125, 0.25, 0.125, 0.875, 0.375, 0.5)
    )

  /** v5 answered: gap asks at 1.0; open 0.9, to 0.125, to-grit 0.125, durable 0.5,
    * anchor-record 0.125: past its gate.
    */
  private def asking =
    new Scripted(Vector(1, 0, 0, 0, 0), Vector(0.9, 0.125, 0.125, 0.5, 0.125))

  /** v5 answered as [[asking]], but to-grit 0.875: put to grit by name, past its gate. */
  private def named =
    new Scripted(Vector(1, 0, 0, 0, 0), Vector(0.9, 0.125, 0.875, 0.5, 0.125))

  private val limits =
    Limits.suggested(
      DailyCap.of("0.25").getOrElse(sys.error("a cap")),
      TriageQuestions.ShippedSpeak
    )

  private val within = Speaking.Within(limits)

  /** What [[decided]] answers v5 with no source: gap a choice weighing every key, then its
    * yes/nos.
    */
  private val decision: Tags.Weighed = Tags.Weighed(
    VectorMap(
      name("gap") -> Answer
        .choice(
          Vector(
            Answer.Weight("asks", 0.0),
            Answer.Weight("owes", 0.125),
            Answer.Weight("closes", 0.75),
            Answer.Weight("states", 0.0),
            Answer.Weight("nothing", 0.125)
          )
        )
        .getOrElse(sys.error("a choice")),
      name("open") -> Answer.YesNo(0.125),
      name("to") -> Answer.YesNo(0.25),
      name("to-grit") -> Answer.YesNo(0.125),
      Earning.Durable -> Answer.YesNo(0.875),
      name("anchor-record") -> Answer.YesNo(0.375)
    ),
    "jev-1.13.0",
    Spent
  )

  /** What a triage logs of [[decision]]. */
  private val Decided =
    "tagged: gap closes 0.75, open 0.125, to 0.25, to-grit 0.125, durable 0.875, anchor-record 0.375 (jev-1.13.0)"

  /** Someone a heard message may name. */
  private val Bo = grit.core.identity.TestAccounts.account("test:u-Bo")

  /** The heard entry of `triage`, in `in`. */
  private def entryOf(w: World, in: ConversationId, triage: TriageRef)(using Tx): EntryId =
    w.entries
      .list(in)
      .toOption
      .flatMap(_.find(_.turnSeq == triage.turn))
      .fold(sys.error("no entry"))(_.id)

  /** What a triage logs of the placement of a room's first opening, which nothing is offered. */
  private val Unoffered = "placed: nothing asked; "

  val tests = Tests {
    test(
      "a triage asks live's question set about the state TriageInput.build makes, with the sources covering its conversation"
    ) {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.say("unrelated", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      val requests = new Requests
      new InMemoryDurable().run(t.workflowId)(w.body(recording(requests), 5, sources = Catalog))
      val read =
        TriageInput.read(
          w.reads,
          w.rooms,
          w.documents,
          FakeDb.as(Subject.Public),
          t.message,
          Tuning.Default,
          TriageRecipe.Shipped
        )
      read.map(_.state.author) ==> Right("Ana")
      requests.sent ==> read.toOption.toVector.map { r =>
        TriageQuestions
          .shipped(grit.core.persona.Persona.Grit)
          .request(
            r.state,
            Corpora.of(Vector(Github)).getOrElse(sys.error("one"))
          )
      }
    }

    test(
      "live asks v5: gap among five, open, to, to-grit, durable and anchor-record, then one yes/no per source covering the conversation"
    ) {
      val w = new World
      val t = w.hear("can someone send me the Q3 deck?", "Ana", 0)
      val classifier = decided
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 5, sources = Catalog))
      classifier.asked.map {
        case q: Question.Choice => q.keys.map(_.name).mkString("|")
        case Question.YesNo(words, _, _) if words.contains(Github.line) => "github"
        case _: Question.YesNo => "yes/no"
        case _: Question.Score => "score"
      } ==> Vector(
        "asks|owes|closes|states|nothing",
        "yes/no",
        "yes/no",
        "yes/no",
        "yes/no",
        "yes/no",
        "github"
      )
      w.tags(t).collect { case Tags.Weighed(answers, _, _) =>
        answers.keys.toVector.map(QuestionName.value)
      } ==> Some(
        Vector(
          "gap",
          "open",
          "to",
          "to-grit",
          "durable",
          "anchor-record",
          "source:github"
        )
      )
    }

    test("a heard message is asked once, in one call, and its tags kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val classifier = decided
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 5)) ==>
        Unoffered + Decided + """; held: {"kind":"off"}"""
      (classifier.calls, durable.recordedSteps(t.workflowId)) ==>
        (1, Vector("stitched", "ask:triage", "ask:triage:record", "record", "consider"))
      w.tags(t) ==> Some(decision)
    }

    test("a resumed triage asks nothing again: the recorded answer is kept") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val first = new InMemoryDurable
      first.run(t.workflowId)(w.body(decided, 5))
      val history = first.history(t.workflowId).take(2)
      // Resumed after the ask was recorded: a classifier that now answers otherwise is not called.
      val again = new World
      val t2 = again.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val other = new Scripted(Vector(0, 0, 0, 0, 1), Vector(0, 0, 0))
      new InMemoryDurable().replay(t2.workflowId, history)(again.body(other, 5)) ==>
        Right(
          Unoffered + Decided + """; held: {"kind":"off"}"""
        )
      (other.calls, again.tags(t2)) ==> (0, Some(decision))
    }

    test("a thread too long to show whole leaves out its oldest messages, counted, whole") {
      val w = new World
      (0 until 40).foreach(i => w.hear(f"message $i%02d " + "x" * 80, "Ben", i.toLong))
      val t = w.hear("the last", "Ana", 41)
      val read =
        TriageInput.read(
          w.reads,
          w.rooms,
          w.documents,
          FakeDb.as(Subject.Public),
          t.message,
          Tuning.Default,
          TriageRecipe.Shipped
        )
      // Forty lines of "Ben: message nn " and 80 x's, a blank line between each two: the `…`
      // line and twenty fit in 2,000 characters.
      read.map(r => (r.state.thread.cut.size, r.state.thread.text.length)) ==> Right((20, 1961))
      read.map(r => (r.state.thread.cut ++ r.state.thread.shown).map(_.text.take(10))) ==>
        Right((0 until 40).map(i => f"message $i%02d").toVector)
      read.map(r => (r.entry, r.state)) ==> TriageInput.build(
        w.reads,
        w.rooms,
        w.documents,
        FakeDb.as(Subject.Public),
        t.message,
        Tuning.Default,
        TriageRecipe.Shipped
      )
    }

    test("the classifier is shown the message, who said it, and the thread before it") {
      val w = new World
      w.hear("when is standup?", "Ben", 0)
      w.say("unrelated", 1)
      val t = w.hear("standup moves to 10:00", "Ana", 2)
      w.hear("after it", "Ben", 3)
      val classifier = decided
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 5))
      classifier.states ==> Vector(
        ujson.Obj(
          "new_message" -> "standup moves to 10:00",
          "author" -> "Ana",
          "thread" -> ujson.Arr(
            ujson.Obj("from" -> "Ben", "text" -> "when is standup?", "ago" -> "2 minutes"),
            ujson.Obj("from" -> "User", "text" -> "unrelated", "ago" -> "1 minute")
          )
        )
      )
    }

    test(
      "a first message's triage waits for its placement, then is asked with the message it follows"
    ) {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      // Exchange 1 at 0.9, then a question at 0.9: the one scripted weight answers both.
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(classifier, 2))
      durable.recordedSteps(t.workflowId).take(2) ==> Vector("stitched", "ask:triage")
      w.awaited.map(_.ref.turn) ==> Vector(TurnRef(b, t.turn))
      classifier.states.lastOption.map(_("earlier_exchange")("opening")("text").str) ==>
        Some("where did we land on the Engine contract term?")
      w.stitches.links(Vector(b))(using grit.dbos.sql.TestTx.fake) ==>
        Right(Vector(grit.core.stitch.Link(b, c)))
    }

    test("a reply's triage waits for no placement") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val reply = w.hear("the twelve-month one", "Ana", 1)
      val durable = new InMemoryDurable
      durable.run(reply.workflowId)(
        w.body(new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2)), 2)
      )
      durable.recordedSteps(reply.workflowId).take(2) ==> Vector("stitched", "ask:triage")
      w.awaited ==> Vector.empty
    }

    test("a heard message is asked about no sooner than Quiet after it was said") {
      val w = new World
      val fresh = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      val older = w.hear("and the retro to Friday", "Bo", 1)
      // Triaged as said, and once its quiet has passed: the first waits 6 s, the second not at all.
      new InMemoryDurable().run(fresh.workflowId)(w.body(decided, 0))
      new InMemoryDurable().run(older.workflowId)(w.body(decided, 2))
      w.askedAt ==> Vector(at(0).plusSeconds(6), at(2))
    }

    test(
      "a second heard message in the thread, queued behind the first, holds the first Continued, and is weighed itself"
    ) {
      val w = new World
      val first = w.hear("is the deploy frozen?", "Ana", 0)
      val second = w.hear("for the release branch, I mean", "Ana", 0, seconds = 2)
      // The conversation's triages run one at a time, in the order heard.
      new InMemoryDurable().run(first.workflowId)(w.body(asking, 0, within))
      new InMemoryDurable().run(second.workflowId)(w.body(asking, 0, within))
      w.speech.decisions.map((h, d, at) => (h.turn, d, at)) ==> Vector(
        (
          first.message,
          Decision.Held(Silence.Continued(entryOf(w, c, second)(using TestTx.fake))),
          at(0).plusSeconds(6)
        ),
        (second.message, Decision.Drafting(second.message), at(0).plusSeconds(8))
      )
    }

    test("a later opening placed to follow its strand holds it; one placed elsewhere does not") {
      given Tx = TestTx.fake
      // The opening's decision, its turn and its author's next opening's entry, that next
      // opening placed on the one exchange offered at 1.0 when it `follows`, else at 0.
      def outcome(follows: Boolean): (Vector[Decision], TurnRef, EntryId) = {
        val w = new World
        w.hear("where did we land on the Engine contract term?", "Nick", 0)
        val (b, d) = (w.thread("2.0"), w.thread("3.0"))
        val t = w.hear("Is it twelve months on the Engine contract", "Ana", 2, in = b)
        val next = w.hear("or six on the Engine contract", "Ana", 3, in = d)
        for {
          conversation <- w.conversations.get(b).toOption.flatten
          entries <- w.entries.list(b).toOption
          opening <- Opening.of(conversation, entries, TurnRef(b, t.turn))
        } w.placements(asking, 2).awaited(opening)
        val later = if (follows) asking else new Scripted(Vector(0, 1, 0, 0, 0), Vector.empty)
        new InMemoryDurable().run(next.workflowId)(w.body(later, 4))
        new InMemoryDurable().run(t.workflowId)(w.body(asking, 4, within))
        (
          w.speech.decisions.collect { case (h, d, _) if h.turn.conversationId == b => d },
          t.message,
          entryOf(w, d, next)
        )
      }
      val (follows, _, next) = outcome(follows = true)
      val (elsewhere, turn, _) = outcome(follows = false)
      (follows, elsewhere) ==> (
        Vector(Decision.Held(Silence.Continued(next))),
        Vector(Decision.Drafting(turn))
      )
    }

    test("a kept placement's seen state is what offered shows for its message now") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      val t = w.hear("Is this a real question", "David", 1, in = b)
      val classifier = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      new InMemoryDurable().run(t.workflowId)(w.body(classifier, 2))
      val turn = TurnRef(b, t.turn)
      val first = w.entries.list(b)(using TestTx.fake).toOption.flatMap(_.headOption).map(_.id)
      val kept = first.flatMap(id =>
        w.stitches.placed(id)(using TestTx.fake).toOption.flatten.map(_.seen.state)
      )
      val now = Stitching
        .offered(w.reads, FakeDb.as(Subject.Public), turn, Tuning.Default)
        .map(_.map(Stitching.shown))
      assert(kept.isDefined)
      now ==> Right(kept)
    }

    test("a message that is not its conversation's opening is offered nothing, and not asked") {
      val w = new World
      w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val b = w.thread("2.0")
      w.hear("Is this a real question", "David", 1, in = b)
      val second = w.hear("and another", "David", 2, in = b)
      val turn = TurnRef(b, second.turn)
      val db = FakeDb.as(Subject.Public)
      (
        Stitching.offered(w.reads, db, turn, Tuning.Default),
        Stitching.unplaced(w.reads, db, turn, Tuning.Default)
      ) ==> (Right(None), Right(None))
    }

    test("a message that says nothing, or only shares a file, is not asked: its tags unanswered") {
      Vector("", "  ", "[file: q3.pdf]").map { text =>
        val w = new World
        val t = w.hear(text, "Ana", 0)
        val classifier = decided
        new InMemoryDurable().run(t.workflowId)(w.body(classifier, 5))
        (classifier.calls, w.tags(t))
      } ==> Vector.fill(3)((0, Some(Tags.Unanswered("nothing was said"))))
    }

    test("an absent classifier's tags are unanswered, and kept") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(
        w.body(new Scripted(Vector.empty, Vector.empty), 5)
      ) ==>
        Unoffered + """tagged: unanswered: unavailable: no classifier; held: {"kind":"off"}"""
      w.tags(t) ==> Some(Tags.Unanswered("unavailable: no classifier"))
    }

    test("a triage over the day's cap asks no classifier: its tags unanswered, for the cap") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      w.ledger.now = at(5)
      w.ledger.record(
        EntryId("elsewhere"),
        TurnRef(ConversationId("c2"), TurnSeq(0)),
        WorkflowId("c2:0"),
        "m",
        None,
        None,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal(1))),
        Tokens(1)
      )(using TestTx.fake)
      val classifier = decided
      val capped = "unavailable: The day's spend reached its cap, so no model was asked."
      new InMemoryDurable().run(t.workflowId)(
        w.body(classifier, 5, cap = DailyCap.of("1").toOption)
      ) ==> Unoffered + s"""tagged: unanswered: $capped; held: {"kind":"off"}"""
      classifier.calls ==> 0
      w.tags(t) ==> Some(Tags.Unanswered(capped))
    }

    test("a triage's classifier call is kept in the ledger once, under its own workflow") {
      val w = new World
      val t = w.hear("standup moves to 10:00 from Monday", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(w.body(decided, 5))
      def rows(of: WorkflowId) =
        w.ledger.of(of)(using TestTx.fake).map(_.map(r => (r.entry, r.model, r.usage)))
      rows(t.workflowId) ==> Right(
        Vector((EntryId(s"move:${WorkflowId.value(t.workflowId)}:triage"), "jev-1.13.0", Spent))
      )
    }

    test(
      "a classifier whose service is not cleared for what the triage read is not asked: its tags unanswered"
    ) {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val classifier = new Scripted(Vector(0.1, 0.9), Vector(0.9))
      new InMemoryDurable().run(t.workflowId)(
        w.body(classifier, 5, db = grit.core.classify.TestClassifiers.above())
      ) ==>
        Unoffered + s"""tagged: unanswered: unavailable: ${grit.core.classify.TestClassifiers.Uncleared}; held: {"kind":"off"}"""
      (w.tags(t), classifier.calls) ==>
        (Some(Tags.Unanswered(s"unavailable: ${grit.core.classify.TestClassifiers.Uncleared}")), 0)
    }

    test("a message gone while it was asked keeps no tags") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val purging = new Scripted(
        Vector(0, 0, 0, 0, 1),
        Vector(0.1, 0.1, 0.1),
        () => w.purge(TurnRef(c, t.turn))
      )
      new InMemoryDurable().run(t.workflowId)(w.body(purging, 5)) ==>
        Unoffered + "ignored: the message is gone or tagged already"
      w.tags(t) ==> None
    }

    test("an id that is not a triage's, or names no heard message, asks nothing") {
      val w = new World
      val said = w.say("hello", 0)
      val classifier = decided
      new InMemoryDurable().run(WorkflowId("c1:0"))(w.body(classifier, 0)) ==>
        "not a triage: c1:0"
      new InMemoryDurable().run(grit.core.id.TriageRef(p1, said.turnSeq).workflowId)(
        w.body(classifier, 0)
      ) ==> Unoffered + "failed: no heard message at turn 0"
      classifier.calls ==> 0
    }

    test("a candidate is kept drafting, and its own turn started once, even across a replay") {
      val w = new World
      val t = w.hear("what did we decide about the refi page?", "Ana", 0)
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(w.body(asking, 1, within)) ==>
        Unoffered + "tagged: gap asks 1.0, open 0.9, to 0.125, to-grit 0.125, durable 0.5, anchor-record 0.125 (jev-1.13.0); drafting: c1:0"
      val turn = TurnRef(c, t.turn)
      (w.started, w.speech.decisions.map(_._2)) ==> (Vector(turn), Vector(Decision.Drafting(turn)))
      val history = durable.history(t.workflowId)
      new InMemoryDurable().replay(t.workflowId, history)(w.body(asking, 1, within))
      w.started ==> Vector(turn)
      history.map(_.name) ==>
        Vector("stitched", "ask:triage", "ask:triage:record", "record", "consider", "start")
    }

    test(
      "a drafting turn whose start fails is kept failed, and holds no later message in its thread"
    ) {
      val w = new World
      val first = w.hear("what did we decide about the refi page?", "Ana", 0)
      val durable = new InMemoryDurable
      val log = durable.run(first.workflowId)(
        w.body(asking, 1, within, start = _ => Left("the queue is down"))
      )
      val second = w.hear("and the pricing page?", "Ana", 2)
      new InMemoryDurable().run(second.workflowId)(w.body(asking, 2, within))
      val (one, two) = (TurnRef(c, first.turn), TurnRef(c, second.turn))
      (
        log,
        durable.recordedSteps(first.workflowId),
        w.speech.outcomes.get(one).map(_._1),
        w.speech.decisions.map(_._2),
        w.started
      ) ==> (
        Unoffered + "tagged: gap asks 1.0, open 0.9, to 0.125, to-grit 0.125, durable 0.5, anchor-record 0.125 (jev-1.13.0); drafting, not started: the queue is down",
        Vector(
          "stitched",
          "ask:triage",
          "ask:triage:record",
          "record",
          "consider",
          "start",
          "unstarted"
        ),
        Some(Outcome.Failed("not started: the queue is down")),
        Vector(Decision.Drafting(one), Decision.Drafting(two)),
        Vector(two)
      )
    }

    test("a message failing the gate is kept held on every bound it failed, and no turn starts") {
      val w = new World
      val t = w.hear("lunch?", "Ana", 0)
      val durable = new InMemoryDurable
      durable.run(t.workflowId)(
        w.body(
          new Scripted(Vector(0, 0, 0, 1), Vector(0.25, 0.125, 0.125, 0.5, 0.125, 0.125)),
          1,
          within
        )
      )
      (w.started, w.speech.decisions.map(_._2), durable.recordedSteps(t.workflowId)) ==> (
        Vector.empty,
        Vector(
          Decision.Held(
            Silence.Gated(
              Gate.Failed(Bound.AtLeast(Reading.Key(name("gap"), "asks"), p(0.5)), p(0.0)),
              Vector(Gate.Failed(Bound.AtLeast(Reading.Yes(name("open")), p(0.5)), p(0.25)))
            )
          )
        ),
        Vector("stitched", "ask:triage", "ask:triage:record", "record", "consider")
      )
    }

    test(
      "a message put to grit is answered: its reply awaited and its mark wanted at its reply address, under shadow as within, once across a replay"
    ) {
      def answered(speaking: Speaking) = {
        val w = new World
        val t = w.hear("Pip, what did we decide about the refi page?", "Ana", 0)
        val durable = new InMemoryDurable
        durable.run(t.workflowId)(w.body(named, 1, speaking))
        val history = durable.history(t.workflowId)
        new InMemoryDurable().replay(t.workflowId, history)(w.body(named, 1, speaking))
        val turn = TurnRef(c, t.turn)
        given Tx = TestTx.fake
        (
          w.speech.decisions.map(_._2),
          w.started,
          w.deliveries.pending().map(_.map(p => (p.turn, p.to))),
          w.acknowledgements.standing()
        ) ==> (
          Vector(Decision.Answering(turn, "C/1.0")),
          Vector(turn),
          Right(Vector((turn, "C/1.0"))),
          Right(Vector(Acknowledgement(turn, "C/1.0", shown = false)))
        )
      }
      answered(within)
      answered(Speaking.Shadow(limits))
    }

    test(
      "an unprompted draft and a held message put to grit await no reply and want no mark"
    ) {
      def wanted(classifier: Classifier^, speaking: Speaking) = {
        val w = new World
        val t = w.hear("Pip, what did we decide about the refi page?", "Ana", 0)
        new InMemoryDurable().run(t.workflowId)(w.body(classifier, 1, speaking))
        val kinds = w.speech.decisions.map(_._2 match {
          case Decision.Drafting(_) => "drafting"
          case Decision.Answering(_, _) => "answering"
          case Decision.Held(_) => "held"
        })
        given Tx = TestTx.fake
        (kinds, w.deliveries.pending(), w.acknowledgements.standing())
      }
      val heldNamed =
        new Scripted(Vector(1, 0, 0, 0), Vector(0.125, 0.125, 0.875, 0.5, 0.125, 0.125))
      Vector(wanted(asking, within), wanted(heldNamed, within)) ==>
        Vector(
          (Vector("drafting"), Right(Vector.empty), Right(Vector.empty)),
          (Vector("held"), Right(Vector.empty), Right(Vector.empty))
        )
    }

    test(
      "consider records whether the message names someone, the assistant's reply after it, its author's next message, the followed exchange's latest verdict and the best record match"
    ) {
      given Tx = TestTx.fake
      val w = new World
      val opening = w.hear("where did we land on the engine contract term?", "Nick", 0)
      val settle = Judgement.Weighed(p(0.125), p(0.75), p(0.125), "settle")
      w.periods.judged(p1, Verdict(at(1), opening.turn, settle)) ==> Right(true)
      val t = w.hear("is it in the contract notes?", "Ana", 2, asked = Set(Bo))
      val reply = w.reply("It is twelve months.", 3)
      w.hear("same question here", "Bo", 4)
      val next = w.hear("never mind, found it", "Ana", 5)
      w.hear("one more thing", "Ana", 7)
      val _ = w.note("engine contract")
      new InMemoryDurable().run(t.workflowId)(w.body(asking, 6, within))
      w.speech.decisions.map(_._1.cues) ==> Vector(
        Cues(
          named = true,
          spoken = Some(reply.id),
          continued = Some(entryOf(w, c, next)),
          settled = Some(settle),
          records = Some(Cues.Match(Notes, 2.0, 1))
        )
      )
    }

    test(
      "an opening placed to follow an exchange records that exchange's latest verdict, and its author's next opening placed to follow it"
    ) {
      given Tx = TestTx.fake
      val w = new World
      val first = w.hear("where did we land on the Engine contract term?", "Nick", 0)
      val settle = Judgement.Unanswered("settle model down")
      w.periods.judged(p1, Verdict(at(1), first.turn, settle)) ==> Right(true)
      val (b, d) = (w.thread("2.0"), w.thread("3.0"))
      val t = w.hear("Is it twelve months on the Engine contract", "Ana", 2, in = b)
      val next = w.hear("or six on the Engine contract", "Ana", 3, in = d)
      // Each opening placed at 0.9 on exchange 1, the one root offered: the message's first,
      // then its author's next opening, under a deployment that keeps no decision on it.
      def placing = new Scripted(Vector(0.9, 0.1, 0, 0, 0), Vector(0.1, 0.1, 0.2))
      for {
        conversation <- w.conversations.get(b).toOption.flatten
        entries <- w.entries.list(b).toOption
        opening <- Opening.of(conversation, entries, TurnRef(b, t.turn))
      } w.placements(placing, 2).awaited(opening)
      new InMemoryDurable().run(next.workflowId)(w.body(placing, 4))
      new InMemoryDurable().run(t.workflowId)(w.body(placing, 4, within))
      (
        w.stitches.links(Vector(b, d)).map(_.map(_.root).distinct),
        w.speech.decisions.collect { case (h, _, _) if h.turn.conversationId == b => h.cues }
      ) ==> (
        Right(Vector(c)),
        Vector(Cues.Empty.copy(continued = Some(entryOf(w, d, next)), settled = Some(settle)))
      )
    }

    test(
      "a deployment that does not speak keeps no decision on a message not put to grit, and answers one put to grit"
    ) {
      val w = new World
      val t = w.hear("what did we decide?", "Ana", 0)
      new InMemoryDurable().run(t.workflowId)(w.body(asking, 1))
      (w.started, w.speech.decisions) ==> (Vector.empty, Vector.empty)
      val n = w.hear("Pip, what did we decide?", "Ana", 1)
      new InMemoryDurable().run(n.workflowId)(w.body(named, 1))
      val turn = TurnRef(c, n.turn)
      (w.started, w.speech.decisions.map(_._2)) ==>
        (Vector(turn), Vector(Decision.Answering(turn, "C/1.0")))
    }
  }
}
