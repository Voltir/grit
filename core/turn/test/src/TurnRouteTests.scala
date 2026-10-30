package grit.turn

import grit.core.act.MovesFixtures.FakeDb as LabelledDb
import grit.core.allot.{Allotment, Features, Headroom, TaskKind, Tasking}
import grit.core.context.{
  AssemblyError,
  AssemblyNote,
  AssemblyRequest,
  Packing,
  Prepared,
  Projection,
  Querying,
  Unfenced,
  Window
}
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, TestCallSlots, ToolCallId, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{
  Assignment,
  Catalog,
  Effort,
  Grade,
  Ladder,
  ModelId,
  ModelRef,
  Pinned,
  Policy,
  Rung,
  Upstream
}
import grit.core.provider.{ModelRequest, Provider, ProviderError, ToolUse}
import grit.core.route.Uncleared
import grit.core.store.{Db, InMemoryEntryStore, InMemoryUsageLedger, Jot, Payload, StoreError, Tx}
import grit.core.tool.{Args, Believed, Gate, Hosted, Outcome, ToolName, ToolSpec, Toolbox}
import grit.core.visibility.TestLabels.place
import grit.core.visibility.{
  Clearance,
  Cleared,
  Integrity,
  Label,
  Level,
  Party,
  Provenance,
  Readers,
  Recorded,
  ResultPart,
  Subject,
  Visibility
}
import grit.dbos.sql.TestTx

import utest.*

/** A turn's calls go only to pairs cleared for what it knows (ADR 0031): the `route` step. */
object TurnRouteTests extends TestSuite {
  import TurnFixtures.*

  private val internal = Label.at(Level.Internal)
  private val restricted = Label.at(Level.Restricted)

  private def ref(model: String, upstream: Option[String]): ModelRef =
    ModelRef(
      ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)),
      upstream.map(u => Upstream.of(u).getOrElse(throw new java.lang.AssertionError(u)))
    )

  /** OpenRouter picks its upstream: no party, so cleared for public alone. */
  private val open = ref("test/open", None)
  private val flash = ref("test/flash", Some("fireworks"))
  private val summarising = ref("test/summary", None)

  private def ladder(refs: ModelRef*): Ladder =
    Ladder
      .of(refs.toVector.map(r => Rung(Grade.Standard, Assignment(r, 100, None))))
      .getOrElse(throw new java.lang.AssertionError())

  /** The turn's ladder is the open pair, then flash; summary and query the open pair alone. */
  private val catalog: Catalog =
    Catalog.of(
      Policy(
        ladder(open, flash),
        ladder(summarising),
        ladder(open),
        ladder(summarising),
        ladder(summarising)
      ),
      Vector.empty
    )

  private val flashCleared: Visibility =
    labelling(
      Vector.empty,
      Vector(Cleared(Party.of(flash).getOrElse(throw new java.lang.AssertionError()), internal))
    )

  /** Records each request it is asked to assemble, reading once through the db it is given,
    * and what its query writer came to (why none was written, when none was); `before` runs
    * first, as a change landing while the turn assembles.
    */
  private final class Asking(before: () -> Unit = () => ()) extends Drawing {
    // Replaced whole by `assemble`, which the turn under test calls on the test's one thread;
    // read by the test's assertions after.
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[AssemblyRequest]
    // As `asked`.
    @caps.unsafe.untrackedCaptures
    var queried = Vector.empty[grit.core.context.Querying]
    override def prepare(request: AssemblyRequest)(using Db^): Either[AssemblyError, Prepared] =
      Right(
        Prepared.Asking(
          grit.core.provider.ModelRequest("Write a search query.", Vector.empty),
          {
            case Left(why) => Querying.Drawn(Vector(AssemblyNote.FellBack(why)))
            case Right(_) => Querying.Drawn(Vector.empty)
          }
        )
      )
    override def assemble(request: AssemblyRequest, packing: Packing)(using
        Db^
    ): Either[AssemblyError, Projection] = {
      queried = queried :+ (packing match {
        case Packing.Drawn(d) => d
        case Packing.Searched(w, _) => w
      })
      window(request).map(boundary)
    }
    def window(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] = {
      asked = asked :+ request
      before()
      db.read(Subject.Turn(request.turn))(Right(Window(Vector.empty)))
        .left
        .map(AssemblyError.Store(_))
    }
  }

  /** Reads at internal, under `visibility`, until [[rise]]; at restricted after. */
  private final class Rising(visibility: Visibility) extends Db {
    // Set once by `rise` and read by `read`, both called on the test's one thread, so each
    // read sees the floor as the calls before it left it.
    @caps.unsafe.untrackedCaptures
    var risen = false
    def rise(): Unit = risen = true
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(Clearance.of(if (risen) restricted else internal), visibility))
  }

  /** Writes at internal, under `visibility`, until [[rise]]; at restricted after. */
  private final class RisingJot(visibility: Visibility) extends Jot {
    // Set once by `rise` and read by `write`, both called on the test's one thread, so each
    // write sees the floor as the calls before it left it.
    @caps.unsafe.untrackedCaptures
    var risen = false
    def rise(): Unit = risen = true
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(Clearance.of(if (risen) restricted else internal), visibility))
  }

  private def pinOf(r: ModelRef, role: Catalog => grit.core.model.Pins): Pinned =
    role(catalog).all.find(_.assignment.ref == r).getOrElse(throw new java.lang.AssertionError())

  // The route `turn` recorded, its pins read back among those of `catalog`'s profile.
  private def routeOf(durable: InMemoryDurable, turn: grit.core.id.TurnRef, catalog: Catalog) =
    durable
      .history(turn.workflowId)
      .collectFirst {
        case InMemoryDurable.Step(Turn.Step.Route, InMemoryDurable.Outcome.Output(v)) => v
      }
      .map(
        TurnJournal.routing
          .decode(_)
          .map(_.flatMap(_.in(catalog.pin).left.map(TurnFailure.Store(_))))
      )

  /** A turn ladder of two grades, each pair at an upstream of its own and asking an effort. */
  private val steady = ref("test/steady", Some("deepinfra"))
  private val mighty = ref("test/mighty", Some("anthropic"))
  private val bare = ref("test/bare", Some("together"))

  private def graded(rungs: (Grade, ModelRef, Option[Effort])*): Ladder =
    Ladder
      .of(rungs.toVector.map((g, r, e) => Rung(g, Assignment(r, 100, e))))
      .getOrElse(throw new java.lang.AssertionError())

  /** The turn's ladder `turn`; summary and query the open pair, uncleared at internal. */
  private def gradedCatalog(turn: Ladder): Catalog =
    Catalog.of(
      Policy(turn, ladder(summarising), ladder(open), ladder(summarising), ladder(summarising)),
      Vector.empty
    )

  private val twoGrades = gradedCatalog(
    graded(
      (Grade.Standard, steady, Some(Effort.Medium)),
      (Grade.Strong, mighty, Some(Effort.Medium))
    )
  )

  private val allCleared: Visibility =
    labelling(
      Vector.empty,
      Vector(steady, mighty, bare).map(r =>
        Cleared(Party.of(r).getOrElse(throw new java.lang.AssertionError()), internal)
      )
    )

  /** Calls an unknown tool every call it may, so the loop runs its whole budget, and says
    * "done" on the last, made with tools off.
    */
  private def looping =
    new Scripted((request, n) =>
      Right(
        if (request.use == ToolUse.Off) said("done")
        else
          Message.Assistant(
            Vector(AssistantBlock.ToolCall(ToolCallId(s"c$n"), "nothing", ujson.Obj())),
            StopReason.ToolUse,
            Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
            "m"
          )
      )
    )

  private val taskUsage = Usage(Tokens(600), Tokens(0), Tokens.Zero, Some(BigDecimal("0.0000252")))

  /** The task questions answered `kind` at p(hard) `hard`, the strand shown. */
  private def answered(kind: TaskKind, hard: Double): Right[Nothing, Tasking.Asked] =
    Right(
      Tasking.Asked(
        Features
          .of(
            Map(kind -> grit.core.period.Probability.clamped(0.8)),
            grit.core.period.Probability.clamped(hard)
          )
          .getOrElse(throw new java.lang.AssertionError()),
        strand = true,
        "jev-1.13.0",
        taskUsage,
        Tokens(612)
      )
    )

  /** The room the turns of [[routed]] are read in. */
  private val room = place("slack:T1/C-ops")

  /** A ledger whose day has spent 0.85 of a cap of 1, so headroom is low, and that cap. */
  private def spentMost(): (InMemoryUsageLedger, Option[grit.core.spend.DailyCap]) = {
    val ledger = new InMemoryUsageLedger
    ledger.now = java.time.Instant.now()
    ledger
      .record(
        EntryId("earlier"),
        say(new InMemoryEntryStore, "earlier"),
        WorkflowId("earlier"),
        "m",
        None,
        None,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("0.85"))),
        Tokens(1)
      )(using TestTx.fake)
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
    (ledger, grit.core.spend.DailyCap.of("1").toOption)
  }

  /** Light, standard and strong pairs, each asking high effort. */
  private val threeGrades = gradedCatalog(
    graded(
      (Grade.Light, bare, Some(Effort.High)),
      (Grade.Standard, steady, Some(Effort.High)),
      (Grade.Strong, mighty, Some(Effort.High))
    )
  )

  /** What a turn under `catalog` did, its root's task questions answering `outcome`: its route,
    * each turn pin's model and effort asked, how many model calls it made, and how many times
    * the task questions were asked; its ledger kept in `ledger`, read for headroom under `cap`;
    * read in [[room]], whose baseline is `baseline`, under `visibility`.
    */
  private def routed(
      catalog: Catalog,
      outcome: Either[grit.core.triage.Weighing.Unweighed, Tasking.Asked],
      ledger: InMemoryUsageLedger = new InMemoryUsageLedger,
      cap: Option[grit.core.spend.DailyCap] = None,
      baseline: Option[Grade] = None,
      visibility: Visibility = allCleared
  ) = {
    val entries = new InMemoryEntryStore
    val turn = say(entries, "why did signups drop in March?")
    val durable = new InMemoryDurable
    val (provider, seen, asks) = (looping, new Pinnings, new Calls)
    val _ = durable.run(turn.workflowId)(
      routedBody(
        entries,
        provider,
        new RecordingProvider,
        new Asking,
        LabelledDb(
          Clearance.inRoom(room, internal, internal),
          visibility,
          Recorded(Map(room -> Recorded.Kept(None, None, quiet = false, baseline)), Map.empty)
        ),
        catalog,
        ledger = ledger,
        routing = routing(new Tasks(outcome, asks), ledger, cap),
        seen = seen
      )
    )
    (
      routeOf(durable, turn, catalog).flatMap(_.toOption).flatMap(_.toOption),
      seen.pins.map(p => p.assignment.ref -> p.assignment.effort).distinct,
      provider.requests.size,
      asks.n,
      turn
    )
  }

  val tests = Tests {
    test("a hard root answers on the strong pin, asking high effort, with twice the calls") {
      val (route, pins, calls, asks, _) = routed(twoGrades, answered(TaskKind.Investigation, 0.72))
      (route.map(r => (r.turn.assignment.ref, r.headroom)), pins, calls, asks) ==> (
        Some((mighty, Headroom.Ample)),
        Vector(mighty -> Some(Effort.High)),
        10,
        1
      )
      route.map(_.task) ==> Some(Route.Task.Read(answered(TaskKind.Investigation, 0.72).value))
    }

    test("a chat root answers on the standard pin at low effort in two calls") {
      val (route, pins, calls, _, _) = routed(twoGrades, answered(TaskKind.Chat, 0.9))
      (route.map(_.turn.assignment.ref), pins, calls) ==>
        (Some(steady), Vector(steady -> Some(Effort.Low)), 2)
    }

    test(
      "at low headroom a hard root answers on the standard pin at medium effort in the default calls"
    ) {
      val (ledger, cap) = spentMost()
      // Three grades, so the low ceiling leaves two to choose between.
      val (route, pins, calls, asks, _) =
        routed(threeGrades, answered(TaskKind.Investigation, 0.72), ledger, cap)
      (route.map(r => (r.turn.assignment.ref, r.headroom)), pins, calls, asks) ==>
        (Some((steady, Headroom.Low)), Vector(steady -> Some(Effort.Medium)), 5, 1)
    }

    test("an unasked turn under a strong floor answers on the strong pin, recording the floor") {
      val late = Left(grit.core.triage.Weighing.Unweighed.Late)
      val (route, pins, calls, _, _) = routed(twoGrades, late, baseline = Some(Grade.Strong))
      (route.map(r => (r.turn.assignment.ref, r.baseline)), pins, calls) ==>
        (Some((mighty, Some(Grade.Strong))), Vector(mighty -> Some(Effort.Medium)), 5)
    }

    test(
      "a chat root under a strong floor answers on the strong pin, at low effort, in the fewest calls"
    ) {
      val (route, pins, calls, asks, _) =
        routed(twoGrades, answered(TaskKind.Chat, 0.9), baseline = Some(Grade.Strong))
      (route.map(_.turn.assignment.ref), pins, calls, asks) ==>
        (Some(mighty), Vector(mighty -> Some(Effort.Low)), 2, 1)
    }

    test("a floor below the grade a root wants changes nothing") {
      val (route, pins, _, _, _) =
        routed(twoGrades, answered(TaskKind.Investigation, 0.72), baseline = Some(Grade.Light))
      (route.map(r => (r.turn.assignment.ref, r.baseline)), pins) ==>
        (Some((mighty, Some(Grade.Light))), Vector(mighty -> Some(Effort.High)))
    }

    test("at low headroom a strong floor raises an unasked turn only to the standard pin") {
      val (ledger, cap) = spentMost()
      val late = Left(grit.core.triage.Weighing.Unweighed.Late)
      val (route, pins, _, _, _) =
        routed(threeGrades, late, ledger, cap, baseline = Some(Grade.Strong))
      (route.map(r => (r.turn.assignment.ref, r.headroom)), pins) ==>
        (Some((steady, Headroom.Low)), Vector(steady -> Some(Effort.Medium)))
    }

    test(
      "at low headroom a chat root under a strong floor answers on the standard pin, at low effort, in the fewest calls"
    ) {
      val (ledger, cap) = spentMost()
      val (route, pins, calls, asks, _) =
        routed(threeGrades, answered(TaskKind.Chat, 0.9), ledger, cap, Some(Grade.Strong))
      (route.map(r => (r.turn.assignment.ref, r.headroom)), pins, calls, asks) ==>
        (Some((steady, Headroom.Low)), Vector(steady -> Some(Effort.Low)), 2, 1)
    }

    test("the floor and the low ceiling leave the summary and query pins alone") {
      val (ledger, cap) = spentMost()
      val catalog = Catalog.of(
        Policy(
          threeGrades.policy.turn,
          // The summary's first is above the low ceiling, the query's below the strong floor.
          graded(
            (Grade.Strong, mighty, Some(Effort.Minimal)),
            (Grade.Standard, bare, Some(Effort.Minimal))
          ),
          graded((Grade.Light, bare, Some(Effort.Low)), (Grade.Strong, mighty, Some(Effort.Low))),
          graded((Grade.Standard, bare, None)),
          graded((Grade.Standard, bare, None))
        ),
        Vector.empty
      )
      val late = Left(grit.core.triage.Weighing.Unweighed.Late)
      val (route, _, _, _, _) = routed(catalog, late, ledger, cap, Some(Grade.Strong))
      route.map(r => (r.summary, r.query).toList.flatten.map(_.assignment.ref)) ==>
        Some(List(mighty, bare))
    }

    test("a strong floor never takes a strong pin not cleared for what the turn knows") {
      val steadyCleared = labelling(
        Vector.empty,
        Vector(
          Cleared(Party.of(steady).getOrElse(throw new java.lang.AssertionError()), internal),
          Cleared(Party.of(bare).getOrElse(throw new java.lang.AssertionError()), internal)
        )
      )
      val late = Left(grit.core.triage.Weighing.Unweighed.Late)
      val (route, _, _, _, _) =
        routed(threeGrades, late, baseline = Some(Grade.Strong), visibility = steadyCleared)
      route.map(_.turn.assignment.ref) ==> Some(steady)
    }

    test("a pin that asks no effort is asked none: a hard root asks none of it") {
      val noEffort =
        gradedCatalog(graded((Grade.Standard, steady, None), (Grade.Strong, mighty, None)))
      val (_, pins, calls, _, _) = routed(noEffort, answered(TaskKind.Investigation, 0.72))
      (pins, calls) ==> (Vector(mighty -> None), 10)
    }

    test("a turn whose cleared pins are one grade asks nothing and keeps its pin and calls") {
      val oneGrade = gradedCatalog(
        graded((Grade.Standard, steady, Some(Effort.Medium)), (Grade.Standard, mighty, None))
      )
      val (route, pins, calls, asks, _) = routed(oneGrade, answered(TaskKind.Investigation, 0.72))
      (route.map(r => (r.task, r.allotment)), pins, calls, asks) ==> (
        Some((Route.Task.Skipped, Allotment.Untouched)),
        Vector(steady -> Some(Effort.Medium)),
        5,
        0
      )
    }

    test("an unanswered task ask takes the pin a turn asked nothing takes, recording why") {
      val late = Left(grit.core.triage.Weighing.Unweighed.Late)
      val (route, pins, calls, _, _) = routed(twoGrades, late)
      (route.map(r => (r.task, r.allotment.grade)), pins, calls) ==> (
        Some((Route.Task.Unanswered(grit.core.triage.Weighing.Unweighed.Late), None)),
        Vector(steady -> Some(Effort.Medium)),
        5
      )
    }

    test("the task call's cost is in the ledger once, under route:") {
      val ledger = new InMemoryUsageLedger
      val (_, _, _, _, turn) = routed(twoGrades, answered(TaskKind.Lookup, 0.1), ledger)
      ledger
        .of(turn.workflowId)(using TestTx.fake)
        .map(_.filter(_.entry == Route.id(turn)).map(r => (r.model, r.usage, r.estimatedInput))) ==>
        Right(Vector(("jev-1.13.0", taskUsage, Tokens(612))))
    }

    test(
      "summary and query keep their first cleared pin and its own effort, the summary made under it"
    ) {
      val catalog = Catalog.of(
        Policy(
          graded(
            (Grade.Standard, steady, Some(Effort.Medium)),
            (Grade.Strong, mighty, Some(Effort.Medium))
          ),
          // A hard root takes the strong turn pin; summary and query keep their first.
          graded(
            (Grade.Standard, bare, Some(Effort.Minimal)),
            (Grade.Strong, mighty, Some(Effort.Low))
          ),
          graded(
            (Grade.Standard, bare, Some(Effort.Low)),
            (Grade.Strong, mighty, Some(Effort.Minimal))
          ),
          graded((Grade.Standard, bare, Some(Effort.Minimal))),
          graded((Grade.Standard, bare, Some(Effort.Minimal)))
        ),
        Vector.empty
      )
      val (route, pins, _, _, _) = routed(catalog, answered(TaskKind.Investigation, 0.72))
      route.map(r =>
        (r.summary, r.query).toList.flatten.map(p => p.assignment.ref -> p.assignment.effort)
      ) ==> Some(List(bare -> Some(Effort.Minimal), bare -> Some(Effort.Low)))
      // The query, the reply, then the summary, each made under its own pin, its effort its own.
      pins ==> Vector(
        bare -> Some(Effort.Low),
        mighty -> Some(Effort.High),
        bare -> Some(Effort.Minimal)
      )
    }

    test(
      "a turn answers on the first of its pairs cleared for what it knows, and the uncleared summary and query make no call"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val (provider, summarizer, assembler) =
        (new RecordingProvider, new RecordingProvider, new Asking)
      val durable = new InMemoryDurable
      val ran = durable.run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          summarizer,
          assembler,
          LabelledDb(Clearance.of(internal), flashCleared),
          catalog
        )
      )
      val flashPin = pinOf(flash, _.pin.turn)
      (
        routeOf(durable, turn, catalog),
        assembler.queried,
        provider.requests.size,
        summarizer.requests.size
      ) ==> (
        Some(
          Right(
            Right(
              Route(
                internal,
                flashPin,
                None,
                None,
                Headroom.Ample,
                Route.Task.Skipped,
                Allotment.Untouched,
                None
              )
            )
          )
        ),
        Vector(Querying.Drawn(Vector(AssemblyNote.FellBack(Prepared.NoPin)))),
        1,
        0
      )
      assert(
        ran.startsWith("replied:"),
        ran.contains(s"no summary: ${TurnFailure.Uncleared(internal)}")
      )
    }

    test(
      "a turn said to grit that no turn pair is cleared for is replied the fixed text, and no call is made"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          new Asking,
          LabelledDb(Clearance.of(restricted), flashCleared),
          catalog
        )
      )
      (texts(entries), provider.requests.size) ==> (
        Vector("user: hello", s"assistant: ${Uncleared.reply(Uncleared(restricted))}"),
        0
      )
    }

    test("a heard turn that no turn pair is cleared for is not replied, and no call is made") {
      val entries = new InMemoryEntryStore
      val turn = hear(entries, "overheard")
      val provider = new RecordingProvider
      val ran = new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          new Asking,
          LabelledDb(Clearance.of(restricted), flashCleared),
          catalog
        )
      )
      (texts(entries), provider.requests.size) ==> (Vector("heard: overheard"), 0)
      assert(ran.startsWith(s"failed: ${TurnFailure.Uncleared(restricted)}"))
    }

    test(
      "a read above the classification the turn was routed for fails its assembly, asking again, and no call is made"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      val db = new Rising(flashCleared)
      val ran = new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          // An assembler is pure; this one stands in for a change landing while the turn
          // assembles. `db` is this test's own, risen on its one thread by the one assembly
          // the turn makes, and read by nothing after the turn returns.
          new Asking(caps.unsafe.unsafeAssumePure(() => db.rise())),
          db,
          catalog
        )
      )
      (provider.requests.size, ran.startsWith("failed: Assembly("), ran.contains("ask again")) ==>
        (0, true, true)
    }

    test(
      "a tool reading the store reads bounded by what the turn was routed for: a read after a rise fails, asking again"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "what is my floor?")
      val db = new Rising(flashCleared)
      val looks = Toolbox
        .of[caps.CapSet^{NoCheckout}](
          Hosted[Unit](
            ToolSpec(ToolName("look"), "Reads the floor.", Args.of(NamedTuple.Empty).map(_ => ())),
            Gate.Free,
            _ => "",
            Believed.Default
          ).reading((_, _, reads) =>
            reads
              .read((tx: Tx^) ?=> Right(Label.shown(Tx.floor(tx))))
              .fold(e => Outcome.Failed(e.toString), Outcome.Done(_))
          )
        )
        .fold(d => throw new java.lang.AssertionError(d.toString), identity)
      // The floor rises as the model calls the tool.
      val provider = new Provider {
        // Counted by `complete`, which the turn under test calls on the test's one thread.
        @caps.unsafe.untrackedCaptures
        var calls = 0
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
          calls += 1
          if (calls == 1) {
            db.rise()
            Right(said("", AssistantBlock.ToolCall(ToolCallId("t1"), "look", ujson.Obj())))
          } else Right(said("done"))
        }
      }
      val _ = new InMemoryDurable().run(turn.workflowId)(
        routedBody(entries, provider, new RecordingProvider, new Asking, db, catalog, tools = looks)
      )
      val result = entries
        .get(TurnTools.Slot(TestCallSlots.at(turn)).resultId)(using TestTx.fake)
        .toOption
        .flatten
        .map(_.payload)
      result.collect { case Payload.Result(r, _) =>
        (r.isError, r.content.contains("ask again"))
      } ==>
        Some((true, true))
    }

    test(
      "a tool keeping in the store keeps bounded by what the turn was routed for: a keep after a rise keeps nothing, asking again"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "keep my floor")
      val db = new Rising(flashCleared)
      val jot = new RisingJot(flashCleared)
      val keeps = Toolbox
        .of[caps.CapSet^{NoCheckout}](
          Hosted.keeping[Unit](
            ToolSpec(ToolName("keep"), "Keeps the floor.", Args.of(NamedTuple.Empty).map(_ => ())),
            _ => "",
            Integrity.Untrusted,
            Believed.Default
          )((_, _, _) => (tx: Tx^) ?=> Right(Outcome.Done(Label.shown(Tx.floor(tx)))))
        )
        .fold(d => throw new java.lang.AssertionError(d.toString), identity)
      // The floor rises as the model calls the tool.
      val provider = new Provider {
        // Counted by `complete`, which the turn under test calls on the test's one thread.
        @caps.unsafe.untrackedCaptures
        var calls = 0
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
          calls += 1
          if (calls == 1) {
            db.rise()
            jot.rise()
            Right(said("", AssistantBlock.ToolCall(ToolCallId("t1"), "keep", ujson.Obj())))
          } else Right(said("done"))
        }
      }
      val _ = new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          provider,
          new RecordingProvider,
          new Asking,
          db,
          catalog,
          tools = keeps,
          jot = jot
        )
      )
      val result = entries
        .get(TurnTools.Slot(TestCallSlots.at(turn)).resultId)(using TestTx.fake)
        .toOption
        .flatten
        .map(_.payload)
      result.collect { case Payload.Result(r, _) =>
        (r.isError, r.content.startsWith("Nothing was kept: "), r.content.contains("ask again"))
      } ==>
        Some((true, true, true))
    }

    test(
      "a tool reading the store answers in parts: the model is next shown each the turn reads, and never one it may not"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "what is in the file?")
      val seen = place("service:drive")
      val unseen = place("service:vault")
      val db = new Rising(
        labelling(
          Vector(seen -> internal, unseen -> restricted),
          Vector(Cleared(Party.of(flash).getOrElse(throw new java.lang.AssertionError()), internal))
        )
      )
      def part(at: grit.core.place.Place, text: String) =
        ResultPart(Provenance(at, Integrity.Member), Readers.AtPlace, text)
      val parts = Toolbox
        .of[caps.CapSet^{NoCheckout}](
          Hosted[Unit](
            ToolSpec(ToolName("look"), "Reads a file.", Args.of(NamedTuple.Empty).map(_ => ())),
            Gate.Free,
            _ => "",
            Believed.Default
          ).reading((_, _, _) =>
            Outcome.Done(
              "",
              parts = Some(Vector(part(seen, "Page 213 says yes."), part(unseen, "A secret.")))
            )
          )
        )
        .fold(d => throw new java.lang.AssertionError(d.toString), identity)
      final class Asked extends Provider {
        // Kept by `complete`, which the turn under test calls on the test's one thread.
        @caps.unsafe.untrackedCaptures
        var asked = Vector.empty[ModelRequest]
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
          asked = asked :+ request
          if (asked.size == 1)
            Right(said("", AssistantBlock.ToolCall(ToolCallId("t1"), "look", ujson.Obj())))
          else Right(said("done"))
        }
      }
      val provider = new Asked
      val _ = new InMemoryDurable().run(turn.workflowId)(
        routedBody(entries, provider, new RecordingProvider, new Asking, db, catalog, tools = parts)
      )
      val results = provider.asked.drop(1).flatMap(_.messages).collect {
        case Message.ToolResult(_, content, isError) => (Unfenced(content), isError)
      }
      results ==> Vector(("Page 213 says yes.", false))
    }

    test("an offer made where the turn knows more than it was routed for fails, asking again") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      val ran =
        new InMemoryDurable(resolve = _ => Clearance.of(restricted), visibility = flashCleared)
          .run(turn.workflowId)(
            routedBody(
              entries,
              provider,
              new RecordingProvider,
              new Asking,
              LabelledDb(Clearance.of(internal), flashCleared),
              catalog
            )
          )
      (provider.requests.size, ran.startsWith("failed: Store("), ran.contains("ask again")) ==>
        (0, true, true)
    }

    test(
      "a turn's reply and summary record the grade of the pin each was made under"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val ledger = new InMemoryUsageLedger
      def graded(g: Grade, r: ModelRef, budget: Int): Ladder =
        Ladder
          .of(Vector(Rung(g, Assignment(r, budget, None))))
          .getOrElse(throw new java.lang.AssertionError())
      // One pair at two grades, each its own assignment, so each role's calls are told apart.
      val strong = Catalog.of(
        Policy(
          graded(Grade.Strong, flash, 100),
          graded(Grade.Light, flash, 200),
          ladder(open),
          ladder(flash),
          graded(Grade.Light, flash, 200)
        ),
        Vector.empty
      )
      val ran = new InMemoryDurable().run(turn.workflowId)(
        routedBody(
          entries,
          new RecordingProvider,
          new RecordingProvider,
          new Asking,
          LabelledDb(Clearance.of(internal), flashCleared),
          strong,
          ledger = ledger
        )
      )
      assert(ran.startsWith("replied:"))
      ledger.of(turn.workflowId)(using TestTx.fake).map(_.map(r => r.entry -> r.grade)) ==>
        Right(
          Vector(
            turn.replyId -> Some(Grade.Strong),
            TurnSummary.id(turn) -> Some(Grade.Light)
          )
        )
    }
  }
}
