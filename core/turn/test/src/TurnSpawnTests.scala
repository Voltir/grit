package grit.turn

import grit.core.agent.{Subagent, SubagentFixtures, Subagents}
import grit.core.document.InMemoryDocuments
import grit.core.durable.{InMemoryDurable, StepRecord}
import grit.core.edge.InMemoryEdges
import grit.core.files.InMemoryPieces
import grit.core.id.ToolCallId
import grit.core.inbox.TurnEnding
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, Grade, Ladder, Policy, Rung}
import grit.core.place.Place
import grit.core.store.{
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPrincipals,
  InMemoryUsageLedger,
  Origin,
  Payload
}
import grit.core.tool.Outcome
import grit.core.visibility.{Clearance, Integrity, Label, Level}

import utest.*

/** A spawned child's turn: it runs as its type says, never with its parent's settings. */
object TurnSpawnTests extends TestSuite {
  import TurnFixtures.*

  private val repository: Place = SubagentFixtures.scratch.place
  private val room: Place = Origin.Slack("T1", "C1", "1.0").place

  /** The tools grit adds to a child's turn. */
  private val every = Set("read_hidden", "extract", "task_status")

  private def calling(text: String, calls: (String, String)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map((id, name) =>
        AssistantBlock.ToolCall(ToolCallId(id), name, ujson.Obj("path" -> "a.txt"))
      ),
      if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

  /** A conversation of `from` and the in-memory inbox over its stores, which keeps each ring; when
    * `from` is a spawned child's, its parent is started, so a ring reaches it.
    */
  private final class Ringing(from: Origin) {
    val entries = new InMemoryEntryStore
    val conversations = new grit.core.store.InMemoryConversationStore
    val inbox: grit.core.inbox.InMemoryInbox^ = new grit.core.inbox.InMemoryInbox(
      conversations,
      entries,
      new grit.core.store.InMemoryPrincipals,
      new InMemoryUsageLedger,
      grit.core.spend.Budget(java.time.ZoneOffset.UTC, None),
      grit.core.visibility.Visibility.Shipped,
      grit.core.identity.TestAccounts.principal,
      new grit.core.visibility.InMemoryRecorded()
    )
    val parent: grit.core.id.TurnRef =
      from match {
        case Origin.Spawned(p, _, _) => p
        case _ =>
          grit.core.id.TurnRef(grit.core.id.ConversationId("none"), grit.core.id.TurnSeq.First)
      }
    inbox.started = Vector(parent)

    // Written and read only on the test's own thread, through the rings the turn makes.
    @caps.unsafe.untrackedCaptures
    var rang = 0

    /** The inbox, each ring it is asked for counted in [[rang]], kept or not. */
    private val counted: grit.core.inbox.SpawnInbox^ = new grit.core.inbox.SpawnInbox {
      def spawned(
          origin: Origin.Spawned,
          agent: grit.core.id.AgentType,
          brief: String,
          spendCap: grit.core.message.SpendCap,
          believed: Integrity,
          classification: Label
      ) = inbox.spawned(origin, agent, brief, spendCap, believed, classification)
      def ended(child: grit.core.id.TurnRef) = inbox.ended(child)
      def reply(child: grit.core.id.TurnRef)(using grit.core.store.Tx^) = inbox.reply(child)
      def spent(turn: grit.core.id.TurnRef)(using grit.core.store.Tx^) = inbox.spent(turn)
      def granted(parent: grit.core.id.TurnRef)(using grit.core.store.Tx^) = inbox.granted(parent)
      def returned(child: grit.core.id.TurnRef, ending: TurnEnding) = {
        rang += 1
        inbox.returned(child, ending)
      }
    }

    /** The turn's body over these stores, offered `subagents`, its model `provider`. */
    def body(subagents: Spawnable, provider: Scripted)(id: grit.core.id.WorkflowId)(using
        grit.core.durable.Durable^
    ): String =
      spawnedBody(
        entries,
        provider,
        edgeAt(new InMemoryDurable, repository),
        new Files(Map.empty),
        subagents,
        from,
        conversations = conversations,
        spawns = counted
      )(id)
  }

  /** A reply calling `calls`, priced at `usd`. */
  private def costing(usd: String, calls: (String, String)*): Message.Assistant =
    calling("", calls*)
      .copy(usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal(usd))))

  /** Routing whose spend is read from `ledger`, asking no task. */
  private def spendingFrom(ledger: InMemoryUsageLedger) =
    routing(
      new Tasks(Left(grit.core.triage.Weighing.Unweighed.Unavailable)),
      spending = ledger
    )

  /** A model that calls `first` on its first call and answers "done" after. */
  private def model(first: (String, String)*): Scripted =
    new Scripted((_, n) => Right(if (n == 0) calling("", first*) else calling("done")))

  /** The edge at `at`, advertising [[hostedTools]] there, answering each request at once. */
  private def edgeAt(durable: InMemoryDurable, at: Place): Served = {
    val edge = new Served(new InMemoryEdges, durable, _ => Serve.Now(Outcome.Done("read")), at)
    edge.advertise(hostedTools)
    edge
  }

  /** A type answering from the docs service, its pins of two grades so its task is asked. */
  private val scholars: Spawnable = {
    def rung(g: Grade, model: String) = Rung(g, Assignment(pair(model, None), 100, None))
    val ladder = Ladder
      .of(
        Vector(rung(Grade.Standard, "test/child-standard"), rung(Grade.Strong, "test/child-strong"))
      )
      .fold(why => throw new java.lang.AssertionError(why), identity)
    val pins = Catalog.of(Policy(ladder, ladder, ladder, ladder, ladder), Vector.empty).pin.turn
    val scholar = Subagent
      .of(
        SubagentFixtures.scholar,
        SubagentFixtures.persona,
        pins,
        SubagentFixtures.services,
        SubagentFixtures.spendCap,
        Integrity.Member
      )
      .flatMap(s => Subagents.of(Vector(s)))
      .fold(why => throw new java.lang.AssertionError(why), identity)
    Spawnable.of(scholar, _ => Vector.empty)
  }

  /** The stub classifier's answers, each call priced. */
  /** The coding type of [[coders]], its coding tools `prod` alone, which asks a person first. */
  private val prodders: Spawnable = Spawnable.of(
    Subagents
      .of(
        Vector(
          SubagentFixtures.ok(
            SubagentFixtures
              .subagent(SubagentFixtures.coder, SubagentFixtures.coding, Integrity.WritesOut)
          )
        )
      )
      .fold(why => throw new java.lang.AssertionError(why), identity),
    _ => Vector(hostedProd)
  )

  private final class PricedClassifier extends grit.core.classify.Classifier {
    protected def answer(
        state: ujson.Value,
        questions: Vector[grit.core.classify.Question]
    ): Either[grit.core.classify.ClassifierError, grit.core.classify.Answers] =
      Right(
        grit.models.StubClassifier
          .answers(state, questions)
          .copy(usage = Usage(Tokens(20), Tokens(1), Tokens.Zero, Some(BigDecimal("0.00001"))))
      )
  }

  private def offered(provider: Scripted): Set[String] =
    provider.requests.headOption.toVector.flatMap(_.tools.map(_.name)).toSet

  val tests = Tests {
    test(
      "a coding child of a run of work is offered its type's coding tools, run by the edge at its repository, and none of its parent's"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val edge = edgeAt(durable, repository)
      val ws = new Files(Map("a.txt" -> "alpha"))
      val provider = model(("t1", "fetch"))
      durable.run(turn.workflowId)(
        spawnedBody(entries, provider, edge, ws, coders, spawnedFrom(repository))
      )
      (offered(provider) -- every, edge.sent.map(_.workspace), ws.reads) ==>
        (Set("fetch"), Vector(repository), 0)
    }

    test(
      "a coding child of a Slack thread is offered no coding tools: nothing serves its repository"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val edge = edgeAt(durable, room)
      val provider = model()
      durable.run(turn.workflowId)(
        spawnedBody(entries, provider, edge, new Files(Map.empty), coders, spawnedFrom(room))
      )
      (offered(provider) -- every, edge.sent) ==> (Set.empty[String], Vector.empty)
    }

    test("a child is offered task_status and never topic: its one message is never asked about") {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val provider = model()
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map.empty),
          coders,
          spawnedFrom(repository)
        )
      )
      offered(provider).intersect(Set("topic", "task_status")) ==> Set("task_status")
    }

    test("a child's task_status is worded for a handoff going back to its parent, on every call") {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val provider = model(("t1", "fetch"))
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map("a.txt" -> "alpha")),
          coders,
          spawnedFrom(repository)
        )
      )
      // The words are pinned in TurnLoopTests; this pins that a child's turn is sent them.
      val child = TaskStatus.schema(false, TaskStatus.Handoff.ToParent).description
      provider.requests.map(_.tools.find(_.name == "task_status").map(_.description)) ==>
        Vector.fill(2)(Some(child))
    }

    test(
      "a child's requests rebuild from its record as sent, and its window record keeps the started line"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val provider = model(("t1", "fetch"))
      val conversations = new InMemoryConversationStore
      val profiles = profilesKept()
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map("a.txt" -> "alpha")),
          coders,
          spawnedFrom(repository),
          conversations = conversations,
          profiles = profiles
        )
      )
      val steps = durable
        .history(turn.workflowId)
        .map(s =>
          StepRecord(
            s.name,
            s.outcome match {
              case InMemoryDurable.Outcome.Output(value) => Some(value)
              case InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker => None
            },
            None
          )
        )
      val reads = TurnRecord.Reads(
        conversations,
        entries,
        new InMemoryPrincipals,
        new InMemoryDocuments,
        new InMemoryPieces,
        Prompts,
        ToolSets,
        profiles
      )
      given grit.core.store.Tx = grit.dbos.sql.TestTx.fake
      val dated = entries.get(Turn.windowId(turn)).toOption.flatten.map(_.payload).collect {
        case w: Payload.Window => w.carry.map(_.dated)
      }
      (
        TurnRecord
          .requests(turn, steps, reads)
          .map(c => c.looped.map(_.request) ++ c.answered.map(_.on).toVector),
        dated
      ) ==> (
        Right(provider.requests),
        Some(
          Some(
            Some(
              "This run was started Thu 1970-01-01 00:00 UTC. Times grit shows you in your view " +
                "are UTC. Write each time in your reply with its zone."
            )
          )
        )
      )
    }

    test("a child's first message dates when its run was started, speaking of no person") {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo in README.md.")
      val durable = new InMemoryDurable
      val provider = model()
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map.empty),
          coders,
          spawnedFrom(repository)
        )
      )
      provider.requests.headOption.flatMap(_.messages.headOption) ==> Some(
        Message.User(
          "This run was started Thu 1970-01-01 00:00 UTC. Times grit shows you in your view " +
            "are UTC. Write each time in your reply with its zone."
        )
      )
    }

    test(
      "a child's first request opens with its task under grit's note, and its prompt says whose task it is and what it is called"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo in README.md.")
      val durable = new InMemoryDurable
      val provider = model()
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map.empty),
          coders,
          spawnedFrom(repository)
        )
      )
      val first = provider.requests.headOption
      // After the note every turn's first message is dated by.
      val task = first.flatMap(_.messages.drop(1).headOption)
      (
        task,
        first.map(
          _.system.linesIterator
            .filter(l => l.startsWith("This run does") || l.startsWith("In this run"))
            .toVector
        )
      ) ==> (
        Some(
          Message.User(
            "<grit_note>\nAnother conversation started this run to do one task. The task, as " +
              "that conversation wrote it, follows this note.\n</grit_note>\n" +
              "Fix the typo in README.md."
          )
        ),
        Some(
          Vector(
            "This run does one task for another conversation. That task follows grit's note " +
              "at the start of your messages. Your last reply is passed back to that " +
              "conversation when the run ends, and nobody can answer a question here, so do " +
              "not ask one: finish the task, then say what you did and what you found.",
            "In this run you are called Coder."
          )
        )
      )
    }

    test(
      "every call a child makes is recorded at its price, its classifier's, its query's and its task's among them"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.scholar, "Find the release date.")
      val durable = new InMemoryDurable
      val ledger = new InMemoryUsageLedger
      val priced = new Scripted((_, _) => Right(calling("done")))
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          priced,
          edgeAt(durable, room),
          new Files(Map.empty),
          scholars,
          spawnedFrom(room),
          ledger = ledger,
          summarizer = new Scripted((_, _) => Right(calling("a summary"))),
          classifier = new PricedClassifier,
          routing = routing(new Tasks(Right(HardInvestigation))),
          queries = true
        )
      )
      given grit.core.store.Tx = grit.dbos.sql.TestTx.fake
      val rows = ledger.of(turn.workflowId).fold(e => sys.error(e.toString), identity)
      (
        rows.map(r => grit.core.id.EntryId.value(r.entry) -> r.usage.costUsd),
        ledger.conversation(turn.conversationId).map(_.cost)
      ) ==> (
        // The task's ask as HardInvestigation prices it, the classifier's as PricedClassifier
        // does, and every model call as `calling`.
        Vector(
          "route:c1:0" -> Some(BigDecimal("0.0000252")),
          "move:c1:0:needs" -> Some(BigDecimal("0.00001")),
          "move:c1:0:query" -> Some(BigDecimal("0.001")),
          "reply:c1:0" -> Some(BigDecimal("0.001")),
          "summary:c1:0" -> Some(BigDecimal("0.001"))
        ),
        Right(grit.core.message.Cost.Exact(BigDecimal("0.0030352")))
      )
    }

    test(
      "every round a child calls is made under its type's pin, not its deployment's turn pin, and its summary under the deployment's"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val seen = new Pinnings
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          // Two rounds, so a later round's pin is seen too.
          model("t1" -> "read_file"),
          edgeAt(durable, repository),
          new Files(Map.empty),
          coders,
          spawnedFrom(repository),
          seen = seen
        )
      )
      // Each round's call under its type's pin; its summary, as every turn's, under the deployment's.
      val typed = SubagentFixtures.pins.first.assignment.ref.toString
      seen.pins.map(_.assignment.ref.toString) ==> Vector(typed, typed, "test/summary")
    }

    test(
      "a child whose type's only pin is not cleared for its room makes no call and ends unfinished"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val provider = model()
      val internal = Label.at(Level.Internal)
      val ran = durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map.empty),
          coders,
          spawnedFrom(repository),
          db = grit.core.act.MovesFixtures.FakeDb(Clearance.of(internal), clearing(internal))
        )
      )
      (provider.requests.size, ran) ==> (0, "failed: Uncleared(internal)")
    }

    test(
      "a child whose spend reaches its cap mid-turn makes no further call and ends OverCap, at a spend its cap refuses"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.scholar, "Find the release date.")
      val durable = new InMemoryDurable
      val ledger = new InMemoryUsageLedger
      // Each call costs 0.03 against the type's cap of 0.05: the third round's gate refuses.
      val provider = new Scripted((_, _) => Right(costing("0.03", "c" -> "lookup")))
      val ran = durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, room),
          new Files(Map.empty),
          scholars,
          spawnedFrom(room),
          ledger = ledger,
          routing = spendingFrom(ledger)
        )
      )
      given grit.core.store.Tx = grit.dbos.sql.TestTx.fake
      (
        ran,
        provider.requests.size,
        ledger.conversation(turn.conversationId).map(s => SubagentFixtures.spendCap.admits(s.cost))
      ) ==> ("failed: OverCap(SpendCap(0.05))", 2, Right(false))
    }

    test(
      "a child that fails rings its parent with its ending, asking again on each rerun and kept once"
    ) {
      val w = new Ringing(spawnedFrom(repository))
      // Its type is not declared: it fails at its first step.
      val turn = spawn(w.entries, SubagentFixtures.coder, "Fix the typo.")
      def body(id: grit.core.id.WorkflowId)(using grit.core.durable.Durable^): String =
        w.body(Spawnable.empty, model())(id)
      val durable = new InMemoryDurable
      val ran = durable.run(turn.workflowId)(body)
      val rerun =
        new InMemoryDurable().replay(turn.workflowId, durable.history(turn.workflowId))(body)
      (ran, rerun, w.rang, w.inbox.returns) ==> (
        "failed: Undeclared(coder)",
        Right("failed: Undeclared(coder)"),
        2,
        Vector((w.parent, turn, TurnEnding.Otherwise))
      )
    }

    test("a child that answers rings its parent Answered, and an ordinary turn rings no one") {
      val child = new Ringing(spawnedFrom(room))
      val asked = spawn(child.entries, SubagentFixtures.scholar, "Find the release date.")
      val answered = new InMemoryDurable().run(asked.workflowId)(
        child.body(scholars, new Scripted((_, _) => Right(calling("done"))))
      )
      val ordinary = new Ringing(Origin.Task("ring", "ordinary"))
      val said = say(ordinary.entries, "Hello.")
      val _ = new InMemoryDurable().run(said.workflowId)(
        ordinary.body(scholars, new Scripted((_, _) => Right(calling("hi"))))
      )
      (TurnEnding.of(answered), child.inbox.returns, ordinary.inbox.returns) ==> (
        TurnEnding.Answered,
        Vector((child.parent, asked, TurnEnding.Answered)),
        Vector.empty
      )
    }

    test(
      "a child whose spend cap cannot be read from its first turn makes no call, failing as the store's"
    ) {
      val entries = new InMemoryEntryStore
      // Its first turn shows it no Payload.Spawn, as a clearance below that note's would.
      val turn = say(entries, "Find the release date.")
      val durable = new InMemoryDurable
      val provider = model()
      val ran = durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, room),
          new Files(Map.empty),
          scholars,
          spawnedFrom(room)
        )
      )
      (ran, provider.requests.size) ==>
        (s"failed: Store(${Rounding.CapUnread})", 0)
    }

    test("a child's later turn keeps to the spend cap its first turn's note set") {
      val entries = new InMemoryEntryStore
      val first = spawn(entries, SubagentFixtures.scholar, "Find the release date.")
      val ledger = new InMemoryUsageLedger
      locally {
        given grit.core.store.Tx = grit.dbos.sql.TestTx.fake
        ledger.called(
          "spent",
          first,
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal("0.06")))
        )
      }
      val later = say(entries, "Go on.")
      val durable = new InMemoryDurable
      val provider = model()
      val ran = durable.run(later.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, room),
          new Files(Map.empty),
          scholars,
          spawnedFrom(room),
          ledger = ledger,
          routing = spendingFrom(ledger)
        )
      )
      (later.turnSeq == first.turnSeq, ran, provider.requests.size) ==>
        (false, "failed: OverCap(SpendCap(0.05))", 0)
    }

    test(
      "a child's call of a tool that asks a person first is refused in its own step at once, asking no one and sending nothing"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val edge = edgeAt(durable, repository)
      val provider = model(("t1", "prod"))
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edge,
          new Files(Map.empty),
          prodders,
          spawnedFrom(repository)
        )
      )
      given grit.core.store.Tx = grit.dbos.sql.TestTx.fake
      val results = entries.list(conversation).toOption.toVector.flatten.collect {
        case grit.core.store.Entry(_, _, _, _, _, Payload.Result(result, _), _, _) =>
          (result.content, result.isError)
      }
      val calls = durable.recordedSteps(turn.workflowId).filter(_.startsWith("call:0:0"))
      (offered(provider).contains("prod"), calls, results, edge.sent) ==> (
        true,
        Vector("call:0:0"),
        Vector(
          (
            "prod asks a person first, and nobody waits on this call to approve it, so it did " +
              "not run.",
            true
          )
        ),
        Vector.empty
      )
    }

    test(
      "a child of a type its deployment does not declare ends at its first step, offered nothing and calling nothing"
    ) {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val provider = model()
      val ran = durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          provider,
          edgeAt(durable, repository),
          new Files(Map.empty),
          Spawnable.empty,
          spawnedFrom(repository)
        )
      )
      (
        ran,
        provider.requests.size,
        durable.history(turn.workflowId).map(_.name).contains(Turn.Step.Offer)
      ) ==> ("failed: Undeclared(coder)", 0, false)
    }

    test(
      "a child's call of delegate or of its parent's own tool runs neither, and neither runs once its type is gone mid-turn"
    ) {
      def offeredOfThem(provider: Scripted): Set[String] =
        provider.requests.flatMap(_.tools.map(_.name)).toSet.intersect(Set("delegate", "peek"))
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val ws = new Files(Map("a.txt" -> "alpha"))
      val first = model(("t1", "peek"), ("t2", "delegate"))
      // Its first call made, it crashes as its calls' results are kept; rerun under a
      // deployment that no longer declares its type.
      val crashing = new CrashOnInsert(
        entries,
        _.payload match {
          case _: grit.core.store.Payload.Result => true
          case _ => false
        }
      )
      try
        durable.run(turn.workflowId)(
          spawnedBody(
            crashing,
            first,
            edgeAt(durable, repository),
            ws,
            coders,
            spawnedFrom(repository)
          )
        )
      catch { case _: InMemoryDurable.Crash => "" }
      val again = model(("t3", "peek"), ("t4", "delegate"))
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          again,
          edgeAt(durable, repository),
          ws,
          Spawnable.empty,
          spawnedFrom(repository)
        )
      )
      (offeredOfThem(first), offeredOfThem(again), ws.reads) ==> (
        Set.empty[String],
        Set.empty[String],
        0
      )
    }
  }
}
