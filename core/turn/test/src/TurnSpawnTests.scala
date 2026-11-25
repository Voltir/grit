package grit.turn

import grit.core.agent.{Subagent, SubagentFixtures, Subagents}
import grit.core.durable.InMemoryDurable
import grit.core.edge.InMemoryEdges
import grit.core.id.ToolCallId
import grit.core.inbox.TurnEnding
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, Grade, Ladder, Policy, Rung}
import grit.core.place.Place
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger, Origin}
import grit.core.tool.Outcome
import grit.core.visibility.{Clearance, Integrity, Label, Level}

import utest.*

/** A spawned child's turn: it runs as its type says, never with its parent's settings. */
object TurnSpawnTests extends TestSuite {
  import TurnFixtures.*

  private val repository: Place = SubagentFixtures.scratch.place
  private val room: Place = Origin.Slack("T1", "C1", "1.0").place

  /** The tools grit adds to every turn briefed for one task. */
  private val every = Set("topic", "read_hidden", "extract", "task_status")

  private def calling(text: String, calls: (String, String)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map((id, name) =>
        AssistantBlock.ToolCall(ToolCallId(id), name, ujson.Obj("path" -> "a.txt"))
      ),
      if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

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
        first.exists(
          _.system.contains(
            "This run does one task for another conversation. That task follows grit's note " +
              "at the start of your messages. Your last reply is passed back to that " +
              "conversation when the run ends, and nobody can answer a question here, so do " +
              "not ask one: finish the task, then say what you did and what you found."
          )
        ),
        first.exists(_.system.contains("In this run you are called Coder."))
      ) ==> (
        Some(
          Message.User(
            "<grit_note>\nAnother conversation started this run to do one task. The task, as " +
              "that conversation wrote it, follows this note.\n</grit_note>\n" +
              "Fix the typo in README.md."
          )
        ),
        true,
        true
      )
    }

    test(
      "every call a child makes is recorded priced, its classifier's, its query's and its task's among them, so its spend is exact"
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
        rows.map(r => grit.core.id.EntryId.value(r.entry) -> r.usage.costUsd.isDefined),
        ledger.conversation(turn.conversationId).map(s => SubagentFixtures.spendCap.admits(s.cost))
      ) ==> (
        Vector(
          "route:c1:0" -> true,
          "move:c1:0:needs" -> true,
          "move:c1:0:query" -> true,
          "reply:c1:0" -> true,
          "summary:c1:0" -> true
        ),
        Right(true)
      )
    }

    test("a child's calls are made under its type's pin, not its deployment's turn pin") {
      val entries = new InMemoryEntryStore
      val turn = spawn(entries, SubagentFixtures.coder, "Fix the typo.")
      val durable = new InMemoryDurable
      val seen = new Pinnings
      durable.run(turn.workflowId)(
        spawnedBody(
          entries,
          model(),
          edgeAt(durable, repository),
          new Files(Map.empty),
          coders,
          spawnedFrom(repository),
          seen = seen
        )
      )
      seen.pins.headOption.map(_.assignment) ==> Some(SubagentFixtures.pins.first.assignment)
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
      (provider.requests.size, ran.startsWith("failed: Uncleared("), TurnEnding.of(ran)) ==>
        (0, true, TurnEnding.Otherwise)
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
        TurnEnding.of(ran),
        provider.requests.size,
        ledger.conversation(turn.conversationId).map(s => SubagentFixtures.spendCap.admits(s.cost))
      ) ==> ("failed: OverCap(SpendCap(0.05))", TurnEnding.Otherwise, 2, Right(false))
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
        TurnEnding.of(ran),
        provider.requests.size,
        durable.history(turn.workflowId).map(_.name).contains(Turn.Step.Offer)
      ) ==> ("failed: Undeclared(coder)", TurnEnding.Otherwise, 0, false)
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
