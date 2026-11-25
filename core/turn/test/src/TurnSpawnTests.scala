package grit.turn

import grit.core.agent.SubagentFixtures
import grit.core.durable.InMemoryDurable
import grit.core.edge.InMemoryEdges
import grit.core.id.ToolCallId
import grit.core.inbox.TurnEnding
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.Place
import grit.core.store.{InMemoryEntryStore, Origin}
import grit.core.tool.Outcome
import grit.core.visibility.{Clearance, Label, Level}

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

  /** A model that calls `first` on its first call and answers "done" after. */
  private def model(first: (String, String)*): Scripted =
    new Scripted((_, n) => Right(if (n == 0) calling("", first*) else calling("done")))

  /** The edge at `at`, advertising [[hostedTools]] there, answering each request at once. */
  private def edgeAt(durable: InMemoryDurable, at: Place): Served = {
    val edge = new Served(new InMemoryEdges, durable, _ => Serve.Now(Outcome.Done("read")), at)
    edge.advertise(hostedTools)
    edge
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
      def calls(provider: Scripted): Set[String] =
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
      (calls(first), calls(again), ws.reads) ==> (Set.empty[String], Set.empty[String], 0)
    }
  }
}
