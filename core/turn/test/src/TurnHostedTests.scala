package grit.turn

import grit.core.approval.Approval
import grit.core.durable.InMemoryDurable
import grit.core.edge.{InMemoryEdges, Permit}
import grit.core.id.{CallSlot, ToolCallId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.{Place, Reaches, Service, WorksIn}
import grit.core.prompt.{Fragment, Layer}
import grit.core.store.Origin
import grit.core.store.{InMemoryEntryStore, InMemoryToolSets}
import grit.core.tool.{Hosted, Outcome, Retry, ToolName, ToolSet}

import utest.*

/** A turn's hosted calls (ADR 0017): sent to the edge serving its directory as requests,
  * waited for, and kept as the calls' results; what the turn was offered, recorded by its
  * `offer` step, holds across a rerun.
  */
object TurnHostedTests extends TestSuite {
  import TurnFixtures.*

  private def calling(text: String, calls: (String, String, String)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map((id, name, p) =>
        AssistantBlock.ToolCall(ToolCallId(id), name, ujson.Obj("path" -> p))
      ),
      if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

  /** A model that calls `first` on its first call and answers "done" after. */
  private def model(first: (String, String, String)*): Scripted =
    new Scripted((_, n) => Right(if (n == 0) calling("", first*) else calling("done")))

  /** The tool results the model was sent on its second call, in order. */
  private def results(provider: Scripted): Vector[Message.ToolResult] =
    provider.requests
      .drop(1)
      .headOption
      .toVector
      .flatMap(_.messages.collect { case r: Message.ToolResult => r })

  private def served(
      durable: InMemoryDurable,
      serve: grit.core.edge.ToolRequest -> Serve
  ): Served = {
    val s = new Served(new InMemoryEdges, durable, serve)
    s.advertise(Vector(hostedFetch, hostedProd))
    s
  }

  private val Done = "replied: reply:c1:0; summarised: summary:c1:0"

  private val github: Service =
    Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)

  /** An edge at `github`'s place advertising `github_search`, which the engine does not
    * describe, serving each request as `serve` says.
    */
  private def atGithub(
      durable: InMemoryDurable,
      serve: grit.core.edge.ToolRequest -> Serve
  ): Served = {
    val s = new Served(new InMemoryEdges, durable, serve, github.place)
    val entry = ToolSet.Entry(
      ToolName("github_search"),
      "Searches GitHub.",
      ujson.Obj("type" -> "object"),
      false,
      Retry.Rerun
    )
    s.advertise(Hosted.advertised(entry).toVector)
    s
  }

  /** The body of a Slack thread's turn linked to `github`, offered no hosted tool of its own. */
  private def linkedBody(entries: InMemoryEntryStore, provider: Scripted, edge: Served)(
      id: grit.core.id.WorkflowId
  )(using grit.core.durable.Durable^): String =
    hostedBody(
      entries,
      provider,
      edge,
      hosted = Vector.empty,
      from = Origin.Slack("T1", "C1", "1.0"),
      worksIn = Vector(WorksIn(Place.Everywhere, github))
    )(id)

  private val elsewhere: Service =
    Service.of("elsewhere").fold(e => throw new java.lang.AssertionError(e), identity)

  /** An edge at `elsewhere`'s place over `edges`, advertising `post_x`, serving each request
    * as `serve` says.
    */
  private def atElsewhere(
      edges: InMemoryEdges,
      durable: InMemoryDurable,
      serve: grit.core.edge.ToolRequest -> Serve
  ): Served = {
    val s = new Served(edges, durable, serve, elsewhere.place)
    val entry =
      ToolSet.Entry(ToolName("post_x"), "Posts.", ujson.Obj("type" -> "object"), false, Retry.Rerun)
    s.advertise(Hosted.advertised(entry).toVector)
    s
  }

  /** The body of the fixture's TUI turn, reaching `elsewhere`, whose edge is `there`. */
  private def reachingBody(
      entries: InMemoryEntryStore,
      provider: Scripted,
      edge: Served,
      there: Served
  )(id: grit.core.id.WorkflowId)(using grit.core.durable.Durable^): String =
    hostedBody(
      entries,
      provider,
      edge,
      reaches = Vector(Reaches(Place.Everywhere, elsewhere)),
      reached = Vector(there)
    )(id)

  private def slot(turn: grit.core.id.TurnRef, index: Int): CallSlot =
    CallSlot.of(turn, 0, index).getOrElse(throw new java.lang.AssertionError())

  val tests = Tests {
    test("a round's free hosted calls are sent in one step before the first is waited on") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "peek twice")
      val durable = new InMemoryDurable
      val edge = served(durable, q => Serve.Now(Outcome.Done(s"read ${q.arguments("path").str}")))
      val provider = model(("t1", "fetch", "a.txt"), ("t2", "fetch", "b.txt"))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "record-call:0").take(4) ==>
        Vector("record-call:0", "dispatch:0", "DBOS.recv", "DBOS.sleep")
      edge.sent.map(q => (q.slot, q.permit)) ==> Vector(
        (slot(turn, 0), Permit.Free),
        (slot(turn, 1), Permit.Free)
      )
      results(provider).map(_.content) ==> Vector("read a.txt", "read b.txt")
    }

    test(
      "a request no edge claims in time is answered that no edge is serving, and cannot be claimed after"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Never)
      val provider = model(("t1", "fetch", "a.txt"))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(
          (
            "No edge is serving this conversation's directory right now, so this call did not run.",
            true
          )
        )
      edge.sent.headOption.map(q => edge.edges.claimAs(edge.registration, q)) ==> Some(false)
      Turn.Step.named(durable.recordedSteps(turn.workflowId)).filter(_.contains(":0:0")) ==>
        Vector("expire:0:0", "tool:0:0")
    }

    test(
      "a conversation linked to a service calls the tools advertised there, run by the edge at that service"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "search")
      val durable = new InMemoryDurable
      val edge = atGithub(durable, q => Serve.Now(Outcome.Done(s"found at ${q.workspace.written}")))
      val provider = model(("t1", "github_search", "grit"))
      durable.run(turn.workflowId)(linkedBody(entries, provider, edge)) ==> Done
      edge.sent.map(_.workspace) ==> Vector(github.place)
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("found at service:github", false))
    }

    test("a request at a service no edge claims in time is answered that nothing serves it") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "search")
      val durable = new InMemoryDurable
      val edge = atGithub(durable, _ => Serve.Never)
      val provider = model(("t1", "github_search", "grit"))
      durable.run(turn.workflowId)(linkedBody(entries, provider, edge)) ==> Done
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("No edge is serving github right now, so this call did not run.", true))
    }

    test(
      "an addressed turn's call to a reached tool is a request at that service, run by the edge there; a workspace call in the same round still goes to the workspace"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch, and post it")
      val durable = new InMemoryDurable
      val edge = served(durable, q => Serve.Now(Outcome.Done(s"read ${q.arguments("path").str}")))
      val there =
        atElsewhere(
          edge.edges,
          durable,
          q => Serve.Now(Outcome.Done(s"posted at ${q.workspace.written}"))
        )
      val provider = model(("t1", "fetch", "a.txt"), ("t2", "post_x", "hi"))
      durable.run(turn.workflowId)(reachingBody(entries, provider, edge, there)) ==> Done
      edge.sent.map(_.workspace) ==> Vector(Place.of(checkout))
      there.sent.map(q => (q.slot, q.workspace)) ==> Vector((slot(turn, 1), elsewhere.place))
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("read a.txt", false), ("posted at service:elsewhere", false))
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "record-call:0").take(3) ==>
        Vector("record-call:0", "dispatch:0", "reach:0:service:elsewhere")
    }

    test("a call to a reached tool when no edge serves that service is answered so, unsent") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "post it")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Never)
      val there = atElsewhere(edge.edges, durable, _ => Serve.Now(Outcome.Done("posted")))
      val edges = edge.edges
      val gone = there.registration.edge
      // The edge at elsewhere is live when the turn is offered its tools, and gone by the call.
      val provider = new Scripted((_, n) => {
        if (n == 0) edges.kill(gone)
        Right(if (n == 0) calling("", ("t1", "post_x", "hi")) else calling("done"))
      })
      durable.run(turn.workflowId)(reachingBody(entries, provider, edge, there)) ==> Done
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("No edge is serving elsewhere right now, so this call did not run.", true))
      there.sent ==> Vector.empty
    }

    test("a hosted call's result is shown as its call is: the tool, then what it acts on") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("alpha")))
      durable.run(turn.workflowId)(
        hostedBody(entries, model(("t1", "fetch", "a.txt")), edge)
      ) ==> Done
      val slot = TurnTools.Slot(turn, TurnLoop.Round.First, 0)
      entries
        .get(slot.resultId)(using grit.dbos.sql.TestTx.fake)
        .toOption
        .flatten
        .map(_.payload) ==>
        Some(
          grit.core.store.Payload
            .Result(Message.ToolResult(ToolCallId("t1"), "alpha", false), "fetch a.txt")
        )
    }

    test("a request its edge answers after the first wait is waited for again, and kept") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "peek slowly")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Later(Outcome.Done("slow")))
      val provider = model(("t1", "fetch", "a.txt"))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      results(provider).map(_.content) ==> Vector("slow")
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "dispatch:0").take(7) ==>
        Vector(
          "dispatch:0",
          "DBOS.recv",
          "DBOS.sleep",
          "expire:0:0",
          "DBOS.recv",
          "DBOS.sleep",
          "tool:0:0"
        )
    }

    test("a turn rung by its edge keeps the outcome its request holds, not the ring's message") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("alpha")))
      durable.run(turn.workflowId)(
        hostedBody(entries, model(("t1", "fetch", "a.txt")), edge)
      ) ==> Done
      durable.history(turn.workflowId).collect { case s if s.name == "DBOS.recv" => s.outcome } ==>
        Vector(InMemoryDurable.Outcome.Output(grit.core.edge.Desk.Doorbell))
      val slot = TurnTools.Slot(turn, TurnLoop.Round.First, 0)
      entries
        .get(slot.resultId)(using grit.dbos.sql.TestTx.fake)
        .toOption
        .flatten
        .map(_.payload) ==>
        Some(
          grit.core.store.Payload
            .Result(Message.ToolResult(ToolCallId("t1"), "alpha", false), "fetch a.txt")
        )
    }

    test(
      "a turn rung while its request holds no answer, or no request is kept, settles the call failed and runs on"
    ) {
      val outcomes = Vector(Serve.Rung, Serve.Lost).map { serve =>
        val entries = new InMemoryEntryStore
        val turn = say(entries, "fetch")
        val durable = new InMemoryDurable
        val provider = model(("t1", "fetch", "a.txt"))
        val ran =
          durable.run(turn.workflowId)(hostedBody(entries, provider, served(durable, _ => serve)))
        (ran, results(provider).map(r => (r.content, r.isError)))
      }
      outcomes ==> Vector(
        (
          Done,
          Vector(
            (
              "The edge rang, but its request holds no answer, so this call's result is unknown.",
              true
            )
          )
        ),
        (
          Done,
          Vector(("This call's request is no longer kept, so its result is unknown.", true))
        )
      )
    }

    test(
      "a claimed request never answered is abandoned as interrupted, and its edge's late answer is refused"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "peek forever")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Claimed)
      val provider = model(("t1", "fetch", "a.txt"))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      results(provider).map(_.isError) ==> Vector(true)
      results(provider).map(_.content) ==> Vector(
        Outcome.Interrupted.result(ToolCallId("t1")).content
      )
      edge.edges.answerAs(edge.registration, slot(turn, 0), Outcome.Done("too late")) ==> false
    }

    test("a gated hosted call becomes a request only once a person approves it") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "poke a")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("poked")))
      val provider = model(("t1", "prod", "a.txt"))
      val atAsk = crashingAtAsk(entries)
      assertThrows[InMemoryDurable.Crash](
        durable.run(turn.workflowId)(hostedBody(atAsk, provider, edge))
      )
      edge.sent ==> Vector()
      durable.send(
        turn.workflowId,
        Approval.topic(ToolCallId("t1")),
        Approval.encode(Approval.Approved)
      )
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      edge.sent.map(q => (q.slot, q.permit)) ==> Vector((slot(turn, 0), Permit.Approved))
      Turn.Step.named(durable.recordedSteps(turn.workflowId)).filter(_.contains(":0:0")) ==>
        Vector("ask:0:0", "wait:0:0", "dispatch:0:0", "tool:0:0")
      results(provider).map(_.content) ==> Vector("poked")
    }

    test("a turn whose directory no edge serves is offered no hosted tools") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      // An edge that is registered but advertises nothing.
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Never)
      val provider = model()
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge))
      provider.requests.headOption.map(_.tools.map(_.name)) ==> Some(Vector())
    }

    test("a turn is sent the instruction files its directory's edge read, after grit's own words") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      val durable = new InMemoryDurable
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Never)
      val file = Fragment(
        Layer.Place,
        "/checkout/AGENTS.md",
        "Instructions from /checkout/AGENTS.md:\n\nBe brief."
      )
      edge.advertise(Vector(hostedFetch), Vector(file))
      val provider = model()
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge))
      provider.requests.headOption.map(_.system.split("\n\n").toVector.takeRight(2)) ==>
        Some(Vector("Instructions from /checkout/AGENTS.md:", "Be brief."))
      // Offered what the edge advertises, not every hosted tool the turn knows.
      provider.requests.headOption.map(_.tools.map(_.name)) ==> Some(Vector("fetch"))
    }

    test(
      "a turn run again is offered the tools its offer step recorded, though its edge now offers others"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      val dies = new Scripted((_, _) => throw new InMemoryDurable.Crash)
      assertThrows[InMemoryDurable.Crash](
        durable.run(turn.workflowId)(hostedBody(entries, dies, edge))
      )
      edge.advertise(Vector(hostedProd))
      val provider = model(("t1", "fetch", "a.txt"))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      provider.requests.headOption.map(_.tools.map(_.name)) ==> Some(Vector("fetch", "prod"))
      results(provider).map(_.content) ==> Vector("read")
    }

    test(
      "a recorded tool this build no longer has is answered that it is gone, and the turn runs on"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      val dies = new Scripted((_, _) => throw new InMemoryDurable.Crash)
      assertThrows[InMemoryDurable.Crash](
        durable.run(turn.workflowId)(hostedBody(entries, dies, edge))
      )
      val provider = model(("t1", "fetch", "a.txt"))
      durable.run(turn.workflowId)(
        hostedBody(entries, provider, edge, hosted = Vector(hostedProd))
      ) ==> Done
      results(provider).map(_.content) ==> Vector("The tool fetch is gone; nothing ran.")
    }

    test("a turn whose recorded tool set is not kept fails, naming it") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Never)
      val dies = new Scripted((_, _) => throw new InMemoryDurable.Crash)
      val kept = new InMemoryToolSets
      assertThrows[InMemoryDurable.Crash](
        durable.run(turn.workflowId)(hostedBody(entries, dies, edge, toolSets = kept))
      )
      val id = kept.sets.map(s => grit.core.tool.ToolSetId.value(s.id)).mkString
      durable.run(turn.workflowId)(
        hostedBody(entries, model(), edge, toolSets = new InMemoryToolSets)
      ) ==>
        s"failed: Store(no tool set $id is kept)"
    }
  }
}
