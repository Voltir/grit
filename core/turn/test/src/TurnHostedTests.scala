package grit.turn

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.approval.Approval
import grit.core.durable.InMemoryDurable
import grit.core.edge.{InMemoryEdges, Permit}
import grit.core.id.{TestCallSlots, TestPrincipalIds, ToolCallId}
import grit.core.identity.Principal
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.{Namespace, Place, Reaches, Service, WorksIn}
import grit.core.prompt.{Fragment, Layer}
import grit.core.store.Origin
import grit.core.store.{InMemoryEntryStore, InMemoryToolSets}
import grit.core.tool.{Believed, CallError, Hosted, Outcome, Retry, ToolName, ToolSet, Writes}
import grit.core.visibility.{
  Accounts,
  Clearance,
  Integrity,
  Itemised,
  Label,
  Level,
  Provenance,
  Readers,
  ResultPart
}

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

  private val internal = Label.at(Level.Internal)
  private val restricted = Label.at(Level.Restricted)

  /** `github` readable and cleared at internal; every pair of [[RoutedCatalog]] at restricted. */
  private val githubInternal: grit.core.visibility.Visibility =
    labelling(
      Vector(github.place -> Label.Public),
      grit.core.visibility.Cleared(grit.core.visibility.Party.Service(github), internal) +:
        Vector(RoutedOpen, RoutedFlash, RoutedSummary, RoutedQuery).flatMap(r =>
          grit.core.visibility.Party.of(r).map(grit.core.visibility.Cleared(_, restricted))
        )
    )

  /** A turn routed at restricted (its reads at restricted) whose transactions run at internal
    * after: its clearance fell mid-turn.
    */
  private def fallen(): InMemoryDurable =
    new InMemoryDurable(resolve = _ => Clearance.of(internal), visibility = githubInternal)

  /** [[linkedBody]] under [[RoutedCatalog]], its reads at restricted: what it is routed for. */
  private def routedRestricted(entries: InMemoryEntryStore, provider: Scripted, edge: Served)(
      id: grit.core.id.WorkflowId
  )(using grit.core.durable.Durable^): String =
    hostedBody(
      entries,
      provider,
      edge,
      hosted = Vector.empty,
      from = Origin.Slack("T1", "C1", "1.0"),
      worksIn = Vector(WorksIn(Place.Everywhere, github)),
      db = grit.core.act.MovesFixtures.FakeDb(Clearance.of(restricted), githubInternal),
      catalog = RoutedCatalog
    )(id)

  private val elsewhere: Service =
    Service.of("elsewhere").fold(e => throw new java.lang.AssertionError(e), identity)

  /** An edge at `elsewhere`'s place over `edges`, advertising `post_x`, serving each request
    * as `serve` says.
    */
  private def atElsewhere(
      edges: InMemoryEdges,
      durable: InMemoryDurable,
      serve: grit.core.edge.ToolRequest -> Serve,
      writes: Option[Writes[Place]] = None
  ): Served = {
    val s = new Served(edges, durable, serve, elsewhere.place)
    val entry = ToolSet.Entry(
      ToolName("post_x"),
      "Posts.",
      ujson.Obj("type" -> "object"),
      false,
      Retry.Rerun,
      writes
    )
    s.advertise(Hosted.advertised(entry).toVector)
    s
  }

  private val general: Place =
    Place.read("slack:T/C1").fold(e => throw new java.lang.AssertionError(e), identity)

  /** `post_x`'s destinations: `general`, at [[general]]. */
  private val toGeneral: Writes[Place] =
    Writes
      .of(VectorMap("general" -> general), p => p, "Where to post.")
      .fold(e => throw new java.lang.AssertionError(e), _.placed)

  /** A model that calls `post_x` with `arguments` on its first call and answers "done" after. */
  private def posting(arguments: ujson.Value): Scripted =
    new Scripted((_, n) =>
      Right(
        if (n > 0) calling("done")
        else
          Message.Assistant(
            Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "post_x", arguments)),
            StopReason.ToolUse,
            Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
            "m"
          )
      )
    )

  /** The body of the fixture's TUI turn, reaching `elsewhere`, whose edge is `there`. */
  private def reachingBody(
      entries: InMemoryEntryStore,
      provider: Scripted,
      edge: Served,
      there: Served,
      room: Option[LabelledRoom] = None
  )(id: grit.core.id.WorkflowId)(using grit.core.durable.Durable^): String =
    room match {
      case Some(r) =>
        hostedBody(
          entries,
          provider,
          edge,
          reaches = Vector(Reaches(Place.Everywhere, elsewhere)),
          reached = Vector(there),
          db = r.db,
          catalog = RoutedCatalog
        )(id)
      case None =>
        hostedBody(
          entries,
          provider,
          edge,
          reaches = Vector(Reaches(Place.Everywhere, elsewhere)),
          reached = Vector(there)
        )(id)
    }

  /** The fixture's TUI room, labelled confidential {trial}, for an asker cleared at `asker`,
    * under a deployment placing elsewhere as `elsewhereAt` says and nothing else but the room,
    * with every pair of [[RoutedCatalog]] cleared at the room's label.
    */
  private final case class LabelledRoom(asker: Label, elsewhereAt: Option[Label]) {
    private val room = Place.of(checkout)
    private val roomLabel = Label.at(Level.Confidential, trial)
    private val clearance = Clearance.inRoom(room, roomLabel, asker)
    private val visibility = labelling(
      Vector(room -> roomLabel) ++ elsewhereAt.map(elsewhere.place -> _),
      Vector(RoutedOpen, RoutedFlash, RoutedSummary, RoutedQuery).flatMap(r =>
        grit.core.visibility.Party.of(r).map(grit.core.visibility.Cleared(_, roomLabel))
      )
    )

    /** A durable whose every transaction is in the room. */
    def durable: InMemoryDurable =
      new InMemoryDurable(resolve = _ => clearance, visibility = visibility)

    /** Reads in the room, as the turn's `route` makes them. */
    def db: grit.core.store.Db = grit.core.act.MovesFixtures.FakeDb(clearance, visibility)
  }

  val tests = Tests {
    test("a round's free hosted calls are sent in one step before the first is waited on") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "peek twice")
      val durable = new InMemoryDurable
      val edge = served(durable, q => Serve.Now(Outcome.Done(s"read ${q.arguments("path").str}")))
      val provider = model(("t1", "fetch", "a.txt"), ("t2", "fetch", "b.txt"))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge)) ==> Done
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "ask:0:record").take(4) ==>
        Vector("ask:0:record", "call:0:send", "DBOS.recv", "DBOS.sleep")
      edge.sent.map(q => (q.slot, q.permit)) ==> Vector(
        (TestCallSlots.at(turn, index = 0), Permit.Free),
        (TestCallSlots.at(turn, index = 1), Permit.Free)
      )
      results(provider).map(_.content) ==> Vector("read a.txt", "read b.txt")
    }

    test(
      "a hosted call at a directory, which is no service, is believed at its tool's output, whatever the edge's outcome says"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val trusted = fetchBelieved(Believed.At(Integrity.Member))
      val edge = new Served(
        new InMemoryEdges,
        durable,
        _ => Serve.Now(Outcome.Done("read", Some(Integrity.Authority)))
      )
      edge.advertise(Vector(trusted))
      val provider = model(("t1", "fetch", "a.txt"))
      durable.run(turn.workflowId)(
        hostedBody(entries, provider, edge, hosted = Vector(trusted))
      ) ==> Done
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
      entries
        .get(slot.resultId)(using grit.dbos.sql.TestTx.fake)
        .map(_.map(_.integrity)) ==> Right(Some(Integrity.Member))
    }

    test("a turn's request records its asker") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      val provider = model(("t1", "fetch", "a.txt"))
      val dana = askedBy(Some(Principal.Person(TestPrincipalIds.stored("dana"), Set.empty)))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge, askers = dana)) ==> Done
      edge.sent.map(_.principal) ==> Vector(TestPrincipalIds.stored("dana"))
    }

    test("a turn with no asker sends no request and answers Failed") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      val provider = model(("t1", "fetch", "a.txt"))
      val nobody = askedBy(None)
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge, askers = nobody)) ==> Done
      edge.sent ==> Vector.empty
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("This turn has no asker to make this call for, so it did not run.", true))
    }

    test("a turn whose asker cannot be read sends no request and fails, naming the store") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      val provider = model(("t1", "fetch", "a.txt"))
      val unread = readsAskers(Vector.empty)
      val out = durable.run(turn.workflowId)(hostedBody(entries, provider, edge, askers = unread))
      (out, edge.sent) ==> ("failed: Store(DatabaseError(down))", Vector.empty)
    }

    test(
      "a call unsent for want of an asker whose asker cannot be read again says why is unknown"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      val provider = model(("t1", "fetch", "a.txt"))
      val once = readsAskers(Vector(Right(None)))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge, askers = once)) ==> Done
      (edge.sent, results(provider).map(r => (r.content, r.isError))) ==> (
        Vector.empty,
        Vector(
          (
            "This call did not run, and why could not be read: Store(DatabaseError(down))",
            true
          )
        )
      )
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
        Vector("call:0:0:expire", "call:0:0")
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

    test(
      "a turn whose clearance fell after it was routed sends only where its routed classification is cleared: a service cleared for the fallen floor alone is not offered, and nothing is sent"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "search")
      val durable = fallen()
      val edge = atGithub(durable, _ => Serve.Now(Outcome.Done("found")))
      val provider = model(("t1", "github_search", "grit"))
      durable.run(turn.workflowId)(routedRestricted(entries, provider, edge)) ==> Done
      (provider.requests.headOption.map(_.tools.map(_.name)), edge.sent.size) ==>
        (Some(Vector()), 0)
    }

    test(
      "a hosted call answered after the turn's clearance rose fails the turn, asking again, and keeps no answer"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "search")
      val rising = new grit.core.visibility.RisingClearance(internal, restricted)
      val durable = new InMemoryDurable(resolve = _ => rising.now, visibility = githubInternal)
      // The edge answers at once, as the conversation's clearance rises.
      val edge = atGithub(
        durable,
        _ => {
          rising.rise()
          Serve.Now(Outcome.Done("found"))
        }
      )
      val ran = durable.run(turn.workflowId)(
        hostedBody(
          entries,
          model(("t1", "github_search", "grit")),
          edge,
          hosted = Vector.empty,
          from = Origin.Slack("T1", "C1", "1.0"),
          worksIn = Vector(WorksIn(Place.Everywhere, github)),
          db = grit.core.act.MovesFixtures.FakeDb(Clearance.of(internal), githubInternal),
          catalog = RoutedCatalog,
          jot = new FakeJot(() => rising.now, githubInternal)
        )
      )
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
      (
        edge.sent.size,
        ran.startsWith("failed: Store("),
        ran.contains("ask again"),
        entries.get(slot.resultId)(using grit.dbos.sql.TestTx.fake)
      ) ==> (1, true, true, Right(None))
    }

    test(
      "a hosted call whose wait ends after the turn's clearance rose fails the turn, asking again, before any further model call: its answer reaches no model"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "search")
      val rising = new grit.core.visibility.RisingClearance(internal, restricted)
      val durable = new InMemoryDurable(resolve = _ => rising.now, visibility = githubInternal)
      // The edge claims the request as the conversation's clearance rises, and answers it
      // only when the turn's wait next looks at the row.
      val edge = atGithub(
        durable,
        _ => {
          rising.rise()
          Serve.Later(Outcome.Done("found above"))
        }
      )
      val provider = model(("t1", "github_search", "grit"))
      val ran = durable.run(turn.workflowId)(
        hostedBody(
          entries,
          provider,
          edge,
          hosted = Vector.empty,
          from = Origin.Slack("T1", "C1", "1.0"),
          worksIn = Vector(WorksIn(Place.Everywhere, github)),
          db = grit.core.act.MovesFixtures.FakeDb(Clearance.of(internal), githubInternal),
          catalog = RoutedCatalog,
          jot = new FakeJot(() => rising.now, githubInternal)
        )
      )
      (
        edge.sent.size,
        provider.requests.size,
        ran.startsWith("failed: Store("),
        ran.contains("ask again"),
        provider.requests.exists(_.toString.contains("found above"))
      ) ==> (1, 1, true, true, false)
    }

    test(
      "a hosted call at a service is believed at its service's declared output, whatever its tool declares"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      // The tool declares member; github declares nothing, so its results are untrusted.
      val trusted = fetchBelieved(Believed.At(Integrity.Member))
      val edge = new Served(
        new InMemoryEdges,
        durable,
        _ => Serve.Now(Outcome.Done("read", Some(Integrity.Authority))),
        github.place
      )
      edge.advertise(Vector(trusted))
      durable.run(turn.workflowId)(
        hostedBody(
          entries,
          model(("t1", "fetch", "a.txt")),
          edge,
          hosted = Vector(trusted),
          from = Origin.Slack("T1", "C1", "1.0"),
          worksIn = Vector(WorksIn(Place.Everywhere, github))
        )
      ) ==> Done
      entries
        .get(TurnTools.Slot(TestCallSlots.at(turn)).resultId)(using grit.dbos.sql.TestTx.fake)
        .map(_.map(_.integrity)) ==> Right(Some(Integrity.Untrusted))
    }
    test(
      "an answer no edge gave is grit's own, believed at the turn's level; an edge's, at its service's"
    ) {
      // The turn runs at Outside, below the Member a turn is said at by default.
      def believed(serve: Serve): Option[Integrity] = {
        val entries = new InMemoryEntryStore
        val turn = say(entries, "search", Integrity.Outside)
        val durable = new InMemoryDurable
        val edge = atGithub(durable, _ => serve)
        val provider = model(("t1", "github_search", "grit"))
        durable.run(turn.workflowId)(linkedBody(entries, provider, edge)) ==> Done
        entries
          .get(TurnTools.Slot(TestCallSlots.at(turn)).resultId)(using grit.dbos.sql.TestTx.fake)
          .toOption
          .flatten
          .map(_.integrity)
      }
      // Unclaimed: "No edge is serving github right now".
      believed(Serve.Never) ==> Some(Integrity.Outside)
      believed(Serve.Now(Outcome.Done("found"))) ==> Some(Integrity.Untrusted)
      // Unsent: "This turn has no asker to make this call for".
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch", Integrity.Outside)
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Now(Outcome.Done("read")))
      durable.run(turn.workflowId)(
        hostedBody(entries, model(("t1", "fetch", "a.txt")), edge, askers = askedBy(None))
      ) ==> Done
      entries
        .get(TurnTools.Slot(TestCallSlots.at(turn)).resultId)(using grit.dbos.sql.TestTx.fake)
        .map(_.map(_.integrity)) ==> Right(Some(Integrity.Outside))
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
      there.sent.map(q => (q.slot, q.workspace)) ==> Vector(
        (TestCallSlots.at(turn, index = 1), elsewhere.place)
      )
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("read a.txt", false), ("posted at service:elsewhere", false))
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "ask:0:record").take(3) ==>
        Vector("ask:0:record", "call:0:send", "call:0:send:service:elsewhere")
    }

    test(
      "a writing call naming a place outside its offer is answered Unwritable, and no request is sent"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "post it")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Never)
      val there = atElsewhere(
        edge.edges,
        durable,
        _ => Serve.Now(Outcome.Done("posted")),
        Some(toGeneral)
      )
      val provider = posting(ujson.Obj("to" -> "random", "text" -> "hi"))
      durable.run(turn.workflowId)(reachingBody(entries, provider, edge, there)) ==> Done
      results(provider).map(r => (r.content, r.isError)) ==> Vector(
        (
          CallError.Unwritable(ToolName("post_x"), Some("random"), Vector("general")).message,
          true
        )
      )
      there.sent ==> Vector.empty
    }

    test(
      "a writing call naming a place of its offer is a request whose destination is that place, its arguments without `to`"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "post it")
      val durable = new InMemoryDurable
      val edge = served(durable, _ => Serve.Never)
      val there = atElsewhere(
        edge.edges,
        durable,
        _ => Serve.Now(Outcome.Done("posted")),
        Some(toGeneral)
      )
      val provider = posting(ujson.Obj("to" -> "general", "text" -> "hi"))
      durable.run(turn.workflowId)(reachingBody(entries, provider, edge, there)) ==> Done
      there.sent.map(q => (q.workspace, q.destination, q.arguments)) ==> Vector(
        (elsewhere.place, Some(general), ujson.Obj("text" -> "hi"))
      )
    }

    test(
      "in a labelled room, a reached tool at an unplaced service is not offered, and a call of it sends no request"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "post it")
      val room = LabelledRoom(Label.at(Level.Internal), None)
      val durable = room.durable
      val edge = served(durable, _ => Serve.Never)
      val there = atElsewhere(edge.edges, durable, _ => Serve.Now(Outcome.Done("posted")))
      val provider = model(("t1", "post_x", "hi"))
      durable.run(turn.workflowId)(
        reachingBody(entries, provider, edge, there, Some(room))
      ) ==> Done
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("There is no tool named `post_x`; the tools are `fetch`, `prod`.", true))
      durable
        .recordedSteps(turn.workflowId)
        .filter(n => Turn.Step.family(n).exists(Set(Turn.Step.Send, Turn.Step.Dispatch))) ==>
        Vector.empty
      there.sent ==> Vector.empty
    }

    test(
      "in a labelled room, a writing tool whose only place is unmapped is not offered, and a call of it sends no request"
    ) {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "post it")
      val room = LabelledRoom(Label.at(Level.Internal), Some(Label.at(Level.Internal)))
      val durable = room.durable
      val edge = served(durable, _ => Serve.Never)
      val there = atElsewhere(
        edge.edges,
        durable,
        _ => Serve.Now(Outcome.Done("posted")),
        Some(toGeneral)
      )
      val provider = posting(ujson.Obj("to" -> "general", "text" -> "hi"))
      durable.run(turn.workflowId)(
        reachingBody(entries, provider, edge, there, Some(room))
      ) ==> Done
      results(provider).map(r => (r.content, r.isError)) ==>
        Vector(("There is no tool named `post_x`; the tools are `fetch`, `prod`.", true))
      durable
        .recordedSteps(turn.workflowId)
        .filter(n => Turn.Step.family(n).exists(Set(Turn.Step.Send, Turn.Step.Dispatch))) ==>
        Vector.empty
      there.sent ==> Vector.empty
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
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
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
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "call:0:send").take(7) ==>
        Vector(
          "call:0:send",
          "DBOS.recv",
          "DBOS.sleep",
          "call:0:0:expire",
          "DBOS.recv",
          "DBOS.sleep",
          "call:0:0"
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
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
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
      edge.edges.answerAs(
        edge.registration,
        TestCallSlots.at(turn, index = 0),
        Outcome.Done("too late")
      ) ==> false
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
      edge.sent.map(q => (q.slot, q.permit)) ==> Vector(
        (TestCallSlots.at(turn, index = 0), Permit.Approved)
      )
      Turn.Step.named(durable.recordedSteps(turn.workflowId)).filter(_.contains(":0:0")) ==>
        Vector("call:0:0:approve", "call:0:0:wait", "call:0:0:dispatch", "call:0:0")
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
      provider.requests.headOption.map(_.system.split("\n\n").toVector.takeRight(3)) ==>
        Some(
          Vector(
            "Instructions from /checkout/AGENTS.md:",
            "Be brief.",
            TurnShown.answering(Instant.EPOCH)
          )
        )
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

    test("the offer step records the tool set and prompt it offers by id alone") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Never)
      val kept = new InMemoryToolSets
      val provider = new Scripted((_, _) => Right(calling("done")))
      durable.run(turn.workflowId)(hostedBody(entries, provider, edge, toolSets = kept)) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      val offered = durable.history(turn.workflowId).collectFirst {
        case InMemoryDurable.Step(Turn.Step.Offer, InMemoryDurable.Outcome.Output(out)) =>
          ujson.read(out)("ok")
      }
      // The texts are kept by content hash in their own stores: the step names them by id.
      val set = kept.sets.map(s => grit.core.tool.ToolSetId.value(s.id)).mkString
      offered.map(o => (o.obj.keySet.toSet, o("workspace").str, o("tools").str)) ==>
        Some((Set("workspace", "tools", "prompt", "shaped"), "fs:/checkout", set))
      val ids = offered.toVector
        .flatMap(_("prompt").arr.toVector)
        .flatMap(v => grit.core.prompt.FragmentId.of(v.str).toOption)
      // The line saying when the message was said is the request's, never a kept fragment.
      Prompts
        .prompt(ids)(using grit.dbos.sql.TestTx.fake)
        .map(p => s"${p.render}\n\n${TurnShown.answering(Instant.EPOCH)}") ==>
        Right(provider.requests.headOption.fold("")(_.system))
    }

    test("a turn rerun offers the tool set kept under the id its offer recorded") {
      val entries = new InMemoryEntryStore
      val turn = say(entries, "fetch")
      val durable = new InMemoryDurable
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Never)
      val dies = new Scripted((_, _) => throw new InMemoryDurable.Crash)
      val kept = new InMemoryToolSets
      assertThrows[InMemoryDurable.Crash](
        durable.run(turn.workflowId)(hostedBody(entries, dies, edge, toolSets = kept))
      )
      val rerun = new Scripted((_, _) => Right(calling("done")))
      durable.run(turn.workflowId)(hostedBody(entries, rerun, edge, toolSets = kept)) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      rerun.requests.headOption.map(_.tools.map(_.name)) ==>
        Some(kept.sets.flatMap(_.tools.map(e => ToolName.value(e.name))))
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

    test(
      "a cut answer's part in a confidential place never reaches an uncleared asker's model " +
        "request, kept entry or recorded step; a cleared asker's turn shows it"
    ) {
      // A service's extension cutting the fake server's answer, one part a line, each at the
      // place under `github` its line names.
      val cutting = new Accounts[String] {
        def items(answer: String): Itemised =
          Itemised.Parts(
            "found:",
            answer.linesIterator.toVector.map { line =>
              val at = Place.under(Namespace.Service, Vector(github.name, line.takeWhile(_ != ' ')))
              ResultPart(Provenance(at, Integrity.Member), Readers.AtPlace, line)
            }
          )
      }
      val answered = cutting.items("open notes\nsecret plans") match {
        case Itemised.Parts(head, parts, _) => Outcome.Done(head, parts = Some(parts))
        case other => throw new java.lang.AssertionError(other.toString)
      }
      val visibility = labelling(
        Vector(
          github.place -> Label.Public,
          Place.under(Namespace.Service, Vector(github.name, "open")) -> Label.Public,
          Place.under(Namespace.Service, Vector(github.name, "secret")) -> internal
        ),
        grit.core.visibility.Cleared(grit.core.visibility.Party.Service(github), internal) +:
          Vector(RoutedOpen, RoutedFlash, RoutedSummary, RoutedQuery).flatMap(r =>
            grit.core.visibility.Party.of(r).map(grit.core.visibility.Cleared(_, restricted))
          )
      )

      /** The model's tool result, the kept result's payload, and whether the secret part's text
        * is in any model request or recorded step, of a turn whose asker is cleared at `at`.
        */
      def asked(at: Label): (Vector[String], Either[String, Option[String]], Boolean, Boolean) = {
        val entries = new InMemoryEntryStore
        val turn = say(entries, "search")
        val durable = new InMemoryDurable(resolve = _ => Clearance.of(at), visibility = visibility)
        val edge = atGithub(durable, _ => Serve.Now(answered))
        val provider = model(("t1", "github_search", "grit"))
        durable.run(turn.workflowId)(
          hostedBody(
            entries,
            provider,
            edge,
            hosted = Vector.empty,
            from = Origin.Slack("T1", "C1", "1.0"),
            worksIn = Vector(WorksIn(Place.Everywhere, github)),
            db = grit.core.act.MovesFixtures.FakeDb(Clearance.of(at), visibility),
            catalog = RoutedCatalog,
            jot = new FakeJot(() => Clearance.of(at), visibility)
          )
        )
        val kept = entries
          .get(TurnTools.Slot(TestCallSlots.at(turn)).resultId)(using grit.dbos.sql.TestTx.fake)
          .map(_.map(_.payload.toString))
          .left
          .map(_.toString)
        (
          results(provider).map(_.content),
          kept,
          provider.requests.exists(_.toString.contains("secret plans")),
          durable.history(turn.workflowId).exists(_.toString.contains("secret plans"))
        )
      }
      val (uncleared, unclearedKept, unclearedSent, unclearedRecorded) = asked(Label.Public)
      val (cleared, clearedKept, _, _) = asked(internal)
      (
        uncleared,
        unclearedKept.map(_.exists(_.contains("open notes"))),
        unclearedKept.map(_.exists(_.contains("secret plans"))),
        unclearedSent,
        unclearedRecorded,
        cleared,
        clearedKept.map(_.exists(_.contains("secret plans")))
      ) ==> (
        Vector("found:\nopen notes"),
        Right(true),
        Right(false),
        false,
        false,
        Vector("found:\nopen notes\nsecret plans"),
        Right(true)
      )
    }
  }
}
