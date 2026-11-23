package grit.core.edge

import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Namespace, Place, Repository, Service, WorkName}
import grit.core.store.Origin
import grit.core.tool.{Retry, ToolName}

import utest.*

/** [[Edges]]: which requests an edge may run. */
object EdgesTests extends TestSuite {

  private def dir(path: String): Directory =
    Directory.of(path).getOrElse(throw new java.lang.AssertionError(path))

  private val api = Place.of(dir("/work/api"))

  private val by = Registration(EdgeId("e1"), PrincipalId.Local, Set(api))

  private def repository(name: String): Repository =
    Repository.of(name).getOrElse(throw new java.lang.AssertionError(name))

  private def run(name: String): WorkName =
    WorkName.of(name).getOrElse(throw new java.lang.AssertionError(name))

  private def request(
      at: Place,
      tool: String = "echo",
      permit: Permit = Permit.Free,
      protocol: Int = ToolRequest.Protocol
  ): ToolRequest = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      protocol,
      turn.conversationId,
      at,
      PrincipalId.Local,
      ToolName.of(tool).getOrElse(throw new java.lang.AssertionError()),
      permit,
      Retry.Rerun,
      ujson.Obj("text" -> "hi"),
      Set.empty,
      None
    )
  }

  val tests = Tests {
    test(
      "a request in a place the edge registered routes to that directory; one below it does not"
    ) {
      Edges.authorize(request(api), by, None) ==> Right(Route.Directory(dir("/work/api")))
      val below = Place.of(dir("/work/api/src"))
      Edges.authorize(request(below), by, None) ==> Left(Refused.NotHosted(below))
    }

    test("an edge hosts a request only in a place it registered, whatever lies within it") {
      val below = Place.of(dir("/work/api/src"))
      (Edges.hosts(request(api), by), Edges.hosts(request(below), by)) ==>
        (Right(()), Left(Refused.NotHosted(below)))
    }

    test("a repository's place routes to the run of that repository the request's origin names") {
      val grit = repository("grit")
      val serving = by.copy(places = Set(grit.place))
      Edges.authorize(request(grit.place), serving, Some(Origin.Work(grit, run("w1")))) ==>
        Right(Route.Repository(grit, run("w1")))
    }

    test(
      "a repository's place has no route for another repository's run, a terminal session, or no origin"
    ) {
      val grit = repository("grit")
      val other = repository("other")
      val serving = by.copy(places = Set(grit.place))
      val tui = Origin.Tui(dir("/work/api"), "default")
      Vector(Some(Origin.Work(other, run("w1"))), Some(tui), None).map(origin =>
        Edges.authorize(request(grit.place), serving, origin)
      ) ==> Vector.fill(3)(Left(Refused.NoRoute(grit.place)))
    }

    test("a place within a repository has no route, even for that repository's run") {
      val grit = repository("grit")
      val within = Place.under(Namespace.Repository, Vector("grit", "w1"))
      Edges.authorize(
        request(within),
        by.copy(places = Set(within)),
        Some(Origin.Work(grit, run("w1")))
      ) ==> Left(Refused.NoRoute(within))
    }

    test("a service place the edge registered routes to its service; one below it does not") {
      val github = Service.of("github").getOrElse(throw new java.lang.AssertionError())
      val issues = Place.under(Namespace.Service, Vector("github", "issues"))
      val serving = by.copy(places = Set(github.place, issues))
      Edges.authorize(request(github.place), serving, None) ==> Right(Route.Service(github))
      Edges.authorize(request(issues), serving, None) ==> Left(Refused.NoRoute(issues))
    }

    test(
      "an edge does not host a registered place where nothing runs: a thread, or a place within a repository"
    ) {
      val thread = Place.under(Namespace.Slack, Vector("acme", "dev"))
      val within = Place.under(Namespace.Repository, Vector("grit", "w1"))
      val grit = repository("grit").place
      val all = by.copy(places = Set(thread, within, grit))
      (
        Edges.hosts(request(thread), all),
        Edges.hosts(request(within), all),
        Edges.hosts(request(grit), all)
      ) ==> (Left(Refused.NoRoute(thread)), Left(Refused.NoRoute(within)), Right(()))
    }
    test("a registered place that is neither a directory nor a service is refused") {
      val thread = Place.under(Namespace.Slack, Vector("acme", "dev"))
      Edges.authorize(request(thread), by.copy(places = Set(thread)), None) ==> Left(
        Refused.NoRoute(thread)
      )
    }
  }
}
