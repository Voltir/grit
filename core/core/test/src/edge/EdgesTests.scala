package grit.core.edge

import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Namespace, Place, Service}
import grit.core.tool.{Retry, ToolName}

import utest.*

/** [[Edges]]: which requests an edge may run. */
object EdgesTests extends TestSuite {

  private def dir(path: String): Directory =
    Directory.of(path).getOrElse(throw new java.lang.AssertionError(path))

  private val api = Place.of(dir("/work/api"))

  private val by = Registration(EdgeId("e1"), PrincipalId.Local, Set(api))

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
      Edges.authorize(request(api), by) ==> Right(Route.Directory(dir("/work/api")))
      val below = Place.of(dir("/work/api/src"))
      Edges.authorize(request(below), by) ==> Left(Refused.NotHosted(below))
    }

    test("a service place the edge registered routes to its service; one below it does not") {
      val github = Service.of("github").getOrElse(throw new java.lang.AssertionError())
      val issues = Place.under(Namespace.Service, Vector("github", "issues"))
      val serving = by.copy(places = Set(github.place, issues))
      Edges.authorize(request(github.place), serving) ==> Right(Route.Service(github))
      Edges.authorize(request(issues), serving) ==> Left(Refused.NoRoute(issues))
    }

    test("a registered place that is neither a directory nor a service is refused") {
      val thread = Place.under(Namespace.Slack, Vector("acme", "dev"))
      Edges.authorize(request(thread), by.copy(places = Set(thread))) ==> Left(
        Refused.NoRoute(thread)
      )
    }
  }
}
