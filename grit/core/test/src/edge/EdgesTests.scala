package grit.core.edge

import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Namespace, Place}
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
      Set.empty
    )
  }

  val tests = Tests {
    test(
      "a request in a place the edge registered routes to that directory; one below it does not"
    ) {
      Edges.authorize(request(api), by).map(_.root) ==> Right(dir("/work/api"))
      val below = Place.of(dir("/work/api/src"))
      Edges.authorize(request(below), by) ==> Left(Refused.NotHosted(below))
    }

    test("a registered place with no directory is refused") {
      val thread = Place.under(Namespace.Slack, Vector("acme", "dev"))
      Edges.authorize(request(thread), by.copy(places = Set(thread))) ==> Left(
        Refused.NoDirectory(thread)
      )
    }
  }
}
