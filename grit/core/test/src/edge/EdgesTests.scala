package grit.core.edge

import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Namespace, Place}
import grit.core.tool.{Args, Field, Gate, Outcome, Retry, Tool, ToolName, ToolSpec, Toolbox}

import utest.*

/** [[Edges]]: which requests an edge may run, and how it runs one. */
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

  /** `echo`, free, and `shout`, which asks first: each answers with its argument. */
  private val tools = {
    val text = Args.of((text = Field.text("What."))).map(_.text)
    Toolbox
      .of(
        new Tool(
          ToolSpec(ToolName("echo"), "Echo.", text),
          Gate.Free,
          t => t,
          t => Outcome.Done(t)
        ),
        new Tool(
          ToolSpec(ToolName("shout"), "Shout.", text),
          Gate.Ask(t => t),
          t => t,
          t => Outcome.Done(t.toUpperCase)
        )
      )
      .getOrElse(throw new java.lang.AssertionError())
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

    test(
      "a request runs its tool as its permit says, and is refused when the permit does not match the gate"
    ) {
      Edges.run(request(api), tools) ==> Outcome.Done("hi")
      Edges.run(request(api, "shout", Permit.Approved), tools) ==> Outcome.Done("HI")
      Edges.run(request(api, "shout", Permit.Free), tools) ==>
        Outcome.Failed("shout asks first here; it did not run.")
      Edges.run(request(api, "echo", Permit.Approved), tools) ==>
        Outcome.Failed("echo does not ask first here; it did not run.")
    }

    test("a request of a later protocol is answered that this edge is too old") {
      Edges.run(request(api, protocol = ToolRequest.Protocol + 1), tools) ==>
        Outcome.Failed(
          s"This edge is too old for protocol ${ToolRequest.Protocol + 1}; it did not run."
        )
    }
  }
}
