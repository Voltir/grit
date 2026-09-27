package grit.edge

import grit.core.edge.{Permit, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.tool.{Args, Field, Gate, Outcome, Retry, Tool, ToolName, ToolSpec, Toolbox}

import utest.*

/** [[Run]]: how an edge runs a request it claimed. */
object RunTests extends TestSuite {

  private val api =
    Place.of(Directory.of("/work/api").getOrElse(throw new java.lang.AssertionError()))

  private def request(
      tool: String = "echo",
      permit: Permit = Permit.Free,
      protocol: Int = ToolRequest.Protocol
  ): ToolRequest = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      protocol,
      turn.conversationId,
      api,
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
      "a request runs its tool as its permit says, and is refused when the permit does not match the gate"
    ) {
      Run.request(request(), tools) ==> Outcome.Done("hi")
      Run.request(request("shout", Permit.Approved), tools) ==> Outcome.Done("HI")
      Run.request(request("shout", Permit.Free), tools) ==>
        Outcome.Failed("shout asks first here; it did not run.")
      Run.request(request("echo", Permit.Approved), tools) ==>
        Outcome.Failed("echo does not ask first here; it did not run.")
    }

    test("a request of a later protocol is answered that this edge is too old") {
      Run.request(request(protocol = ToolRequest.Protocol + 1), tools) ==>
        Outcome.Failed(
          s"This edge is too old for protocol ${ToolRequest.Protocol + 1}; it did not run."
        )
    }
  }
}
