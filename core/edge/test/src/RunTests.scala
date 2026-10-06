package grit.edge

import grit.core.edge.{Permit, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, Retry, Tool, ToolName, ToolSpec, Toolbox}

import utest.*

/** [[Run]]: how an edge runs a request it claimed. */
object RunTests extends TestSuite {

  private val api =
    Place.of(Directory.of("/work/api").getOrElse(throw new java.lang.AssertionError()))

  private def request(
      tool: String = "echo",
      permit: Permit = Permit.Free,
      protocol: Int = ToolRequest.Protocol,
      slot: CallSlot = slot(0, 0)
  ): ToolRequest = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    ToolRequest(
      slot,
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

  private def slot(round: Int, index: Int): CallSlot =
    CallSlot
      .of(TurnRef(ConversationId("c"), TurnSeq.First), round, index)
      .getOrElse(throw new java.lang.AssertionError())

  /** `echo`, free, and `shout`, which asks first: each answers with its argument. */
  private val tools = {
    val text = Args.of((text = Field.text("What."))).map(_.text)
    Toolbox
      .of(
        Tool(
          ToolSpec(ToolName("echo"), "Echo.", text),
          Gate.Free,
          t => t,
          t => Outcome.Done(t)
        ),
        Tool(
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

    test("a tool made with calling is told the request's slot, free or approved") {
      val text = Args.of((text = Field.text("What."))).map(_.text)
      def where(name: ToolName, gate: Gate[String]) =
        new Hosted(ToolSpec(name, "Where.", text), gate, t => t)
          .calling((_, at) => Outcome.Done(at.key))
      val told = Toolbox
        .of(where(ToolName("where"), Gate.Free), where(ToolName("asked"), Gate.Ask(t => t)))
        .getOrElse(throw new java.lang.AssertionError())
      Run.request(request("where", slot = slot(1, 2)), told) ==> Outcome.Done("tool:c:0:1:2")
      Run.request(request("asked", Permit.Approved, slot = slot(2, 1)), told) ==>
        Outcome.Done("tool:c:0:2:1")
    }

    test("a request of a later protocol is answered that this edge is too old") {
      Run.request(request(protocol = ToolRequest.Protocol + 1), tools) ==>
        Outcome.Failed(
          s"This edge is too old for protocol ${ToolRequest.Protocol + 1}; it did not run."
        )
    }
  }
}
