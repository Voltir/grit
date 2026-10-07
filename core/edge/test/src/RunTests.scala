package grit.edge

import scala.collection.immutable.VectorMap

import grit.core.edge.{Permit, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, PrincipalId, TestCallSlots, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.tool.{
  Args,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSpec,
  Toolbox,
  Writes
}

import utest.*

/** [[Run]]: how an edge runs a request it claimed. */
object RunTests extends TestSuite {

  private val api =
    Place.of(Directory.of("/work/api").getOrElse(throw new java.lang.AssertionError()))

  private def request(
      tool: String = "echo",
      permit: Permit = Permit.Free,
      protocol: Int = ToolRequest.Protocol,
      slot: CallSlot = TestCallSlots.First,
      arguments: ujson.Value = ujson.Obj("text" -> "hi"),
      destination: Option[Place] = None
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
      arguments,
      Set.empty,
      destination
    )
  }

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
      Run.request(request("where", slot = TestCallSlots.at(round = 1, index = 2)), told) ==> Outcome
        .Done("tool:c:0:1:2")
      Run.request(
        request("asked", Permit.Approved, slot = TestCallSlots.at(round = 2, index = 1)),
        told
      ) ==>
        Outcome.Done("tool:c:0:2:1")
    }

    test(
      "a writing tool is told its request's destination, not its arguments' `to`; with none, or one it has not, it fails unrun"
    ) {
      final case class Channel(id: String, at: Place) extends caps.Pure
      def at(written: String): Place =
        Place.read(written).getOrElse(throw new java.lang.AssertionError(written))
      val general = Channel("C1", at("slack:T/C1"))
      val random = Channel("C2", at("slack:T/C2"))
      val writes = Writes
        .of(VectorMap("general" -> general, "random" -> random), _.at, "Where.")
        .getOrElse(throw new java.lang.AssertionError())
      val ran = new StringBuilder
      val post = Hosted
        .writing(
          ToolSpec(ToolName("post"), "Posts.", Args.of((text = Field.text("What."))).map(_.text)),
          Gate.Free,
          t => t,
          writes
        )
        .getOrElse(throw new java.lang.AssertionError())
        .over { (text, to) =>
          ran.append(to.id)
          Outcome.Done(s"$text in ${to.id}")
        }
      val box = Toolbox.of(post).getOrElse(throw new java.lang.AssertionError())
      val sent = ujson.Obj("to" -> "random", "text" -> "hi")
      Run.request(request("post", arguments = sent, destination = Some(general.at)), box) ==>
        Outcome.Done("hi in C1")
      Run.request(request("post", arguments = sent), box) ==>
        Outcome.Failed("post was told no place to write to; it did not run.")
      Run.request(request("post", arguments = sent, destination = Some(at("slack:T/C9"))), box) ==>
        Outcome.Failed("post does not write to slack:T/C9 here; it did not run.")
      Run.request(request(destination = Some(general.at)), tools) ==>
        Outcome.Failed("echo does not write to slack:T/C1 here; it did not run.")
      ran.result() ==> "C1"
    }

    test("a request of a later protocol is answered that this edge is too old") {
      Run.request(request(protocol = ToolRequest.Protocol + 1), tools) ==>
        Outcome.Failed(
          s"This edge is too old for protocol ${ToolRequest.Protocol + 1}; it did not run."
        )
    }
  }
}
