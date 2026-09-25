package grit.core.tool

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message}

import utest.*

/** [[Toolbox]]: distinct names, schemas in order, and what binding a call comes to; and the
  * result each [[Outcome]] gives the model.
  */
object ToolboxTests extends TestSuite {

  private val echo = new Tool(
    ToolSpec(
      ToolName("echo"),
      "Says it back.",
      Args.of((text = Field.text("What to say."))).map(_.text)
    ),
    Gate.Free,
    t => Outcome.Done(t)
  )

  private val shout = new Tool(
    ToolSpec(
      ToolName("shout"),
      "Says it loudly.",
      Args.of((text = Field.text("What to shout."))).map(_.text)
    ),
    Gate.Ask(t => s"shout $t"),
    t => Outcome.Done(t.toUpperCase)
  )

  private def call(name: String, args: ujson.Value): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), name, args)

  private val box = Toolbox.of(echo, shout) match {
    case Right(b) => b
    case Left(d) => throw new java.lang.AssertionError(s"duplicate $d")
  }

  val tests = Tests {
    test("a name offered twice is refused") {
      Toolbox.of(echo, shout, echo).map(_.names) ==> Left(DuplicateName(ToolName("echo")))
    }

    test("schemas and names come in the order given") {
      box.names ==> Vector(ToolName("echo"), ToolName("shout"))
      box.schemas(strict = true).map(s => (s.name, s.strict)) ==>
        Vector(("echo", true), ("shout", true))
    }

    test("a free call binds with nothing to ask, and runs") {
      val bound = box.bind(call("echo", ujson.Obj("text" -> "hi")))
      bound.map(b => (b.tool, b.ask, b())) ==> Right((ToolName("echo"), None, Outcome.Done("hi")))
    }

    test("a gated call carries what the person is shown, and runs only when asked to") {
      val bound = box.bind(call("shout", ujson.Obj("text" -> "hi")))
      bound.map(_.ask) ==> Right(Some("shout hi"))
      bound.map(_()) ==> Right(Outcome.Done("HI"))
    }

    test("an unknown tool is refused, naming the tools there are") {
      val refused = box.bind(call("sing", ujson.Obj()))
      refused.map(_.tool) ==> Left(CallError.Unknown("sing", box.names))
      refused.left.map(_.message) ==>
        Left("There is no tool named `sing`; the tools are `echo`, `shout`.")
    }

    test("arguments that do not read are refused, echoing what was sent") {
      val refused = box.bind(call("echo", ujson.Obj("text" -> 3)))
      refused.left.map(_.message) ==> Left(
        "The call to `echo` was not run: `text` takes text, not 3. You sent: {\"text\":3}"
      )
    }

    test("the echo of what was sent is cut") {
      val long = "x" * 1000
      box.bind(call("echo", ujson.Obj("text" -> 1, "pad" -> long))).left.map {
        case CallError.BadArgs(_, _, sent) => sent.length
        case _ => -1
      } ==> Left(CallError.Echoed)
    }

    test("an empty toolbox says no tool is offered") {
      val empty = Toolbox.of[{}]() match {
        case Right(b) => b
        case Left(d) => throw new java.lang.AssertionError(s"duplicate $d")
      }
      empty.bind(call("echo", ujson.Obj())).left.map(_.message) ==>
        Left("There is no tool named `echo`, and no tool is offered.")
    }

    test("each outcome as the model reads it") {
      val id = ToolCallId("c1")
      Outcome.Done("ok").result(id) ==> Message.ToolResult(id, "ok", isError = false)
      Outcome.Failed("bad").result(id) ==> Message.ToolResult(id, "bad", isError = true)
      Outcome.Denied(None).result(id) ==>
        Message.ToolResult(id, "The person declined this call; it did not run.", isError = true)
      Outcome.Denied(Some("not now")).result(id).content ==>
        "The person declined this call; it did not run. They said: not now"
      Outcome.Interrupted.result(id).isError ==> true
      assert(Outcome.Interrupted.result(id).content.contains("may have partly run"))
      CallError.Unknown("x", Vector.empty).outcome ==>
        Outcome.Failed("There is no tool named `x`, and no tool is offered.")
    }
  }
}
