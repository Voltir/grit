package grit.core.tool

import grit.core.approval.Approval
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{ArgRepair, NameRepair}

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
    t => t,
    t => Outcome.Done(t)
  )

  private val shout = new Tool(
    ToolSpec(
      ToolName("shout"),
      "Says it loudly.",
      Args.of((text = Field.text("What to shout."))).map(_.text)
    ),
    Gate.Ask(t => s"shout $t"),
    t => s"\n  $t\nloudly ",
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

    test("joined: the first's tools, then the second's; a name both hold is refused") {
      val echoes =
        Toolbox.of(echo).fold(d => throw new java.lang.AssertionError(d.toString), identity)
      val shouts =
        Toolbox.of(shout).fold(d => throw new java.lang.AssertionError(d.toString), identity)
      Toolbox.joined(shouts, echoes).map(_.names) ==> Right(
        Vector(ToolName("shout"), ToolName("echo"))
      )
      Toolbox.joined(box, echoes).map(_.names) ==> Left(DuplicateName(ToolName("echo")))
    }

    test("schemas and names come in the order given") {
      box.names ==> Vector(ToolName("echo"), ToolName("shout"))
      box.schemas(strict = true).map(s => (s.name, s.strict)) ==>
        Vector(("echo", true), ("shout", true))
    }

    test(
      "a tool that asks first tells the model that calling it is how the person is asked; a free one is as written"
    ) {
      // Pinned whole: the model is told this of every gated tool, hosted and gone ones too.
      val asks = "Calling this tool asks the person to approve the call; do not ask in your " +
        "reply. If they decline, nothing runs."
      box.schemas(strict = false).map(_.description) ==>
        Vector("Says it back.", s"Says it loudly. $asks")
      val hosted = new Hosted(shout.spec, Gate.Ask((t: String) => t), (t: String) => t)
      hosted.schema(false).description ==> s"Says it loudly. $asks"
      Tool.gone(hosted.entry).schema(false).description ==> s"Says it loudly. $asks"
      Tool.gone(echo.entry).schema(false).description ==> "Says it back."
    }

    test(
      "an advertised tool is offered as its entry, a call bound as sent and shown as its compact JSON cut to ShownMax; one that asks first is not offered"
    ) {
      val schema = ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("query" -> ujson.Obj("type" -> "string"))
      )
      val entry = ToolSet.Entry(ToolName("github_search"), "Searches.", schema, false, Retry.Rerun)
      val advertised =
        Hosted.advertised(entry).getOrElse(throw new java.lang.AssertionError("not offered"))
      advertised.entry ==> entry
      val sent = ujson.Obj("query" -> ("x" * 200), "page" -> 2)
      val boxed = Toolbox.of(advertised).getOrElse(throw new java.lang.AssertionError())
      boxed.bind(call("github_search", sent), Repairs.All) match {
        case Right(b: Bound.Hosted) =>
          (b.shown, b.arguments, b.ask, b.retry) ==>
            (s"github_search ${sent.render().take(Hosted.ShownMax)}", sent, None, Retry.Rerun)
        case other => throw new java.lang.AssertionError(s"not hosted: $other")
      }
      Hosted.ShownMax ==> 120
      boxed.bind(call("github_search", ujson.Str("x")), Repairs.All).map(_.tool) ==>
        Left(CallError.BadArgs(ToolName("github_search"), ArgsError.NotAnObject("\"x\""), "x"))
      Hosted.advertised(entry.copy(asks = true)).map(_.entry) ==> None
    }

    test("a free call binds with nothing to ask, and runs") {
      box.bind(call("echo", ujson.Obj("text" -> "hi")), Repairs.All) match {
        case Right(b: Bound.Free) =>
          (b.tool, b.shown, b()) ==> (ToolName("echo"), "echo hi", Outcome.Done("hi"))
        case other => throw new java.lang.AssertionError(s"not free: $other")
      }
    }

    test("a gated call carries what the person is shown, and runs only when approved") {
      box.bind(call("shout", ujson.Obj("text" -> "hi")), Repairs.All) match {
        case Right(b: Bound.Gated) =>
          b.ask ==> "shout hi"
          b.shown ==> "shout hi loudly"
          b(Approval.Approved) ==> Outcome.Done("HI")
          b(Approval.Declined(Some("too loud"))) ==> Outcome.Declined(Some("too loud"))
          b(Approval.Declined(None)) ==> Outcome.Declined(None)
          // Nobody answering is not the person declining.
          b(Approval.TimedOut) ==> Outcome.Unanswered
        case other => throw new java.lang.AssertionError(s"not gated: $other")
      }
    }

    test("a pure tool included comes first, and not under a name already offered") {
      box.including(shout).map(_.names) ==> Left(DuplicateName(ToolName("shout")))
      val loud = new Tool(
        ToolSpec(ToolName("loud"), "Loud.", Args.of((text = Field.text("What."))).map(_.text)),
        Gate.Free,
        _ => " ",
        t => Outcome.Done(t)
      )
      // Shown with nothing after its name when what it acts on is blank.
      Toolbox
        .of(loud)
        .toOption
        .flatMap(_.bind(call("loud", ujson.Obj("text" -> "x")), Repairs.All).toOption)
        .map(_.shown) ==> Some("loud")
      box.including(loud).map(_.names) ==>
        Right(Vector(ToolName("loud"), ToolName("echo"), ToolName("shout")))
    }

    test("an unknown tool is refused, naming the tools there are") {
      val refused = box.bind(call("sing", ujson.Obj()), Repairs.All)
      refused.map(_.tool) ==> Left(CallError.Unknown("sing", box.names))
      refused.left.map(_.message) ==>
        Left("There is no tool named `sing`; the tools are `echo`, `shout`.")
    }

    test("arguments that do not read are refused, echoing what was sent") {
      val refused = box.bind(call("echo", ujson.Obj("text" -> 3)), Repairs.All)
      refused.left.map(_.message) ==> Left(
        "The call to `echo` was not run: `text` takes text, not 3. You sent: {\"text\":3}"
      )
    }

    test("a name with leaked harmony tokens binds to the tool it names") {
      val bound =
        box.bind(call("echo<|channel|>commentary", ujson.Obj("text" -> "hi")), Repairs.All)
      bound.map(_.tool) ==> Right(ToolName("echo"))
      Toolbox.named("grep<|channel|>json", NameRepair.HarmonyCut) ==> "grep"
      Toolbox.named("<|x", NameRepair.HarmonyCut) ==> ""
      box.bind(call("sing<|channel|>x", ujson.Obj()), Repairs.All).left.map(_.message) ==>
        Left("There is no tool named `sing<|channel|>x`; the tools are `echo`, `shout`.")
    }

    test("a name is taken as sent when its pair's names are not repaired") {
      val asSent = Repairs(NameRepair.AsSent, ArgRepair.values.toSet)
      Toolbox.named("grep<|channel|>json", NameRepair.AsSent) ==> "grep<|channel|>json"
      box
        .bind(call("echo<|channel|>commentary", ujson.Obj("text" -> "hi")), asSent)
        .left
        .map(_.message) ==>
        Left("There is no tool named `echo<|channel|>commentary`; the tools are `echo`, `shout`.")
      box.bind(call("echo", ujson.Obj("text" -> "hi")), asSent).map(_.tool) ==> Right(
        ToolName("echo")
      )
    }

    test("arguments that were not JSON are refused, echoing their text") {
      // The wire keeps text that does not parse as a JSON string (OpenRouterJson).
      box.bind(call("echo", ujson.Str("{text: hi")), Repairs.All).left.map(_.message) ==> Left(
        "The call to `echo` was not run: The arguments must be a JSON object, not " +
          "\"{text: hi\". You sent: {text: hi"
      )
    }

    test("the echo of what was sent is cut") {
      val long = "x" * 1000
      box.bind(call("echo", ujson.Obj("text" -> 1, "pad" -> long)), Repairs.All).left.map {
        case CallError.BadArgs(_, _, sent) => sent.length
        case _ => -1
      } ==> Left(CallError.Echoed)
    }

    test("an empty toolbox says no tool is offered") {
      val empty = Toolbox.of[{}]() match {
        case Right(b) => b
        case Left(d) => throw new java.lang.AssertionError(s"duplicate $d")
      }
      empty.bind(call("echo", ujson.Obj()), Repairs.All).left.map(_.message) ==>
        Left("There is no tool named `echo`, and no tool is offered.")
    }

    test("each outcome as the model reads it") {
      val id = ToolCallId("c1")
      Outcome.Done("ok").result(id) ==> Message.ToolResult(id, "ok", isError = false)
      Outcome.Failed("bad").result(id) ==> Message.ToolResult(id, "bad", isError = true)
      // Each labelled first, so the model reads neither as a broken result; and nobody
      // answering is never told as the person declining, nor grit's words as theirs.
      Outcome.Declined(None).result(id) ==>
        Message.ToolResult(
          id,
          "Declined: the person declined this call; it did not run.",
          isError = true
        )
      Outcome.Declined(Some("not now")).result(id).content ==>
        "Declined: the person declined this call; it did not run. Their reason: not now"
      Outcome.Unanswered.result(id) ==>
        Message.ToolResult(
          id,
          "Unanswered: nobody answered in time; the call did not run.",
          isError = true
        )
      Outcome.Interrupted.result(id).isError ==> true
      assert(Outcome.Interrupted.result(id).content.contains("may have partly run"))
      CallError.Unknown("x", Vector.empty).outcome ==>
        Outcome.Failed("There is no tool named `x`, and no tool is offered.")
    }
  }
}
