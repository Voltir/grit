package grit.tools

import grit.core.approval.Approval
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Catalog, Pinned}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}

import utest.*

object ProbesTests extends TestSuite {

  private def reply(blocks: AssistantBlock*): Message.Assistant =
    Message.Assistant(blocks.toVector, StopReason.ToolUse, Usage(Tokens(1), Tokens(1), Tokens.Zero, None), "m")

  private def call(name: String, args: ujson.Value): AssistantBlock =
    AssistantBlock.ToolCall(ToolCallId("p1"), name, args)

  /** A pair that ignores strict, leaks harmony tokens, quotes numbers, takes its reasoning
    * back, and refuses a user message after a tool result. It answers `pick` without
    * reasoning; the checks after the strict one ride on `count`.
    */
  final class Quirky extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      requests = requests :+ request
      val last = request.messages.lastOption
      (request.tools.map(_.name), last) match {
        case (_, Some(Message.User(t))) if request.messages.size > 1 =>
          Left(ProviderError.Refused("HTTP 400: a user message may not follow a tool result"))
        case (_, Some(_: Message.ToolResult)) => Right(reply(AssistantBlock.Text("done")))
        case (Vector("pick"), _) => Right(reply(call("pick", ujson.Obj("color" -> "purple"))))
        case (Vector("count"), _) =>
          Right(
            reply(
              AssistantBlock.Reasoning("hm", Some(ujson.Arr(ujson.Obj("type" -> "reasoning.text")))),
              call("count<|channel|>commentary", ujson.Obj("n" -> "5"))
            )
          )
        case _ => Left(ProviderError.Refused("unexpected request"))
      }
    }
  }

  /** Models whose every pin is answered by `answer`, keeping the pins asked for. */
  final class Probed(answer: Provider^) extends Models {
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Pinned]
    def catalog(): Either[String, Catalog] = Left("unused")
    def provider(pinned: Pinned): Provider^ = { asked = asked :+ pinned; answer }
  }

  private val args = ujson.Obj("model" -> "deepseek/deepseek-v4.1-flash-20260910", "upstream" -> "fireworks", "runs" -> 2)

  val tests = Tests {
    test("probe_pair asks first, then measures each setting over its runs, calling the pair it names") {
      val provider = new Quirky
      val models = new Probed(provider)
      val box = Toolbox.of[{models}](Probes.probe(models)).fold(d => sys.error(d.toString), identity)
      box.bind(AssistantBlock.ToolCall(ToolCallId("c1"), "probe_pair", args), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated.ask ==> "Probe deepseek/deepseek-v4.1-flash-20260910 @ fireworks: 2 runs, " +
            "about 8 model calls at up to 300 output tokens each."
          provider.requests ==> Vector.empty
          gated(Approval.Approved) ==> Outcome.Done(
            """Probed deepseek/deepseek-v4.1-flash-20260910 @ fireworks, 2 runs:
              |- strict: a value off the schema's list came back in 2 of 2 answered runs → propose strict = ignored, held 2 of 2.
              |- names: a tool name carried `<|` in 2 of 2 answered runs → propose names = harmony-cut, held 2 of 2.
              |- repairs: a number came back quoted in 2 of 2 answered runs → propose repairs = quoted-number, held 2 of 2.
              |- replay: reasoning sent back was accepted in 2 of 2 answered runs → propose replay = details, held 2 of 2.
              |- afterResult: a user message after a tool result was refused in 2 of 2 answered runs → propose afterResult = in-last-result, held 2 of 2.
              |Propose each as written with propose_model_setting, probe `probe_pair`; a check with no answered run, or nothing to propose, is left out.""".stripMargin
          )
          models.asked.map(_.assignment.ref).distinct.map(_.toString) ==>
            Vector("deepseek/deepseek-v4.1-flash-20260910 @ fireworks")
          models.asked.map(_.assignment.maxTokens).distinct ==> Vector(300)
          // The strict check sends its schema strict.
          provider.requests.filter(_.tools.exists(_.name == "pick")).forall(_.tools.forall(_.strict)) ==> true
        case other => sys.error(s"not gated: $other")
      }
    }

    test("a pair that fails a check's call is counted as not answering it") {
      val down = new Provider {
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
          Left(ProviderError.Unavailable("HTTP 503"))
      }
      val models = new Probed(down)
      val box = Toolbox.of[{models}](Probes.probe(models)).fold(d => sys.error(d.toString), identity)
      box.bind(AssistantBlock.ToolCall(ToolCallId("c1"), "probe_pair", args), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated(Approval.Approved) ==> Outcome.Failed(
            "No check was answered in 2 runs; the last error: HTTP 503"
          )
        case other => sys.error(s"not gated: $other")
      }
    }

    test("a pair that behaves well is proposed as such; a check with nothing to propose says so") {
      val plain = new Provider {
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
          (request.tools.map(_.name), request.messages.lastOption) match {
            case (_, Some(_: Message.ToolResult)) | (_, Some(Message.User("Now say done."))) =>
              Right(reply(AssistantBlock.Text("done")))
            case (Vector("pick"), _) => Left(ProviderError.Unavailable("HTTP 503"))
            case _ => Right(reply(call("count", ujson.Obj("n" -> 5))))
          }
      }
      val models = new Probed(plain)
      val box = Toolbox.of[{models}](Probes.probe(models)).fold(d => sys.error(d.toString), identity)
      val one = ujson.Obj("model" -> "x/plain", "runs" -> 1)
      box.bind(AssistantBlock.ToolCall(ToolCallId("c1"), "probe_pair", one), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated(Approval.Approved) ==> Outcome.Done(
            """Probed x/plain, 1 runs:
              |- strict: not answered in 1 runs.
              |- names: a tool name carried `<|` in 0 of 1 answered runs → propose names = as-sent, held 1 of 1.
              |- repairs: a number came back quoted in 0 of 1 answered runs → nothing to propose.
              |- replay: no reasoning to send back in 1 runs.
              |- afterResult: a user message after a tool result was refused in 0 of 1 answered runs → propose afterResult = user-message, held 1 of 1.
              |Propose each as written with propose_model_setting, probe `probe_pair`; a check with no answered run, or nothing to propose, is left out.""".stripMargin
          )
        case other => sys.error(s"not gated: $other")
      }
    }
  }
}
