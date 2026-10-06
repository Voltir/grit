package grit.tools

import grit.core.approval.Approval
import grit.core.id.{CallSlot, ConversationId, ToolCallId, TurnRef, TurnSeq}
import grit.core.message.AssistantBlock
import grit.core.model.{
  ModelId,
  ModelRef,
  ModelSetting,
  ModelSettings,
  NameRepair,
  Setting,
  Upstream
}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}

import utest.*

object TuningTests extends TestSuite {

  /** The call each test's tool is run as. */
  private val slot: CallSlot = CallSlot
    .of(TurnRef(ConversationId("c"), TurnSeq.First), 0, 0)
    .getOrElse(throw new java.lang.AssertionError("a slot"))

  /** A book keeping every setting it is given, or refusing all with `refuse`. */
  final class Book(refuse: Option[String] = None) extends ModelSettings {
    @caps.unsafe.untrackedCaptures
    var kept = Vector.empty[ModelSetting]

    def keep(setting: ModelSetting): Either[String, Unit] =
      refuse.toLeft { kept = kept :+ setting }
  }

  private def call(args: (String, ujson.Value)*): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), "propose_model_setting", ujson.Obj.from(args))

  private val flash = ModelRef(
    ModelId.of("deepseek/deepseek-v4.1-flash-20260910").getOrElse(sys.error("id")),
    Upstream.of("fireworks")
  )

  private val asSent = Seq[(String, ujson.Value)](
    "model" -> "deepseek/deepseek-v4.1-flash-20260910",
    "upstream" -> "fireworks",
    "setting" -> "names",
    "value" -> "as-sent",
    "probe" -> "tool-probe",
    "runs" -> 5,
    "held" -> 4
  )

  val tests = Tests {
    test("propose_model_setting asks a person with the setting, and keeps it only once approved") {
      val book = new Book
      val box = Toolbox
        .of[caps.CapSet^{book}](Tuning.propose(book))
        .fold(d => sys.error(d.toString), identity)
      box.bind(call(asSent*), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated.ask ==> "Keep a model setting for deepseek/deepseek-v4.1-flash-20260910 @ fireworks: " +
            "names = as-sent, held in 4 of 5 runs of tool-probe."
          gated(Approval.Declined(None), slot)
          book.kept ==> Vector.empty
          gated(Approval.Approved, slot) ==> Outcome.Done(
            "Kept. The next turn's catalog has it; this turn keeps the one it started with."
          )
          book.kept ==> Vector(
            ModelSetting(flash, Setting.Names(NameRepair.AsSent), "tool-probe", 5, 4)
          )
        case other => sys.error(s"not gated: $other")
      }
    }

    test(
      "a value the setting does not take, or more held than run, is refused before anyone is asked"
    ) {
      val book = new Book
      val box = Toolbox
        .of[caps.CapSet^{book}](Tuning.propose(book))
        .fold(d => sys.error(d.toString), identity)
      def refused(args: (String, ujson.Value)*) =
        box.bind(call(args*), Repairs.All).left.map(_.message)
      val sent = asSent.toMap
      refused((sent + ("value" -> ujson.Str("sometimes"))).toSeq*) ==> Left(
        "The call to `propose_model_setting` was not run: `value` takes one of as-sent, harmony-cut, not \"sometimes\"." +
          " You sent: " + ujson.Obj
            .from((sent + ("value" -> ujson.Str("sometimes"))).toSeq)
            .render()
      )
      refused((sent + ("held" -> ujson.Num(6))).toSeq*) ==> Left(
        "The call to `propose_model_setting` was not run: `held` takes at most `runs` (5), not 6." +
          " You sent: " + ujson.Obj.from((sent + ("held" -> ujson.Num(6))).toSeq).render()
      )
      refused((sent + ("model" -> ujson.Str("DeepSeek"))).toSeq*) ==> Left(
        "The call to `propose_model_setting` was not run: `model` takes an OpenRouter model id, such as openai/gpt-oss-120b, not \"DeepSeek\"." +
          " You sent: " + ujson.Obj.from((sent + ("model" -> ujson.Str("DeepSeek"))).toSeq).render()
      )
    }

    test("a setting the book will not keep is a failed outcome saying why") {
      val book = new Book(Some("the database is down"))
      val box = Toolbox
        .of[caps.CapSet^{book}](Tuning.propose(book))
        .fold(d => sys.error(d.toString), identity)
      box.bind(call(asSent*), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated(Approval.Approved, slot) ==> Outcome.Failed("Not kept: the database is down")
        case other => sys.error(s"not gated: $other")
      }
    }
  }
}
