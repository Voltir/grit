package grit.tools

import grit.core.approval.Approval
import grit.core.id.ToolCallId
import grit.core.message.AssistantBlock
import grit.core.model.{Fact, FactBook, ModelId, ModelRef, NameRepair, Setting, Upstream}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}

import utest.*

object FactsTests extends TestSuite {

  /** A book keeping every fact it is given, or refusing all with `refuse`. */
  final class Book(refuse: Option[String] = None) extends FactBook {
    @caps.unsafe.untrackedCaptures
    var kept = Vector.empty[Fact]

    def keep(fact: Fact): Either[String, Unit] =
      refuse.toLeft { kept = kept :+ fact }
  }

  private def call(args: (String, ujson.Value)*): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), "propose_fact", ujson.Obj.from(args))

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
    test("propose_fact asks a person with the fact, and keeps it only once approved") {
      val book = new Book
      val box = Toolbox.of[{book}](Facts.propose(book)).fold(d => sys.error(d.toString), identity)
      box.bind(call(asSent*), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated.ask ==> "Keep a fact about deepseek/deepseek-v4.1-flash-20260910 @ fireworks: " +
            "names = as-sent, held in 4 of 5 runs of tool-probe."
          gated(Approval.Declined(None))
          book.kept ==> Vector.empty
          gated(Approval.Approved) ==> Outcome.Done(
            "Kept. The next turn's catalog has it; this turn keeps the one it started with."
          )
          book.kept ==> Vector(Fact(flash, Setting.Names(NameRepair.AsSent), "tool-probe", 5, 4))
        case other => sys.error(s"not gated: $other")
      }
    }

    test("a value the setting does not take, or more held than run, is refused before anyone is asked") {
      val book = new Book
      val box = Toolbox.of[{book}](Facts.propose(book)).fold(d => sys.error(d.toString), identity)
      def refused(args: (String, ujson.Value)*) = box.bind(call(args*), Repairs.All).left.map(_.message)
      val sent = asSent.toMap
      refused((sent + ("value" -> ujson.Str("sometimes"))).toSeq*) ==> Left(
        "The call to `propose_fact` was not run: `value` takes one of as-sent, harmony-cut, not \"sometimes\"." +
          " You sent: " + ujson.Obj.from((sent + ("value" -> ujson.Str("sometimes"))).toSeq).render()
      )
      refused((sent + ("held" -> ujson.Num(6))).toSeq*).left.map(_.contains("`held` takes at most `runs` (5), not 6")) ==>
        Left(true)
      refused((sent + ("model" -> ujson.Str("DeepSeek"))).toSeq*).left.map(_.contains("`model` takes an OpenRouter model id")) ==>
        Left(true)
    }

    test("a fact the book will not keep is a failed outcome saying why") {
      val book = new Book(Some("the database is down"))
      val box = Toolbox.of[{book}](Facts.propose(book)).fold(d => sys.error(d.toString), identity)
      box.bind(call(asSent*), Repairs.All) match {
        case Right(gated: Bound.Gated) =>
          gated(Approval.Approved) ==> Outcome.Failed("Not kept: the database is down")
        case other => sys.error(s"not gated: $other")
      }
    }
  }
}
