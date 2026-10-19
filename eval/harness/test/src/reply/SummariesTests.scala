package grit.eval.harness.reply

import grit.core.id.WorkflowId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.ModelRequest

import utest.*

object SummariesTests extends TestSuite {

  private def asked(n: Int) = ModelRequest("system", Vector(Message.User(s"exchange $n")))

  private def answer(text: String, cost: Option[BigDecimal]): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens(1500), Tokens(150), Tokens.Zero, cost),
      "m"
    )

  val tests = Tests {
    test("the review lists each case's live summary and each model's, in order") {
      val cases = Vector(
        Summarised(WorkflowId("turn-1"), Some("live one"), asked(1)),
        Summarised(WorkflowId("turn-2"), None, asked(2))
      )
      val review = Summaries.review(
        "20261018",
        cases,
        Vector(
          "a/small" -> Map(
            asked(1) -> answer("a one", Some(BigDecimal("0.0002"))),
            asked(2) -> answer("a two", None)
          ),
          "b/mid" -> Map(asked(1) -> answer("b one", Some(BigDecimal("0.0008"))))
        )
      )
      review ==>
        """# Summaries: capture 20261018
          |
          |## turn-1
          |
          |**live**
          |
          |live one
          |
          |**a/small**
          |
          |a one
          |
          |(1500 in, 150 out, 0.02¢)
          |
          |**b/mid**
          |
          |b one
          |
          |(1500 in, 150 out, 0.08¢)
          |
          |## turn-2
          |
          |**live**
          |
          |none
          |
          |**a/small**
          |
          |a two
          |
          |(1500 in, 150 out, unpriced)
          |
          |**b/mid**
          |
          |not asked
          |""".stripMargin
    }
  }
}
