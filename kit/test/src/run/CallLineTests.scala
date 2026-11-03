package grit.kit.run

import grit.core.message.{Tokens, Usage}
import grit.core.model.{ModelId, ModelRef, Upstream}

import utest.*

object CallLineTests extends TestSuite {

  private val ref = ModelRef(
    ModelId.of("anthropic/claude-haiku-5-5").getOrElse(sys.error("model")),
    Upstream.of("api.anthropic.com")
  )

  val tests = Tests {
    test(
      "a call's line names its pair, its input with cache reads and writes, its output and its cost in cents"
    ) {
      CallLine.answered(
        ref,
        Usage(Tokens(1200), Tokens(40), Tokens(1000), Some(BigDecimal("0.0002")), Tokens(150))
      ) ==> "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 1200 · " +
        "read 1000 · write 150 · out 40 · 0.02¢"
    }

    test(
      "an answered line reads back as its model and its tokens, the cost left unread; any other line as none"
    ) {
      val usage =
        Usage(Tokens(1200), Tokens(40), Tokens(1000), Some(BigDecimal("0.0002")), Tokens(150))
      (
        CallLine.read(CallLine.answered(ref, usage)),
        CallLine.read(CallLine.answered(ref.copy(upstream = None), usage)),
        CallLine.read("[provider] anthropic/claude-haiku-5-5 called with 3 message(s)"),
        CallLine.read("[main] w -> done: hi")
      ) ==> (
        Some(("anthropic/claude-haiku-5-5", usage.copy(costUsd = None))),
        Some(("anthropic/claude-haiku-5-5", usage.copy(costUsd = None))),
        None,
        None
      )
    }

    test("a call its provider did not price says its cost is unknown") {
      CallLine.spent(Usage(Tokens(10), Tokens(2), Tokens(0), None)) ==>
        "in 10 · read 0 · write 0 · out 2 · cost unknown"
    }
  }
}
