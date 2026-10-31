package grit.eval.harness.bench

import grit.core.message.{Tokens, Usage}

import utest.*

/** [[Consumed]] read from [[Streams]]' canned run, and from small streams made for one rule. */
object ConsumedTests extends TestSuite {

  private val checkout = "/work/task"

  private def call(id: String, usage: String, content: String = "") =
    s"""{"type":"assistant","message":{"id":"$id","model":"claude-haiku-5-5","content":[$content],"usage":{$usage}},"session_id":"s"}"""

  val tests = Tests {
    test(
      "each call is counted once, at its last usage, input holding the cache's reads and writes"
    ) {
      Consumed.of(Streams.at(checkout), checkout).map { c =>
        (
          c.calls.map(k => (k.id, Tokens.value(k.usage.output))),
          c.usage.copy(costUsd = None)
        )
      } ==> Right(
        (
          Vector("msg_01a" -> 40L, "msg_01b" -> 20L, "msg_01c" -> 30L),
          Usage(Tokens(6262), Tokens(90), Tokens(4100), None, Tokens(2150))
        )
      )
    }

    test("before the first edit under the checkout: the calls made and the bytes tools returned") {
      Consumed
        .of(Streams.at(checkout), checkout)
        .map(c => (c.callsBeforeEdit, c.readBeforeEdit)) ==> Right((Some(2), Some(48L)))
    }

    test("an edit outside the checkout is not the first edit") {
      val lines = Vector(
        call(
          "m1",
          """"input_tokens":1,"output_tokens":1""",
          """{"type":"tool_use","id":"t1","name":"Write","input":{"file_path":"/tmp/x.scala"}}"""
        ),
        """{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"t1","content":"done"}]}}""",
        call(
          "m2",
          """"input_tokens":1,"output_tokens":1""",
          s"""{"type":"tool_use","id":"t2","name":"MultiEdit","input":{"file_path":"$checkout/b.scala"}}"""
        )
      )
      Consumed.of(lines, checkout).map(c => (c.callsBeforeEdit, c.readBeforeEdit)) ==>
        Right((Some(1), Some(4L)))
    }

    test("a run that edits nothing under its checkout has neither measure before an edit") {
      Consumed
        .of(Streams.at(checkout).filterNot(_.contains("toolu_03")), checkout)
        .map(c => (c.callsBeforeEdit, c.readBeforeEdit)) ==> Right((None, None))
    }

    test("each call is priced at its own prompt's rates, never at the total's") {
      // Two Claude Haiku 5.5 calls of 60,000 uncached input tokens: each under the long-context
      // card's 100,000, though together over it. $0.10 a million: $0.006 each.
      val usage = """"input_tokens":60000,"output_tokens":0"""
      Consumed.of(Vector(call("m1", usage), call("m2", usage)), checkout).map(_.usage.costUsd) ==>
        Right(Some(BigDecimal("0.012")))
    }

    test("the result gives the run's session, its reason, Claude Code's cost and each denial") {
      Consumed.of(Streams.at(checkout), checkout).map(c => (c.session, c.ended)) ==> Right(
        (
          Some("5e55-a1"),
          Some(
            Ended(
              "error_max_turns",
              error = true,
              Some(BigDecimal("0.0031")),
              Vector(Denial("Bash", """{"command":"curl example.com"}"""))
            )
          )
        )
      )
    }

    test("the init line gives the source of the key claude used") {
      Consumed.of(Streams.at(checkout), checkout).map(_.apiKeySource) ==> Right(
        Some("apiKeyHelper")
      )
    }

    test("a run killed before its result has no ending") {
      Consumed.of(Streams.at(checkout).dropRight(1), checkout).map(_.ended) ==> Right(None)
    }

    test("a line that is not a JSON object is refused, by its number") {
      Consumed.of(Vector(Streams.run(0), "Waiting for background work"), checkout) ==>
        Left("line 2 of the run's output is not a JSON object")
    }
  }
}
