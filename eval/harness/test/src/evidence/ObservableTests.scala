package grit.eval.harness.evidence

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.eval.harness.sent.{Cut, SentMarkdown, TurnSents}

import utest.*

/** An amendment's pattern matched over a trajectory ([[Observable]]), the trajectory rendered as
  * the bench keeps a grit coder's turns: [[SentMarkdown.render]], each tool result whole.
  */
object ObservableTests extends TestSuite {

  private def calls(id: String, name: String, args: ujson.Obj): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.ToolCall(ToolCallId(id), name, args)),
      StopReason.ToolUse,
      Usage.Zero,
      "m"
    )

  private def answered(id: String, content: String) =
    Message.ToolResult(ToolCallId(id), content, false)

  /** A coder's turn that read a file, ran a suite, read again and ran the suite again. */
  private val trajectory: String =
    SentMarkdown.render(
      TurnSents.sent(
        TurnSents.request(
          TurnSents.asked,
          calls("t1", "read", ujson.Obj("path" -> "a.scala")),
          answered("t1", "object A"),
          calls("t2", "run", ujson.Obj("command" -> "scripts/check grit.core")),
          answered("t2", "unit: 3 passed, 1 failed"),
          calls("t3", "read", ujson.Obj("path" -> "b.scala")),
          answered("t3", "object B"),
          calls("t4", "run", ujson.Obj("command" -> "scripts/check grit.core")),
          answered("t4", "unit: 4 passed, 0 failed")
        )
      ),
      "grit_bench_coder",
      Cut.Whole
    )

  private def observable(pattern: String): Observable =
    Observable.of(pattern).fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test("an observable names each trajectory line it matched, whole and in order") {
      Observable.matched(observable("calls `run` .*scripts/check"), trajectory) ==> Vector(
        "- calls `run` (`t2`) with `{\"command\":\"scripts/check grit.core\"}`",
        "- calls `run` (`t4`) with `{\"command\":\"scripts/check grit.core\"}`"
      )
    }
    test("an observable that never fired within a line matches nothing, though it spans two") {
      // The call's head and its line are in the trajectory, a line apart.
      Observable.matched(observable("assistant\\*\\*\\s+- calls `run`"), trajectory) ==>
        Vector.empty
    }
    test("a pattern is refused when empty, not compiling, or matching an empty line, saying why") {
      Vector("", "calls (run", "x*").map(Observable.of(_).left.map(_.takeWhile(_ != ':'))) ==>
        Vector(
          Left("the pattern is empty"),
          Left("the pattern does not compile"),
          Left("the pattern matches an empty line, so every line")
        )
    }
  }
}
