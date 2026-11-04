package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolSchema}

import utest.*

/** What nothing later points back at ([[Referenced]]): which lines of a tool's result the
  * model's later output repeats a new name, path or line of, and which tools it never called.
  */
object ReferencedTests extends TestSuite {

  private val estimator = new CharEstimate(4.0, 4.0, Tokens.Zero)

  private val offered: Vector[ToolSchema] = Vector("read", "search", "edit", "write").map(n =>
    ToolSchema(n, s"The $n tool.", ujson.Obj("type" -> "object"))
  )

  private def calling(id: String, tool: String, args: (String, ujson.Value)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.ToolCall(ToolCallId(id), tool, ujson.Obj.from(args))),
      StopReason.ToolUse,
      Usage.Zero,
      "m"
    )

  private def said(text: String): Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, Usage.Zero, "m")

  /** The person asks `asked`; the model calls `search`, whose result is `content`; then it
    * writes `later`, the last of which is the reply.
    */
  private def pointed(asked: String, content: String, later: Message.Assistant*): Option[Int] = {
    val request = ModelRequest(
      "You are grit.",
      Vector(
        Message.User(asked),
        calling("t1", "search", "pattern" -> "x"),
        Message.ToolResult(ToolCallId("t1"), content, false)
      ) ++ later.dropRight(1),
      offered
    )
    Referenced.results(request, later.lastOption, estimator).headOption.map(_.pointed)
  }

  private val found = "core/src/host/Blobs.scala:9: trait Blobs extends Shared"
  private val other = "core/src/files/Other.scala:3: object Other"

  val tests = Tests {
    test("a result's line is pointed back at when a later call names a new path it holds") {
      pointed(
        "find the blobs",
        s"$found\n$other",
        calling("t2", "read", "path" -> "core/src/host/Blobs.scala"),
        said("done")
      ) ==> Some(found.length)
    }

    test("a name already sent before the result points back at none of it") {
      pointed("the FileArea, please", "FileArea is here", said("FileArea it is")) ==> Some(0)
    }

    test("a long line the later output repeats whole is pointed back at, behind a line number") {
      val line = "def remove(key) = drop(key) and then done"
      pointed(
        "fix it",
        s"     1\t$line\n     2\tthe end",
        calling("t2", "edit", "oldText" -> s"  $line\n"),
        said("fixed")
      ) ==> Some(s"     1\t$line".length)
    }

    test("a plain lower-case word is no reference, and a snake_case name is") {
      (
        pointed("go", "remove the blobs", said("I will remove them")),
        pointed("go", "grit.bench_trials keeps rows", said("bench_trials it is"))
      ) ==> (Some(0), Some("grit.bench_trials keeps rows".length))
    }

    test("a result names its tool, and costs its estimate") {
      val result = Message.ToolResult(ToolCallId("t1"), found, false)
      val request = ModelRequest(
        "s",
        Vector(Message.User("go"), calling("t1", "search", "pattern" -> "x"), result),
        offered
      )
      Referenced.results(request, None, estimator) ==> Vector(
        Referenced.Result(2, ToolCallId("t1"), "search", estimator.message(result), found.length, 0)
      )
    }

    test("each tool offered says whether any call named it, and what offering it costs") {
      val request = ModelRequest(
        "s",
        Vector(Message.User("go"), calling("t1", "search", "pattern" -> "x")),
        offered
      )
      val unused = estimator.tools(offered) + Tokens(
        -Tokens.value(estimator.tools(offered.filterNot(_.name == "write")))
      )
      Referenced
        .tools(request, Some(calling("t2", "read", "path" -> "a")), estimator)
        .map(t => (t.name, t.called)) ==>
        Vector(("read", true), ("search", true), ("edit", false), ("write", false))
      Referenced.tools(request, None, estimator).lastOption.map(_.cost) ==> Some(unused)
    }
  }
}
