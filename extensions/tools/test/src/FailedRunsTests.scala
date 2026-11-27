package grit.tools

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{CheckoutState, Entry, Payload, TestWentOn}
import grit.core.visibility.Integrity

import utest.*

object FailedRunsTests extends TestSuite {

  /** A turn's entries: each call, its id made its own, then its answer, in order. */
  def turn(ran: (AssistantBlock.ToolCall, String)*): Vector[Entry] =
    ran.toVector.zipWithIndex.flatMap { case ((asked, answer), i) =>
      val call: AssistantBlock.ToolCall =
        AssistantBlock.ToolCall(ToolCallId(s"c$i"), asked.name, asked.arguments)
      Vector(
        Payload.Message(Message.said("", "test").copy(blocks = Vector(call))),
        Payload.Message(Message.ToolResult(call.id, answer, isError = false))
      ).zipWithIndex.map((p, k) =>
        Entry(
          EntryId(s"e$i-$k"),
          ConversationId("c"),
          TurnSeq(0),
          None,
          EntrySeq(2L * i + k),
          p,
          Instant.EPOCH,
          Integrity.Member
        )
      )
    }

  /** A call of `run` with `program` and `args`. */
  def run(program: String, args: String*): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(
      ToolCallId("c"),
      "run",
      ujson.Obj("program" -> program, "args" -> ujson.Arr.from(args))
    )

  private val failing = "Exit code 2. Output:\nok\n[error] a.scala:3: bad\n  detail\nok"

  val tests = Tests {
    test(
      "a turn's last failed run is its command, its exit code and its lines that look like failures, each after its line number, with the 3 after each"
    ) {
      FailedRuns.last(turn(run("make", "test") -> failing), None) ==> Some(
        CheckoutState.Failed.of(
          "make test",
          Some(2),
          "     2\t[error] a.scala:3: bad\n     3\t  detail\n     4\tok"
        )
      )
    }

    test(
      "a failure a later run of the same command passed after is passed over for the last before it; a killed run has no exit code"
    ) {
      val killed = "The command was killed after 60 seconds. Its output until then:\nFAILED one"
      FailedRuns.last(
        turn(
          run("make", "a") -> killed,
          run("make", "b") -> failing,
          run("make", "b") -> "Exit code 0. Output:\nok"
        ),
        None
      ) ==> Some(CheckoutState.Failed.of("make a", None, "     1\tFAILED one"))
    }

    test(
      "a failure with no line looking like one, a run not ended, a passing tail and the check's own command are not read"
    ) {
      val tail: AssistantBlock.ToolCall =
        AssistantBlock.ToolCall(ToolCallId("t"), "tail", ujson.Obj("id" -> "x"))
      FailedRuns.last(
        turn(
          run("grep", "-r", "nothing") -> "Exit code 1; no output.",
          run(
            "make",
            "long"
          ) -> "Still running after 30 seconds, as `x`; it is killed at 60 seconds.",
          tail -> "`make long`\nExit code 0; no output.",
          run("make", "check") -> failing
        ),
        Some(TestWentOn.command("make check"))
      ) ==> None
    }
  }
}
