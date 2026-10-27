package grit.app.probe

import grit.core.message.Message
import grit.core.prompt.Fenced
import grit.core.provider.ModelRequest

import utest.*

/** [[FenceProbe]]'s requests, without a model: its before and after arms. */
object FenceProbeTests extends TestSuite {

  /** Each tool result's content among `messages`, and the message after it. */
  private def afterResults(messages: Vector[Message]): Vector[(String, Option[Message])] =
    messages.zipWithIndex.collect { case (r: Message.ToolResult, i) =>
      (r.content, messages.lift(i + 1))
    }

  /** What each turn arm made of `after`: the reminder arm's added message, the user-role
    * arm's message count over `after`'s and what follows each of its results, and whether the
    * material arm's system text holds its line right after the rule.
    */
  private def shapes(after: ModelRequest, arms: Vector[(String, ModelRequest)]) =
    arms.map {
      case ("reminder", r) =>
        ("reminder", r.messages.dropRight(1) == after.messages, r.messages.lastOption)
      case ("user-role", r) =>
        ("user-role", r.messages.size - after.messages.size, afterResults(r.messages))
      case ("material", r) =>
        (
          "material",
          r.system.contains(Fenced.Rule + FenceProbe.NeverAsks),
          r.messages == after.messages
        )
      case (other, _) => (other, "unknown arm")
    }

  val tests = Tests {
    test("each sample's request before fencing has its request's shape, and differs from it") {
      val literals = FenceProbe.literals
      FenceRequests.all.map(b =>
        (b.name, FenceProbe.before(b.request, literals(b.name)).map(_ == b.request))
      ) ==> FenceRequests.all.map(b => (b.name, Right(false)))
    }

    test(
      "each turn arm changes its sample's request one way: a reminder last, each result's output in the user message after it, or the never-asks line after the rule"
    ) {
      val turns = FenceRequests.all.filter(_.name.startsWith("turn-"))
      assert(turns.nonEmpty)
      turns.map(b => (b.name, shapes(b.request, FenceProbe.turnArms(b.request)))) ==>
        turns.map { b =>
          val fenced = afterResults(b.request.messages).map(_._1)
          assert(fenced.nonEmpty && fenced.forall(_.startsWith("<tool_output")))
          (
            b.name,
            Vector(
              ("reminder", true, Some(Message.User(FenceProbe.Reminder))),
              (
                "user-role",
                fenced.size,
                fenced.map(f => ("Its output is in the next message.", Some(Message.User(f))))
              ),
              ("material", true, true)
            )
          )
        }
    }
  }
}
