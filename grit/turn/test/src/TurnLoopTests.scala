package grit.turn

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.ToolUse
import grit.core.tool.Outcome

import utest.*
import TurnLoop.{Budget, Looped, Next, Pending, Round}

/** [[TurnLoop.next]] as a table, and [[TurnLoop.run]] over a scripted [[TurnLoop.Moves]]:
  * which moves a loop makes, in which order, with what each call sees. No `Durable`.
  */
object TurnLoopTests extends TestSuite {

  private def budget(n: Int): Budget = Budget.of(n) match {
    case Right(b) => b
    case Left(why) => throw new java.lang.AssertionError(why)
  }

  private def round(n: Int): Round = (0 until n).foldLeft(Round.First)((r, _) => r.next)

  private def call(id: String): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId(id), "read", ujson.Obj("path" -> id))

  private def reply(
      text: String,
      calls: Vector[String] = Vector.empty,
      stop: StopReason = StopReason.EndTurn
  ): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map(call),
      stop,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, None),
      "m"
    )

  /** Replies to each call as scripted, in order, and runs each tool call as `done:<id>`,
    * noting every move. A fake of the turn's durable steps, so it may hold its log.
    */
  final class Scripted(
      replies: Vector[Either[TurnFailure, Message.Assistant]],
      recorded: Either[TurnFailure, Unit] = Right(())
  ) extends TurnLoop.Moves {
    @caps.unsafe.untrackedCaptures
    var made = Vector.empty[String]

    def call(
        round: Round,
        use: ToolUse,
        exchange: Vector[Message]
    ): Either[TurnFailure, Message.Assistant] = {
      made = made :+ s"${round.step} $use seeing ${exchange.size}"
      replies.lift(round.index).getOrElse(Left(TurnFailure.Model("no reply scripted")))
    }

    def record(round: Round, reply: Message.Assistant): Either[TurnFailure, Unit] = {
      made = made :+ s"record ${round.index}"
      recorded
    }

    def settle(round: Round, index: Int, pending: Pending): Either[TurnFailure, Outcome] = {
      made = made :+ s"settle ${round.index}:$index"
      pending match {
        case Pending.Run(c) => Right(Outcome.Done(s"done:${ToolCallId.value(c.id)}"))
        case Pending.Refused(_, outcome) => Right(outcome)
      }
    }
  }

  private def run(
      n: Int,
      replies: Either[TurnFailure, Message.Assistant]*
  ): (Either[TurnFailure, Looped], Vector[String]) = {
    val moves = new Scripted(replies.toVector)
    val looped = TurnLoop.run(budget(n), moves)
    (looped, moves.made)
  }

  private def result(id: String, text: String): Message.ToolResult =
    Message.ToolResult(ToolCallId(id), text, isError = false)

  val tests = Tests {
    test("a budget is at least 2 calls") {
      Budget.of(1).isLeft ==> true
      Budget.of(2).map(_.calls) ==> Right(2)
    }

    test("a round's step is call-model, then call-model:n") {
      Vector(0, 1, 7).map(n => round(n).step) ==>
        Vector("call-model", "call-model:1", "call-model:7")
    }

    test("next") {
      val b = budget(3)
      val calling = reply("", Vector("a", "b"))
      val table: Vector[(Round, Message.Assistant, Next)] = Vector(
        (round(0), reply("hi"), Next.Answer(reply("hi"))),
        (round(0), reply(""), Next.Silent(reply(""))),
        (round(0), reply("  \n"), Next.Silent(reply("  \n"))),
        (
          round(0),
          calling,
          Next.Settle(Vector(Pending.Run(call("a")), Pending.Run(call("b"))), round(1))
        ),
        (
          round(1),
          calling,
          Next.Last(Vector(Pending.Run(call("a")), Pending.Run(call("b"))), round(2))
        ),
        (round(2), reply("done", Vector("a")), Next.Answer(reply("done"))),
        (round(2), calling, Next.Silent(reply(""))),
        (round(5), reply("late", Vector("a")), Next.Answer(reply("late"))),
        (
          round(0),
          reply("cut", Vector("a"), StopReason.MaxTokens),
          Next.Settle(Vector(Pending.Refused(call("a"), TurnLoop.CutOff)), round(1))
        ),
        (
          round(0),
          reply("cut", stop = StopReason.MaxTokens),
          Next.Answer(reply("cut", stop = StopReason.MaxTokens))
        )
      )
      table.foreach { (r, sent, expected) =>
        (r.index, TurnLoop.next(b, r, sent)) ==> (r.index, expected)
      }
    }

    test("a reply that calls no tool answers at once") {
      run(3, Right(reply("hi"))) ==> (
        Right(Looped(reply("hi"), 1, Vector.empty)),
        Vector("call-model Auto seeing 0")
      )
    }

    test("each call sees the exchange so far, every call paired with its result") {
      val first = reply("", Vector("a", "b"))
      val second = reply("more", Vector("c"))
      val (looped, made) = run(4, Right(first), Right(second), Right(reply("answer")))
      made ==> Vector(
        "call-model Auto seeing 0",
        "record 0",
        "settle 0:0",
        "settle 0:1",
        "call-model:1 Auto seeing 3",
        "record 1",
        "settle 1:0",
        "call-model:2 Auto seeing 5"
      )
      looped ==> Right(
        Looped(
          reply("answer"),
          3,
          Vector(
            first,
            result("a", "done:a"),
            result("b", "done:b"),
            second,
            result("c", "done:c")
          )
        )
      )
    }

    test("the budget's last call is made with tools off, and its calls are dropped") {
      val calling = reply("", Vector("a"))
      val (looped, made) = run(2, Right(calling), Right(reply("so", Vector("b"))))
      made ==> Vector(
        "call-model Auto seeing 0",
        "record 0",
        "settle 0:0",
        "call-model:1 Off seeing 2"
      )
      looped.map(_.answer) ==> Right(reply("so"))
    }

    test("a silent reply fails the turn") {
      val (looped, made) = run(3, Right(reply("", Vector("a"))), Right(reply("")))
      looped ==> Left(
        TurnFailure.Model("the reply to call-model:1 said nothing and called no tool")
      )
      made.lastOption ==> Some("call-model:1 Auto seeing 2")
    }

    test("a reply cut off at max tokens has its calls answered unrun") {
      val cut = reply("", Vector("a"), StopReason.MaxTokens)
      val (looped, _) = run(3, Right(cut), Right(reply("ok")))
      looped.map(_.exchange) ==> Right(Vector(cut, TurnLoop.CutOff.result(ToolCallId("a"))))
    }

    test("a failed call ends the loop with no move after it") {
      val down = TurnFailure.Model("HTTP 529")
      val (looped, made) = run(3, Right(reply("", Vector("a"))), Left(down))
      looped ==> Left(down)
      made.lastOption ==> Some("call-model:1 Auto seeing 2")
    }

    test("a reply that cannot be recorded settles nothing") {
      val moves = new Scripted(
        Vector(Right(reply("", Vector("a")))),
        recorded = Left(TurnFailure.Store("disk"))
      )
      TurnLoop.run(budget(3), moves) ==> Left(TurnFailure.Store("disk"))
      moves.made ==> Vector("call-model Auto seeing 0", "record 0")
    }
  }
}
