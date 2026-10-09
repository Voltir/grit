package grit.turn

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.provider.ToolUse

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

  /** Replies to each call as scripted, in order, noting every move: each call with the
    * tools' use, each settled call with whether it runs or is refused. A fake of the turn's
    * durable steps, so it may hold its log.
    */
  final class Scripted(
      replies: Vector[Either[TurnFailure, Message.Assistant]],
      recorded: Either[TurnFailure, Unit] = Right(())
  ) extends TurnLoop.Moves {
    @caps.unsafe.untrackedCaptures
    var made = Vector.empty[String]

    def call(round: Round, use: ToolUse): Either[TurnFailure, Message.Assistant] = {
      made = made :+ s"${round.step} $use"
      replies.lift(round.index).getOrElse(Left(TurnFailure.Model("no reply scripted")))
    }

    def record(
        round: Round,
        reply: Message.Assistant,
        calls: Vector[Pending]
    ): Either[TurnFailure, Unit] = {
      made = made :+ s"record ${round.index}"
      recorded
    }

    def settle(round: Round, index: Int, pending: Pending): Either[TurnFailure, Unit] = {
      val how = pending match {
        case Pending.Run(c) => s"run ${ToolCallId.value(c.id)}"
        case Pending.Refused(c, _) => s"refuse ${ToolCallId.value(c.id)}"
      }
      made = made :+ s"settle ${round.index}:$index $how"
      Right(())
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
          Next.Settle(Vector(Pending.Run(call("a")), Pending.Run(call("b"))), round(2))
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

    test("a call's tools are on before the budget's last call, and off from it") {
      Vector(0, 1, 2, 3).map(n => TurnLoop.use(budget(3), round(n))) ==>
        Vector(ToolUse.Auto, ToolUse.Auto, ToolUse.Off, ToolUse.Off)
    }

    test("a reply that calls no tool answers at once") {
      run(3, Right(reply("hi"))) ==> (
        Right(Looped(reply("hi"), round(0), Vector(reply("hi")))),
        Vector("call-model Auto")
      )
    }

    test("each reply that calls tools is recorded, then each call settled in order") {
      val first = reply("", Vector("a", "b"))
      val second = reply("more", Vector("c"))
      val (looped, made) = run(4, Right(first), Right(second), Right(reply("answer")))
      made ==> Vector(
        "call-model Auto",
        "record 0",
        "settle 0:0 run a",
        "settle 0:1 run b",
        "call-model:1 Auto",
        "record 1",
        "settle 1:0 run c",
        "call-model:2 Auto"
      )
      looped ==> Right(Looped(reply("answer"), round(2), Vector(first, second, reply("answer"))))
    }

    test("the budget's last call is made with tools off, and its calls are dropped") {
      val calling = reply("", Vector("a"))
      val (looped, made) = run(2, Right(calling), Right(reply("so", Vector("b"))))
      made ==> Vector("call-model Auto", "record 0", "settle 0:0 run a", "call-model:1 Off")
      looped ==> Right(Looped(reply("so"), round(1), Vector(calling, reply("so", Vector("b")))))
    }

    test("a silent reply fails the turn") {
      val (looped, made) = run(3, Right(reply("", Vector("a"))), Right(reply("")))
      looped ==> Left(
        TurnFailure.Model("the reply to call-model:1 said nothing and called no tool")
      )
      made.lastOption ==> Some("call-model:1 Auto")
    }

    test("a reply cut off at max tokens has its calls refused, not run") {
      val cut = reply("", Vector("a"), StopReason.MaxTokens)
      val (looped, made) = run(3, Right(cut), Right(reply("ok")))
      made ==> Vector("call-model Auto", "record 0", "settle 0:0 refuse a", "call-model:1 Auto")
      looped.map(_.answer) ==> Right(reply("ok"))
    }

    test("a failed call ends the loop with no move after it") {
      val down = TurnFailure.Model("HTTP 529")
      val (looped, made) = run(3, Right(reply("", Vector("a"))), Left(down))
      looped ==> Left(down)
      made.lastOption ==> Some("call-model:1 Auto")
    }

    test("a reply that cannot be recorded settles nothing") {
      val moves = new Scripted(
        Vector(Right(reply("", Vector("a")))),
        recorded = Left(TurnFailure.Store("disk"))
      )
      TurnLoop.run(budget(3), moves) ==> Left(TurnFailure.Store("disk"))
      moves.made ==> Vector("call-model Auto", "record 0")
    }

    test("from goes on after a first reply made elsewhere") {
      val moves = new Scripted(Vector(Left(TurnFailure.Model("not called")), Right(reply("done"))))
      TurnLoop.from(budget(3), reply("", Vector("a")), moves) ==>
        Right(Looped(reply("done"), round(1), Vector(reply("", Vector("a")), reply("done"))))
      moves.made ==> Vector("record 0", "settle 0:0 run a", "call-model:1 Auto")
    }

    test("the last call's note allows not having found it, and asks for no guess") {
      // Pinned whole: "answer now" alone pushed a model to manufacture an answer.
      TurnLoop.LastCall ==>
        "[grit: this is your last call in this turn, and it has no tools: call none. Answer " +
        "now from what you have found so far: say what you found, and plainly what you did " +
        "not find or could not check. \"I could not find it\" is an answer; do not fill the " +
        "gap with a guess.]"
    }

    test(
      "the last call's note follows the messages, or ends the last result where a pair refuses a user there"
    ) {
      val result: Message.ToolResult =
        Message.ToolResult(ToolCallId("c1"), "3 lines", isError = false)
      val asked = grit.core.provider.ModelRequest("s", Vector(Message.User("hi"), result))
      def told(
          use: ToolUse,
          after: AfterToolResult,
          request: grit.core.provider.ModelRequest = asked
      ) =
        TurnLoop.told(use, request, after).messages
      told(ToolUse.Off, AfterToolResult.UserMessage) ==>
        Vector(Message.User("hi"), result, Message.User(TurnLoop.LastCall))
      told(ToolUse.Off, AfterToolResult.InLastResult) ==>
        Vector(Message.User("hi"), result.copy(content = s"3 lines\n\n${TurnLoop.LastCall}"))
      // With no tool result last, the note is a user message whatever the pair.
      val plain = grit.core.provider.ModelRequest("s", Vector(Message.User("hi")))
      told(ToolUse.Off, AfterToolResult.InLastResult, plain) ==>
        Vector(Message.User("hi"), Message.User(TurnLoop.LastCall))
      told(ToolUse.Auto, AfterToolResult.InLastResult) ==> asked.messages
      told(ToolUse.Required, AfterToolResult.InLastResult) ==> asked.messages
    }

    test("tool guidance names each offered tool in the system prompt, for a pair that needs it") {
      val tools = Vector(
        grit.core.provider.ToolSchema("read", "Reads a file.\nMore about reading.", ujson.Obj()),
        grit.core.provider.ToolSchema("run", "Runs a command.", ujson.Obj())
      )
      val asked =
        grit.core.provider.ModelRequest("You are grit.", Vector(Message.User("hi")), tools)
      TurnLoop.guided(asked, ToolGuidance.SystemLines).system ==>
        "You are grit.\n\nTools you may call:\n- read: Reads a file.\n- run: Runs a command."
      TurnLoop.guided(asked, ToolGuidance.SchemaOnly) ==> asked
      TurnLoop
        .guided(asked.copy(tools = Vector.empty), ToolGuidance.SystemLines)
        .system ==> "You are grit."
    }
  }
}
