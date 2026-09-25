package grit.turn

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.topic.Verdict

import utest.*
import TurnVerdict.{Replied, Round, Shape}

/** [[TurnVerdict.round]] over a scripted [[TurnVerdict.Calls]]: which further calls a round
  * makes, in which order, and what it comes to. No `Durable`: the turn's steps are
  * [[TurnVerdictTests]]'.
  */
object TurnVerdictRoundTests extends TestSuite {

  private val c = TurnTopics.Classification(Vector.empty, None, Vector.empty, None, None)

  private def said(text: String, call: Option[String] = None): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++
        call.map(about =>
          AssistantBlock.ToolCall(
            ToolCallId("t1"),
            grit.core.tool.ToolName.value(TurnVerdict.Name),
            ujson.Obj("about" -> about)
          )
        ),
      StopReason.EndTurn,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, None),
      "m"
    )

  /** Answers `again` and `plain` as scripted, noting each call it is asked to make. */
  final class Scripted(
      second: Either[TurnFailure, Message.Assistant],
      plainly: Either[TurnFailure, Message.Assistant]
  ) extends TurnVerdict.Calls {
    @caps.unsafe.untrackedCaptures
    var made = Vector.empty[String]

    def again(shape: Shape.Again): Either[TurnFailure, Message.Assistant] = {
      made = made :+ "again"
      second
    }

    def plain(): Either[TurnFailure, Message.Assistant] = {
      made = made :+ "plain"
      plainly
    }
  }

  private val calling = said("thinking", Some("current"))
  private val down = Left(TurnFailure.Model("HTTP 529"))

  /** The round after `first`, with the calls it made. */
  private def run(
      first: Message.Assistant,
      again: Either[TurnFailure, Message.Assistant] = down,
      plain: Either[TurnFailure, Message.Assistant] = down
  ): (Round, Vector[String]) = {
    val calls = new Scripted(again, plain)
    val round = TurnVerdict.round(c, first, calls)
    (round, calls.made)
  }

  val tests = Tests {
    test("no call to the tool: the first reply answers, nothing more is called or spent") {
      val first = said("hello")
      run(first) ==> (
        Round(
          Right(Replied(first, Shape.Offered(c))),
          Verdict.Unreadable("answered without calling topic"),
          None,
          Vector.empty
        ),
        Vector.empty
      )
    }

    test("the second call answers: its reply, the first spent") {
      val second = said("the answer")
      run(calling, again = Right(second)) ==> (
        Round(
          Right(Replied(second, Shape.Again(c, calling))),
          Verdict.Current,
          None,
          Vector(Replied(calling, Shape.Offered(c)))
        ),
        Vector("again")
      )
    }

    test("the second call answers and calls again: its calls dropped, and noted") {
      val (round, made) = run(calling, again = Right(said("the answer", Some("new"))))
      made ==> Vector("again")
      round.answer ==> Right(Replied(said("the answer"), Shape.Again(c, calling)))
      round.anomaly ==> Some("the second call called a tool again; its calls were dropped")
    }

    test("the second call says nothing: a plain call answers, both earlier replies spent") {
      val silent = said("", Some("current"))
      val plain = said("plainly")
      run(calling, again = Right(silent), plain = Right(plain)) ==> (
        Round(
          Right(Replied(plain, Shape.Plain)),
          Verdict.Current,
          Some("the second call called a tool again and said nothing; a plain call answered"),
          Vector(Replied(calling, Shape.Offered(c)), Replied(silent, Shape.Again(c, calling)))
        ),
        Vector("again", "plain")
      )
    }

    test("the second call fails: a plain call answers, only the first spent") {
      val (round, made) = run(calling, plain = Right(said("plainly")))
      made ==> Vector("again", "plain")
      round.spent ==> Vector(Replied(calling, Shape.Offered(c)))
      round.anomaly ==> Some(
        "the second call failed (Model(HTTP 529)); a plain call answered"
      )
    }

    test("the plain call fails: the round has no answer, and the verdict stands") {
      val (round, made) = run(calling)
      made ==> Vector("again", "plain")
      round.answer ==> down
      round.verdict ==> Verdict.Current
    }
  }
}
