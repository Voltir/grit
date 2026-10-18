package grit.kit.rehearse

import grit.core.classify.{Answer, ClassifierError, Question}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.models.{StubClassifier, StubProvider}

import utest.*

/** How a dry rehearsal's stand-ins answer: each call by its step, every answer marked. */
object StandInsTests extends TestSuite {

  private val request = ModelRequest("system", Vector(Message.User("lunch?")))

  private val yesNo = Question.YesNo("Is it about lunch?", None, None)

  private val choice =
    Question
      .choice("Which room?", Question.Key("here", None), Question.Key("there", None))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private val supplied =
    StandIns.supplied(Map("ask:query" -> "lunch places", "ask:triage" -> "0.2;0.3,0.7"))

  val tests = Tests {
    test("a step supplied is answered with its text as grit/supplied, every other as the stub") {
      val query = supplied.reply("ask:query", request)
      val other = supplied.reply("ask:1", request)
      assert(query.by == StandIns.StandIn.Supplied)
      assert(query.answer.blocks == Vector(AssistantBlock.Text("lunch places")))
      assert(query.answer.model == "grit/supplied")
      assert(other.by == StandIns.StandIn.Stub)
      assert(other.answer.model == StubProvider.Model)
    }

    test("a supplied reply to a call that must use a tool calls the first with the text") {
      val json =
        ModelRequest(
          "system",
          Vector(Message.User("lunch?")),
          Vector(ToolSchema("reply", "the reply", ujson.Obj())),
          ToolUse.Required
        )
      val reply =
        StandIns.supplied(Map("ask:summary" -> """{"gist":"lunch"}""")).reply("ask:summary", json)
      reply.answer.blocks ==> Vector(
        AssistantBlock.ToolCall(StubProvider.CallId, "reply", ujson.Obj("gist" -> "lunch"))
      )
    }

    test("a supplied classifier answer reads one list of probabilities per question") {
      val judged = supplied.judge("ask:triage", ujson.Obj(), Vector(yesNo, choice))
      assert(judged.by == StandIns.StandIn.Supplied)
      val answers =
        judged.answer.fold(e => throw new java.lang.AssertionError(e.toString), identity)
      assert(answers.model == "grit/supplied")
      answers.answers.collect { case Answer.YesNo(yes) => yes } ==> Vector(0.2)
      answers.answers.collect { case Answer.Choice(key, _, _) => key } ==> Vector("there")
    }

    test("a supplied classifier answer of the wrong count of questions is unreadable") {
      val judged = supplied.judge("ask:triage", ujson.Obj(), Vector(yesNo))
      judged.answer ==> Left(ClassifierError.Unreadable("supplied 2 answers for 1 questions"))
    }

    test("an unsupplied yes/no question is answered as the stub classifier answers it") {
      val judged = StandIns.Stubbed.judge("ask:triage", ujson.Obj(), Vector(yesNo))
      assert(judged.by == StandIns.StandIn.Stub)
      judged.answer ==> Right(StubClassifier.answers(ujson.Obj(), Vector(yesNo)))
    }

    test(
      "an unsupplied choice is answered with its first key, and the stub judge's line says what the stub answers"
    ) {
      val score =
        Question
          .score("How hungry?", "not", "a little", "very")
          .fold(e => sys.error(e.toString), identity)
      val judged = StandIns.Stubbed.judge("ask:triage", ujson.Obj(), Vector(yesNo, choice, score))
      val answers =
        judged.answer.fold(e => throw new java.lang.AssertionError(e.toString), _.answers)
      (
        answers.map {
          case Answer.YesNo(yes) => s"yes $yes"
          case Answer.Choice(key, _, _) => s"key $key"
          case Answer.Score(_, ps, _) => s"level ${ps.indexOf(ps.max)}"
        },
        Rehearsed.StubJudge.contains("every yes/no question yes at 0.9"),
        Rehearsed.StubJudge.contains("every choice its first option"),
        Rehearsed.StubJudge.contains("every score its last level")
      ) ==> (Vector("yes 0.9", "key here", "level 2"), true, true, true)
    }
  }
}
