package grit.core.speech

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, EntryId, PrincipalId, TurnRef, TurnSeq}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.period.Probability
import grit.core.spend.{DailyCap, Spend}

import utest.*

// These pin the stored form: a grit.speech row and a workflow's journal are written in it.
object SpeechJsonTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)
  private val turn = TurnRef(ConversationId("c"), TurnSeq(3))
  private val cap = DailyCap.of("0.25").getOrElse(sys.error("a cap"))
  private val judged =
    Judged(
      p(0.5),
      p(0.625),
      "jev",
      Usage(Tokens(812), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.000034")))
    )

  private val silences: Vector[Silence] = Vector(
    Silence.Off,
    Silence.NoAddress,
    Silence.Stale(11.minutes),
    Silence.Unweighed("down"),
    Silence.Chatter,
    Silence.Below(p(0.5), p(0.6)),
    Silence.AskedOf(PrincipalId("slack:T/U1")),
    Silence.Unanswered(turn),
    Silence.Thread(1),
    Silence.Room(2),
    Silence.Deployment(10),
    Silence.OverSpeechCap(Spend(3, Cost.AtLeast(BigDecimal("0.25"))), cap),
    Silence.OverBudget
  )

  private val outcomes: Vector[Outcome] = Vector(
    Outcome.Passed,
    Outcome.NothingRecalled,
    Outcome.Answered(EntryId("in:c:m2")),
    Outcome.Withdrawn,
    Outcome.Unjudged("down"),
    Outcome.Below(judged, p(0.5)),
    Outcome.Shadowed(judged),
    Outcome.Posted(judged),
    Outcome.Failed("no model")
  )

  val tests = Tests {
    test("every decision, silence and outcome reads back as written, through text too") {
      val decisions = Decision.Drafting(turn) +: silences.map(Decision.Held(_))
      decisions.map(d =>
        SpeechJson.readDecision(ujson.read(SpeechJson.writeDecision(d).render()))
      ) ==>
        decisions.map(Right(_))
      outcomes.map(o => SpeechJson.readOutcome(ujson.read(SpeechJson.writeOutcome(o).render()))) ==>
        outcomes.map(Right(_))
    }

    test("the stored forms") {
      SpeechJson.writeDecision(Decision.Drafting(turn)).render() ==> """{"drafting":"c:3"}"""
      SpeechJson.writeDecision(Decision.Held(Silence.Below(p(0.5), p(0.6)))).render() ==>
        """{"held":{"kind":"below","helps":0.5,"helps_at":0.6}}"""
      SpeechJson.writeOutcome(Outcome.Posted(judged)).render() ==>
        """{"kind":"posted","judged":{"grounded":0.5,"worth":0.625,"model":"jev",""" +
        """"usage":{"input":812,"output":0,"cachedInput":0,"costUsd":"0.000034"}}}"""
      outcomes.map(SpeechJson.outcomeName) ==> Vector(
        "passed",
        "nothing_recalled",
        "answered",
        "withdrawn",
        "unjudged",
        "below",
        "shadowed",
        "posted",
        "failed"
      )
    }

    test("a judgement recorded with the dropped adds question reads, without it") {
      SpeechJson.readJudged(
        ujson.read(
          """{"adds":0.9,"grounded":0.5,"worth":0.625,"model":"jev",""" +
            """"usage":{"input":812,"output":0,"cachedInput":0,"costUsd":"0.000034"}}"""
        )
      ) ==> Right(judged)
    }

    test("a form no build wrote is a Left, not a throw") {
      SpeechJson.readDecision(ujson.read("""{"drafting":"no turn"}""")) ==>
        Left("decision: no turn is not a turn")
      SpeechJson.readOutcome(ujson.read("""{"kind":"lunch"}""")) ==> Left(
        "outcome: unknown kind lunch"
      )
      SpeechJson.readSilence(ujson.read("""{"kind":"below","helps":2,"helps_at":0.6}""")) ==>
        Left("helps: expected a probability")
    }
  }
}
