package grit.core.speech

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, EntryId, PrincipalId, QuestionName, TurnRef, TurnSeq}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.period.Probability
import grit.core.spend.{DailyCap, Spend}
import grit.core.triage.{Bound, Gate, Reading, Tags}

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

  private val gap = QuestionName.of("gap").getOrElse(sys.error("a name"))
  private val notChatter = Bound.Below(Reading.Chosen(Tags.V1.kind, "chatter"), p(0.5))

  private val silences: Vector[Silence] = Vector(
    Silence.Off,
    Silence.NoAddress,
    Silence.Stale(11.minutes),
    Silence.Unweighed("down"),
    Silence.Gated(Gate.Failed(notChatter, p(1)), Vector.empty),
    Silence.Gated(
      Gate.Failed(Bound.AtLeast(Reading.Key(gap, "asks"), p(0.5)), p(0.25)),
      Vector(Gate.Failed(Bound.AtLeast(Reading.Yes(Tags.V1.helps), p(0.6)), p(0.5)))
    ),
    Silence.Unasked(Reading.Yes(Tags.V1.helps)),
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
    Outcome.Spoken(EntryId("in:c:m2")),
    Outcome.Withdrawn,
    Outcome.Unjudged("down"),
    Outcome.Below(judged, p(0.5)),
    Outcome.Shadowed(Cleared.Scored(judged)),
    Outcome.Shadowed(Cleared.Named),
    Outcome.Posted(Cleared.Scored(judged)),
    Outcome.Posted(Cleared.Named),
    Outcome.Replied,
    Outcome.Failed("no model")
  )

  val tests = Tests {
    test("every decision, silence and outcome reads back as written, through text too") {
      val decisions =
        Decision.Drafting(turn) +: Decision.Answering(turn, "C/1") +: silences.map(Decision.Held(_))
      decisions.map(d =>
        SpeechJson.readDecision(ujson.read(SpeechJson.writeDecision(d).render()))
      ) ==>
        decisions.map(Right(_))
      outcomes.map(o => SpeechJson.readOutcome(ujson.read(SpeechJson.writeOutcome(o).render()))) ==>
        outcomes.map(Right(_))
    }

    test("a stale age is stored in whole seconds, its fraction dropped") {
      // decide measures an age to the nanosecond; the row and the journal keep the seconds.
      val held = Decision.Held(Silence.Stale(61_500.millis))
      SpeechJson.writeDecision(held).render() ==> """{"held":{"kind":"stale","seconds":61}}"""
      SpeechJson.readDecision(SpeechJson.writeDecision(held)) ==>
        Right(Decision.Held(Silence.Stale(61.seconds)))
    }

    test("the stored forms") {
      SpeechJson.writeDecision(Decision.Drafting(turn)).render() ==> """{"drafting":"c:3"}"""
      SpeechJson.writeDecision(Decision.Answering(turn, "C/1/2")).render() ==>
        """{"answering":"c:3","to":"C/1/2"}"""
      SpeechJson
        .writeDecision(Decision.Held(Silence.Gated(Gate.Failed(notChatter, p(1)), Vector.empty)))
        .render() ==>
        """{"held":{"kind":"gated","failed":[{"reading":{"reads":"chosen","name":"kind",""" +
        """"key":"chatter"},"bound":"below","p":0.5,"read":1}]}}"""
      SpeechJson.writeSilence(Silence.Unasked(Reading.Key(gap, "asks"))).render() ==>
        """{"kind":"unasked","reading":{"reads":"key","name":"gap","key":"asks"}}"""
      SpeechJson.writeOutcome(Outcome.Posted(Cleared.Named)).render() ==>
        """{"kind":"posted","named":true}"""
      SpeechJson.writeOutcome(Outcome.Posted(Cleared.Scored(judged))).render() ==>
        """{"kind":"posted","judged":{"grounded":0.5,"worth":0.625,"model":"jev",""" +
        """"usage":{"input":812,"output":0,"cachedInput":0,"costUsd":"0.000034"}}}"""
      outcomes.map(SpeechJson.outcomeName) ==> Vector(
        "passed",
        "nothing_recalled",
        "spoken",
        "withdrawn",
        "unjudged",
        "below",
        "shadowed",
        "shadowed",
        "posted",
        "posted",
        "replied",
        "failed"
      )
    }

    test(
      "an outcome stored as answered, before a person's reply stopped holding a draft, reads as spoken"
    ) {
      // The stored name of the hold before ADR 0023: rows kept under it must keep reading.
      SpeechJson.readOutcome(ujson.Obj("kind" -> "answered", "by" -> "in:c:m2")) ==>
        Right(Outcome.Spoken(EntryId("in:c:m2")))
    }

    test("a hold an earlier build stored as chatter or below reads as gated on v1's bound") {
      // Before holds named the gate's bounds, chatter and a helps under helpsAt were their own
      // kinds: rows and journals kept under them must keep reading.
      Vector(
        SpeechJson.readSilence(ujson.read("""{"kind":"chatter"}""")),
        SpeechJson.readSilence(ujson.read("""{"kind":"below","helps":0.42,"helps_at":0.6}"""))
      ) ==> Vector(
        Right(Silence.Gated(Gate.Failed(notChatter, p(1)), Vector.empty)),
        Right(
          Silence.Gated(
            Gate.Failed(Bound.AtLeast(Reading.Yes(Tags.V1.helps), p(0.6)), p(0.42)),
            Vector.empty
          )
        )
      )
    }

    test(
      "a named draft's outcome recorded with the named judge's score reads as named, posted or shadowed; held below, refused"
    ) {
      // Kept by grit.speech rows and record-speech outputs written while named drafts were
      // judged: reviews and a resumed turn read them.
      val retired =
        """"judged":{"answers":0.4,"model":"jev",""" +
          """"usage":{"input":812,"output":0,"cachedInput":0,"costUsd":"0.000034"}}"""
      Vector("posted", "shadowed", "below").map(k =>
        SpeechJson.readOutcome(
          ujson.read(s"""{"kind":"$k",$retired${if (k == "below") ""","post_at":0.5""" else ""}}""")
        )
      ) ==> Vector(
        Right(Outcome.Posted(Cleared.Named)),
        Right(Outcome.Shadowed(Cleared.Named)),
        Left("outcome: a named draft held below postAt by a judge grit no longer has")
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
