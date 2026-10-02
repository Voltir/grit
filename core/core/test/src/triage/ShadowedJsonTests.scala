package grit.core.triage

import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.message.{Tokens, Usage}

import utest.*

object ShadowedJsonTests extends TestSuite {

  private val answers: Vector[Answer] = Vector(
    Answer.Choice(
      "decision",
      Vector(Answer.Weight("question", 0.25), Answer.Weight("decision", 0.75)),
      Answer.confidence(Vector(0.25, 0.75))
    ),
    Answer.YesNo(0.125)
  )

  private val answered = Shadowed.Answered(
    "d1g35t",
    answers,
    Usage(Tokens(812), Tokens(40), Tokens.Zero, Some(BigDecimal("0.000034104"))),
    "jev-1.13.0",
    "jev-1.13.0+r2",
    1234.millis
  )

  val tests = Tests {
    test("a shadow's row is stored as recorded journals read it, and reads back") {
      // A pin of the recorded form: a shadow in flight reads back what an earlier build wrote.
      ShadowedJson.write(Shadowed.Failed("f41l3d", ClassifierError.Kind.Unreadable, 30.seconds)) ==>
        ujson.read("""{"failed":{"request":"f41l3d","kind":"unreadable","ms":30000}}""")
      ShadowedJson.writeAnswers(answers) ==> ujson.read(
        """[{"choice":"decision","weights":[{"key":"question","p":0.25},{"key":"decision","p":0.75}]},{"yes":0.125}]"""
      )
      ShadowedJson.read(ShadowedJson.write(answered)) ==> Right(answered)
      ShadowedJson.read(
        ShadowedJson.write(Shadowed.Failed("f41l3d", ClassifierError.Kind.Unavailable, 2.millis))
      ) ==> Right(Shadowed.Failed("f41l3d", ClassifierError.Kind.Unavailable, 2.millis))
    }

    test("a stored form that is neither answered nor failed does not read") {
      ShadowedJson.read(ujson.read("""{"gone":{}}""")).isLeft ==> true
      ShadowedJson.readAnswers(ujson.read("""[{"yes":0.5,"choice":"x"}]""")).isLeft ==> true
      ShadowedJson
        .read(ujson.read("""{"failed":{"request":"r","kind":"lost","ms":1}}""")) ==>
        Left("shadowed: unknown failure kind lost")
    }
  }
}
