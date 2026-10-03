package grit.core.triage

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.id.{KnowledgeSourceName, QuestionName}
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

  private def answered(answers: ShadowAnswers): Shadowed = Shadowed.Answered(
    "d1g35t",
    answers,
    Usage(Tokens(812), Tokens(40), Tokens.Zero, Some(BigDecimal("0.000034104"))),
    "jev-1.13.0",
    "jev-1.13.0+r2",
    1234.millis
  )

  private def question(text: String): QuestionName =
    QuestionName.of(text).getOrElse(throw new java.lang.AssertionError(text))

  private val github: QuestionName = QuestionName.per(
    question("source"),
    KnowledgeSourceName.of("github").getOrElse(throw new java.lang.AssertionError("github"))
  )

  /** A question set's answers, in an order that is not their names' sorted order. */
  private val named: ShadowAnswers.Named = ShadowAnswers.Named(
    VectorMap(
      question("to") -> Answer.YesNo(0.125),
      question("gap") -> answers(0),
      github -> Answer.YesNo(0.5)
    )
  )

  /** `answers` in order: a `VectorMap`'s equality ignores it. */
  private def ordered(answers: Either[String, ShadowAnswers]) = answers.map {
    case ShadowAnswers.Named(as) => as.toVector.map((n, a) => (QuestionName.value(n), a))
    case ShadowAnswers.Worded(as) => as.map(("unnamed", _))
  }

  val tests = Tests {
    test("a shadow's row is stored as recorded journals read it, and reads back") {
      // A pin of the recorded form, to the byte: a shadow in flight reads back what an
      // earlier build wrote.
      ShadowedJson.write(Shadowed.Failed("f41l3d", ClassifierError.Kind.Unreadable, 30.seconds)) ==>
        ujson.read("""{"failed":{"request":"f41l3d","kind":"unreadable","ms":30000}}""")
      ShadowedJson.writeAnswers(ShadowAnswers.Worded(answers)).render() ==>
        """[{"choice":"decision","weights":[{"key":"question","p":0.25},{"key":"decision","p":0.75}]},{"yes":0.125}]"""
      val row = answered(ShadowAnswers.Worded(answers))
      ShadowedJson.read(ShadowedJson.write(row)) ==> Right(row)
      ShadowedJson.read(
        ShadowedJson.write(Shadowed.Failed("f41l3d", ClassifierError.Kind.Unavailable, 2.millis))
      ) ==> Right(Shadowed.Failed("f41l3d", ClassifierError.Kind.Unavailable, 2.millis))
    }

    test(
      "a question set's answers are stored as a wording's, each with its name, and read back in the order asked"
    ) {
      // A pin of the stored form: the harness reads these rows by name.
      ShadowedJson.writeAnswers(named).render() ==>
        """[{"name":"to","yes":0.125},{"name":"gap","choice":"decision","weights":[{"key":"question","p":0.25},{"key":"decision","p":0.75}]},{"name":"source:github","yes":0.5}]"""
      ordered(ShadowedJson.readAnswers(ShadowedJson.writeAnswers(named))) ==> ordered(Right(named))
      val row = answered(named)
      ShadowedJson.read(ShadowedJson.write(row)) ==> Right(row)
    }

    test("stored answers that mix named and unnamed, or repeat a name, do not read") {
      ShadowedJson.readAnswers(ujson.read("""[{"name":"to","yes":0.5},{"yes":0.5}]""")) ==>
        Left("answers: some named, some not")
      ShadowedJson.readAnswers(ujson.read("""[{"yes":0.5},{"name":"to","yes":0.5}]""")) ==>
        Left("answers: some named, some not")
      ShadowedJson.readAnswers(
        ujson.read("""[{"name":"to","yes":0.5},{"name":"to","yes":0.25}]""")
      ) ==> Left("answers: to is named twice")
      ShadowedJson.readAnswers(ujson.read("""[{"name":"To","yes":0.5}]""")) ==>
        Left("a question's name is a lowercase letter, then lowercase letters, digits or -: To")
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
