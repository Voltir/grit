package grit.core.classify

import scala.collection.immutable.VectorMap

import grit.core.id.QuestionName

import utest.*

object AnswersJsonTests extends TestSuite {

  private def name(text: String): QuestionName =
    QuestionName.of(text).getOrElse(throw new java.lang.AssertionError(text))

  private val named: VectorMap[QuestionName, Answer] = VectorMap(
    name("gap") -> Answer.Choice(
      "asks",
      Vector(Answer.Weight("asks", 0.75), Answer.Weight("nothing", 0.25)),
      Answer.confidence(Vector(0.75, 0.25))
    ),
    name("durable") -> Answer.YesNo(0.125),
    name("frustration") -> Answer.Score(
      1.25,
      Vector(0.0, 0.75, 0.25),
      Answer.scoreConfidence(Vector(0.0, 0.75, 0.25))
    )
  )

  val tests = Tests {
    test("named answers are stored with their names first, and read back in the order asked") {
      // A pin of the stored form: rows and journals read it back by name.
      AnswersJson.writeNamed(named).render() ==>
        """[{"name":"gap","choice":"asks","weights":[{"key":"asks","p":0.75},{"key":"nothing","p":0.25}]},{"name":"durable","yes":0.125},{"name":"frustration","score":1.25,"levels":[0,0.75,0.25]}]"""
      AnswersJson.readNamed(AnswersJson.writeNamed(named)).map(_.toVector) ==>
        Right(named.toVector)
    }

    test("a score is stored as its position and its levels' weights, its confidence recomputed") {
      AnswersJson.read(ujson.read("""{"score":0.5,"levels":[0.5,0.5,0]}""")) ==>
        Right(Answer.Score(0.5, Vector(0.5, 0.5, 0.0), 0.5))
      AnswersJson.read(ujson.read("""{"score":0.5,"levels":[0.5,"half"]}""")) ==>
        Left("answer: a level's weight is not a number")
      AnswersJson.read(ujson.read("""{"score":0.5,"yes":0.5}""")) ==>
        Left("answer: expected {yes}, {choice, weights} or {score, levels}")
    }

    test("named answers do not read when one has no name, or a name repeats") {
      AnswersJson.readNamed(ujson.read("""[{"name":"to","yes":0.5},{"yes":0.5}]""")) ==>
        Left("answers: an answer has no name")
      AnswersJson.readNamed(
        ujson.read("""[{"name":"to","yes":0.5},{"name":"to","yes":0.25}]""")
      ) ==>
        Left("answers: to is named twice")
      AnswersJson.readNamed(ujson.read("""{"name":"to","yes":0.5}""")) ==>
        Left("answers: expected an array")
    }
  }
}
