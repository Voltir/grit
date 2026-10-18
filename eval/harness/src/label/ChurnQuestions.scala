package grit.eval.harness.label

import scala.collection.immutable.VectorMap
import scala.util.Try

import grit.core.either.Traversal
import grit.eval.harness.capture.{CaseId, Fields}

/** One question asked twice as a new thread: the message that asked it `before` a change, and
  * the one that asked it `after`.
  */
final case class ChurnPair(before: CaseId, after: CaseId)

/** A question of churn comparisons: its gold, and the message that asked it in each round, by
  * the round's name.
  */
final case class ChurnQuestion(gold: Gold, asked: VectorMap[String, CaseId])

/** The questions of churn comparisons, by a person's id for each, in the order the file gives
  * them; their words kept elsewhere. No message asks two of them, nor one twice.
  */
final case class ChurnQuestions private (questions: VectorMap[String, ChurnQuestion]) {

  /** The questions asked in both rounds, each its `before` and `after` message, in file order;
    * why not, naming the round, when no question was asked in it, or naming both, when none was
    * asked in both.
    */
  def paired(before: String, after: String): Either[String, Vector[(String, ChurnPair)]] = {
    def unasked(round: String) = !questions.values.exists(_.asked.contains(round))
    val both = questions.toVector.flatMap { (q, c) =>
      for {
        b <- c.asked.get(before)
        a <- c.asked.get(after)
      } yield q -> ChurnPair(b, a)
    }
    Vector(before, after).find(unasked) match {
      case Some(round) => Left(s"churn questions: no question was asked in round $round")
      case None if both.isEmpty =>
        Left(s"churn questions: no question was asked in both rounds $before and $after")
      case None => Right(both)
    }
  }
}

object ChurnQuestions {

  /** The questions `text` holds, as `questions.json` is written: `{"<question id>": {"gold":
    * {"facts": [["<string>", …], …], "never": ["<string>", …]}, "asked": {"<round>": "<case
    * id>", …}}, …}`. Why not, naming the question, when it is not JSON of that form, its gold
    * has no fact or a fact no string, or a case id is not one; or naming the message, when it
    * asks two questions or one twice.
    */
  def read(text: String): Either[String, ChurnQuestions] =
    for {
      json <- Try(ujson.read(text)).toOption.toRight("churn questions: not JSON")
      entries <- json.objOpt.map(_.toVector).toRight("churn questions: not an object")
      read <- Traversal.traverse(entries)((id, v) => question(id, v).map(id -> _))
      cases = read.flatMap((_, q) => q.asked.values)
      checked <- cases.diff(cases.distinct).headOption match {
        case Some(c) => Left(s"churn questions: ${c.written} asks more than once")
        case None => Right(ChurnQuestions(VectorMap.from(read)))
      }
    } yield checked

  private def question(id: String, v: ujson.Value): Either[String, ChurnQuestion] = {
    val what = s"churn questions: $id"
    def strings(vs: Vector[ujson.Value]) = Traversal.traverse(vs)(Fields.str(what, _))
    def nonEmpty[A](as: Vector[A], why: String): Either[String, ::[A]] = as.toList match {
      case first :: rest => Right(::(first, rest))
      case Nil => Left(s"$what: $why")
    }
    val f = Fields(what, v)
    for {
      gold <- f.obj("gold").map(Fields(s"$what: gold", _))
      factsJson <- gold.arr("facts")
      facts <- Traversal.traverse(factsJson) { fact =>
        fact.arrOpt
          .map(_.toVector)
          .toRight(s"$what: a fact is not a list")
          .flatMap(strings)
          .flatMap(nonEmpty(_, "a fact holds no string"))
          .map(Gold.Fact(_))
      }
      some <- nonEmpty(facts, "its gold holds no fact")
      never <- gold.arr("never").flatMap(strings)
      askedJson <- f.obj("asked")
      rounds <- askedJson.objOpt.map(_.toVector).toRight(s"$what: asked is not an object")
      asked <- Traversal.traverse(rounds) { (round, c) =>
        Fields
          .str(s"$what: asked: $round", c)
          .flatMap(CaseId.read(_).left.map(why => s"$what: asked: $round: $why"))
          .map(round -> _)
      }
    } yield ChurnQuestion(Gold(some, never), VectorMap.from(asked))
  }
}
