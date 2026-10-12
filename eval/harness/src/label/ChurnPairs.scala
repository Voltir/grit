package grit.eval.harness.label

import scala.collection.immutable.VectorMap
import scala.util.Try

import grit.eval.harness.capture.{CaseId, Fields}

/** One question asked twice as a new thread: the message that asked it `before` a change, and
  * the one that asked it `after`.
  */
final case class ChurnPair(before: CaseId, after: CaseId)

/** The questions of a churn comparison, by a person's id for each, in the order the file gives
  * them: ids alone, the questions' words kept elsewhere. No message asks two of them, nor one
  * twice.
  */
final case class ChurnPairs private (questions: VectorMap[String, ChurnPair])

object ChurnPairs {

  /** The pairs `text` holds, as `churn-pairs.json` is written: `{"<question id>": {"before":
    * "<case id>", "after": "<case id>"}, …}`. Why not, naming the question, when it is not JSON
    * of that form, a case id is not one, or a message asks two questions or one twice.
    */
  def read(text: String): Either[String, ChurnPairs] =
    for {
      json <- Try(ujson.read(text)).toOption.toRight("churn pairs: not JSON")
      entries <- json.objOpt.map(_.toVector).toRight("churn pairs: not an object")
      asked <- Fields.each(entries) { (question, v) =>
        val f = Fields(s"churn pairs: $question", v)
        def side(k: String) =
          f.str(k).flatMap(CaseId.read(_).left.map(why => s"churn pairs: $question: $k: $why"))
        for {
          before <- side("before")
          after <- side("after")
        } yield question -> ChurnPair(before, after)
      }
      pairs <- of(asked)
    } yield pairs

  /** `asked` by question id; why not, naming the message, when one asks two questions or one
    * twice, or naming the id, when an id comes twice.
    */
  def of(asked: Vector[(String, ChurnPair)]): Either[String, ChurnPairs] = {
    val ids = asked.map(_._1)
    val cases = asked.flatMap((_, a) => Vector(a.before, a.after))
    (
      ids.diff(ids.distinct).headOption,
      cases.diff(cases.distinct).headOption
    ) match {
      case (Some(id), _) => Left(s"churn pairs: question $id comes twice")
      case (_, Some(c)) => Left(s"churn pairs: ${c.written} asks more than once")
      case (None, None) => Right(ChurnPairs(VectorMap.from(asked)))
    }
  }
}
