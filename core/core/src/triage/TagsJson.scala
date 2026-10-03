package grit.core.triage

import grit.core.classify.AnswersJson
import grit.core.period.Probability
import grit.core.store.PayloadJson

/** The recorded form of [[Tags]] in a workflow's journal: triage's, and a turn's that read
  * them. A workflow in flight reads back what an earlier build wrote (ADR 0004), so a form
  * once written is read by every later build.
  */
object TagsJson {

  /** The probabilities a build before named answers recorded, under these keys. */
  private val Weights: Vector[String] = Vector("kindP", "waiting", "durable", "helps")

  /** `tags` as recorded: `{"answers": [...], "model", "usage"}`, the answers as
    * [[AnswersJson.writeNamed]] writes them; or `{"unanswered": why}`.
    */
  def write(tags: Tags): ujson.Value = tags match {
    case Tags.Weighed(answers, model, usage) =>
      ujson.Obj(
        "answers" -> AnswersJson.writeNamed(answers),
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage)
      )
    case Tags.Unanswered(why) => ujson.Obj("unanswered" -> why)
  }

  /** What [[write]] wrote; or what a build before named answers wrote, `{"kind", "kindP",
    * "waiting", "durable", "helps", "model", "usage"}`, read as v1's answers
    * ([[Tags.V1.answers]]).
    */
  def read(v: ujson.Value): Either[String, Tags] =
    v.objOpt.toRight("tags: expected an object").flatMap { o =>
      (o.get("unanswered"), o.get("answers")) match {
        case (Some(ujson.Str(why)), _) => Right(Tags.Unanswered(why))
        case (Some(_), _) => Left("tags: unanswered is not a string")
        case (None, answered) =>
          for {
            answers <- answered.fold(v1(o))(AnswersJson.readNamed)
            model <- o.get("model").flatMap(_.strOpt).toRight("tags: missing model")
            usage <- o.get("usage").toRight("tags: missing usage").flatMap(PayloadJson.readUsage)
          } yield Tags.Weighed(answers, model, usage)
      }
    }

  /** The answers a build before named answers recorded in `o`, as v1's. */
  private def v1(o: collection.Map[String, ujson.Value]) = {
    val ps = Weights.flatMap(k => o.get(k).flatMap(_.numOpt).flatMap(Probability.of))
    for {
      kind <- o.get("kind").flatMap(_.strOpt).flatMap(Kind.read).toRight("tags: bad kind")
      answers <- ps match {
        case Vector(kindP, waiting, durable, helps) =>
          Right(Tags.V1.answers(kind, kindP, waiting, durable, helps))
        case _ => Left("tags: expected four probabilities")
      }
    } yield answers
  }
}
