package grit.core.period

/** The stored form of a [[Judgement]], kept in a settle's recorded steps and a triage
  * decision's cues: a form once written is read by every later build.
  */
object JudgementJson {

  /** `{"nobody", "onPerson", "onOther", "model"}` for a weighed judgement, `{"unanswered": why}`
    * for none.
    */
  def write(j: Judgement): ujson.Value = j match {
    case Judgement.Weighed(nobody, onPerson, onOther, model) =>
      ujson.Obj(
        "nobody" -> Probability.value(nobody),
        "onPerson" -> Probability.value(onPerson),
        "onOther" -> Probability.value(onOther),
        "model" -> model
      )
    case Judgement.Unanswered(why) => ujson.Obj("unanswered" -> why)
  }

  /** What [[write]] wrote; `Left` for anything else, a probability outside 0 to 1 included. */
  def read(v: ujson.Value): Either[String, Judgement] = v match {
    case ujson.Obj(o) =>
      o.get("unanswered") match {
        case Some(ujson.Str(why)) => Right(Judgement.Unanswered(why))
        case Some(_) => Left("judgement: unanswered is not a string")
        case None =>
          for {
            nobody <- probability(o, "nobody")
            onPerson <- probability(o, "onPerson")
            onOther <- probability(o, "onOther")
            model <- o.get("model").flatMap(_.strOpt).toRight("judgement: no model")
          } yield Judgement.Weighed(nobody, onPerson, onOther, model)
      }
    case _ => Left("judgement: expected an object")
  }

  private def probability(
      o: collection.Map[String, ujson.Value],
      k: String
  ): Either[String, Probability] =
    o.get(k)
      .flatMap(_.numOpt)
      .flatMap(Probability.of)
      .toRight(s"judgement: $k is not a probability")
}
