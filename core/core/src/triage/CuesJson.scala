package grit.core.triage

import grit.core.id.{EntryId, PluginName}
import grit.core.period.{Judgement, Probability}

/** The stored form of [[Cues]], kept with a decision on a heard message. A row read back is
  * what an earlier build wrote, so a form once written is read by every later build.
  */
object CuesJson {

  /** `{"named", "spoken"?, "continued"?, "settled"?, "records"?}`, an absent cue left out: the
    * entry ids as strings, `settled` as `{"nobody", "on_person", "on_other", "model"}` or
    * `{"unanswered": why}`, and `records` as `{"plugin", "score", "matched"}`.
    */
  def write(cues: Cues): ujson.Value = {
    val o = ujson.Obj("named" -> cues.named)
    cues.spoken.foreach(e => o("spoken") = EntryId.value(e))
    cues.continued.foreach(e => o("continued") = EntryId.value(e))
    cues.settled.foreach(j => o("settled") = judgement(j))
    cues.records.foreach(m =>
      o("records") = ujson.Obj(
        "plugin" -> PluginName.value(m.plugin),
        "score" -> m.score,
        "matched" -> m.matched
      )
    )
    o
  }

  /** What [[write]] wrote; [[Cues.Empty]] for `null`, what a decision kept before cues holds.
    * `Left` for anything else.
    */
  def read(v: ujson.Value): Either[String, Cues] = v match {
    case ujson.Null => Right(Cues.Empty)
    case ujson.Obj(o) =>
      for {
        named <- o.get("named").flatMap(_.boolOpt).toRight("cues: named is not a boolean")
        spoken <- entry(o, "spoken")
        continued <- entry(o, "continued")
        settled <- o.get("settled").fold(Right(None))(s => verdict(s).map(Some(_)))
        records <- o.get("records").fold(Right(None))(r => matched(r).map(Some(_)))
      } yield Cues(named, spoken, continued, settled, records)
    case _ => Left("cues: expected an object")
  }

  private def judgement(j: Judgement): ujson.Value = j match {
    case Judgement.Weighed(nobody, onPerson, onOther, model) =>
      ujson.Obj(
        "nobody" -> Probability.value(nobody),
        "on_person" -> Probability.value(onPerson),
        "on_other" -> Probability.value(onOther),
        "model" -> model
      )
    case Judgement.Unanswered(why) => ujson.Obj("unanswered" -> why)
  }

  private def verdict(v: ujson.Value): Either[String, Judgement] = v match {
    case ujson.Obj(o) =>
      o.get("unanswered") match {
        case Some(ujson.Str(why)) => Right(Judgement.Unanswered(why))
        case Some(_) => Left("cues: settled's unanswered is not a string")
        case None =>
          for {
            nobody <- probability(o, "nobody")
            onPerson <- probability(o, "on_person")
            onOther <- probability(o, "on_other")
            model <- o.get("model").flatMap(_.strOpt).toRight("cues: settled has no model")
          } yield Judgement.Weighed(nobody, onPerson, onOther, model)
      }
    case _ => Left("cues: settled is not an object")
  }

  private def matched(v: ujson.Value): Either[String, Cues.Match] = v match {
    case ujson.Obj(o) =>
      for {
        plugin <- o
          .get("plugin")
          .flatMap(_.strOpt)
          .toRight("cues: records has no plugin")
          .flatMap(PluginName.of)
        score <- o.get("score").flatMap(_.numOpt).toRight("cues: records has no score")
        count <- o
          .get("matched")
          .flatMap(_.numOpt)
          .filter(n => n.isWhole && n >= 1)
          .toRight("cues: records' matched is not a count")
      } yield Cues.Match(plugin, score, count.toInt)
    case _ => Left("cues: records is not an object")
  }

  private def entry(
      o: collection.Map[String, ujson.Value],
      k: String
  ): Either[String, Option[EntryId]] =
    o.get(k)
      .fold(Right(None))(_.strOpt.map(e => Some(EntryId(e))).toRight(s"cues: $k is not an id"))

  private def probability(
      o: collection.Map[String, ujson.Value],
      k: String
  ): Either[String, Probability] =
    o.get(k).flatMap(_.numOpt).flatMap(Probability.of).toRight(s"cues: $k is not a probability")
}
