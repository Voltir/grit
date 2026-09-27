package grit.core.edge

import grit.core.tool.Outcome

/** An [[Outcome]]'s stored and wire form: what an edge writes on a request's row and sends
  * to the turn waiting on it, and what the turn records. `{"kind":"done","text":…}`,
  * `{"kind":"failed","why":…}`, `{"kind":"denied","reason":…|null}` (a declined call; the
  * key predates the case's name), `{"kind":"unanswered"}`, `{"kind":"interrupted"}`. A
  * `denied` with the reason an earlier build gave a timed-out answer reads as unanswered.
  */
object OutcomeJson {

  /** The reason an earlier build stored for a call nobody answered in time. */
  private val EarlierUnanswered = "No answer came in time."

  def write(outcome: Outcome): ujson.Value = outcome match {
    case Outcome.Done(text) => ujson.Obj("kind" -> "done", "text" -> text)
    case Outcome.Failed(why) => ujson.Obj("kind" -> "failed", "why" -> why)
    case Outcome.Declined(reason) =>
      ujson.Obj("kind" -> "denied", "reason" -> reason.fold[ujson.Value](ujson.Null)(ujson.Str(_)))
    case Outcome.Unanswered => ujson.Obj("kind" -> "unanswered")
    case Outcome.Interrupted => ujson.Obj("kind" -> "interrupted")
  }

  /** The outcome stored as `v` ([[write]]'s form), or why it is none. */
  def read(v: ujson.Value): Either[String, Outcome] = {
    def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
      o.get(key).flatMap(_.strOpt).toRight(s"an outcome has no $key")
    for {
      o <- v.objOpt.toRight("an outcome is not an object")
      kind <- str(o, "kind")
      outcome <- kind match {
        case "done" => str(o, "text").map(Outcome.Done(_))
        case "failed" => str(o, "why").map(Outcome.Failed(_))
        case "denied" =>
          o.get("reason").flatMap(_.strOpt) match {
            // An earlier build stored a timed-out answer as declined, with its own words.
            case Some(EarlierUnanswered) => Right(Outcome.Unanswered)
            case reason => Right(Outcome.Declined(reason))
          }
        case "unanswered" => Right(Outcome.Unanswered)
        case "interrupted" => Right(Outcome.Interrupted)
        case other => Left(s"no outcome $other")
      }
    } yield outcome
  }
}
