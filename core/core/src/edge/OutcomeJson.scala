package grit.core.edge

import grit.core.place.Place
import grit.core.tool.Outcome
import grit.core.visibility.{Accounted, Integrity, Provenance}

/** An [[Outcome]]'s stored and wire form: what an edge writes on a request's row and sends
  * to the turn waiting on it, and what the turn records. `{"kind":"done","text":…}`, with
  * `"integrity"` ([[Integrity.written]]) only when the outcome says one, so one that does not
  * keeps the form it had before, and `"items"` only when not unread:
  * `[{"place":…,"author":…}]` ([[grit.core.place.Place.written]], [[Integrity.written]]) or
  * `"unaccountable"`; `{"kind":"failed","why":…}`, `{"kind":"denied","reason":…|null}` (a declined call; the
  * key predates the case's name), `{"kind":"unanswered"}`, `{"kind":"interrupted"}`.
  */
object OutcomeJson {

  def write(outcome: Outcome): ujson.Value = outcome match {
    case Outcome.Done(text, integrity, items) =>
      val done = ujson.Obj("kind" -> "done", "text" -> text)
      integrity.foreach(i => done("integrity") = Integrity.written(i))
      items match {
        case Accounted.Unread => ()
        case Accounted.Unaccountable => done("items") = "unaccountable"
        case Accounted.Items(provenances) =>
          done("items") = ujson.Arr.from(provenances.map { p =>
            ujson.Obj("place" -> p.place.written, "author" -> Integrity.written(p.vouched))
          })
      }
      done
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
        case "done" =>
          for {
            text <- str(o, "text")
            integrity <- o
              .get("integrity")
              .fold[Either[String, Option[Integrity]]](Right(None))(i =>
                i.strOpt
                  .toRight("an outcome's integrity is not a string")
                  .flatMap(Integrity.read)
                  .map(Some(_))
              )
            items <- o
              .get("items")
              .fold[Either[String, Accounted]](Right(Accounted.Unread))(itemsOf)
          } yield Outcome.Done(text, integrity, items)
        case "failed" => str(o, "why").map(Outcome.Failed(_))
        case "denied" => Right(Outcome.Declined(o.get("reason").flatMap(_.strOpt)))
        case "unanswered" => Right(Outcome.Unanswered)
        case "interrupted" => Right(Outcome.Interrupted)
        case other => Left(s"no outcome $other")
      }
    } yield outcome
  }

  /** The items stored as `v`: `"unaccountable"`, or an array of `{"place","author"}`. */
  private def itemsOf(v: ujson.Value): Either[String, Accounted] = v match {
    case ujson.Str("unaccountable") => Right(Accounted.Unaccountable)
    case ujson.Arr(stored) =>
      stored.toVector
        .foldLeft[Either[String, Vector[Provenance]]](Right(Vector.empty)) { (acc, item) =>
          for {
            read <- acc
            o <- item.objOpt.toRight("an outcome's item is not an object")
            place <- o
              .get("place")
              .flatMap(_.strOpt)
              .toRight("an outcome's item has no place")
              .flatMap(Place.read)
            author <- o
              .get("author")
              .flatMap(_.strOpt)
              .toRight("an outcome's item has no author")
              .flatMap(Integrity.read)
          } yield read :+ Provenance(place, author)
        }
        .map(Accounted.of)
    case _ => Left("an outcome's items are neither unaccountable nor an array")
  }
}
