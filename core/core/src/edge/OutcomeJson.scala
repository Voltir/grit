package grit.core.edge

import grit.core.identity.Email
import grit.core.place.Place
import grit.core.tool.Outcome
import grit.core.visibility.{Accounted, Integrity, Part, Provenance, Readers}

/** An [[Outcome]]'s stored and wire form: what an edge writes on a request's row and sends
  * to the turn waiting on it, and what the turn records. `{"kind":"done","text":…}`, with
  * `"integrity"` ([[Integrity.written]]) only when the outcome says one, so one that does not
  * keeps the form it had before, and `"items"` only when not unread:
  * `[{"place":…,"author":…}]` ([[grit.core.place.Place.written]], [[Integrity.written]]) or
  * `"unaccountable"`; `{"kind":"failed","why":…}`, `{"kind":"denied","reason":…|null}` (a declined call; the
  * key predates the case's name), `{"kind":"unanswered"}`, `{"kind":"interrupted"}`.
  */
object OutcomeJson {

  /** As a step records it: a `Done`'s parts never written, so it is recorded as its text,
    * items and integrity.
    */
  def write(outcome: Outcome): ujson.Value = outcome match {
    case Outcome.Done(text, integrity, items, _) => done(text, integrity, items)
    case Outcome.Failed(why) => ujson.Obj("kind" -> "failed", "why" -> why)
    case Outcome.Declined(reason) =>
      ujson.Obj("kind" -> "denied", "reason" -> reason.fold[ujson.Value](ujson.Null)(ujson.Str(_)))
    case Outcome.Unanswered => ujson.Obj("kind" -> "unanswered")
    case Outcome.Interrupted => ujson.Obj("kind" -> "interrupted")
  }

  /** As an edge answers it on a request's row: [[write]]'s form, with a cut `Done`'s
    * `"parts":[{"place","author","readers","text"}]`, `readers` `"place"` or
    * `{"originator":<email>}`.
    */
  def answer(outcome: Outcome): ujson.Value = outcome match {
    case Outcome.Done(text, integrity, items, Some(parts)) =>
      val answered = done(text, integrity, items)
      answered("parts") = ujson.Arr.from(parts.map { p =>
        ujson.Obj.from(
          provenance(p.item) ++
            Vector("readers" -> readers(p.readers), "text" -> ujson.Str(p.text))
        )
      })
      answered
    case other => write(other)
  }

  /** The outcome stored as `v`, in either form ([[write]], [[answer]]), or why it is none. */
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
            parts <- o
              .get("parts")
              .fold[Either[String, Option[Vector[Part]]]](Right(None))(partsOf(_).map(Some(_)))
          } yield Outcome.Done(text, integrity, items, parts)
        case "failed" => str(o, "why").map(Outcome.Failed(_))
        case "denied" => Right(Outcome.Declined(o.get("reason").flatMap(_.strOpt)))
        case "unanswered" => Right(Outcome.Unanswered)
        case "interrupted" => Right(Outcome.Interrupted)
        case other => Left(s"no outcome $other")
      }
    } yield outcome
  }

  /** A `Done` as its text, integrity and items: no parts. */
  private def done(text: String, integrity: Option[Integrity], items: Accounted): ujson.Obj = {
    val done = ujson.Obj("kind" -> "done", "text" -> text)
    integrity.foreach(i => done("integrity") = Integrity.written(i))
    items match {
      case Accounted.Unread => ()
      case Accounted.Unaccountable => done("items") = "unaccountable"
      case Accounted.Items(provenances) =>
        done("items") = ujson.Arr.from(provenances.map(p => ujson.Obj.from(provenance(p))))
    }
    done
  }

  private def provenance(p: Provenance): Vector[(String, ujson.Value)] =
    Vector(
      "place" -> ujson.Str(p.place.written),
      "author" -> ujson.Str(Integrity.written(p.vouched))
    )

  private def readers(r: Readers): ujson.Value = r match {
    case Readers.AtPlace => "place"
    case Readers.Originator(email) => ujson.Obj("originator" -> Email.value(email))
  }

  /** The items stored as `v`: `"unaccountable"`, or an array of `{"place","author"}`. */
  private def itemsOf(v: ujson.Value): Either[String, Accounted] = v match {
    case ujson.Str("unaccountable") => Right(Accounted.Unaccountable)
    case ujson.Arr(stored) => each(stored.toVector, provenanceOf).map(Accounted.of)
    case _ => Left("an outcome's items are neither unaccountable nor an array")
  }

  /** The parts stored as `v`: an array of `{"place","author","readers","text"}`. */
  private def partsOf(v: ujson.Value): Either[String, Vector[Part]] = v match {
    case ujson.Arr(stored) =>
      each(
        stored.toVector,
        stored =>
          for {
            item <- provenanceOf(stored)
            o <- stored.objOpt.toRight("an outcome's part is not an object")
            readers <- o
              .get("readers")
              .toRight("an outcome's part has no readers")
              .flatMap(readersOf)
            text <- o.get("text").flatMap(_.strOpt).toRight("an outcome's part has no text")
          } yield Part(item, readers, text)
      )
    case _ => Left("an outcome's parts are not an array")
  }

  /** Who reads a part, stored as `"place"` or `{"originator":<email>}`. */
  private def readersOf(v: ujson.Value): Either[String, Readers] = v match {
    case ujson.Str("place") => Right(Readers.AtPlace)
    case ujson.Obj(o) =>
      o.get("originator")
        .flatMap(_.strOpt)
        .toRight("an outcome's part's readers name no originator")
        .flatMap(Email.of)
        .map(Readers.Originator(_))
    case _ => Left("an outcome's part's readers are neither its place nor an originator")
  }

  private def each[A](
      stored: Vector[ujson.Value],
      one: ujson.Value => Either[String, A]
  ): Either[String, Vector[A]] =
    stored.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, item) =>
      acc.flatMap(read => one(item).map(read :+ _))
    )

  /** One item stored as `{"place","author"}`. */
  private def provenanceOf(item: ujson.Value): Either[String, Provenance] =
    for {
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
    } yield Provenance(place, author)
}
