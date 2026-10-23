package grit.knowledge.card

import grit.core.schema.{JsonSchema, Typed}

/** A unit's summary: at most [[CardSummary.Words]] words, never blank, and at most
  * [[CardSummary.Terms]] search terms (each one to three words) for what it holds that the
  * summary leaves out.
  */
final case class CardSummary private (summary: String, terms: Vector[String]) extends caps.Pure

object CardSummary {

  /** 60: the most words a summary keeps. */
  val Words: Int = 60

  /** 8: the most search terms a summary holds. */
  val Terms: Int = 8

  /** These, trimmed, or why not: `summary` blank, or more than [[Terms]] terms that are not
    * blank. A summary over [[Words]] words is cut after its 60th; a term over three words, to
    * its first three; a blank term is left out.
    */
  def of(summary: String, terms: Vector[String]): Either[String, CardSummary] = {
    val kept = terms.map(_.trim.split("\\s+").take(3).mkString(" ")).filter(_.nonEmpty)
    if (summary.isBlank) Left("a summary is not blank")
    else if (kept.size > Terms) Left(s"a summary has at most $Terms terms, not ${kept.size}")
    else Right(new CardSummary(cut(summary.trim), kept))
  }

  /** `text` up to the end of its [[Words]]th word, its spacing kept. */
  private def cut(text: String): String =
    "\\S+".r.findAllMatchIn(text).drop(Words - 1).nextOption().fold(text)(m => text.take(m.end))

  /** The reply a file's summary ask takes ([[replyOf]] with the file's descriptions). */
  val Reply: Typed[CardSummary] = replyOf(
    s"What the file is and what it holds, in at most $Words words.",
    "The search terms it defines, each one to three words, for what the file holds " +
      "that the summary leaves out, or null if none."
  )

  /** The reply a summary ask takes: its schema (`summary` a string, described to the model as
    * `summary` says, `terms` an array of at most [[Terms]] strings or `null`, as `terms` says),
    * read as [[of]] reads it, refusing a blank summary and cutting one over [[Words]] words. A
    * `terms` that is `null`, or left out, reads as no terms.
    */
  def replyOf(summary: String, terms: String): Typed[CardSummary] = {
    val schema = ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "summary" -> ujson.Obj("type" -> "string", "description" -> summary),
        "terms" -> ujson.Obj(
          "type" -> ujson.Arr("array", "null"),
          "items" -> ujson.Obj("type" -> "string"),
          "maxItems" -> Terms,
          "description" -> terms
        )
      ),
      "required" -> ujson.Arr("summary", "terms"),
      "additionalProperties" -> false
    )
    // A literal in JsonSchema's subset but for the descriptions, which are text: every test of
    // a reply throws here if not.
    JsonSchema
      .read(schema)
      .fold(
        why => throw new IllegalStateException(why.toString),
        s => Typed(s, c => read(c.json))
      )
  }

  private def read(json: ujson.Value): Either[String, CardSummary] =
    (for {
      o <- json.objOpt
      summary <- o.get("summary").flatMap(_.strOpt)
      terms <- o.get("terms") match {
        case None | Some(ujson.Null) => Some(Vector.empty[ujson.Value])
        case Some(v) => v.arrOpt.map(_.toVector)
      }
    } yield (summary, terms.flatMap(_.strOpt)))
      .toRight("a summary reply holds a string summary and an array of terms")
      .flatMap(of)
}
