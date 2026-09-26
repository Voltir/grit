package grit.core.period

/** The stored form of a [[Closing]], which outlives every raw entry of its period: each
  * version ever written stays readable. Written by hand, never derived.
  */
object ClosingJson {

  /** The version [[write]] writes. */
  private val Version = 1

  private val Sections: Vector[String] = Vector("decisions", "facts", "open", "sources")

  def write(c: Closing): ujson.Value = {
    val o = ujson.Obj("v" -> Version, "prose" -> c.prose)
    c.outcome.foreach(x => o("outcome") = x)
    Sections.zip(Vector(c.decisions, c.facts, c.open, c.sources)).foreach {
      (key: String, lines: Vector[String]) =>
        o(key) = ujson.Arr.from(lines.map(ujson.Str(_)))
    }
    o
  }

  /** The closing `v` encodes, or why it encodes none: not an object, a version this code
    * does not know, a blank or missing `prose`, or a section that is not a list of strings.
    * A missing section reads as empty.
    */
  def read(v: ujson.Value): Either[String, Closing] =
    for {
      o <- v.objOpt.toRight("expected an object")
      version <- o.get("v").toRight("missing field: v")
      _ <- version.numOpt
        .filter(_ == Version)
        .toRight(s"unknown closing version: ${version.render()}")
      prose <- o.get("prose").flatMap(_.strOpt).toRight("prose is missing or not a string")
      outcome <- o.get("outcome") match {
        case None => Right(None)
        case Some(ujson.Str(s)) => Right(Some(s))
        case Some(_) => Left("outcome is not a string")
      }
      sections <- Sections.foldLeft[Either[String, Vector[Vector[String]]]](Right(Vector.empty)) {
        (acc, key) =>
          acc.flatMap { done =>
            o.get(key) match {
              case None => Right(done :+ Vector.empty)
              case Some(ujson.Arr(items)) =>
                items.toVector
                  .foldLeft[Option[Vector[String]]](Some(Vector.empty))((ls, i) =>
                    ls.flatMap(l => i.strOpt.map(l :+ _))
                  )
                  .map(done :+ _)
                  .toRight(s"$key is not a list of strings")
              case Some(_) => Left(s"$key is not a list of strings")
            }
          }
      }
      closing <- sections match {
        case Vector(decisions, facts, open, sources) =>
          Closing.of(prose, outcome, decisions, facts, open, sources).toRight("prose is blank")
        case _ => Left("unreachable: four sections")
      }
    } yield closing
}
