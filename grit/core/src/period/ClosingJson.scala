package grit.core.period

import grit.core.id.PeriodSeq

/** The stored form of a [[Closing]], which outlives every raw entry of its period. Version 2
  * is the first kept: every version from it on stays readable. Written by hand, never
  * derived. A topic line's `summary` is an optional key of version 2, added after its first
  * closings were written: a line without it reads as having none.
  */
object ClosingJson {

  /** The version [[write]] writes. */
  private val Version = 2

  def write(c: Closing): ujson.Value = {
    val flows = ujson.Obj("prose" -> c.flows.prose)
    c.flows.outcome.foreach(o => flows("outcome") = o)
    flows("changes") = ujson.Arr.from(c.flows.changes.map(writeChange))
    ujson.Obj("v" -> Version, "flows" -> flows, "balance" -> writeBalance(c.balance))
  }

  /** The closing `v` encodes, or why it encodes none: not an object, a version other than 2,
    * blank or missing prose, a change or line that does not read, or two lines of one section
    * with the same text. A missing section reads as empty.
    */
  def read(v: ujson.Value): Either[String, Closing] =
    for {
      o <- v.objOpt.toRight("expected an object")
      version <- o.get("v").toRight("missing field: v")
      _ <- version.numOpt
        .filter(_ == Version)
        .toRight(s"unknown closing version: ${version.render()}")
      f <- o.get("flows").flatMap(_.objOpt).toRight("flows is missing or not an object")
      prose <- f.get("prose").flatMap(_.strOpt).toRight("prose is missing or not a string")
      outcome <- optionalString(f, "outcome")
      changes <- f.get("changes") match {
        case None => Right(Vector.empty)
        case Some(ujson.Arr(items)) => all(items.toVector)(readChange)
        case Some(_) => Left("changes is not a list")
      }
      flows <- Flows.of(prose, outcome, changes).toRight("prose is blank")
      balance <- o.get("balance").fold[Either[String, Balance]](Right(Balance.empty))(readBalance)
    } yield Closing(flows, balance)

  /** A balance's stored form: its lines by section, each in the balance's order. */
  def writeBalance(b: Balance): ujson.Value =
    ujson.Obj.from(Section.values.toVector.map { s =>
      s.key -> ujson.Arr.from(b.in(s).map(l => writeLine(l, withSection = false)))
    })

  /** The balance `v` encodes, or why none: not an object, a line that does not read, or two
    * lines of one section with the same text. A missing section reads as empty.
    */
  def readBalance(v: ujson.Value): Either[String, Balance] =
    for {
      o <- v.objOpt.toRight("balance is not an object")
      lines <- all(Section.values.toVector) { s =>
        o.get(s.key) match {
          case None => Right(Vector.empty)
          case Some(ujson.Arr(items)) => all(items.toVector)(readLine(Some(s), _))
          case Some(_) => Left(s"balance ${s.key} is not a list")
        }
      }
      balance <- Balance.of(lines.flatten)
    } yield balance

  private def writeLine(l: Line, withSection: Boolean): ujson.Obj = {
    val o = ujson.Obj()
    if (withSection) o("section") = l.section.key
    o("text") = l.text
    o("since") = PeriodSeq.value(l.since).toDouble
    o("touched") = PeriodSeq.value(l.touched).toDouble
    l.summary.foreach(summary => o("summary") = summary)
    o
  }

  /** A line, in `section` or else in the section its `section` field names. */
  private def readLine(section: Option[Section], v: ujson.Value): Either[String, Line] =
    for {
      o <- v.objOpt.toRight("a line is not an object")
      s <- section.fold(
        o.get("section").flatMap(_.strOpt).flatMap(Section.of).toRight("a line has no section")
      )(Right(_))
      text <- o.get("text").flatMap(_.strOpt).toRight("a line has no text")
      since <- period(o, "since")
      touched <- period(o, "touched")
      summary <- optionalString(o, "summary")
      line <- Line.of(s, text, since, touched, summary)
    } yield line

  private def period(
      o: collection.Map[String, ujson.Value],
      key: String
  ): Either[String, PeriodSeq] =
    o.get(key)
      .collect { case ujson.Num(n) if n.isWhole => n.toLong }
      .flatMap(PeriodSeq.of)
      .toRight(s"a line's $key is not a period")

  private def writeChange(c: Change): ujson.Value = c match {
    case Change.Added(l) => ujson.Obj("added" -> writeLine(l, withSection = true))
    case Change.Resolved(l, how) =>
      ujson.Obj("resolved" -> writeLine(l, withSection = true), "how" -> how)
    case Change.Dropped(l, why) =>
      ujson.Obj("dropped" -> writeLine(l, withSection = true), "why" -> why)
    case Change.Evicted(l) => ujson.Obj("evicted" -> writeLine(l, withSection = true))
    case Change.Refused(l) => ujson.Obj("refused" -> writeLine(l, withSection = true))
    case Change.Ignored(edit, why) => ujson.Obj("ignored" -> edit, "why" -> why)
  }

  private def readChange(v: ujson.Value): Either[String, Change] = {
    def line(o: collection.Map[String, ujson.Value], key: String) =
      o.get(key).toRight(s"$key has no line").flatMap(readLine(None, _))
    def text(o: collection.Map[String, ujson.Value], key: String) =
      o.get(key).flatMap(_.strOpt).toRight(s"a change's $key is not a string")
    v.objOpt.toRight("a change is not an object").flatMap { o =>
      Vector("added", "resolved", "dropped", "evicted", "refused", "ignored")
        .find(o.contains) match {
        case Some("added") => line(o, "added").map(Change.Added(_))
        case Some("resolved") =>
          for { l <- line(o, "resolved"); how <- text(o, "how") } yield Change.Resolved(l, how)
        case Some("dropped") =>
          for { l <- line(o, "dropped"); why <- text(o, "why") } yield Change.Dropped(l, why)
        case Some("evicted") => line(o, "evicted").map(Change.Evicted(_))
        case Some("refused") => line(o, "refused").map(Change.Refused(_))
        case Some("ignored") =>
          for { e <- text(o, "ignored"); why <- text(o, "why") } yield Change.Ignored(e, why)
        case _ => Left(s"not a change: ${v.render()}")
      }
    }
  }

  private def optionalString(
      o: collection.Map[String, ujson.Value],
      key: String
  ): Either[String, Option[String]] =
    o.get(key) match {
      case None => Right(None)
      case Some(ujson.Str(s)) => Right(Some(s))
      case Some(_) => Left(s"$key is not a string")
    }

  private def all[A, B](as: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    as.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => f(a).map(done :+ _))
    )
}
