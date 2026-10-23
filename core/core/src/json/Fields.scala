package grit.core.json

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Try

/** The fields of `v`, an object read as `what`: each read is `Left` naming `what` and the field
  * when it is missing or not of its form, or when `v` is no object.
  */
final case class Fields(what: String, v: ujson.Value) {
  def field(k: String): Either[String, ujson.Value] =
    v.objOpt.flatMap(_.get(k)).toRight(s"$what names no $k")

  /** `None` when the field is JSON `null`. */
  def optional(k: String): Either[String, Option[ujson.Value]] =
    field(k).map {
      case ujson.Null => None
      case x => Some(x)
    }

  /** `None` when the field is JSON `null` or missing: a field a later version of the file
    * added, which an earlier one never wrote.
    */
  def added(k: String): Either[String, Option[ujson.Value]] =
    Right(v.objOpt.flatMap(_.get(k)).filter(_ != ujson.Null))

  /** As [[added]], a string when there. */
  def addedStr(k: String): Either[String, Option[String]] =
    added(k).flatMap(Fields.opt(_)(Fields.str(s"$what's $k", _)))

  def obj(k: String): Either[String, ujson.Value] =
    field(k).filterOrElse(_.objOpt.isDefined, s"$what's $k is not an object")

  def arr(k: String): Either[String, Vector[ujson.Value]] =
    field(k).flatMap(_.arrOpt.map(_.toVector).toRight(s"$what's $k is not an array"))

  def str(k: String): Either[String, String] =
    field(k).flatMap(x => Fields.str(s"$what's $k", x))

  def num(k: String): Either[String, Double] =
    field(k).flatMap(_.numOpt.toRight(s"$what's $k is not a number"))

  def int(k: String): Either[String, Int] =
    num(k)
      .filterOrElse(n => n.isWhole && n.abs <= Int.MaxValue, s"$what's $k is not an integer")
      .map(_.toInt)

  def long(k: String): Either[String, Long] =
    num(k).filterOrElse(_.isWhole, s"$what's $k is not whole").map(_.toLong)

  def millis(k: String): Either[String, FiniteDuration] =
    num(k).filterOrElse(_.isWhole, s"$what's $k is not whole milliseconds").map(_.toLong.millis)

  def instant(k: String): Either[String, Instant] =
    str(k).flatMap(t => Try(Instant.parse(t)).toOption.toRight(s"$what's $k is not an instant"))

  def bool(k: String): Either[String, Boolean] =
    field(k).flatMap(_.boolOpt.toRight(s"$what's $k is not a boolean"))

  /** A decimal written as a string, so it keeps every digit. */
  def decimal(k: String): Either[String, BigDecimal] =
    str(k).flatMap(t => Try(BigDecimal(t)).toOption.toRight(s"$what's $k is not a decimal"))
}

object Fields {

  def str(what: String, v: ujson.Value): Either[String, String] =
    v.strOpt.toRight(s"$what is not a string")

  /** `read` of `o`'s value, when there is one. */
  def opt[A](o: Option[ujson.Value])(
      read: ujson.Value => Either[String, A]
  ): Either[String, Option[A]] = o.fold(Right(None))(read(_).map(Some(_)))
}
