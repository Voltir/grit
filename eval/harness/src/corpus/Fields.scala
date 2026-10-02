package grit.eval.harness.corpus

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Try

import grit.core.period.Probability

/** The fields of `v`, an object read as `what`, for the harness's files: each read is `Left`
  * naming `what` and the field when it is missing or not of its form.
  */
final case class Fields(what: String, v: ujson.Value) {
  def field(k: String): Either[String, ujson.Value] =
    v.objOpt.flatMap(_.get(k)).toRight(s"$what: no $k")

  /** `None` when the field is JSON `null`. */
  def optional(k: String): Either[String, Option[ujson.Value]] =
    field(k).map {
      case ujson.Null => None
      case x => Some(x)
    }

  def obj(k: String): Either[String, ujson.Value] =
    field(k).filterOrElse(_.objOpt.isDefined, s"$what: $k is not an object")

  def arr(k: String): Either[String, Vector[ujson.Value]] =
    field(k).flatMap(_.arrOpt.map(_.toVector).toRight(s"$what: $k is not an array"))

  def str(k: String): Either[String, String] =
    field(k).flatMap(x => Fields.str(s"$what: $k", x))

  def num(k: String): Either[String, Double] =
    field(k).flatMap(_.numOpt.toRight(s"$what: $k is not a number"))

  def int(k: String): Either[String, Int] =
    num(k)
      .filterOrElse(n => n.isWhole && n.abs <= Int.MaxValue, s"$what: $k is not an integer")
      .map(_.toInt)

  def long(k: String): Either[String, Long] =
    num(k).filterOrElse(_.isWhole, s"$what: $k is not whole").map(_.toLong)

  def millis(k: String): Either[String, FiniteDuration] =
    num(k).filterOrElse(_.isWhole, s"$what: $k is not whole milliseconds").map(_.toLong.millis)

  def probability(k: String): Either[String, Probability] =
    num(k).flatMap(Probability.of(_).toRight(s"$what: $k is not a probability"))

  def instant(k: String): Either[String, Instant] =
    str(k).flatMap(t => Try(Instant.parse(t)).toOption.toRight(s"$what: $k is not an instant"))

  def bool(k: String): Either[String, Boolean] =
    field(k).flatMap(_.boolOpt.toRight(s"$what: $k is not a boolean"))

  /** A decimal written as a string, so it keeps every digit. */
  def decimal(k: String): Either[String, BigDecimal] =
    str(k).flatMap(t => Try(BigDecimal(t)).toOption.toRight(s"$what: $k is not a decimal"))
}

object Fields {

  def str(what: String, v: ujson.Value): Either[String, String] =
    v.strOpt.toRight(s"$what is not a string")

  /** `read` of `o`'s value, when there is one. */
  def opt[A](o: Option[ujson.Value])(
      read: ujson.Value => Either[String, A]
  ): Either[String, Option[A]] = o.fold(Right(None))(read(_).map(Some(_)))

  /** `f` of each of `as`, in order; the first `Left`. */
  def each[A, B](as: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    as.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => f(a).map(done :+ _))
    )
}
