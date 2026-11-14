package grit.commands.routes

import grit.edge.http.Request

/** A request's query, each parameter one a route reads, sent at most once. */
private[commands] final class Queried private (request: Request) {

  /** `name`'s value as `read` reads it, `Left` naming `name` and why; `None` when not sent. */
  def read[A](name: String)(read: String => Either[String, A]): Either[String, Option[A]] =
    request.query(name) match {
      case None => Right(None)
      case Some(v) => read(v).left.map(why => s"$name: $why").map(Some(_))
    }

  /** `name`'s value as `read` reads it, or why not: `no <name>` when not sent. */
  def need[A](name: String)(read: String => Either[String, A]): Either[String, A] =
    this.read(name)(read).flatMap(_.toRight(s"no $name"))
}

private[commands] object Queried {

  /** `request`'s query, or why not: a parameter not in `reads`, or one sent twice, each named. */
  def of(request: Request, reads: Vector[String]): Either[String, Queried] =
    request.queryNames
      .collectFirst {
        case n if reads.isEmpty => s"no parameter $n is read here; this route reads none"
        case n if !reads.contains(n) =>
          s"no parameter $n is read here; these are: ${reads.mkString(", ")}"
        case n if request.queryValues(n).size > 1 => s"$n is sent twice"
      }
      .toLeft(new Queried(request))
}
