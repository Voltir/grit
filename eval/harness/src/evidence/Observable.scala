package grit.eval.harness.evidence

import scala.util.Try
import scala.util.matching.Regex

/** What an amendment says its coder will be seen doing: a pattern found within one line of a
  * trajectory as the bench kept it (`java.util.regex`, anywhere in the line).
  */
opaque type Observable = Regex

object Observable {

  /** `pattern`, or why not: it is empty, it does not compile, or it is found in an empty line (so
    * in every line, and fires whatever the coder did).
    */
  def of(pattern: String): Either[String, Observable] =
    for {
      _ <- Either.cond(pattern.nonEmpty, (), "the pattern is empty")
      r <- Try(pattern.r).toEither.left.map(e => s"the pattern does not compile: ${e.getMessage}")
      _ <- Either.cond(
        r.findFirstIn("").isEmpty,
        (),
        "the pattern matches an empty line, so every line"
      )
    } yield r

  /** The lines of `trajectory` in which `o` is found, whole and in order, split at `\n` or `\r\n`;
    * empty when it never fired.
    */
  def matched(o: Observable, trajectory: String): Vector[String] =
    trajectory.split("\r?\n").toVector.filter(o.findFirstIn(_).isDefined)
}
