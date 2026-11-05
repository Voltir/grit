package grit.databases.declare

import scala.concurrent.duration.*

/** At most `rows` rows and `bytes` bytes of UTF-8 cell text in a result, the clip said; a
  * statement stopped after `timeout`, and opening waits as long, rounded up to seconds.
  */
final case class Limits private (rows: Int, bytes: Int, timeout: FiniteDuration)

object Limits {

  /** Or why not: rows or bytes not positive, or timeout under one millisecond or over
    * `Int.MaxValue` milliseconds (Postgres's most).
    */
  def of(rows: Int, bytes: Int, timeout: FiniteDuration): Either[String, Limits] =
    if (rows <= 0) Left(s"a result's rows must be positive: $rows")
    else if (bytes <= 0) Left(s"a result's bytes must be positive: $bytes")
    else if (timeout.toMillis < 1)
      Left(s"a statement's timeout is at least a millisecond: $timeout")
    else if (timeout.toMillis > Int.MaxValue)
      Left(s"a statement's timeout is at most ${Int.MaxValue} ms: $timeout")
    else Right(new Limits(rows, bytes, timeout))

  /** 200 rows, 32 KB, 10 seconds. */
  val Default: Limits = new Limits(200, 32 * 1024, 10.seconds)
}
