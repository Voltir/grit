package grit.core.durable

import scala.annotation.implicitNotFound

import grit.core.id.WorkflowId

/** How a step's output is recorded, so a replayed step returns what the first run did.
  *
  * The encoding is persisted: it must read back every value an earlier build wrote, so
  * instances are written by hand, never derived. A `decode` failure on replay is a code
  * change the journal cannot follow, and is thrown as [[UnreadableJournal]].
  */
@implicitNotFound(
  "A step's output is journaled and replayed, so ${A} needs a Journaled instance. A step returning Unit should return a value describing what it did."
)
// String, the type DBOS stores outputs as: its default Jackson serializer tags every
// non-final class with its JVM name, so grit hands it nothing else.
trait Journaled[A] {
  def encode(a: A): String
  def decode(s: String): Either[String, A]
}

object Journaled {

  given Journaled[String] with {
    def encode(a: String): String = a
    def decode(s: String): Either[String, String] = Right(s)
  }

  /** An instance over a hand-written JSON codec. */
  def json[A](write: A -> ujson.Value, read: ujson.Value -> Either[String, A]): Journaled[A] =
    new Journaled[A] {
      def encode(a: A): String = ujson.write(write(a))
      def decode(s: String): Either[String, A] =
        scala.util.Try(ujson.read(s)).toEither.left.map(_.getMessage).flatMap(read)
    }

  /** A step's outcome: its value in `write`'s form, as `{"ok": value}`, or why it failed, as
    * `{"failed": reason}`. A recorded output that is not an object, or holds both or neither,
    * fails to decode.
    */
  def outcome[A](
      write: A -> ujson.Value,
      read: ujson.Value -> Either[String, A]
  ): Journaled[Either[String, A]] =
    json[Either[String, A]](
      {
        case Right(a) => ujson.Obj("ok" -> write(a))
        case Left(reason) => ujson.Obj("failed" -> reason)
      },
      v =>
        v.objOpt match {
          case None => Left("expected an object")
          case Some(o) =>
            (o.get("ok"), o.get("failed").flatMap(_.strOpt)) match {
              case (Some(value), None) => read(value).map(Right(_))
              case (None, Some(reason)) => Right(Left(reason))
              case _ => Left("expected {ok} or {failed}")
            }
        }
    )
}

/** A recorded step output that the current code cannot decode: the workflow was started
  * by a build whose step outputs differ from this one's. Not retryable.
  */
final class UnreadableJournal(workflowId: WorkflowId, step: String, reason: String)
    extends RuntimeException(
      s"step '$step' of workflow ${WorkflowId.value(workflowId)}: recorded output unreadable: $reason"
    )
