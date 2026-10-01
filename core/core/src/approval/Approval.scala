package grit.core.approval

import grit.core.id.ToolCallId

/** A person's answer to a gated tool call. */
enum Approval {

  /** It may run. */
  case Approved

  /** It may not, for `reason` if they gave one. */
  case Declined(reason: Option[String])

  /** No answer came before the wait ran out; the call does not run. */
  case TimedOut
}

object Approval {

  /** The topic a turn waits on for the answer to its gated call `call`. */
  def topic(call: ToolCallId): String = s"approval:${ToolCallId.value(call)}"

  /** `approval` as the message that carries it to the waiting turn. */
  def encode(approval: Approval): String = ujson.write(approval match {
    case Approved => ujson.Obj("answer" -> "approved")
    case Declined(reason) =>
      ujson.Obj(
        "answer" -> "declined",
        "reason" -> reason.fold[ujson.Value](ujson.Null)(ujson.Str(_))
      )
    case TimedOut => ujson.Obj("answer" -> "timed-out")
  })

  /** The approval `message` carries, as [[encode]] wrote it; or why it carries none. */
  def decode(message: String): Either[String, Approval] =
    scala.util.Try(ujson.read(message)).toOption.flatMap(_.objOpt) match {
      case None => Left("not a JSON object")
      case Some(o) =>
        (o.get("answer").flatMap(_.strOpt), o.get("reason")) match {
          case (Some("approved"), _) => Right(Approved)
          case (Some("declined"), None | Some(ujson.Null)) => Right(Declined(None))
          case (Some("declined"), Some(ujson.Str(reason))) => Right(Declined(Some(reason)))
          case (Some("timed-out"), _) => Right(TimedOut)
          case _ => Left(s"not an answer: ${message.take(80)}")
        }
    }
}
