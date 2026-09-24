package grit.turn

import grit.core.context.Window
import grit.core.durable.Journaled
import grit.core.id.EntryId
import grit.core.message.Message
import grit.core.store.{Payload, PayloadJson}

/** How the turn's step outputs are recorded: `{"ok": value}` or
  * `{"failed": kind, "reason": text}`. In-flight turns must read back what an earlier
  * build wrote, so change these only with the workflow version (`workflow-versioning`).
  */
private[turn] object TurnJournal {

  given window: Journaled[Either[TurnFailure, Window]] =
    outcome(
      w => ujson.Arr.from(w.entries.map(id => ujson.Str(EntryId.value(id)))),
      v =>
        v.arrOpt match {
          case Some(ids) =>
            val strs = ids.flatMap(_.strOpt).toVector
            if (strs.size == ids.size) Right(Window(strs.map(EntryId(_))))
            else Left("window: expected an array of strings")
          case None => Left("window: expected an array")
        }
    )

  given reply: Journaled[Either[TurnFailure, Message.Assistant]] =
    outcome(
      m => PayloadJson.write(Payload.Message(m)),
      v =>
        PayloadJson.read(v).flatMap {
          case Payload.Message(m: Message.Assistant) => Right(m)
          case Payload.Message(_) => Left("reply: expected an assistant message")
        }
    )

  given entryId: Journaled[Either[TurnFailure, EntryId]] =
    outcome(
      id => ujson.Str(EntryId.value(id)),
      v => v.strOpt.map(EntryId(_)).toRight("entry id: expected a string")
    )

  private def outcome[A](
      write: A -> ujson.Value,
      read: ujson.Value -> Either[String, A]
  ): Journaled[Either[TurnFailure, A]] =
    Journaled.json[Either[TurnFailure, A]](
      {
        case Right(a) => ujson.Obj("ok" -> write(a))
        case Left(failure) =>
          val (kind, reason) = failure match {
            case TurnFailure.Assembly(r) => ("assembly", r)
            case TurnFailure.Model(r) => ("model", r)
            case TurnFailure.Store(r) => ("store", r)
          }
          ujson.Obj("failed" -> kind, "reason" -> reason)
      },
      v =>
        v.objOpt match {
          case None => Left("expected an object")
          case Some(o) =>
            (
              o.get("ok"),
              o.get("failed").flatMap(_.strOpt),
              o.get("reason").flatMap(_.strOpt)
            ) match {
              case (Some(value), None, None) => read(value).map(Right(_))
              case (None, Some("assembly"), Some(r)) => Right(Left(TurnFailure.Assembly(r)))
              case (None, Some("model"), Some(r)) => Right(Left(TurnFailure.Model(r)))
              case (None, Some("store"), Some(r)) => Right(Left(TurnFailure.Store(r)))
              case _ => Left("expected {ok} or {failed, reason}")
            }
        }
    )
}
