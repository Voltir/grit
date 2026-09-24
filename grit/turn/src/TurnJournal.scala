package grit.turn

import grit.core.context.{AssemblyNote, Window}
import grit.core.durable.Journaled
import grit.core.id.{EntryId, TurnSeq}
import grit.core.message.{Message, Tokens}
import grit.core.store.{Payload, PayloadJson}

/** How the turn's step outputs are recorded: `{"ok": value}` or
  * `{"failed": kind, "reason": text}`. In-flight turns must read back what an earlier
  * build wrote, so change these only with the workflow version (`workflow-versioning`).
  */
private[turn] object TurnJournal {

  /** A window with no notes is the bare array of its ids, as every build has written it; one
    * with notes is `{"entries": [...], "notes": [...]}`.
    */
  given window: Journaled[Either[TurnFailure, Window]] =
    outcome(
      w => {
        val ids = ujson.Arr.from(w.entries.map(id => ujson.Str(EntryId.value(id))))
        if (w.notes.isEmpty) ids
        else ujson.Obj("entries" -> ids, "notes" -> ujson.Arr.from(w.notes.map(writeNote)))
      },
      v =>
        v match {
          case ujson.Arr(_) => readIds(v).map(Window(_))
          case o: ujson.Obj =>
            for {
              ids <- o.value.get("entries").toRight("window: missing entries").flatMap(readIds)
              raw <- o.value.get("notes").flatMap(_.arrOpt).toRight("window: missing notes")
              notes <- raw.toVector
                .foldLeft[Either[String, Vector[AssemblyNote]]](Right(Vector.empty)) { (acc, n) =>
                  acc.flatMap(ns => readNote(n).map(ns :+ _))
                }
            } yield Window(ids, notes)
          case _ => Left("window: expected an array or an object")
        }
    )

  private def readIds(v: ujson.Value): Either[String, Vector[EntryId]] =
    v.arrOpt match {
      case Some(ids) =>
        val strs = ids.flatMap(_.strOpt).toVector
        if (strs.size == ids.size) Right(strs.map(EntryId(_)))
        else Left("window: expected an array of strings")
      case None => Left("window: expected an array")
    }

  private def writeNote(n: AssemblyNote): ujson.Value = n match {
    case AssemblyNote.Queried(query, model, usage, estimate) =>
      ujson.Obj(
        "queried" -> query,
        "model" -> model,
        "usage" -> PayloadJson.writeUsage(usage),
        "estimate" -> Tokens.value(estimate).toDouble
      )
    case AssemblyNote.FellBack(reason) => ujson.Obj("fellBack" -> reason)
    case AssemblyNote.Recalled(turns) =>
      ujson.Obj("recalled" -> ujson.Arr.from(turns.map(t => ujson.Num(TurnSeq.value(t).toDouble))))
  }

  private def readNote(v: ujson.Value): Either[String, AssemblyNote] = v match {
    case o: ujson.Obj =>
      (o.value.get("queried"), o.value.get("fellBack"), o.value.get("recalled")) match {
        case (Some(ujson.Str(query)), None, None) =>
          for {
            model <- o.value.get("model").flatMap(_.strOpt).toRight("note: missing model")
            usage <- o.value
              .get("usage")
              .toRight("note: missing usage")
              .flatMap(PayloadJson.readUsage)
            estimate <- o.value
              .get("estimate")
              .collect { case ujson.Num(n) if n.isWhole && n >= 0 => Tokens(n.toLong) }
              .toRight("note: bad estimate")
          } yield AssemblyNote.Queried(query, model, usage, estimate)
        case (None, Some(ujson.Str(reason)), None) => Right(AssemblyNote.FellBack(reason))
        case (None, None, Some(ujson.Arr(turns))) =>
          val seqs = turns.toVector.collect {
            case ujson.Num(n) if n.isWhole && n >= 0 => TurnSeq(n.toLong)
          }
          if (seqs.size == turns.size) Right(AssemblyNote.Recalled(seqs))
          else Left("note: a recalled turn is not a non-negative whole number")
        case _ => Left("note: expected queried, fellBack or recalled")
      }
    case _ => Left("note: expected an object")
  }

  given reply: Journaled[Either[TurnFailure, Message.Assistant]] =
    outcome(
      m => PayloadJson.write(Payload.Message(m)),
      v =>
        PayloadJson.read(v).flatMap {
          case Payload.Message(m: Message.Assistant) => Right(m)
          case _ => Left("reply: expected an assistant message")
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
