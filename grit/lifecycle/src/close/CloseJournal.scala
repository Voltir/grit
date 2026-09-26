package grit.lifecycle.close

import grit.core.durable.Journaled
import grit.core.id.{EntryId, TurnSeq}
import grit.core.message.Tokens
import grit.core.period.ClosingJson
import grit.core.store.{PayloadJson, Sealed}

import Close.{Checked, Cost, Summarised}

/** How the close's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`. A close
  * in flight must read back what an earlier build wrote, so change these only with the
  * workflow's epoch (ADR 0004).
  */
private[close] object CloseJournal {

  given checked: Journaled[Either[String, Checked]] =
    outcome(
      {
        case Checked.Due(first, reason) =>
          ujson.Obj(
            "due" -> PayloadJson.reasonName(reason),
            "first" -> TurnSeq.value(first).toDouble
          )
        case Checked.Closed => ujson.Str("closed")
        case Checked.Abandoned(why) => ujson.Obj("abandoned" -> why)
      },
      {
        case ujson.Str("closed") => Right(Checked.Closed)
        case o: ujson.Obj =>
          (o.value.get("due"), o.value.get("first"), o.value.get("abandoned")) match {
            case (Some(ujson.Str(reason)), Some(ujson.Num(n)), None) if n.isWhole && n >= 0 =>
              PayloadJson.readReason(reason).map(Checked.Due(TurnSeq(n.toLong), _))
            case (None, None, Some(ujson.Str(why))) => Right(Checked.Abandoned(why))
            case _ => Left("checked: expected {due, first} or {abandoned}")
          }
        case _ => Left("checked: expected \"closed\" or an object")
      }
    )

  given asked: Journaled[(Asked, Option[String])] =
    Journaled.json[(Asked, Option[String])](
      (a, note) => {
        val o = ujson.Obj(
          "outcome" -> a.outcome,
          "decisions" -> a.decisions,
          "facts" -> a.facts,
          "open" -> a.open,
          "sources" -> a.sources
        )
        note.foreach(n => o("note") = n)
        o
      },
      v =>
        for {
          o <- v.objOpt.toRight("asked: expected an object")
          flags <- Vector("outcome", "decisions", "facts", "open", "sources")
            .foldLeft[Either[String, Vector[Boolean]]](Right(Vector.empty)) { (acc, key) =>
              acc.flatMap(bs =>
                o.get(key).flatMap(_.boolOpt).map(bs :+ _).toRight(s"asked: missing $key")
              )
            }
          asked <- flags match {
            case Vector(outcome, decisions, facts, open, sources) =>
              Right(Asked(outcome, decisions, facts, open, sources))
            case _ => Left("asked: expected five flags")
          }
          note <- o.get("note") match {
            case None => Right(None)
            case Some(ujson.Str(n)) => Right(Some(n))
            case Some(_) => Left("asked: note is not a string")
          }
        } yield (asked, note)
    )

  given summarised: Journaled[Summarised] =
    Journaled.json[Summarised](
      s => {
        val o = ujson.Obj()
        s.closing.foreach(c => o("closing") = ClosingJson.write(c))
        s.cost.foreach { c =>
          o("cost") = ujson.Obj(
            "model" -> c.model,
            "usage" -> PayloadJson.writeUsage(c.usage),
            "estimate" -> Tokens.value(c.estimate).toDouble
          )
        }
        s.note.foreach(n => o("note") = n)
        o
      },
      v =>
        for {
          o <- v.objOpt.toRight("summarised: expected an object")
          closing <- o.get("closing") match {
            case None => Right(None)
            case Some(c) => ClosingJson.read(c).map(Some(_))
          }
          cost <- o.get("cost") match {
            case None => Right(None)
            case Some(c) =>
              for {
                co <- c.objOpt.toRight("cost: expected an object")
                model <- co.get("model").flatMap(_.strOpt).toRight("cost: missing model")
                usage <- co
                  .get("usage")
                  .toRight("cost: missing usage")
                  .flatMap(PayloadJson.readUsage)
                estimate <- co
                  .get("estimate")
                  .collect { case ujson.Num(n) if n.isWhole && n >= 0 => Tokens(n.toLong) }
                  .toRight("cost: bad estimate")
              } yield Some(Cost(model, usage, estimate))
          }
          note <- o.get("note") match {
            case None => Right(None)
            case Some(ujson.Str(n)) => Right(Some(n))
            case Some(_) => Left("summarised: note is not a string")
          }
        } yield Summarised(closing, cost, note)
    )

  given sealing: Journaled[Either[String, Sealed]] =
    outcome(
      {
        case Sealed.Closed(entry) => ujson.Obj("closed" -> EntryId.value(entry))
        case Sealed.Abandoned => ujson.Str("abandoned")
      },
      {
        case ujson.Str("abandoned") => Right(Sealed.Abandoned)
        case o: ujson.Obj =>
          o.value
            .get("closed")
            .flatMap(_.strOpt)
            .map(id => Sealed.Closed(EntryId(id)))
            .toRight("sealed: expected {closed}")
        case _ => Left("sealed: expected \"abandoned\" or an object")
      }
    )

  private def outcome[A](
      write: A -> ujson.Value,
      read: ujson.Value -> Either[String, A]
  ): Journaled[Either[String, A]] =
    Journaled.json[Either[String, A]](
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
