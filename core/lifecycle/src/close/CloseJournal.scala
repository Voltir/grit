package grit.lifecycle.close

import grit.core.durable.Journaled
import grit.core.durable.Journaled.outcome
import grit.core.id.{EntryId, TurnSeq}
import grit.core.period.ClosingJson
import grit.core.store.{PayloadJson, Sealed}

import Close.{Checked, Summarised}

/** How the close's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`. A close
  * in flight must read back what an earlier build wrote, so change these only with the
  * workflow's epoch (ADR 0004).
  */
private[close] object CloseJournal {

  given checked: Journaled[Either[String, Checked]] =
    outcome(
      {
        case Checked.Due(first, reason, known, cap) =>
          val o = ujson.Obj(
            "due" -> PayloadJson.reasonName(reason),
            "first" -> TurnSeq.value(first).toDouble,
            "known" -> ClosingJson.writeBalance(known),
            "cap" -> cap.toDouble,
            // The version `known` is written in: a record from before versions reads as 2.
            "v" -> ClosingJson.Version.toDouble
          )
          PayloadJson.reasonConfidence(reason).foreach(c => o("confidence") = c)
          o
        case Checked.Closed => ujson.Str("closed")
        case Checked.Abandoned(why) => ujson.Obj("abandoned" -> why)
      },
      {
        case ujson.Str("closed") => Right(Checked.Closed)
        case o: ujson.Obj =>
          (o.value.get("due"), o.value.get("first"), o.value.get("abandoned")) match {
            case (Some(ujson.Str(reason)), Some(ujson.Num(n)), None) if n.isWhole && n >= 0 =>
              for {
                confidence <- o.value.get("confidence") match {
                  case None => Right(None)
                  case Some(ujson.Num(c)) => Right(Some(c))
                  case Some(_) => Left("checked: confidence is not a number")
                }
                r <- PayloadJson.readReason(reason, confidence)
                version <- o.value.get("v") match {
                  case None => Right(2)
                  case Some(ujson.Num(v)) if v.isWhole => Right(v.toInt)
                  case Some(_) => Left("checked: v is not a version")
                }
                known <- o.value
                  .get("known")
                  .toRight("checked: missing known")
                  .flatMap(ClosingJson.readBalance(_, version))
                cap <- o.value
                  .get("cap")
                  .collect { case ujson.Num(c) if c.isWhole => c.toInt }
                  .toRight("checked: missing cap")
              } yield Checked.Due(TurnSeq(n.toLong), r, known, cap)
            case (None, None, Some(ujson.Str(why))) => Right(Checked.Abandoned(why))
            case _ => Left("checked: expected {due, first, known, cap} or {abandoned}")
          }
        case _ => Left("checked: expected \"closed\" or an object")
      }
    )

  given asked: Journaled[(Asked, Option[String])] =
    Journaled.json[(Asked, Option[String])](
      (a, note) => {
        val o = ujson.Obj(
          "outcome" -> a.outcome,
          "open" -> a.open,
          "standing" -> a.standing,
          "settled" -> a.settled
        )
        note.foreach(n => o("note") = n)
        o
      },
      v =>
        for {
          o <- v.objOpt.toRight("asked: expected an object")
          flags <- Vector("outcome", "open", "standing", "settled")
            .foldLeft[Either[String, Vector[Boolean]]](Right(Vector.empty)) { (acc, key) =>
              acc.flatMap(bs =>
                o.get(key).flatMap(_.boolOpt).map(bs :+ _).toRight(s"asked: missing $key")
              )
            }
          asked <- flags match {
            case Vector(outcome, open, standing, settled) =>
              Right(Asked(outcome, open, standing, settled))
            case _ => Left("asked: expected four flags")
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
          note <- o.get("note") match {
            case None => Right(None)
            case Some(ujson.Str(n)) => Right(Some(n))
            case Some(_) => Left("summarised: note is not a string")
          }
        } yield Summarised(closing, note)
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
}
