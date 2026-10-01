package grit.lifecycle.settle

import grit.core.durable.Journaled
import grit.core.id.TurnSeq
import grit.core.period.{Judgement, Probability}

import Settle.Checked

/** How the settle's step outputs are recorded. A settle in flight must read back what an
  * earlier build wrote, so change these only with the workflow's epoch (ADR 0004).
  */
private[settle] object SettleJournal {

  given checked: Journaled[Either[String, Checked]] =
    outcome(
      {
        case Checked.Asking(first) => ujson.Obj("asking" -> TurnSeq.value(first).toDouble)
        case Checked.Abandoned(why) => ujson.Obj("abandoned" -> why)
      },
      v =>
        v.objOpt.map(o => (o.get("asking"), o.get("abandoned"))) match {
          case Some((Some(ujson.Num(n)), None)) if n.isWhole && n >= 0 =>
            Right(Checked.Asking(TurnSeq(n.toLong)))
          case Some((None, Some(ujson.Str(why)))) => Right(Checked.Abandoned(why))
          case _ => Left("checked: expected {asking} or {abandoned}")
        }
    )

  private val Options: Vector[String] = Vector("nobody", "onPerson", "onOther")

  given judged: Journaled[Judgement] =
    Journaled.json[Judgement](
      {
        case Judgement.Weighed(nobody, onPerson, onOther, model) =>
          val o = ujson.Obj()
          Options.zip(Vector(nobody, onPerson, onOther)).foreach { (key: String, p: Probability) =>
            o(key) = Probability.value(p)
          }
          o("model") = model
          o
        case Judgement.Unanswered(why) => ujson.Obj("unanswered" -> why)
      },
      v =>
        v.objOpt match {
          case None => Left("judged: expected an object")
          case Some(o) =>
            o.get("unanswered") match {
              case Some(ujson.Str(why)) => Right(Judgement.Unanswered(why))
              case Some(_) => Left("judged: unanswered is not a string")
              case None =>
                val ps = Options.flatMap(k => o.get(k).flatMap(_.numOpt).flatMap(Probability.of))
                (ps, o.get("model").flatMap(_.strOpt)) match {
                  case (Vector(n, p, x), Some(model)) =>
                    Right(Judgement.Weighed(n, p, x, model))
                  case _ => Left("judged: expected three probabilities and a model")
                }
            }
        }
    )

  given recorded: Journaled[Either[String, Boolean]] =
    outcome(ujson.Bool(_), _.boolOpt.toRight("recorded: expected a boolean"))

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
