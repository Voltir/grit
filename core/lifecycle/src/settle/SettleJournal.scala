package grit.lifecycle.settle

import grit.core.durable.Journaled
import grit.core.durable.Journaled.outcome
import grit.core.id.TurnSeq
import grit.core.period.{Judgement, JudgementJson}

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

  given judged: Journaled[Judgement] =
    Journaled.json[Judgement](JudgementJson.write, JudgementJson.read)

  given recorded: Journaled[Either[String, Boolean]] = Journaled.flag
}
