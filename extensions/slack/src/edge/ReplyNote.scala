package grit.slack.edge

import grit.core.model.Grade
import grit.core.spend.TurnSpend

/** What a reply notes under it, in small print: nothing, the grade and model its answer was made
  * with, or those and what its turn spent ([[TurnSpend]]). Each note but `Off` is made only for
  * an answer made above [[ReplyNote.NotedAbove]]. `word` is the note's name in a deployment's
  * settings, as [[ReplyNote.named]] reads it: `off`, `strength`, `strength-cost`.
  */
enum ReplyNote(val word: String) derives CanEqual {
  case Off extends ReplyNote("off")
  case Strength extends ReplyNote("strength")
  case StrengthAndCost extends ReplyNote("strength-cost")

  /** The note for a reply whose turn spent `spend`: `strong · <model> at <upstream>`, with
    * `StrengthAndCost` the spend between (`strong · m$4.2 · …`); `None` when this is `Off`, or
    * the answer was not made above [[ReplyNote.NotedAbove]].
    */
  def of(spend: TurnSpend): Option[String] =
    for {
      grade <- spend.answer.flatMap(_.grade).filter(_ => spend.above(ReplyNote.NotedAbove))
      served <- spend.served
      shown <- this match {
        case Off => None
        case Strength => Some(Vector(grade.word, served))
        case StrengthAndCost => Some(Vector(grade.word, spend.cost.inMills, served))
      }
    } yield shown.mkString(" · ")
}

object ReplyNote {

  /** A reply is noted only when its answer was made above this grade. */
  val NotedAbove: Grade = Grade.Standard

  /** What a deployment that names no note gets. */
  val Default: ReplyNote = Strength

  /** The note `word` names; `None` for any other. */
  def named(word: String): Option[ReplyNote] = values.find(_.word == word)
}
