package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.triage.Weighing
import grit.eval.harness.corpus.TurnCase
import grit.turn.{TurnRecord, TurnWeighing}

/** How turns recorded their offers and weighing: `shaped`, `unshaped` (an offer recorded
  * before shapes, offered as recorded under every variant), `unoffered` (no offer recorded);
  * and their `weigh` steps: `kept`, `asked`, `failed` by kind, in [[Weighing.Unweighed]]'s
  * order, `none` (weighed nothing), `unrecorded` (no step).
  */
final case class Shapes(
    shaped: Int,
    unshaped: Int,
    unoffered: Int,
    kept: Int,
    asked: Int,
    failed: VectorMap[Weighing.Unweighed, Int],
    none: Int,
    unrecorded: Int
)

object Shapes {

  def of(turns: Vector[TurnCase]): Shapes = {
    val failed = turns.flatMap(_.weighed match {
      case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Failed(why))) => Some(why)
      case _ => None
    })
    Shapes(
      turns.count(_.offered.exists(_.shape.nonEmpty)),
      turns.count(_.offered.exists(_.shape.isEmpty)),
      turns.count(_.offered.isEmpty),
      turns.count(_.weighed match {
        case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Kept(_))) => true
        case _ => false
      }),
      turns.count(_.weighed match {
        case TurnRecord.Weigh.Recorded(Some(TurnWeighing.Weighed.Asked(_))) => true
        case _ => false
      }),
      VectorMap.from(
        Weighing.Unweighed.values.toVector
          .map(why => why -> failed.count(_ == why))
          .filter(_._2 > 0)
      ),
      turns.count(_.weighed == TurnRecord.Weigh.Recorded(None)),
      turns.count(_.weighed == TurnRecord.Weigh.Unrecorded)
    )
  }
}
