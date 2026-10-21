package grit.eval.harness.score

import java.time.ZoneOffset

import grit.core.review.Verdict
import grit.core.speech.{Decision, Heard, Ledger, Limits, Speaking, Speech}
import grit.core.spend.{Budget, Spend}
import grit.core.triage.{Gate, GateJson}
import grit.eval.harness.capture.CaseId
import grit.eval.harness.label.Verdicts
import grit.eval.harness.stats.Proportion

/** The gate sweep: draft gates a deployment could ship, each decided by the shipped
  * [[Speech.decide]] over the messages live's speech decisions weighed, against the review
  * verdicts standing on them.
  */
object GateSweep {

  /** Named gates to sweep, each checked under `limits` by [[Speech.decide]] over every case's
    * recorded tags, reach and cues; `limits`' own gate is never read.
    */
  final case class Grid(points: Vector[(String, Gate)], limits: Limits)

  object Grid {

    /** The grid `text` holds under `limits`: `{"points": [{"name", "gate"}, ...]}`, in order,
      * each gate `{"all": [gate, ...]}`, `{"any": [gate, ...]}` (at least one) or a bound
      * ([[GateJson.readBound]]). `Left` naming the point and what is wrong, for anything else
      * or a name given twice.
      */
    def read(text: String, limits: Limits): Either[String, Grid] =
      for {
        v <- scala.util.Try(ujson.read(text)).toOption.toRight("grid: not JSON")
        points <- v.objOpt
          .flatMap(_.get("points"))
          .flatMap(_.arrOpt)
          .toRight("grid: expected {\"points\": [...]}")
        read <- points.toVector.zipWithIndex.foldLeft[Either[String, Vector[(String, Gate)]]](
          Right(Vector.empty)
        ) { case (acc, (p, i)) =>
          acc.flatMap { done =>
            for {
              name <- p.objOpt
                .flatMap(_.get("name"))
                .flatMap(_.strOpt)
                .toRight(s"grid: point $i: expected a name")
              _ <- Either.cond(!done.exists(_._1 == name), (), s"grid: $name given twice")
              gate <- p.objOpt
                .flatMap(_.get("gate"))
                .toRight(s"grid: $name: no gate")
                .flatMap(gateOf)
                .left
                .map(why => s"grid: $name: $why")
            } yield done :+ (name -> gate)
          }
        }
      } yield Grid(read, limits)

    /** A gate as [[read]] reads one. */
    private def gateOf(v: ujson.Value): Either[String, Gate] = {
      def parts(items: ujson.Value): Either[String, Vector[Gate]] =
        items.arrOpt
          .toRight("expected an array of gates")
          .flatMap(
            _.toVector.foldLeft[Either[String, Vector[Gate]]](Right(Vector.empty))((acc, g) =>
              acc.flatMap(done => gateOf(g).map(done :+ _))
            )
          )
      val o = v.objOpt
      (o.flatMap(_.get("all")), o.flatMap(_.get("any"))) match {
        case (Some(all), None) => parts(all).map(Gate.All(_))
        case (None, Some(any)) =>
          parts(any).flatMap {
            case first +: rest => Right(Gate.AnyOf(first, rest))
            case _ => Left("any: expected at least one gate")
          }
        case (None, None) => GateJson.readBound(v).map(Gate.Holds(_))
        case (Some(_), Some(_)) => Left("expected all or any, not both")
      }
    }
  }

  /** A case to sweep: its `id`, and its message as live's speech decision weighed it. */
  final case class Case(id: CaseId, heard: Heard)

  /** One grid point's decisions: its `name`; the cases it drafts, of all (`drafted`); the
    * welcome among those it drafts with a verdict standing (`welcome`), each clustered by room;
    * how many it drafts a verdict calls an interruption or a cut-in (`interrupting`); and how
    * many with a welcome verdict it holds (`welcomeHeld`).
    */
  final case class Point(
      name: String,
      drafted: Proportion,
      welcome: Proportion,
      interrupting: Int,
      welcomeHeld: Int
  )

  /** Each point's decisions over `cases`, against `verdicts` standing, in the grid's order. A
    * case is decided as it was said, over an empty ledger and an uncapped budget, so no rate,
    * cap or staleness holds it; drafted is [[Decision.Drafting]] (a message answered as said to
    * grit is not). A case with no verdict counts in drafted alone; one decided before cues
    * reads every fact 0.
    */
  def swept(grid: Grid, cases: Vector[Case], verdicts: Verdicts): Vector[Point] =
    grid.points.map { (name, gate) =>
      val speaking = Speaking.Shadow(grid.limits.copy(drafts = gate))
      val decided: Vector[(Case, Boolean, Option[Verdict])] = cases.map { c =>
        val drafts = Speech.decide(speaking, c.heard, Empty, Uncapped, c.heard.said) match {
          case Decision.Drafting(_) => true
          case Decision.Answering(_, _) | Decision.Held(_) => false
        }
        (c, drafts, verdicts.cases.get(c.id).map(_.verdict))
      }
      Point(
        name,
        Proportion.of(decided.map((c, drafts, _) => c.heard.room -> drafts)),
        Proportion.of(decided.collect { case (c, true, Some(v)) => c.heard.room -> v.speaks }),
        decided.count { case (_, drafts, v) => drafts && v.exists(!_.speaks) },
        decided.count { case (_, drafts, v) => !drafts && v.exists(_.speaks) }
      )
    }

  /** No turn spoken and nothing spent. */
  private val Empty = Ledger(Vector.empty, Spend.Zero, Spend.Zero)

  /** No daily cap; its zone is never read with no cap. */
  private val Uncapped = Budget(ZoneOffset.UTC, None)
}
