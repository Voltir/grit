package grit.eval.harness.label

import java.time.Instant

import scala.util.Try

import grit.core.id.ShadowName
import grit.core.identity.Account
import grit.core.review.{Reason, Verdict}
import grit.eval.harness.capture.{CaseId, Fields}

/** The verdict standing on one reviewed message's prompt: the shadow it was picked against
  * and why, the verdict, who gave it and when.
  */
final case class Rated(
    shadow: ShadowName,
    reason: Reason,
    verdict: Verdict,
    rater: Account,
    at: Instant
)

/** The verdicts standing on reviewed messages, by case. Kept apart from [[Labels]], never
  * merged with them: a verdict judges the speech decision at its moment, a label the inputs a
  * call was given.
  */
final case class Verdicts(cases: Map[CaseId, Rated])

object Verdicts {

  /** No verdict standing. */
  val Empty: Verdicts = Verdicts(Map.empty)

  /** `v` as `pull` writes it: `{"cases": {<case id>: {"shadow", "reason", "verdict", "rater",
    * "at"}}}`, the cases in id order; `reason` one of `shadow-only`, `live-only`, `both` and
    * `neither`, `verdict` one of `welcome`, `interruption` and `cut-in`.
    */
  def written(v: Verdicts): String =
    ujson.write(
      ujson.Obj(
        "cases" -> ujson.Obj.from(v.cases.toVector.sortBy(_._1).map { (id, r) =>
          id.written -> ujson.Obj(
            "shadow" -> ShadowName.value(r.shadow),
            "reason" -> reason(r.reason),
            "verdict" -> verdictWritten(r.verdict),
            "rater" -> Account.written(r.rater),
            "at" -> r.at.toString
          )
        })
      ),
      indent = 2
    ) + "\n"

  /** The verdicts `text` holds, as [[written]] writes them; why not, naming the case and
    * field, when it is not of that form.
    */
  def read(text: String): Either[String, Verdicts] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("verdicts: not JSON")
      cases <- Fields("verdicts", root).obj("cases")
      each <- Fields.each(cases.obj.toVector) { (written, v) =>
        val at = Fields(s"verdicts: $written", v)
        for {
          id <- CaseId.read(written).left.map(why => s"verdicts: $why")
          shadow <- at
            .str("shadow")
            .flatMap(n => ShadowName.of(n).left.map(w => s"verdicts: $written: shadow $w"))
          why <- at
            .str("reason")
            .flatMap(r =>
              Reason.values.find(reason(_) == r).toRight(s"verdicts: $written: reason $r")
            )
          verdict <- at
            .str("verdict")
            .flatMap(r =>
              Verdict.values.find(verdictWritten(_) == r).toRight(s"verdicts: $written: verdict $r")
            )
          rater <- at
            .str("rater")
            .flatMap(r => Account.read(r).left.map(w => s"verdicts: $written: rater $w"))
          when <- at.instant("at")
        } yield id -> Rated(shadow, why, verdict, rater, when)
      }
    } yield Verdicts(each.toMap)

  /** `r` as the file writes it. */
  def reason(r: Reason): String = r match {
    case Reason.ShadowOnly => "shadow-only"
    case Reason.LiveOnly => "live-only"
    case Reason.Both => "both"
    case Reason.Neither => "neither"
  }

  private def verdictWritten(v: Verdict): String = v match {
    case Verdict.Welcome => "welcome"
    case Verdict.Interruption => "interruption"
    case Verdict.CutIn => "cut-in"
  }
}
