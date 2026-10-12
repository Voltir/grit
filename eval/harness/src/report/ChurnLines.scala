package grit.eval.harness.report

import grit.core.id.WorkflowId
import grit.eval.harness.capture.Support
import grit.eval.harness.score.Churn
import grit.eval.harness.stats.{Estimate, Proportion}

/** [[Report.churn]]'s sections. */
private[report] object ChurnLines {

  def of(capture: String, c: Churn): String = {
    import Churn.Reads
    def table(head: String*)(rows: Vector[Vector[String]]): Vector[String] =
      Vector(head.mkString("| ", " | ", " |"), head.map(_ => "---").mkString("|", "|", "|")) ++
        rows.map(_.mkString("| ", " | ", " |"))
    def num(d: Double): String = f"$d%.3f"
    def estimate(what: String, e: Option[Estimate]): Vector[String] =
      e.fold(Vector(what, "—", "—", "—", "—")) { x =>
        val (lo, hi) = x.interval
        Vector(what, s"${x.n}", num(x.mean), s"[${num(lo)}, ${num(hi)}]", num(x.mde))
      }
    def count(p: Churn.Pair => Churn.Side, hit: Reads => Boolean): Int =
      c.pairs.count(pair => hit(p(pair).reads))
    val unread: Reads => Boolean = _ == Reads.Unread
    val useless: Reads => Boolean = {
      case Reads.Read(_, _, None) | Reads.Unread => true
      case Reads.Read(_, _, Some(_)) => false
    }
    def reads(s: Churn.Side): String = {
      val read = s.reads match {
        case Reads.Unread => "unread"
        case Reads.Read(before, followed, useful) =>
          s"$before${if (followed) " followed" else ""}, useful " + useful.fold("never")(_.toString)
      }
      s"${WorkflowId.value(s.workflow)} $read${if (s.confounded) " (confounded)" else ""}"
    }
    val confounded = c.pairs.flatMap(p => Vector(p.before, p.after)).filter(_.confounded)
    (Vector(
      s"# churn: $capture",
      "",
      s"pairs: ${c.pairs.size}; confounded turns: ${confounded.size}" +
        (if (confounded.isEmpty) ""
         else confounded.map(s => WorkflowId.value(s.workflow)).mkString(" (", ", ", ")")),
      "",
      "Text-free: question ids, workflow ids and counts. A turn's calls are its hosted calls, " +
        "its own topic tool's left out; it reads when a call follows the pointer of a document " +
        "current at capture, and usefully when the reply's support in that call's result is at " +
        s"${Support.Used.value} or over, a lexical heuristic. A turn is confounded when its window held a part of " +
        "another thread of the pairs. Each difference is the before-turn's calls before its " +
        "first read less the after-turn's, positive when after read sooner, over the pairs both " +
        "of whose turns read; a turn that never reads is counted apart, never imputed. A mean's " +
        "interval is 95%, Student's t clustered by question (CR1), none under two questions; " +
        "MDE is the least difference a paired comparison of its precision detects.",
      "",
      "## Calls before the first read",
      ""
    ) ++ table("read", "pairs", "before − after", "interval", "MDE")(
      Vector(
        estimate("indexed", c.indexed),
        estimate("useful", c.useful),
        estimate("indexed, unconfounded", c.indexedApart),
        estimate("useful, unconfounded", c.usefulApart)
      )
    ) ++ Vector(
      "",
      s"never read: before ${count(_.before, unread)}, after ${count(_.after, unread)}; " +
        s"never useful: before ${count(_.before, useless)}, after ${count(_.after, useless)}",
      s"after-turns whose first read followed a card their window held: ${rate(c.followed)}",
      "",
      "## Pairs",
      ""
    ) ++ table("question", "before", "after")(
      c.pairs.map(p => Vector(p.question, reads(p.before), reads(p.after)))
    )).mkString("\n") + "\n"
  }

  private def rate(r: Proportion): String =
    r.rate.fold("—") { rate =>
      val bounds = r.interval match {
        case Proportion.Interval.TooFewClusters => "too few questions for an interval"
        case Proportion.Interval.Wilson(low, high, _) => f"[$low%.3f, $high%.3f]"
      }
      s"${f"$rate%.3f"} (${r.hits}/${r.n} in ${r.clusters} questions) $bounds"
    }
}
