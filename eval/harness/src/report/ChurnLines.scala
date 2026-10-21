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
      val flags = (if (s.confounded) " (confounded)" else "") + (if (s.echoed) " (echoed)" else "")
      val grade = s.grade match {
        case Churn.Grade.Ungraded => ""
        case Churn.Grade.Unreplied => s"; ${s.calls} calls, no reply"
        case Churn.Grade.Graded(m) =>
          s"; ${s.calls} calls, ${if (m.correct) "right" else "wrong"} " +
            s"(facts ${m.found}/${m.of}, never ${m.never})"
      }
      s"${WorkflowId.value(s.workflow)} $read$flags$grade"
    }
    val sides = c.pairs.flatMap(p => Vector(p.before, p.after))
    def turns(what: String, of: Vector[Churn.Side]) =
      s"$what turns: ${of.size}" +
        (if (of.isEmpty) ""
         else of.map(s => WorkflowId.value(s.workflow)).mkString(" (", ", ", ")"))
    val answers = c.correctness.fold(Vector.empty[String]) { k =>
      Vector(
        "## Answers",
        "",
        "Each reply matched against its question's gold, counts kept and no words: right when " +
          "it holds every fact and no never-string, a turn with no reply wrong. To correct is " +
          "the before-turn's calls less the after-turn's over the pairs both of whose turns " +
          "were right; better is 1 a pair when the after-turn ranks above the before-turn, −1 " +
          "below, 0 the same, ranking right after fewer calls above right after more, above " +
          "wrong having read, above wrong having never read.",
        "",
        s"correct: before ${rate(k.before)}; after ${rate(k.after)}",
        ""
      ) ++ table("measure", "pairs", "mean", "interval", "MDE")(
        Vector(
          estimate("to correct", k.toCorrect),
          estimate("better", k.better),
          estimate("to correct, clean", k.toCorrectClean),
          estimate("better, clean", k.betterClean)
        )
      ) :+ ""
    }
    (Vector(
      s"# churn: $capture",
      "",
      s"pairs: ${c.pairs.size}; ${turns("confounded", sides.filter(_.confounded))}; " +
        turns("echoed", sides.filter(_.echoed)),
      "",
      "Text-free: question ids, workflow ids and counts. A turn's calls are its hosted calls, " +
        "its own topic tool's left out; it reads when a call follows the pointer of a document " +
        "current at capture, and usefully when the reply's support in that call's result is at " +
        s"${Support.Used.value} or over, a lexical heuristic. A turn is confounded when its window held a part of " +
        "another thread of the pairs, echoed when it held a part of the thread of another asking " +
        "of its own question, and a pair is clean when neither of its turns is either. Each difference is the before-turn's calls before its " +
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
        estimate("indexed, clean", c.indexedClean),
        estimate("useful, clean", c.usefulClean)
      )
    ) ++ Vector(
      "",
      s"never read: before ${count(_.before, unread)}, after ${count(_.after, unread)}; " +
        s"never useful: before ${count(_.before, useless)}, after ${count(_.after, useless)}",
      s"after-turns whose first read followed a card their window held: ${rate(c.followed)}",
      ""
    ) ++ answers ++ Vector(
      "## Pairs",
      ""
    ) ++ table("question", "before", "after")(
      c.pairs.map(p => Vector(p.question, reads(p.before), reads(p.after)))
    )).mkString("\n") + "\n"
  }

  private def rate(r: Proportion): String = RecipeLines.rate(r, "question", "—")
}
