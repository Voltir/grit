package grit.eval.harness.report

import grit.core.message.Cost
import grit.core.period.Probability
import grit.core.stitch.Tuning
import grit.eval.Room
import grit.eval.harness.score.{Against, Ranged, RoomPlacing, RoomRepeats, RoomScore}
import grit.eval.harness.stats.Proportion

/** [[Report.room]]'s sections. */
private[report] object RoomLines {

  /** How many characters of an opening's text its row shows. */
  private val TextChars = 72

  def lab(room: Room, model: String, tuning: Tuning, runs: Vector[LabRun]): String = {
    def ranged(r: Ranged): String =
      if (r.min == r.max) f"${r.mean}%.0f" else f"${r.mean}%.1f [${r.min}%.0f, ${r.max}%.0f]"
    val head = Vector(
      s"# stitch lab: ${room.name}",
      "",
      s"model $model; horizon ${tuning.horizon}, recent ${tuning.recent}, lexical " +
        s"${tuning.lexical}, followsAt ${Probability.value(tuning.followsAt)}",
      s"runs: ${runs.size}; calls: ${runs.map(_.calls).sum} (cached ${runs.map(_.cached).sum}); " +
        s"spent ${Cost.cents(runs.map(_.spent).sum)}"
    ) ++ Against.values.toVector.flatMap { a =>
      val these = runs.filter(_.against == a)
      if (these.isEmpty) Vector.empty
      else
        Vector(
          "",
          s"## ${a.label}, over ${these.size} runs (mean [least, most])",
          "",
          "| measure | count | of |",
          "|---|---|---|"
        ) ++ RoomRepeats
          .of(these.map(_.score), a)
          .map(m => s"| ${m.name} | ${ranged(m.count)} | ${m.of.fold("")(ranged)} |")
    }
    (head ++ runs.flatMap(r => "" +: of(room, r))).mkString("", "\n", "\n")
  }

  /** One run's section, under its label. */
  private def of(room: Room, run: LabRun): Vector[String] = {
    val placings = run.placings
    val score = run.score
    val truths = Room.truth(room).map(t => t.opening.id -> t).toMap
    val topic = room.openings.map(m => m.id -> m.topic).toMap
    def topicOf(id: String): String = topic.get(id).flatten.getOrElse("-")
    def rate(p: Proportion): String = {
      val bounds = p.interval match {
        case Proportion.Interval.TooFewClusters => s"point estimate, ${p.clusters} topics"
        case Proportion.Interval.Wilson(low, high, _) => f"[$low%.2f, $high%.2f]"
      }
      s"${p.hits}/${p.n} " + p.rate.fold("")(r => f"$r%.2f ") + s"($bounds)"
    }
    def made(m: RoomPlacing.Made): String = m match {
      case RoomPlacing.Made.Unoffered => "offered nothing"
      case RoomPlacing.Made.Begins(p) => f"begins (best $p%.2f)"
      case RoomPlacing.Made.Follows(root, p) => f"follows $root ($p%.2f)"
      case RoomPlacing.Made.Unread(why) => s"unread: $why"
      case RoomPlacing.Made.Skipped => "skipped"
    }
    def offered(o: RoomPlacing.Offer): String =
      s"${o.root}(${topicOf(o.root)}, ${if (o.recent) "recent" else "lexical"}" +
        o.p.fold(")")(p => f" $p%.2f)")
    val b = score.bursts
    Vector(
      s"## ${run.label}",
      "",
      s"calls: ${run.calls} (cached ${run.cached}); spent ${Cost.cents(run.spent)}",
      s"openings: ${score.openings}, skipped at the cap ${score.skipped}",
      "",
      s"- right: ${rate(score.right)}",
      s"- offer recall (an exchange of its topic offered, of those that should follow): ${rate(score.recall)}",
      s"- strict recall (its topic's first opening offered): ${rate(score.strict)}",
      s"- Jev given offered (followed its topic): ${rate(score.chosen)}",
      s"- wrong follows: ${score.wrongFollows.size}" +
        (if (score.wrongFollows.isEmpty) "" else score.wrongFollows.mkString(" (", ", ", ")")),
      "",
      "### confusion",
      "",
      "| truth | got | n |",
      "|---|---|---|"
    ) ++ score.confusion.map { case ((follows, got), n) =>
      s"| ${if (follows) "follows" else "begins"} | ${got.label} | $n |"
    } ++ Vector(
      "",
      s"### bursts (same author, at most ${Room.BurstGap.toSeconds} s)",
      "",
      s"${b.size} bursts: the rule (follow the previous) right ${b.count(_.continues)}, Jev right " +
        s"${b.count(_.right)}, Jev agrees with the rule ${b.count(_.agrees)}",
      "",
      "| opening | after | gap s | continues | Jev right | agrees |",
      "|---|---|---|---|---|---|"
    ) ++ b.map(x =>
      s"| ${x.id} | ${x.previous} | ${x.gapSeconds} | ${x.continues} | ${x.right} | ${x.agrees} |"
    ) ++ (run.against match {
      case Against.Truth => Vector.empty
      case Against.Placings =>
        Vector(
          "",
          "### strands",
          "",
          "split topics: " +
            (if (score.split.isEmpty) "none"
             else score.split.map((t, n) => s"$t in $n").mkString(", ")),
          "mixed strands: " +
            (if (score.mixed.isEmpty) "none"
             else score.mixed.map(m => s"${m._1} (${m._2.mkString(", ")})").mkString("; "))
        )
    }) ++ Vector(
      "",
      "### openings (the room's own synthetic text)",
      "",
      "| ok | opening | who | topic | should | got | offered | text |",
      "|---|---|---|---|---|---|---|---|"
    ) ++ placings.flatMap(p =>
      truths.get(p.id).map { t =>
        val ok = score.got.find(_._1 == p.id).fold("-") {
          case (_, RoomScore.Got.Skipped, _) => "-"
          case (_, _, right) => if (right) "ok" else "XX"
        }
        val should = t.follows.fold("begin")(r => s"follow $r")
        val text = t.opening.text.take(TextChars).replace("|", "/")
        s"| $ok | ${p.id} | ${t.opening.who} | ${topicOf(p.id)} | $should | ${made(p.made)} | " +
          s"${p.offered.map(offered).mkString(", ")} | $text |"
      }
    )
  }
}
