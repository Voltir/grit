package grit.eval.harness.report

import grit.core.id.{DocumentVersion, WorkflowId}
import grit.eval.harness.reply.Refound
import grit.eval.harness.stats.Proportion

/** [[Report.recall]]'s sections. */
private[report] object RecallLines {
  import Refound.{Found, Rank}

  def of(
      capture: String,
      replayed: Vector[(WorkflowId, Either[String, Vector[Refound.Followed]])]
  ): String = {
    val found = replayed.collect { case (w, Right(fs)) => w -> fs }
    val failed = replayed.collect { case (w, Left(why)) => w -> why }
    val s = Refound.summary(found)
    def table(head: String*)(rows: Vector[Vector[String]]): Vector[String] =
      Vector(head.mkString("| ", " | ", " |"), head.map(_ => "---").mkString("|", "|", "|")) ++
        rows.map(_.mkString("| ", " | ", " |"))
    def row(what: String, a: Refound.At): Vector[String] =
      what +: (a.cuts.values.toVector :+ a.ranked :+ a.recorded).map(rate)
    def followed(f: Refound.Followed): String = {
      val v = DocumentVersion.value(f.version)
      f.found match {
        case Found.Forgotten => s"$v forgotten"
        case Found.Kept(source, recorded, now) =>
          val was = recorded.fold("unranked")(p => s"at $p")
          val is = now match {
            case Rank.At(p) => s"at $p"
            case Rank.Missed => "missed"
            case Rank.Gone => "gone"
          }
          s"$v ${source.written} $was → $is"
      }
    }
    (Vector(
      s"# recall: $capture",
      "",
      s"turns replayed: ${s.turns}; not replayed: ${failed.size}",
      "",
      "Text-free: workflow ids, document versions and counts. Each turn that recorded its search " +
        "and followed a document had its recorded query searched again by the shipped assembler " +
        "over the database as it stood at the capture's dump, every row read whatever the turn " +
        "was cleared for (a restored database records no deployment's groups), so a document " +
        "hidden from the turn can rank above one it followed; each document it followed is " +
        "ranked by its current version (matched by plugin and key) among all the search ranked, " +
        "1 the best. \"Own search\" is the share the turn's own search ranked when it ran; the " +
        "rest reached its window otherwise, such as placed at its room. A turn line gives each " +
        "version's source, its rank then and its rank now. Matching by plugin and key cannot tell a card crowded out from one a later " +
        "document absorbed: a miss is not by itself a regression. Gone (no current version) " +
        "and forgotten (retention no longer keeps the version followed) are counted apart, in " +
        "no share. A share's interval is Wilson's, 95%, clustered by turn; under " +
        s"${Proportion.MinClusters} turns it is a point estimate.",
      "",
      "## Ranked at or above each cut",
      ""
    ) ++ table(("source" +: Refound.Cuts.map(c => s"≤ $c")) :+ "ranked" :+ "own search"*)(
      Vector(row("all", s.all)) ++ s.bySource.toVector.map((source, a) => row(source.written, a))
    ) ++ Vector(
      "",
      s"gone: ${s.gone}; forgotten: ${s.forgotten}",
      "",
      "## Turns",
      ""
    ) ++ table("turn", "followed")(
      found.map((w, fs) => Vector(WorkflowId.value(w), fs.map(followed).mkString(", ")))
    ) ++ (if (failed.isEmpty) Vector.empty
          else
            Vector("", "## Not replayed", "") ++
              failed.map((w, why) => s"- ${WorkflowId.value(w)}: $why"))).mkString("\n") + "\n"
  }

  private def rate(r: Proportion): String =
    r.rate.fold("—") { rate =>
      val bounds = r.interval match {
        case Proportion.Interval.TooFewClusters => "point estimate"
        case Proportion.Interval.Wilson(low, high, _) => f"[$low%.3f, $high%.3f]"
      }
      s"${f"$rate%.3f"} (${r.hits}/${r.n} in ${r.clusters} turns) $bounds"
    }
}
