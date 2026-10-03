package grit.eval.harness.report

import grit.core.id.{ConversationId, WorkflowId}
import grit.eval.harness.corpus.Part
import grit.eval.harness.reply.Judged
import grit.eval.harness.score.{Recipe, Shift}
import grit.eval.harness.stats.Proportion

/** One variant of a recipes run: its name, its [[Recipe]] against shipped, and each reference
  * turn judged under it, with the turn's conversation, in the reference's order.
  */
final case class Varied(
    name: String,
    recipe: Recipe,
    judged: Vector[(WorkflowId, ConversationId, Judged)]
)

/** [[Report.recipes]]'s sections. */
private[report] object RecipeLines {

  def of(
      corpus: String,
      notes: Vector[String],
      shipped: Varied,
      variants: Vector[Varied]
  ): String = {
    def table(head: String*)(rows: Vector[Vector[String]]): Vector[String] =
      Vector(head.mkString("| ", " | ", " |"), head.map(_ => "---").mkString("|", "|", "|")) ++
        rows.map(_.mkString("| ", " | ", " |"))
    val header = Vector(s"# recipes: $corpus", "") ++ notes ++ Vector(
      "",
      "Each variant against shipped, paired on the same turns, shipped's window rebuilt as of " +
        "the turn's assembly like the variant's. Text-free: ids, counts and tokens. A rate's " +
        "interval is Wilson's at 95% on its effective number, clustered by conversation (a " +
        s"thread), and none under ${Proportion.MinClusters} threads; a per-turn figure is a mean.",
      ""
    )

    val tools = Vector(
      "## Tools",
      "",
      "Called-tool recall: of the tools each turn called (each once; the turn's topic tool " +
        "and unnamed calls apart), those the variant still offers. Withholding a needed tool " +
        "is the costly error, so this is the main measure.",
      ""
    ) ++ table(
      "variant",
      "turns",
      "tools per turn",
      "schema tokens per turn",
      "saved in all",
      "called-tool recall"
    )(variants.map { v =>
      val r = v.recipe
      Vector(
        v.name,
        s"${r.n}",
        shift(r.tools),
        shift(r.schema),
        s"${r.saved}",
        rate(r.called)
      )
    }) ++ Vector("")

    val kinds = Part.Kind.values.toVector.filter(k => variants.exists(_.recipe.parts.contains(k)))
    val window = Vector("## Window", "", "Tokens per turn, shipped → variant.", "") ++ table(
      (Vector("variant", "windows", "whole") ++ kinds.map(Part.Kind.written))*
    )(variants.map { v =>
      Vector(v.name, s"${v.recipe.windows}", shift(v.recipe.window)) ++
        kinds.map(k => shift(v.recipe.parts.get(k)))
    }) ++ Vector("")

    val used = Vector(
      "## Used-section recall",
      "",
      "Of the window parts a recorded reply used (support at or over 0.3, a lexical " +
        "heuristic), those each window still holds; shipped's rebuilt window differs from the " +
        "recorded one by drift alone.",
      ""
    ) ++ (if (variants.forall(_.recipe.used.n == 0))
            Vector(
              "Undefined: no reply used a part of its window, so no variant can lose one.",
              ""
            )
          else
            table("variant", "shipped's", "variant's")(
              variants.map(v => Vector(v.name, rate(v.recipe.usedShipped), rate(v.recipe.used)))
            ) ++ Vector(""))

    val changed = Vector("## Turns changed", "") ++ variants.flatMap { v =>
      if (v.recipe.changed.isEmpty) Vector(s"- ${v.name}: none")
      else Vector(s"- ${v.name}: " + v.recipe.changed.map(WorkflowId.value).mkString(", "))
    } ++ Vector("")

    val reference = Vector(
      "## Reference",
      "",
      "Each reference turn passes when every expectation of it holds; unjudged when none " +
        "fails and one expects a window that was not rebuilt.",
      ""
    ) ++ (if (shipped.judged.isEmpty) Vector("No reference turn.", "")
          else
            table("variant", "rate", "unjudged", "failed")((shipped +: variants).map { v =>
              val judged = v.judged.collect {
                case (_, c, Judged.Pass) => c -> true
                case (_, c, Judged.Fail) => c -> false
              }
              Vector(
                v.name,
                rate(Proportion.of(judged)),
                s"${v.judged.count(_._3 == Judged.Unjudged)}",
                v.judged.collect { case (w, _, Judged.Fail) => WorkflowId.value(w) } match {
                  case Vector() => "none"
                  case ids => ids.mkString(", ")
                }
              )
            }) ++ Vector(""))

    (header ++ tools ++ window ++ used ++ changed ++ reference).mkString("\n")
  }

  private def shift(s: Option[Shift]): String =
    s.fold("—")(x => s"${count(x.shipped)} → ${count(x.variant)}")

  private def count(x: Double): String =
    if (x.isWhole) f"$x%.0f" else f"$x%.1f"

  private def rate(r: Proportion): String =
    r.rate.fold("— (none)") { rate =>
      val bounds = r.interval match {
        case Proportion.Interval.TooFewClusters => "too few threads for an interval"
        case Proportion.Interval.Wilson(low, high, _) => f"[$low%.3f, $high%.3f]"
      }
      val threads = if (r.clusters == 1) "1 thread" else s"${r.clusters} threads"
      s"${f"$rate%.3f"} (${r.hits}/${r.n} in $threads) $bounds"
    }
}
