package grit.eval.harness.report

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, WorkflowId}
import grit.eval.harness.corpus.Part
import grit.eval.harness.reply.Judged
import grit.eval.harness.score.{Priced, Rate, Recipe, Shift}
import grit.eval.harness.stats.{Mills, Proportion}

/** One variant of a recipes run: its name, its [[Recipe]] against shipped, and each reference
  * turn judged under it, with the turn's conversation, in the reference's order.
  */
final case class Varied(
    name: String,
    recipe: Recipe,
    judged: Vector[(WorkflowId, ConversationId, Judged)]
)

/** A turn of the synthetic reference judged: its name (`{case}/{variant}`), the case it is a
  * variant of, and how it stood.
  */
final case class CaseJudged(name: String, of: String, judged: Judged)

/** [[Report.recipes]]'s sections. */
private[report] object RecipeLines {

  def of(
      corpus: String,
      notes: Vector[String],
      rates: VectorMap[String, Either[String, Rate]],
      shipped: Varied,
      variants: Vector[Varied]
  ): String = {
    val header = Vector(s"# recipes: $corpus", "") ++ notes ++ Vector(
      "",
      "Each variant against shipped, paired on the same turns, shipped's window rebuilt as of " +
        "the turn's assembly like the variant's. Text-free: ids, counts, tokens and prices (in " +
        "mills, a mill $0.001). A rate's " +
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
      "saved in all, once a turn",
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

    val price = Vector(
      "## Price of the tools withheld",
      "",
      "A turn's tool definitions are sent on every main-model call (each round of its tool " +
        "loop, and its reply), so the tokens a variant withholds count once a call, priced at " +
        "the rates below of the model that made it. Effective: each call's tokens withheld " +
        "cached as its own input was, that share at the cached rate and the rest at the input " +
        "rate, as though the variant's calls hit the cache as the recorded ones did. The range " +
        "runs from every token cached (the least they are worth) to none (the most). Cut now: how much the hit rate shown takes off the uncached price; at most: " +
        "how much it would at a hit rate of 1. The tokens are the definitions' as capture costs " +
        "them, grit's estimate rather than the provider's count. Unpriced calls withheld tokens " +
        "and are left out of every price.",
      ""
    ) ++ table(
      "variant",
      "calls",
      "hit rate",
      "tokens saved",
      "effective",
      "range",
      "cut now",
      "at most",
      "unpriced"
    )(variants.map { v =>
      val p = v.recipe.priced
      Vector(
        v.name,
        s"${p.calls}",
        rate(p.hit),
        s"${p.tokens}",
        mills(p.effective),
        s"${p.cached.fold("—")(mills)} to ${mills(p.uncached)}",
        cut(p, p.effective),
        p.cached.fold("—")(cut(p, _)),
        s"${p.unpriced}"
      )
    }) ++ Vector(
      "",
      "### Rates",
      "",
      "grit records no price, so each model's is read from the corpus's ledger rows: least " +
        "squares over its priced rows of cost = input × (input − cached tokens) + cached × " +
        "cached tokens + output × output tokens, the cached rate left out where no row has a " +
        "cached token. Off by: the most the rates misprice one of those rows. In mills per " +
        "1000 tokens, the same number as dollars per million.",
      ""
    ) ++ (if (rates.isEmpty) Vector("No ledger row.")
          else
            table("model", "input", "cached", "output", "rows", "cached rows", "off by")(
              rates.toVector.map {
                case (m, Right(r)) =>
                  Vector(
                    m,
                    perThousand(r.input),
                    r.cached.fold("—")(perThousand),
                    perThousand(r.output),
                    s"${r.rows}",
                    s"${r.cachedRows}",
                    f"${r.worst * 100}%.1f%%"
                  )
                case (m, Left(why)) => Vector(m, s"none: $why", "—", "—", "—", "—", "—")
              }
            )) ++ Vector("")

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
            judgedTable(
              (shipped +: variants)
                .map(v => v.name -> v.judged.map((w, c, j) => (WorkflowId.value(w), c, j))),
              "thread"
            ))

    (header ++ tools ++ price ++ window ++ used ++ changed ++ reference).mkString("\n")
  }

  def synthetic(notes: Vector[String], judged: Vector[(String, Vector[CaseJudged])]): String =
    (Vector("# recipes: synthetic", "") ++ notes ++ Vector(
      "",
      "Each case of grit.eval, as written and buried in filler, is a turn said to grit, which " +
        "passes when its window holds every [must] entry and no [never] entry; unjudged when its " +
        "window was not " +
        "drawn. A rate's interval is Wilson's at 95% on its effective number, clustered by " +
        s"case (its plain and buried turns one cluster), and none under " +
        s"${Proportion.MinClusters} cases.",
      ""
    ) ++ judgedTable(
      judged.map((v: String, cs: Vector[CaseJudged]) => v -> cs.map(c => (c.name, c.of, c.judged))),
      "case"
    )).mkString("\n")

  /** Each variant's judged turns, each by name with its cluster's key, as a table of its pass
    * rate (clustered by key, each key a `cluster`), its unjudged count and its failing turns by
    * name.
    */
  private def judgedTable[K](
      judged: Vector[(String, Vector[(String, K, Judged)])],
      cluster: String
  ): Vector[String] =
    table("variant", "rate", "unjudged", "failed")(judged.map {
      (name: String, js: Vector[(String, K, Judged)]) =>
        val held = js.collect {
          case (_, c, Judged.Pass) => c -> true
          case (_, c, Judged.Fail) => c -> false
        }
        Vector(
          name,
          rate(Proportion.of(held), cluster),
          s"${js.count(_._3 == Judged.Unjudged)}",
          js.collect { case (n, _, Judged.Fail) => n } match {
            case Vector() => "none"
            case names => names.mkString(", ")
          }
        )
    }) ++ Vector("")

  private def table(head: String*)(rows: Vector[Vector[String]]): Vector[String] =
    Vector(head.mkString("| ", " | ", " |"), head.map(_ => "---").mkString("|", "|", "|")) ++
      rows.map(_.mkString("| ", " | ", " |"))

  private def mills(usd: Double): String = Mills.of(BigDecimal(usd))

  /** A per-token rate in USD as mills per 1000 tokens, four significant digits. */
  private def perThousand(usd: Double): String = f"${usd * 1e6}%.4g"

  /** How much `price` is below the uncached price, as a share of it. */
  private def cut(p: Priced, price: Double): String =
    if (p.uncached == 0) "—" else f"${(1 - price / p.uncached) * 100}%.0f%%"

  private def shift(s: Option[Shift]): String =
    s.fold("—")(x => s"${count(x.shipped)} → ${count(x.variant)}")

  private def count(x: Double): String =
    if (x.isWhole) f"$x%.0f" else f"$x%.1f"

  private def rate(r: Proportion, cluster: String = "thread"): String =
    r.rate.fold("— (none)") { rate =>
      val bounds = r.interval match {
        case Proportion.Interval.TooFewClusters => s"too few ${cluster}s for an interval"
        case Proportion.Interval.Wilson(low, high, _) => f"[$low%.3f, $high%.3f]"
      }
      val clusters = if (r.clusters == 1) s"1 $cluster" else s"${r.clusters} ${cluster}s"
      s"${f"$rate%.3f"} (${r.hits}/${r.n} in $clusters) $bounds"
    }
}
