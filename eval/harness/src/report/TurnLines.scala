package grit.eval.harness.report

import grit.core.id.WorkflowId
import grit.core.message.Tokens
import grit.core.tool.ToolName
import grit.eval.harness.corpus.{Called, Drafted, Ended, Part, Support, TurnCase}
import grit.eval.harness.label.Verdicts
import grit.eval.harness.score.{Calls, Paid, Quantiles, Slice, Structure, Unreplied, Where}
import grit.eval.harness.stats.{Mills, Proportion}
import grit.turn.TurnRecord

/** [[Report.turns]]'s sections. */
private[report] object TurnLines {

  def of(corpus: String, turns: Vector[TurnCase], verdicts: Verdicts): String = {
    val slices = Slice.every.map(s => s -> Structure.of(turns, s, verdicts)).filter(_._2.n > 0)
    def table(head: String*)(rows: Vector[Vector[String]]): Vector[String] =
      Vector(head.mkString("| ", " | ", " |"), head.map(_ => "---").mkString("|", "|", "|")) ++
        rows.map(_.mkString("| ", " | ", " |"))
    def bySlice(head: String*)(row: Structure => Vector[String]): Vector[String] =
      table(("slice" +: head)*)(slices.map((s, st) => name(s) +: row(st)))
    val all = Structure.of(turns, Slice.All, verdicts)

    val header = Vector(
      s"# turns: $corpus",
      "",
      s"turns: ${turns.size}; " + slices
        .drop(1)
        .map((s, st) => s"${name(s)} ${st.n}")
        .mkString(", "),
      s"verdicts standing: ${verdicts.cases.size}, of which no turn answers " +
        Structure.unjoined(turns, verdicts),
      "",
      "Text-free: ids, counts, tokens and prices. A price is in mills (a mill is $0.001), and an " +
        "exact amount in dollars beside it. A rate's interval is Wilson's at 95% on its " +
        "turns' effective number, clustered by conversation (a thread), and none under " +
        s"${Proportion.MinClusters} threads; a spread is mean / p50 / p90 by nearest rank.",
      ""
    )

    val passing = Vector(
      "## Pass rate",
      "",
      "Of the turns that replied, those whose reply said nothing.",
      ""
    ) ++
      bySlice("replied", "passed", "rate")(st =>
        Vector(s"${st.passed.n}", s"${st.passed.hits}", rate(st.passed))
      ) ++
      Vector("")

    val failing = Vector("## Not replied", "") ++
      bySlice("n", "failed or unfinished")(st =>
        Vector(
          s"${st.n}",
          if (st.unreplied.isEmpty) "none"
          else
            st.unreplied.toVector
              .map((u, k) =>
                (u match {
                  case Unreplied.Failed(step, why) =>
                    s"failed at $step (${why.toString.toLowerCase})"
                  case Unreplied.Unfinished(status) => s"unfinished ($status)"
                }) + s": $k"
              )
              .mkString("; ")
        )
      ) ++ Vector("")

    val looping = Vector("## Rounds", "") ++
      bySlice("with a loop", "rounds")(st => Vector(s"${st.loops}", spread(st.rounds, count))) ++
      Vector("")

    val tooling = Vector(
      "## Tools",
      "",
      "Offered: each offer's tool count and its definitions' tokens. Called: each tool's calls, the " +
        "turns that called it, and how they settled (ok, failed, expired, abandoned, unsettled); " +
        "`topic` is the turn's own topic tool, offered beside the set when a message's topic was " +
        "unsure; `unnamed`, a name neither offered nor the topic tool's.",
      ""
    ) ++ bySlice("offers", "tools", "schema tokens")(st =>
      Vector(s"${st.offered.fold(0)(_.n)}", spread(st.offered, count), spread(st.schema, count))
    ) ++ Vector("") ++ slices.flatMap { (s, st) =>
      val rows = st.tools.toVector.map((n, u) => (ToolName.value(n), s"${u.offeredIn}", u.calls)) ++
        Vector(("topic", "—", st.topic), ("unnamed", "—", st.unnamed))
      Vector(s"### Tools: ${name(s)}", "") ++
        table("tool", "offered in", "calls", "turns", "settled")(
          rows.map((n, o, c) => Vector(n, o, s"${c.times}", s"${c.turns}", settled(c)))
        ) ++ Vector("")
    }

    val tokens = Vector(
      "## Tokens",
      "",
      "Prompt: each system prompt layer's tokens, over the turns with an offer. Window: each part " +
        "kind's tokens, over the turns with a window, as `Shown` shows it today.",
      ""
    ) ++ slices.flatMap { (s, st) =>
      val prompt =
        st.prompt.toVector.map((l, q) => Vector(s"prompt ${l.key}", spread(Some(q), count)))
      val window = st.window.toVector.flatMap(w =>
        w.parts.toVector.map((k, q) =>
          Vector(s"window ${Part.Kind.written(k)}", spread(Some(q), count))
        ) ++
          Vector(
            Vector("window gaps", spread(Some(w.gaps), count)),
            Vector("window own", spread(Some(w.own), count)),
            Vector("window total", spread(Some(w.total), count))
          )
      )
      Vector(s"### Tokens: ${name(s)}", "") ++
        (if (prompt.isEmpty && window.isEmpty) Vector("no offer or window recorded")
         else table("part", "tokens")(prompt ++ window)) ++ Vector("")
    }

    val estimating = Vector(
      "## Estimate against the ledger",
      "",
      "The first call to the main model (a loop's first round, else the reply): grit's estimate of " +
        "its input, the input the ledger recorded, and their ratio over inputs above 0.",
      ""
    ) ++ bySlice("calls", "estimated", "recorded", "estimated / recorded")(st =>
      st.estimate.fold(Vector("0", "—", "—", "—"))(e =>
        Vector(
          s"${e.estimated.n}",
          spread(Some(e.estimated), count),
          spread(Some(e.actual), count),
          spread(e.ratio, x => f"$x%.2f")
        )
      )
    ) ++ Vector("")

    val paid = Paid.values.toVector.filter(p => all.cost.contains(p))
    val costing = Vector(
      "## Cost per turn",
      "",
      "By what each call paid for, 0 in a turn without it; rounds are a loop's calls together. " +
        "A ledger row with no cost adds nothing and is counted as unpriced. In mills.",
      ""
    ) ++ bySlice((paid.map(paidName) ++ Vector("total", "unpriced"))*)(st =>
      paid.map(p => spread(st.cost.get(p), mills)) ++
        Vector(spread(st.total, mills), s"${st.unpriced}")
    ) ++ Vector("")

    val kinds = Drafted.Kind.values.toVector.filter(k => all.online.exists(_.outcomes.contains(k)))
    val speaking = Vector(
      "## Speech",
      "",
      "What became of heard roots' drafts. Posted: the rate posted; held: neither passed nor " +
        "posted, its reasons the outcomes besides passed and posted.",
      ""
    ) ++ (if (all.online.isEmpty) Vector("no heard root recorded an outcome")
          else
            bySlice(
              (Vector("drafts") ++ kinds
                .map(_.toString.toLowerCase) ++ Vector("posted", "held", "grounded", "worth"))*
            )(st =>
              st.online.fold(Vector.fill(kinds.size + 5)("—"))(o =>
                Vector(s"${o.n}") ++ kinds.map(k => s"${o.outcomes.getOrElse(k, 0)}") ++
                  Vector(
                    rate(o.posted),
                    rate(o.held),
                    spread(o.grounded, prob),
                    spread(o.worth, prob)
                  )
              )
            )) ++ Vector("")

    val judged = all.verdicts.keys.toVector
    val reviewing = Vector(
      "## Verdicts",
      "",
      "The review verdicts standing on a turn's message, by why it was picked.",
      ""
    ) ++
      (if (judged.isEmpty) Vector("no verdict stands on a captured turn's message")
       else
         bySlice(judged.map((r, v) => s"${Verdicts.reason(r)} ${v.toString.toLowerCase}")*)(st =>
           judged.map(k => s"${st.verdicts.getOrElse(k, 0)}")
         )) ++ Vector("")

    val using = Vector(
      "## Used parts",
      "",
      f"A part is used at support ${Support.Used.value}%.1f or over (`Support.Used`): the share of " +
        "a reply's distinctive words, less the asked message's, that the part shows. A lexical " +
        "heuristic until labels validate it. Over the turns whose reply said something and that " +
        "recorded a window; each turn's top parts by support are in its line below.",
      ""
    ) ++ bySlice("turns", "parts", "used", "turns using one", "top support")(st =>
      st.used.fold(Vector("0", "—", "—", "—", "undefined: no reply said anything"))(u =>
        Vector(
          s"${u.turns}",
          s"${u.parts}",
          s"${u.used}",
          s"${u.using}",
          spread(Some(u.top), x => f"$x%.2f")
        )
      )
    ) ++ Vector(
      "",
      "Used-section recall is undefined in a slice where no reply said anything: a passed " +
        "reply carries no words for a part to support.",
      ""
    )

    val cases = Vector(
      "## Turns by cost",
      "",
      "One line a turn, most costly first: where said, its root and focus, how it ended, its " +
        "rounds and calls, its window's tokens, its first main call's estimate and recorded " +
        "input, its cost, and its top 3 parts by support (kind and support).",
      ""
    ) ++ table(
      "workflow",
      "said",
      "root",
      "focus",
      "ended",
      "rounds",
      "calls",
      "window",
      "first call",
      "cost",
      "top 3"
    )(
      Structure.byCost(turns).map((t, c) => line(t, c))
    )

    (header ++ passing ++ failing ++ looping ++ tooling ++ tokens ++ estimating ++ costing ++
      speaking ++ reviewing ++ using ++ cases).mkString("\n") + "\n"
  }

  private def line(t: TurnCase, c: BigDecimal): Vector[String] = {
    val calls = t.rounds.flatMap(_.calls).map(_.tool)
    val named = calls.distinct.map(k => s"${called(k)} ${calls.count(_ == k)}")
    val first = t.spend.find(s =>
      s.role match {
        case Some(TurnRecord.Role.Round(_)) | Some(TurnRecord.Role.Reply) => true
        case _ => false
      }
    )
    Vector(
      WorkflowId.value(t.workflow),
      Where.of(t.said).toString.toLowerCase,
      t.root.toString.toLowerCase,
      t.focus.toString.toLowerCase,
      t.ended match {
        case Ended.Replied(_, true) => "passed"
        case Ended.Replied(length, false) => s"replied $length"
        case Ended.Failed(step, why) => s"failed at $step (${why.toString.toLowerCase})"
        case Ended.Unfinished(status) => s"unfinished ($status)"
      },
      s"${t.rounds.size}",
      if (named.isEmpty) "—" else named.mkString(", "),
      t.window.fold("—")(w => s"${Tokens.value(w.parts.foldLeft(w.gaps + w.own)(_ + _.tokens))}"),
      first.fold("—")(s => s"${Tokens.value(s.estimated)} / ${Tokens.value(s.usage.input)}"),
      Mills.withUsd(c),
      t.window
        .map(w => Support.top(w.parts, 3))
        .filter(_.nonEmpty)
        .fold("—")(_.map((p, s) => f"${Part.Kind.written(p.kind)} ${s.value}%.2f").mkString(", "))
    )
  }

  private def name(s: Slice): String = s match {
    case Slice.All => "all"
    case Slice.Rooted(r) => r.toString.toLowerCase
    case Slice.At(w) => w.toString.toLowerCase
  }

  private def called(c: Called): String = c match {
    case Called.Tool(n) => ToolName.value(n)
    case Called.Topic => "topic"
    case Called.Unnamed => "unnamed"
  }

  private def paidName(p: Paid): String = p.toString.toLowerCase

  private def settled(c: Calls): String = {
    val s = c.settled
    s"${s.ok}, ${s.failed}, ${s.expired}, ${s.abandoned}, ${s.unsettled}"
  }

  private def rate(r: Proportion): String =
    r.rate.fold("—") { rate =>
      val bounds = r.interval match {
        case Proportion.Interval.TooFewClusters => "too few threads for an interval"
        case Proportion.Interval.Wilson(low, high, _) => f"[$low%.3f, $high%.3f]"
      }
      val threads = if (r.clusters == 1) "1 thread" else s"${r.clusters} threads"
      s"${f"$rate%.3f"} (${r.hits}/${r.n} in $threads) $bounds"
    }

  private def spread(q: Option[Quantiles], f: Double => String): String =
    q.fold("—")(x => s"${f(x.mean)} / ${f(x.p50)} / ${f(x.p90)}")

  private def count(x: Double): String =
    if (x.isWhole) f"$x%.0f" else f"$x%.1f"

  private def mills(x: Double): String = Mills.of(BigDecimal(x))

  private def prob(x: Double): String = f"$x%.2f"
}
