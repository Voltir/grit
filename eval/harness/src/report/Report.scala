package grit.eval.harness.report

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{QuestionName, ShadowName}
import grit.core.period.Probability
import grit.core.store.Focus
import grit.core.triage.{Bound, Earning, Gate, Reading}
import grit.eval.harness.corpus.{Case, CaseId, TurnCase}
import grit.eval.harness.label.{Context, Labelled, Labels, Verdicts}
import grit.eval.harness.log.{Log, Row, Suite, Weights}
import grit.eval.harness.pull.Pull
import grit.eval.harness.score.{
  Agreement,
  Answers,
  Apart,
  Brier,
  Cells,
  Changed,
  Changes,
  Clustered,
  Comparison,
  Cost,
  Decision,
  Drafts,
  Judgement,
  Measure,
  Moved,
  MovedOn,
  PerCall,
  Proportions,
  Rate,
  Refusal,
  Reliability,
  Repeated,
  Rule,
  Scoring,
  Size,
  Spending,
  Split,
  Sweep,
  Tag,
  Target
}
import grit.eval.harness.stats.{Estimate, Mills, Proportion}

/** A run to report on: its log's file `name`, the log, its corpus's cases, and the labels in
  * force.
  */
final case class Scored(
    name: String,
    log: Log[Vector[Weights]],
    cases: Vector[Case],
    labels: Labels
) {
  val answers: Answers = Answers.of(log.rows)

  /** How many of the corpus's cases carry any label. */
  def labelled: Int = cases.count(c => labels.of(c.id) != Labelled.Blank)

  /** This run with no row of any of `ids`. */
  def without(ids: Set[CaseId]): Scored =
    copy(log = log.copy(rows = log.rows.filterNot((r: Row[Vector[Weights]]) => ids.contains(r.id))))
}

/** Reports as markdown, text-free: ids, numbers and labels, never a message's words. Every mean
  * and every proportion is printed with its 95% interval clustered by exchange and, beside it,
  * by author, as [[Intervals]] says; one without an interval prints `—` or `[—]`.
  */
object Report {

  /** How a report's intervals are drawn, printed under its title. */
  val Intervals: String = "A mean's interval is 95%, Student's t clustered (CR1), `—` under " +
    "2 clusters; a proportion's is Wilson's at 95% on its effective n, `[—]` under " +
    s"${Proportion.MinClusters} clusters. g is the clusters."

  /** What a report says of a log of live triage's kept tags by position ([[Pull.Kept]]), as
    * an earlier build's pull wrote it: a pull now writes them under their names.
    */
  val KeptNote: String = "kept: live triage keeps only the likeliest kind's probability; " +
    "the other kinds share the rest evenly, so its kind compares the likeliest kind and its " +
    "probability only"

  /** `run` scored. With no case labelled it says `unlabelled: 0 of N cases labelled` and gives
    * only what needs no label: spend and latency, its repeats' spread, and its agreement with
    * what was kept live. A log of live triage's kept tags carries [[KeptNote]].
    */
  def score(run: Scored): String = {
    val h = run.log.header
    val lines = Vector(
      s"# score: ${run.name}",
      "",
      s"run: variant ${h.variant}, model ${h.model}, repeats ${h.repeats}, cache " +
        s"${if (h.cache) "on" else "off"}, corpus ${h.corpus}, started ${h.started}",
      s"cases: ${run.cases.size}; answered: triage ${run.answers.triage.size}, stitch " +
        s"${run.answers.stitch.size}",
      labelLine(run),
      Intervals
    ) ++ kept(run) ++ Vector("") ++ spending(run) ++ repeats(run) ++ live(run) ++
      (if (run.labelled == 0) Vector.empty else labelled(run))
    lines.mkString("\n") + "\n"
  }

  /** Run `b` against run `a`, paired on the cases both answered: B − A per question, the cases
    * whose decision changed by id, once labels exist the paired difference in Brier score, all
    * and by context, with its MDE, the cases whose triage inputs changed between them
    * ([[Changed]]: counted by context and focus, B − A over them alone, each run's call size,
    * and the MDE A's repeat noise implies on them), and what `decided` says a rule made of
    * them, when given;
    * [[KeptNote]] when either is a log of live triage's kept tags. With `movedOn`, the cases
    * it found are left out of both runs, and listed with the tolerance they were found under.
    */
  def compare(
      runA: Scored,
      runB: Scored,
      decided: Option[(Rule, Decision)] = None,
      movedOn: Option[Moved] = None
  ): String = {
    val gone = movedOn.fold(Set.empty[CaseId])(_.cases.toSet)
    val (a, b) = (runA.without(gone), runB.without(gone))
    val lines = Vector(
      s"# compare: ${a.name} (A) and ${b.name} (B)",
      "",
      s"A: variant ${a.log.header.variant}, model ${a.log.header.model}, repeats ${a.log.header.repeats}",
      s"B: variant ${b.log.header.variant}, model ${b.log.header.model}, repeats ${b.log.header.repeats}",
      s"answered by both: triage ${a.answers.triage.keySet.intersect(b.answers.triage.keySet).size}, " +
        s"stitch ${a.answers.stitch.keySet.intersect(b.answers.stitch.keySet).size}",
      labelLine(b),
      Intervals
    ) ++ kept(a, b) ++ Vector("") ++ spending(a, "A") ++ spending(b, "B") ++
      movedOn.toVector.flatMap(replica) ++ moved(a, b) ++ changes(
        a,
        b
      ) ++
      (if (b.labelled == 0) Vector.empty else brierPaired(a, b)) ++
      inputsChanged(a, b) ++
      decided.toVector.flatMap(decision)
    lines.mkString("\n") + "\n"
  }

  /** A corpus's recorded `turns` read structurally ([[Structure]]), with the review
    * `verdicts` standing joined by case: the pass rate first, then the turns that did not
    * reply, rounds, tools offered against called, prompt and window tokens, the first main
    * call's estimate against the ledger, cost per turn, speech outcomes, verdicts and used
    * parts, each over every turn and by root and by where said (a slice with no turn left
    * out); then one line a turn, most costly first. Used parts are those at support
    * [[grit.eval.harness.corpus.Support.Used]] or over, stated as a lexical heuristic, and
    * used-section recall is stated undefined where no reply said anything. `corpus` names it.
    */
  def turns(corpus: String, turns: Vector[TurnCase], verdicts: Verdicts): String =
    TurnLines.of(corpus, turns, verdicts)

  /** A recipes run over `corpus`: `notes` (lines on how it ran) first; then each of
    * `variants` against shipped on tools offered (called-tool recall first among them), what
    * the tools withheld are worth (tokens saved, their effective price in mills with the range
    * from every token cached to none, the hit rate beside it, and how far the cache moves it)
    * and the `rates` they were priced at (each model's, with how it was read, or why it has
    * none); window tokens by part, used-section recall (stated undefined when no reply used a
    * part), the turns each changed, by id; and the reference turns' pass rate under `shipped`
    * and each variant, with the failing turns by id (none stated when there is no reference
    * turn).
    */
  def recipes(
      corpus: String,
      notes: Vector[String],
      rates: VectorMap[String, Either[String, Rate]],
      shipped: Varied,
      variants: Vector[Varied]
  ): String =
    RecipeLines.of(corpus, notes, rates, shipped, variants)

  /** The synthetic reference ([[grit.eval.harness.reply.Synthetic]]) judged under each named
    * variant, shipped first by the caller: `notes` (lines on how it ran) first, then each
    * variant's pass rate, clustered by case, its unjudged count (a window that was not drawn)
    * and its failing turns by name.
    */
  def synthetic(notes: Vector[String], judged: Vector[(String, Vector[CaseJudged])]): String =
    RecipeLines.synthetic(notes, judged)

  /** One side of a drafts report: its log's `file` name, the `log`, the question set it asks,
    * by name, and that set's `gate`.
    */
  final case class Side(
      file: String,
      log: Log[VectorMap[QuestionName, Answer]],
      set: String,
      gate: Gate
  )

  /** A question set's pulled shadow log (`shadow`) against live triage's (`live`), as `drafts`
    * found them: how many cases both answered and how many were undecided; the 2×2 of live's
    * draft against the set's, each by its own gate over its answers alone (not live's speech
    * decision, which checks more), by focus and over all; each of the set's probabilities'
    * mean and spread; the two durables, when both name one; and each pick reason's `verdicts`
    * against both drafts and each side's `to`, or that none were given (`None`) or none stand.
    * Counts only, never a case's id.
    */
  def drafts(live: Side, shadow: Side, drafts: Drafts, verdicts: Option[Judgement]): String = {
    val all = drafts.gate.values.foldLeft(Cells.Empty)((x, y) =>
      Cells(x.both ++ y.both, x.aOnly ++ y.aOnly, x.bOnly ++ y.bOnly, x.neither ++ y.neither)
    )
    def size(c: Cells) = c.both.size + c.aOnly.size + c.bOnly.size + c.neither.size
    def agree(c: Cells) = s"${c.both.size + c.neither.size} of ${size(c)}"
    def row(what: String, c: Cells) =
      s"| $what | ${c.both.size} | ${c.aOnly.size} | ${c.bOnly.size} | ${c.neither.size} | ${agree(c)} |"
    val foci: Vector[(String, Option[Focus])] =
      Focus.values.toVector.map(f => f.toString.toLowerCase -> Some(f)) :+ ("focus unknown" -> None)
    def side(which: String, s: Side) =
      s"$which: ${s.file}, variant ${s.log.header.variant}, model ${s.log.header.model}, set " +
        s"${s.set}, questions " +
        s.log.header.questions.fold("unknown")(_.map(QuestionName.value).mkString(", "))
    val set = shadow.set
    val durableAt = num(Probability.value(Earning.DurableAt))
    val lines = Vector(
      s"# drafts: ${live.file} (A) and ${shadow.file} (B)",
      "",
      side("A, live", live),
      side("B", shadow),
      s"answered by both: ${size(all) + drafts.undecided}; undecided: ${drafts.undecided} " +
        "(a question a gate reads unanswered)",
      "",
      "## Draft against live's",
      "",
      s"A, live's draft by ${live.set}'s gate: ${gate(live.gate)}. Triage's answers alone: " +
        "not live's speech decision, which also checks the address, freshness, who was " +
        "asked, the thread and the rate limits.",
      s"B, $set's draft: ${gate(shadow.gate)}.",
      "",
      s"| focus | both | live only | $set only | neither | agree |",
      "|---|---|---|---|---|---|"
    ) ++ foci.flatMap((what, f) => drafts.gate.get(f).map(row(what, _))) ++ Vector(
      row("all", all),
      "",
      "## Questions",
      "",
      "Each probability over the cases B answered: a yes/no's of yes, a choice's by key.",
      "",
      "| question | mean | sd | cases |",
      "|---|---|---|---|"
    ) ++ drafts.columns.map(c => s"| ${c.name} | ${num(c.mean)} | ${num(c.sd)} | ${c.n} |") ++
      Vector("", "## Durable", "") ++ drafts.durable.fold(
        Vector(s"live's ${live.set} or $set names no durable question to compare.")
      )(c =>
        Vector(
          s"A, live's durable ≥ $durableAt; B, $set's durable ≥ $durableAt (both at " +
            "Earning.DurableAt), over the cases both answered.",
          "",
          s"| | $set yes | $set no |",
          "|---|---|---|",
          s"| live yes | ${c.both.size} | ${c.aOnly.size} |",
          s"| live no | ${c.bOnly.size} | ${c.neither.size} |",
          "",
          s"agree: ${agree(c)}"
        )
      ) ++ against(set, verdicts)
    lines.mkString("\n") + "\n"
  }

  /** The section on `verdicts` against live's draft and set `set`'s, by pick reason. */
  private def against(set: String, verdicts: Option[Judgement]): Vector[String] = {
    val head = Vector("", "## Against verdicts", "")
    val toAt = num(Probability.value(Judgement.ToAt))
    def to(m: Option[Judgement.Matched]) = m.fold("—")(m => s"${m.matched} of ${m.of}")
    verdicts match {
      case None => head :+ "No verdicts given (`--verdicts`, a file `pull` writes)."
      case Some(Judgement(Vector(), 0)) =>
        head :+ "No verdicts: none standing in the file given."
      case Some(j) =>
        head ++ Vector(
          "A rater's verdict on a message a review picked: a reply there would have been " +
            "welcome (speak), would have interrupted, or the message was meant for someone " +
            "in particular (to a person). Over the cases both gates decided; counts only, by " +
            "why each message was picked, not weighted back by how often each reason is " +
            "picked. A side's to is — when its set asks none.",
          "",
          s"| shadow | picked | verdicts | live's draft = speak | $set's draft = speak | " +
            s"live's to ≥ $toAt = to a person | $set's to ≥ $toAt = to a person |",
          "|---|---|---|---|---|---|---|"
        ) ++ j.reasons.map(r =>
          s"| ${ShadowName.value(r.shadow)} | ${Verdicts.reason(r.reason)} | ${r.n} | " +
            s"${r.live} | ${r.set} | ${to(r.liveTo)} | ${to(r.setTo)} |"
        ) ++ Vector(
          "",
          s"verdicts on a case either gate could not decide, left out: ${j.undecided}"
        )
    }
  }

  /** `g` as the report writes it: each bound as its reading `≥` or `<` its probability,
    * every one of some joined by `∧`, any one of them by `∨`, each part joining two or more
    * in parentheses; `everything` for the gate with none.
    */
  private[report] def gate(g: Gate): String = {
    def part(p: Gate) = p match {
      case Gate.All(gates) if gates.size > 1 => s"(${gate(p)})"
      case Gate.AnyOf(_, rest) if rest.nonEmpty => s"(${gate(p)})"
      case Gate.Holds(_) | Gate.All(_) | Gate.AnyOf(_, _) => gate(p)
    }
    g match {
      case Gate.Holds(Bound.AtLeast(on, p)) => s"${reading(on)} ≥ ${num(Probability.value(p))}"
      case Gate.Holds(Bound.Below(on, p)) => s"${reading(on)} < ${num(Probability.value(p))}"
      case Gate.All(gates) if gates.isEmpty => "everything"
      case Gate.All(gates) => gates.map(part).mkString(" ∧ ")
      case Gate.AnyOf(first, rest) => (first +: rest).map(part).mkString(" ∨ ")
    }
  }

  /** What a gate reads, as the report names it: a yes/no's name, a choice key's
    * `<name>.<key>`, and a key chosen `<name>=<key>`.
    */
  private def reading(r: Reading): String = r match {
    case Reading.Yes(name) => QuestionName.value(name)
    case Reading.Key(name, key) => s"${QuestionName.value(name)}.$key"
    case Reading.Chosen(name, key) => s"${QuestionName.value(name)}=$key"
  }

  /** The cases `m` found moved on, and the tolerance it found them under. */
  private def replica(m: Moved): Vector[String] = {
    val n = m.noise
    def tol(sd: Double) = num(MovedOn.Multiple * sd)
    Vector(
      "## Moved on",
      "",
      f"A replica's answer more than ${MovedOn.Multiple}%.0f × Jev's repeat spread from live's: " +
        s"kind ${tol(n.kind)}, waiting ${tol(n.waiting)}, durable ${tol(n.durable)}, " +
        s"helps ${tol(n.helps)}.",
      "",
      s"moved on, left out of this comparison: ${m.cases.size}"
    ) ++ m.cases.map(id => s"- ${id.written}") :+ ""
  }

  /** [[KeptNote]], once, when any of `runs` is a log of live triage's kept tags. */
  private def kept(runs: Scored*): Vector[String] =
    Vector(KeptNote).filter(_ => runs.exists(_.log.header.variant == Pull.Kept))

  private def decision(rule: Rule, d: Decision): Vector[String] = {
    def diff(e: Estimate) = s"B − A: ${est(Some(e))}, MDE ${num(e.mde)}"
    val (what, by) = d match {
      case Decision.Adopted(e) => ("adopt B", Some(e))
      case Decision.Kept(e) => ("keep A", Some(e))
      case Decision.Refused(Refusal.OtherRule) =>
        ("refused: run B was not started under this rule", None)
      case Decision.Refused(Refusal.Unclustered) =>
        ("refused: the difference has under two clusters", None)
      case Decision.Refused(Refusal.UnderMde(e)) =>
        ("refused: the difference is under its MDE", Some(e))
      case Decision.Refused(Refusal.UnderLeast(e)) =>
        (s"refused: the difference is under the rule's least, ${num(rule.least)}", Some(e))
      case Decision.Refused(Refusal.GuardBroken(e, x)) =>
        (
          s"refused: B's ${Target.written(x.question)} Brier, ${within(x.context)}, is worse " +
            s"by ${num(x.difference)}",
          Some(e)
        )
    }
    Vector(
      "## Decision",
      "",
      s"rule: ${Measure.written(rule.measure)} by ${rule.metric.toString.toLowerCase}, " +
        s"${within(rule.context)}, least ${num(rule.least)}, digest ${rule.digest.hex.take(12)}"
    ) ++ rule.guards.map(g =>
      s"guard: ${g.questions.map(Target.written).mkString(", ")}, ${within(g.context)}, " +
        s"no worse than ${num(g.worseBy)} on the mean"
    ) ++ by.map(diff).toVector ++ Vector(s"decision: $what", "")
  }

  private def within(context: Option[Context]): String =
    context.fold("all cases")(c => s"context ${Context.written(c)}")

  private def labelLine(run: Scored): String =
    if (run.labelled == 0) s"unlabelled: 0 of ${run.cases.size} cases labelled"
    else
      s"labelled: ${run.labelled} of ${run.cases.size} cases, guides " +
        run.labels.guides.toVector.sorted.mkString(", ")

  private def spending(run: Scored, which: String = ""): Vector[String] = {
    val s = Spending.of(run.log)
    Vector(
      s"## Spend and latency${if (which.isEmpty) "" else s": $which"}",
      "",
      s"spent: ${s.spent.fold("no footer")(Mills.withUsd)}; rows ${s.rows}: answered " +
        s"${s.counts.answered} (cached ${s.counts.cached}), failed ${s.counts.failed}, " +
        s"skipped ${s.counts.skipped}",
      s"tokens of the rows answered: input ${s.input}, output ${s.output}",
      s.latency.fold("latency: none answered")(l =>
        s"latency ms: median ${l.median}, p90 ${l.p90}, max ${l.max}"
      ),
      ""
    )
  }

  private def repeats(run: Scored): Vector[String] =
    Vector(
      "## Repeat spread",
      "",
      "Each case's max − min across its repeats; cases answered at least twice.",
      "",
      Header,
      Rule
    ) ++ Target.all.map { q =>
      val values = Repeated.spread(q, run.cases, run.answers)
      row(
        s"${Target.written(q)} (largest ${num(values.map(_._2).maxOption.getOrElse(0.0))})",
        Clustered.of(values)
      )
    } :+ ""

  private def live(run: Scored): Vector[String] = {
    val (cases, answers) = (run.cases, run.answers)
    val tags = Tag.values.toVector
    Vector("## Against live", "", Header, Rule) ++
      Vector(rateRow("kind: likeliest agrees", Proportions.of(Agreement.kind(cases, answers)))) ++
      tags.map(t =>
        row(s"${Tag.written(t)}: \\|run − live\\|", Clustered.of(Agreement.tag(t, cases, answers)))
      ) ++
      tags.flatMap(t =>
        Tag
          .shipped(t)
          .map(at =>
            rateRow(
              s"${Tag.written(t)}: decided alike at ${num(at)}",
              Proportions.of(Agreement.decided(t, at, cases, answers))
            )
          )
      ) ++
      Vector(
        row("place: largest \\|run − live\\|", Clustered.of(Agreement.place(cases, answers))),
        ""
      )
  }

  private def labelled(run: Scored): Vector[String] = {
    val s = Scoring(run.cases, run.answers, run.labels)
    val briers = Target.all.map(q => q -> Brier.of(q, s))
    Vector(
      "## Brier score",
      "",
      "Lower is better; 0 is perfect.",
      "",
      ContextHeader,
      ContextRule
    ) ++
      briers.map((q, values) => contextRow(Target.written(q), Split.of(values, run.labels))) ++
      Vector(
        "",
        s"place: ${s.unplaced} labelled cases left out (their exchange not offered)",
        ""
      ) ++
      (None +: Context.values.toVector.map(Some(_))).flatMap { (only: Option[Context]) =>
        tagged(
          only.fold("all contexts")(c => s"context ${Context.written(c)}"),
          Scoring(
            run.cases.filter(c => only.forall(run.labels.of(c.id).context.contains)),
            run.answers,
            run.labels
          )
        )
      }
  }

  private def tagged(name: String, s: Scoring): Vector[String] =
    Vector(s"## Tags, $name", "") ++ Tag.values.toVector.flatMap { t =>
      val judged = s.tag(t)
      val shipped = Tag.shipped(t)
      Vector(
        s"### ${Tag.written(t)}: reliability (${judged.size} cases)",
        "",
        "| predicted | n | mean predicted | labelled yes (by exchange) | (by author) |",
        "|---|---|---|---|---|"
      ) ++ Reliability
        .of(judged)
        .map(b =>
          s"| ${num(b.from)}–${num(b.to)} | ${b.n} | ${b.predicted.fold("—")(num)} | " +
            s"${proportion(b.rate.exchange)} | ${proportion(b.rate.author)} |"
        ) ++ Vector(
        "",
        s"### ${Tag.written(t)}: threshold sweep${shipped
            .fold(" (grit decides nothing on it)")(x => s" (shipped ${num(x)})")}",
        "",
        "| yes at or above | TPR (by exchange) | TNR (by exchange) | TPR (by author) | TNR (by author) |",
        "|---|---|---|---|---|"
      ) ++ Sweep
        .of(judged, shipped)
        .map(p =>
          s"| ${num(p.threshold)}${if (p.shipped) " **shipped**" else ""} | " +
            s"${proportion(p.tpr.exchange)} | ${proportion(p.tnr.exchange)} | " +
            s"${proportion(p.tpr.author)} | ${proportion(p.tnr.author)} |"
        ) ++ shipped.toVector.flatMap { at =>
        val curve = Cost.curve(judged, at)
        Vector(
          "",
          s"### ${Tag.written(t)}: expected cost per case, in false positives",
          "",
          "r is a false negative's cost over a false positive's, an experiment's parameter.",
          "",
          "| r | cheapest threshold | cost there | cost at shipped | shipped within error |",
          "|---|---|---|---|---|"
        ) ++ curve.map(c =>
          s"| ${ratio(c.ratio)} | ${num(c.best)} | ${est(c.atBest.exchange)} | " +
            s"${est(c.atShipped.exchange)} | ${c.within.fold("—")(w => if (w) "yes" else "no")} |"
        ) ++ Vector(
          "",
          "shipped within error at r: " + {
            val within = curve.filter(_.within.contains(true)).map(c => ratio(c.ratio))
            if (within.isEmpty) "none" else within.mkString(", ")
          }
        )
      } :+ ""
    }

  private def moved(a: Scored, b: Scored): Vector[String] = {
    val both = b.cases
    def paired(f: Scored => Vector[(Case, Double)]): Clustered = {
      val was = f(a).map((c, v) => c.id -> v).toMap
      Clustered.of(f(b).flatMap((c, v) => was.get(c.id).map(x => c -> (v - x))))
    }
    def yes(t: Tag)(run: Scored) =
      both.flatMap(c => run.answers.triage.get(c.id).map(x => c -> Tag.of(t, x.mean)))
    Vector("## B − A, unlabelled", "", Header, Rule) ++
      Tag.values.toVector.map(t => row(s"${Tag.written(t)}: p(yes)", paired(yes(t)))) ++
      Vector(
        rateRow(
          "kind: likeliest the same",
          Proportions.of(
            both.flatMap(c =>
              for {
                x <- a.answers.triage.get(c.id)
                y <- b.answers.triage.get(c.id)
              } yield c -> (x.mean.likeliest == y.mean.likeliest)
            )
          )
        ),
        ""
      )
  }

  private def changes(a: Scored, b: Scored): Vector[String] = {
    def listed(what: String, ids: Vector[CaseId]) =
      s"$what (${ids.size}): ${if (ids.isEmpty) "—" else ids.map(_.written).mkString(" ")}"
    def section(title: String, c: Changes) = Vector(
      s"### $title",
      "",
      listed("fixed", c.fixed),
      listed("broken", c.broken),
      listed("moved", c.moved),
      s"unchanged: ${c.unchanged}",
      ""
    )
    Vector("## Changes, case by case", "") ++
      Tag.values.toVector.flatMap { t =>
        val at = Tag.shipped(t).getOrElse(0.5)
        val why = if (Tag.shipped(t).isDefined) "shipped" else "grit decides nothing on it"
        section(
          s"${Tag.written(t)} at ${num(at)} ($why)",
          Comparison.tag(t, at, a.answers, b.answers, b.labels)
        )
      } ++ section("kind: likeliest", Comparison.kind(a.answers, b.answers, b.labels))
  }

  private def brierPaired(a: Scored, b: Scored): Vector[String] =
    Vector("## Brier score, B − A", "", "Negative: B is better.", "", ContextHeader, ContextRule) ++
      brierRows(a, b, b.cases) :+ ""

  /** B − A's Brier score on each question and on triage's four's mean, over `cases`, all and
    * by context.
    */
  private def brierRows(a: Scored, b: Scored, cases: Vector[Case]): Vector[String] = {
    val (sa, sb) = (Scoring(cases, a.answers, b.labels), Scoring(cases, b.answers, b.labels))
    Measures.map { m =>
      val was = Measure.brier(m, sa).map((c, v) => c.id -> v).toMap
      val diff = Measure.brier(m, sb).flatMap((c, v) => was.get(c.id).map(x => c -> (v - x)))
      contextRow(Measure.written(m), Split.of(diff, b.labels))
    }
  }

  /** Every question, then triage's four's mean. */
  private val Measures: Vector[Measure] = Target.all.map(Measure.Of(_)) :+ Measure.Mean

  /** The cases whose triage question A and B asked differently, and what changed on them. */
  private def inputsChanged(a: Scored, b: Scored): Vector[String] = {
    val changed = Changed.of(b.cases, a.log.rows, b.log.rows)
    def triaged(run: Scored) = run.log.rows.filter(_.suite == Suite.Triage).map(_.id).toSet
    val asked = triaged(a).intersect(triaged(b)).size
    val focus = Changed.focus(a.log.rows) ++ Changed.focus(b.log.rows)
    val foci: Vector[Option[Focus]] = Focus.values.toVector.map(Some(_)) :+ None
    def counts(name: String, these: Vector[Case]): String =
      s"| $name | ${(foci.map(f => these.count(c => focus.get(c.id) == f)) :+ these.size)
          .mkString(" | ")} |"
    val contexts: Vector[(String, Option[Context])] =
      Context.values.toVector.map(x => Context.written(x) -> Some(x)) :+ ("no context" -> None)
    def yes(t: Tag)(answers: Answers): Vector[(Case, Double)] =
      changed.flatMap(c => answers.triage.get(c.id).map(x => c -> Tag.of(t, x.mean)))
    val labelled = changed.filter(c => b.labels.of(c.id) != Labelled.Blank)
    val repeats = a.log.header.repeats
    def implied(what: String, measure: Answers => Vector[(Case, Double)]): String = {
      val apart = Apart.of(a.log.rows, measure)
      s"| $what | ${apart.n} | ${est(apart.exchange)} | " +
        s"${apart.exchange.fold("—")(e => num(e.mde))} | " +
        s"${apart.exchange.fold("—")(e => num(Apart.implied(e, repeats)))} |"
    }
    Vector(
      "## Inputs changed",
      "",
      s"triage asked differently: ${changed.size} of the $asked cases both runs asked",
      "",
      s"| context | ${Focus.values.map(_.toString.toLowerCase).mkString(" | ")} | focus unknown | all |",
      "|---|---|---|---|---|"
    ) ++ contexts.map((name, x) =>
      counts(name, changed.filter(c => b.labels.of(c.id).context == x))
    ) ++
      Vector(counts("all", changed), "") ++
      (if (changed.isEmpty) Vector.empty
       else
         Vector("### B − A over the changed cases, unlabelled", "", Header, Rule) ++
           Tag.values.toVector.map(t =>
             row(s"${Tag.written(t)}: p(yes)", pairedOn(yes(t), a.answers, b.answers))
           ) ++ Vector("") ++
           (if (labelled.isEmpty) Vector("no changed case is labelled", "")
            else
              Vector(
                "### Brier score over the changed cases, B − A",
                "",
                "Negative: B is better.",
                "",
                ContextHeader,
                ContextRule
              ) ++ brierRows(a, b, changed) :+ "")) ++
      Vector(
        "### Size of triage's calls",
        "",
        "| run | calls answered | input tokens: mean | p90 | cost: mean | p90 |",
        "|---|---|---|---|---|---|",
        size("A", Size.of(a.log.rows)),
        size("B", Size.of(b.log.rows)),
        ""
      ) ++
      (if (changed.isEmpty) Vector.empty
       else
         Vector(
           "### MDE implied by A's repeats, over the changed cases",
           "",
           s"Each case's measure on A's second repeat less its first: what Jev's noise alone " +
             s"reads as a difference. Implied: its MDE over √$repeats, about the least " +
             s"difference two runs of $repeats repeats can show on these cases.",
           "",
           "| measure | n | repeat 2 − 1, 95% by exchange | MDE, one repeat | implied MDE |",
           "|---|---|---|---|---|"
         ) ++ Tag.values.toVector.map(t => implied(s"${Tag.written(t)}: p(yes)", yes(t))) ++
           Measures
             .filterNot(_ == Measure.Of(Target.Places))
             .map(m =>
               implied(
                 s"${Measure.written(m)}: Brier, labelled",
                 answers => Measure.brier(m, Scoring(labelled, answers, b.labels))
               )
             ) :+ "")
  }

  /** `measure` of B less that of A, per case both hold. */
  private def pairedOn(
      measure: Answers => Vector[(Case, Double)],
      a: Answers,
      b: Answers
  ): Clustered = {
    val was = measure(a).map((c, v) => c.id -> v).toMap
    Clustered.of(measure(b).flatMap((c, v) => was.get(c.id).map(x => c -> (v - x))))
  }

  private def size(run: String, s: Size): String = {
    def per(p: Option[PerCall], f: Double => String) =
      p.fold("— | —")(x => s"${f(x.mean)} | ${f(x.p90)}")
    s"| $run | ${s.calls} | ${per(s.input, x => f"$x%.0f")} | ${per(s.cost, x => Mills.of(BigDecimal(x)))} |"
  }

  private val Header = "| measure | n | mean, 95% by exchange | 95% by author | MDE |"
  private val Rule = "|---|---|---|---|---|"
  private val ContextHeader =
    "| question | all: n, mean, 95% by exchange | MDE | by author | context ok | context short |"
  private val ContextRule = "|---|---|---|---|---|---|"

  private def row(what: String, c: Clustered): String =
    s"| $what | ${c.n} | ${est(c.exchange)} | ${interval(
        c.author
      )} | ${c.exchange.fold("—")(e => num(e.mde))} |"

  /** A proportion's row: no MDE, which is a paired difference's. */
  private def rateRow(what: String, p: Proportions): String =
    s"| $what | ${p.exchange.n} | ${proportion(p.exchange)} | ${bounds(p.author)} | — |"

  private def proportion(p: Proportion): String = p.rate.fold("—")(r => s"${num(r)} ${bounds(p)}")

  private def bounds(p: Proportion): String = p.interval match {
    case Proportion.Interval.TooFewClusters => s"[—] g=${p.clusters}"
    case Proportion.Interval.Wilson(low, high, _) => s"[${num(low)}, ${num(high)}] g=${p.clusters}"
  }

  private def contextRow(what: String, s: Split[Clustered]): String =
    s"| $what | ${s.all.n}: ${est(s.all.exchange)} | ${s.all.exchange.fold("—")(e => num(e.mde))} | " +
      s"${interval(s.all.author)} | ${s.ok.n}: ${est(s.ok.exchange)} | ${s.short.n}: ${est(s.short.exchange)} |"

  private def est(e: Option[Estimate]): String =
    e.fold("—")(x => s"${num(x.mean)} ${interval(e)}")

  private def interval(e: Option[Estimate]): String =
    e.fold("—") { x =>
      val (lo, hi) = x.interval
      s"[${num(lo)}, ${num(hi)}] g=${x.g}"
    }

  private def num(d: Double): String = f"$d%.3f"

  private def ratio(r: Double): String = if (r >= 1) f"$r%.0f" else s"1/${math.round(1 / r)}"
}
