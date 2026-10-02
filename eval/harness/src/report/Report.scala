package grit.eval.harness.report

import grit.eval.harness.corpus.{Case, CaseId}
import grit.eval.harness.label.{Context, Labelled, Labels}
import grit.eval.harness.log.{Log, Row, Weights}
import grit.eval.harness.pull.Pull
import grit.eval.harness.score.{
  Agreement,
  Answers,
  Brier,
  Changes,
  Clustered,
  Comparison,
  Cost,
  Decision,
  Moved,
  MovedOn,
  Refusal,
  Reliability,
  Repeated,
  Rule,
  Scoring,
  Spending,
  Split,
  Sweep,
  Tag,
  Target
}
import grit.eval.harness.stats.Estimate

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
  * is printed with its 95% interval clustered by exchange and, beside it, by author; one with
  * under two clusters prints `—`, never a bare number.
  */
object Report {

  /** What a report says of a log of live triage's kept tags ([[Pull.Kept]]). */
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
      labelLine(run)
    ) ++ kept(run) ++ Vector("") ++ spending(run) ++ repeats(run) ++ live(run) ++
      (if (run.labelled == 0) Vector.empty else labelled(run))
    lines.mkString("\n") + "\n"
  }

  /** Run `b` against run `a`, paired on the cases both answered: B − A per question, the cases
    * whose decision changed by id, once labels exist the paired difference in Brier score, all
    * and by context, with its MDE, and what `decided` says a rule made of them, when given;
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
      labelLine(b)
    ) ++ kept(a, b) ++ Vector("") ++ spending(a, "A") ++ spending(b, "B") ++
      movedOn.toVector.flatMap(replica) ++ moved(a, b) ++ changes(
        a,
        b
      ) ++
      (if (b.labelled == 0) Vector.empty else brierPaired(a, b)) ++
      decided.toVector.flatMap(decision)
    lines.mkString("\n") + "\n"
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
    }
    Vector(
      "## Decision",
      "",
      s"rule: ${Target.written(rule.question)} by ${rule.metric.toString.toLowerCase}, " +
        s"least ${num(rule.least)}, digest ${rule.digest.hex.take(12)}"
    ) ++ by.map(diff).toVector ++ Vector(s"decision: $what", "")
  }

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
      s"spent: ${s.spent.fold("no footer")(x => s"$$$x")}; rows ${s.rows}: answered " +
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
      Vector(row("kind: likeliest agrees", Clustered.of(Agreement.kind(cases, answers)))) ++
      tags.map(t =>
        row(s"${Tag.written(t)}: \\|run − live\\|", Clustered.of(Agreement.tag(t, cases, answers)))
      ) ++
      tags.flatMap(t =>
        Tag
          .shipped(t)
          .map(at =>
            row(
              s"${Tag.written(t)}: decided alike at ${num(at)}",
              Clustered.of(Agreement.decided(t, at, cases, answers))
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
            s"${est(b.rate.exchange)} | ${est(b.rate.author)} |"
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
          s"| ${num(p.threshold)}${if (p.shipped) " **shipped**" else ""} | ${est(p.tpr.exchange)} | " +
            s"${est(p.tnr.exchange)} | ${est(p.tpr.author)} | ${est(p.tnr.author)} |"
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
        row(
          "kind: likeliest the same",
          Clustered.of(
            both.flatMap(c =>
              for {
                x <- a.answers.triage.get(c.id)
                y <- b.answers.triage.get(c.id)
              } yield c -> (if (x.mean.likeliest == y.mean.likeliest) 1.0 else 0.0)
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

  private def brierPaired(a: Scored, b: Scored): Vector[String] = {
    val (sa, sb) = (Scoring(a.cases, a.answers, b.labels), Scoring(b.cases, b.answers, b.labels))
    Vector("## Brier score, B − A", "", "Negative: B is better.", "", ContextHeader, ContextRule) ++
      Target.all.map { q =>
        val was = Brier.of(q, sa).map((c, v) => c.id -> v).toMap
        val diff = Brier.of(q, sb).flatMap((c, v) => was.get(c.id).map(x => c -> (v - x)))
        contextRow(Target.written(q), Split.of(diff, b.labels))
      } :+ ""
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
