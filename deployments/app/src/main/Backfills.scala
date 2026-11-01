package grit.app.main

import scala.concurrent.duration.FiniteDuration

import grit.core.plugin.Plugin
import grit.kit.deployment.Deployment
import grit.kit.run.Kit
import grit.knowledge.plugin.TopicDocuments

import PluginChoices.*

/** `grit backfill`'s agreement, and `grit backfill topics`. */
private[main] object Backfills {

  /** `grit backfill topics`'s arguments: how many `days` before today, at most `perRun` topics
    * a run, and whether to run without asking (`yes`).
    */
  private[main] final case class BackfillTopics(days: Int, perRun: Int, yes: Boolean)

  /** `grit backfill topics`'s arguments after those two words, in any order: `--days N` (1 to
    * [[TopicDocuments.BackfillMost]], required), `--per-run n` (above zero; [[TopicDocuments.PerRun]] when
    * absent) and `--yes`; or why not, naming the argument.
    */
  private[main] def backfillTopics(args: Vector[String]): Either[String, BackfillTopics] = {
    def read(
        at: Int,
        got: (Option[Int], Option[Int], Boolean)
    ): Either[String, (Option[Int], Option[Int], Boolean)] =
      (args.lift(at), args.lift(at + 1)) match {
        case (None, _) => Right(got)
        case (Some("--yes"), _) => read(at + 1, got.copy(_3 = true))
        case (Some("--days"), n) =>
          n.flatMap(_.trim.toIntOption)
            .filter(d => d >= 1 && d <= TopicDocuments.BackfillMost)
            .toRight(
              s"--days is a whole number of days from 1 to ${TopicDocuments.BackfillMost}, not '${n.getOrElse("")}'"
            )
            .flatMap(d => read(at + 2, got.copy(_1 = Some(d))))
        case (Some("--per-run"), n) =>
          n.flatMap(_.trim.toIntOption)
            .filter(_ >= 1)
            .toRight(s"--per-run is a whole number of topics above zero, not '${n.getOrElse("")}'")
            .flatMap(p => read(at + 2, got.copy(_2 = Some(p))))
        case (Some(other), _) =>
          Left(s"backfill topics takes --days, --per-run and --yes, not '$other'")
      }
    read(0, (None, None, false)).flatMap((days, perRun, yes) =>
      days
        .toRight(
          s"--days is required: a whole number of days from 1 to ${TopicDocuments.BackfillMost}"
        )
        .map(BackfillTopics(_, perRun.getOrElse(TopicDocuments.PerRun), yes))
    )
  }

  /** 10 minutes: the longest `grit backfill topics` waits for one run. */
  private[main] val BackfillLongest: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.Duration(10, "minutes")

  /** `yes`, or the person's yes to `question`, asked on the terminal. */
  private[main] def agreed(yes: Boolean)(question: String): Boolean =
    yes || {
      print(s"$question [y/N] ")
      Option(scala.io.StdIn.readLine()).exists(a => Set("y", "yes").contains(a.trim.toLowerCase))
    }

  /** `grit backfill topics`: the topics plugin's runs for the `b.days` days before `today`
    * ([[TopicDocuments.backfill]], started at `started`), run on `deployment`'s database by
    * [[Kit.runOnce]], asked first ([[backfillQuestion]]) unless `b.yes`; each run's line
    * printed, then their spend (`nothing ran` when none ran). Refused when the deployment has no topics plugin, and as
    * [[Kit.runOnce]] refuses.
    */
  private[main] def topicsBackfilled(
      deployment: Deployment,
      env: Map[String, String],
      b: BackfillTopics,
      today: java.time.LocalDate,
      started: java.time.Instant
  ): Either[String, Unit] = {
    val days = (b.days to 1 by -1).toVector.map(today.minusDays(_))
    for {
      topics <- deployment.plugins
        .collectFirst { case t: TopicDocuments => t }
        .toRight(s"the topics plugin is off: $PluginsVar names no topics")
      runs <- topics.backfill(days, b.perRun, started)
      ran <- Kit
        .runOnce(
          deployment,
          topics.name,
          runs,
          TopicDocuments.backfillQuestion(
            today.minusDays(b.days),
            today.minusDays(1),
            runs.map(_.clearance).distinct,
            runs.size,
            b.perRun
          ),
          BackfillLongest,
          env,
          agreed(b.yes),
          println
        )
        .left
        .map(_.message)
    } yield println(TopicDocuments.backfilled(ran.map(_.spent)))
  }
}
