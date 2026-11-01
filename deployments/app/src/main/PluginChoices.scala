package grit.app.main

import grit.app.config.Listed.csv
import grit.core.files.Formats
import grit.core.id.{CorpusName, PluginName, ScheduleKey}
import grit.core.job.SlotRule
import grit.core.place.{Place, Service}
import grit.core.plugin.Plugin
import grit.core.triage.Corpus
import grit.core.visibility.{Label, Visibility}
import grit.digest.Digest
import grit.knowledge.drive.DriveCards
import grit.knowledge.github.GitHubCards
import grit.knowledge.plugin.{
  CardSource,
  Knowledge,
  StandingDocuments,
  StrandDocuments,
  TopicDocuments
}
import grit.knowledge.sync.SyncSchedule
import grit.mcp.drive.DriveTools
import grit.remind.Reminders

import ServedEdges.*

/** Which plugins a deployment runs (`GRIT_PLUGINS`), and the knowledge plugin's documents and
  * card sources.
  */
private[main] object PluginChoices {

  private[main] val PluginsVar = "GRIT_PLUGINS"

  /** The plugins `GRIT_PLUGINS` turns on, a comma-separated list of names; none when unset.
    * Those there are: `digest` ([[Digest]]), `remind` ([[Reminders]]), `knowledge`
    * ([[knowledge]], under `visibility`, its days beginning in `zone`, carding the Drive files
    * `formats` reads), `strands`
    * ([[strands]], likewise), `topics` ([[topics]], likewise, reading the knowledge plugin's
    * cards when it is listed, which it must be before `topics`) and `standing` ([[StandingDocuments]], leaving out what a reminder
    * tool's result established).
    */
  private[main] def pluginChoice(
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId,
      formats: Formats
  ): Either[String, Vector[Plugin]] =
    csv(env, PluginsVar).distinct
      .foldLeft[Either[String, Vector[Plugin]]](Right(Vector.empty)) { (acc, raw) =>
        acc.flatMap { done =>
          PluginName.of(raw).left.map(why => s"$PluginsVar: $why").flatMap { name =>
            raw match {
              case "digest" => Right(done :+ new Digest(name))
              case "remind" => Right(done :+ new Reminders(name))
              case "knowledge" =>
                knowledge(name, env, visibility, zone, formats)
                  .map(done :+ _)
                  .left
                  .map(why => s"$PluginsVar: $why")
              case "strands" =>
                strands(name, env, visibility, zone)
                  .map(done :+ _)
                  .left
                  .map(why => s"$PluginsVar: $why")
              case "topics" =>
                val cards = done.collectFirst { case k: Knowledge => k }
                if (cards.isEmpty && csv(env, PluginsVar).contains("knowledge"))
                  Left(
                    s"$PluginsVar: topics reads the knowledge plugin's cards, so knowledge comes before it"
                  )
                else
                  topics(name, env, visibility, zone, cards)
                    .map(done :+ _)
                    .left
                    .map(why => s"$PluginsVar: $why")
              case "standing" =>
                // A reminder's line goes stale at its date, and the reminders keep it: left out
                // whether or not they are on, since a closing can carry one from when they were.
                Right(
                  done :+ new StandingDocuments(
                    name,
                    Reminders.tools.map(_.described.spec.name).toSet
                  )
                )
              case other =>
                Left(
                  s"$PluginsVar: no plugin $other; there are digest, remind, knowledge, strands, topics and standing"
                )
            }
          }
        }
      }

  /** The labels the strands plugin's schedules are declared at, written forms, comma-separated.
    */
  private[main] val StrandLabelsVar = "GRIT_STRAND_LABELS"

  /** The strands plugin `name` ([[StrandDocuments]]), its runs daily at 04:30 in `zone`, one
    * schedule at each label [[StrandLabelsVar]] names; or why not: the variable unset, blank,
    * or naming no label, or [[StrandDocuments.of]]'s refusal under `visibility`.
    */
  private[main] def strands(
      name: PluginName,
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId
  ): Either[String, StrandDocuments] =
    for {
      labels <- labelsOf(env, StrandLabelsVar, "strands")
      plugin <- StrandDocuments
        .of(name, labels, java.time.LocalTime.of(4, 30), zone, visibility)
        .left
        .map(why => s"strands: $why")
    } yield plugin

  /** The labels the topics plugin's schedules are declared at, written forms, comma-separated.
    */
  private[main] val TopicLabelsVar = "GRIT_TOPIC_LABELS"

  /** The last day the topics plugin's nightly runs seed, `yyyy-mm-dd` ([[TopicDocuments.of]]'s
    * `seedUntil`); unset, none do.
    */
  private[main] val TopicSeedVar = "GRIT_TOPICS_SEED_UNTIL"

  /** The topics plugin `name` ([[TopicDocuments]]), its runs daily at 05:00 in `zone`, after
    * the knowledge plugin's syncs and the strands', one schedule at each label
    * [[TopicLabelsVar]] names, reading `cards`' cards when given, seeding through the day
    * [[TopicSeedVar]] names; or why not: the labels' variable unset, blank, or naming no label,
    * the seed's not `yyyy-mm-dd`, or [[TopicDocuments.of]]'s refusal under `visibility`.
    */
  private[main] def topics(
      name: PluginName,
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId,
      cards: Option[Knowledge]
  ): Either[String, TopicDocuments] =
    for {
      labels <- labelsOf(env, TopicLabelsVar, "topics")
      seed <- env.get(TopicSeedVar).map(_.trim).filter(_.nonEmpty) match {
        case None => Right(None)
        case Some(d) =>
          scala.util
            .Try(java.time.LocalDate.parse(d))
            .toOption
            .map(Some(_))
            .toRight(s"topics: $TopicSeedVar is yyyy-mm-dd: $d")
      }
      plugin <- TopicDocuments
        .of(name, labels, java.time.LocalTime.of(5, 0), zone, visibility, cards, seed)
        .left
        .map(why => s"topics: $why")
    } yield plugin

  /** The labels `variable` names, for the plugin `what`; or why not: unset, blank, or a label
    * that does not read.
    */
  private[main] def labelsOf(
      env: Map[String, String],
      variable: String,
      what: String
  ): Either[String, Vector[Label]] =
    Some(csv(env, variable))
      .filter(_.nonEmpty)
      .toRight(s"$what keeps documents at the labels $variable names, and it is unset or blank")
      .flatMap(
        _.foldLeft[Either[String, Vector[Label]]](Right(Vector.empty))((acc, w) =>
          acc.flatMap(done => Label.read(w).map(done :+ _).left.map(why => s"$variable: $why"))
        )
      )

  /** The variable naming the owner of the repository the knowledge plugin reads. */
  private[main] val GithubOwnerVar = "GRIT_GITHUB_OWNER"

  /** The variable naming the repository the knowledge plugin reads, under its owner. */
  private[main] val GithubRepoVar = "GRIT_GITHUB_REPO"

  /** The variable naming the ref (a branch, tag or commit) the knowledge plugin reads at. */
  private[main] val GithubRefVar = "GRIT_GITHUB_REF"

  /** The knowledge plugin `name` as `env` declares it ([[Knowledge]]), each source synced daily
    * at 03:00 in `zone`, at most 30 summaries a run. With [[GithubTokenVar]] set, the
    * repository [[GithubOwnerVar]]/[[GithubRepoVar]] at [[GithubRefVar]], its markdown files
    * (`md`), at most 600 cards, read through the GitHub tools [[github]]'s edge serves (`grit
    * serve` with [[GithubTokenVar]] set; a run while none serves them stops at its first call,
    * and its reply says so), kept at public, the corpus `github`, and drawn on where a
    * turn may read `service:github`. With [[DriveKeyVar]] set,
    * the shared drives grit is a member of, as [[DriveCards]] reads them through the Drive
    * tools [[drive]]'s edge serves, PDFs, Word and Excel files among them when `formats` reads
    * that kind, at most 600 cards at each label [[DriveCardLabelsVar]] names, the corpus `drive`, drawn on where a turn may read `service:drive`; Drive's place is
    * internal, so no label below it is accepted. Why not: neither variable set; a part of the
    * repository unset or blank, or [[DriveCardLabelsVar]] unset, blank or naming no label,
    * naming its variable; or [[DriveCards.of]]'s or [[Knowledge.of]]'s refusal under
    * `visibility`.
    */
  private[main] def knowledge(
      name: PluginName,
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId,
      formats: Formats
  ): Either[String, Knowledge] = {
    for {
      _ <- Either.cond(
        env.contains(GithubTokenVar) || env.contains(DriveKeyVar),
        (),
        s"knowledge reads GitHub or Drive, and neither $GithubTokenVar nor $DriveKeyVar is set"
      )
      key <- ScheduleKey.of("daily")
      daily <- SyncSchedule.of(key, SlotRule.Daily(java.time.LocalTime.of(3, 0), zone), 30)
      github <-
        if (env.contains(GithubTokenVar)) githubCards(env, daily).map(Vector(_))
        else Right(Vector.empty)
      drive <-
        if (env.contains(DriveKeyVar)) driveCards(env, daily, formats).map(Vector(_))
        else Right(Vector.empty)
      plugin <- Knowledge
        .of(name, github ++ drive, visibility)
        .left
        .map(why => s"knowledge: $why")
    } yield plugin
  }

  /** The repository [[GithubOwnerVar]]/[[GithubRepoVar]] at [[GithubRefVar]] as
    * [[knowledge]] says, synced on `daily`; or why not, as it says.
    */
  private[main] def githubCards(
      env: Map[String, String],
      daily: SyncSchedule
  ): Either[String, CardSource] = {
    def named(variable: String): Either[String, String] =
      env
        .get(variable)
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(s"knowledge reads the repository $variable names, and it is unset or blank")
    for {
      owner <- named(GithubOwnerVar)
      repo <- named(GithubRepoVar)
      ref <- named(GithubRefVar)
      service <- Service.of("github")
      corpus <- CorpusName.of("github")
      cards <- GitHubCards.of(
        Corpus(corpus, s"the GitHub repository $owner/$repo", Place.Everywhere, Some(service)),
        "github",
        owner,
        repo,
        ref,
        Vector(""),
        Vector.empty,
        Set("md"),
        "Markdown documents",
        600,
        Some(Label.Public),
        service.place,
        Vector(daily)
      )
    } yield CardSource.GitHub(cards)
  }

  /** The shared drives grit is a member of, as [[knowledge]] says, synced on `daily` at each
    * label [[DriveCardLabelsVar]] names; or why not, as it says.
    */
  private[main] def driveCards(
      env: Map[String, String],
      daily: SyncSchedule,
      formats: Formats
  ): Either[String, CardSource] =
    for {
      written <- Some(csv(env, DriveCardLabelsVar))
        .filter(_.nonEmpty)
        .toRight(
          s"knowledge reads Drive at the labels $DriveCardLabelsVar names, and it is unset or blank"
        )
      labels <- written.foldLeft[Either[String, Vector[Label]]](Right(Vector.empty))((acc, w) =>
        acc.flatMap(done =>
          Label.read(w).map(done :+ _).left.map(why => s"$DriveCardLabelsVar: $why")
        )
      )
      service <- DriveAt
      corpus <- CorpusName.of("drive")
      cards <- DriveCards.of(
        Corpus(corpus, "the shared drives grit is a member of", Place.Everywhere, Some(service)),
        DriveTools.List.spec.name,
        DriveTools.Read.spec.name,
        labels,
        600,
        service.place,
        Vector(daily),
        formats
      )
    } yield CardSource.Drive(cards)

  /** The variable listing the labels the knowledge plugin cards Drive's files at: their
    * written forms ([[Label.written]], e.g. `internal` or `confidential+finance`),
    * comma-separated.
    */
  private[main] val DriveCardLabelsVar = "GRIT_DRIVE_CARD_LABELS"
}
