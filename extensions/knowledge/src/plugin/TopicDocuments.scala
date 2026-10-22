package grit.knowledge.plugin

import java.time.{Instant, LocalDate, LocalTime, ZoneId}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Declared, Grace, Job, SlotRule}
import grit.core.message.Cost
import grit.core.plugin.{Documents, Exports, Plugin}
import grit.core.visibility.{Label, Visibility}
import grit.knowledge.card.CardShelf
import grit.knowledge.topic.{TopicDream, TopicParams}

/** The topics plugin: one topic per subject its runs find, by its job [[TopicDream]], daily at
  * `at` in `zone`, on a schedule per label in `labels`, keyed `topics-dream-{label}` with the
  * label's written form, `+` written `-`, and declared at that label, each run writing at most
  * [[TopicDocuments.PerRun]] topics from the strands spoken in and the cards changed within
  * [[TopicDocuments.Recent]]. It needs `cards`, a plugin exporting its cards (the knowledge plugin), when given, and reads no
  * cards otherwise. Its documents: label "topic", weight 1, kept 30 days after a version stops
  * being current, bounded at [[TopicDocuments.Bound]] times its labels; at each start, a
  * document at a label not among them is withdrawn ([[Documents.sweep]]); it posts nothing.
  */
final class TopicDocuments private (
    val name: PluginName,
    keyed: Vector[(ScheduleKey, Label)],
    rule: SlotRule,
    zone: ZoneId,
    terms: DocumentTerms,
    cards: Option[Exports[CardShelf]],
    seedUntil: Option[LocalDate]
) extends Plugin {

  private val labels: Vector[Label] = keyed.map(_._2)

  /** 2: its topics' data is of version 2 ([[grit.knowledge.topic.Topic.write]]). Its runs
    * withdraw a topic of version 1 when they scan, and open one again only from strands spoken
    * in within [[TopicDocuments.Recent]]; one of older strands comes back through runs for
    * their past days ([[backfill]]). A topic of a later version is left as it is.
    */
  val version: Int = 2

  override val needs: Vector[Exports[?]] = cards.toVector

  private val dream = new TopicDream(TopicDocuments.PerRun, zone, cards)

  override val documents: Option[Documents] = Some(DreamSchedules.documents(terms, labels))

  override val jobs: Vector[Job[?]] = Vector(dream)

  override val schedules: Vector[Declared[?]] =
    keyed.map((key, l) =>
      Declared(
        key,
        dream,
        rule,
        TopicParams(
          TopicDocuments.PerRun,
          TopicDocuments.Recent,
          TopicDocuments.Recent,
          None,
          seedUntil
        ),
        l
      )
    )

  /** The runs that backfill `days`, in the order given, at each of its labels in turn: each a
    * once schedule of its job, due from `started` with no grace, keyed
    * `topics-backfill-{label}-{day}-{epoch second of started}` (the label as its nightly
    * schedule's key writes it), at that label, with a nightly run's params but for `day` and at
    * most `perRun` topics; or why not: `perRun` under 1. A day run again writes only what
    * changed since: what a topic's lineage holds touches nothing.
    */
  def backfill(
      days: Vector[LocalDate],
      perRun: Int,
      started: Instant
  ): Either[String, Vector[Declared[TopicParams]]] =
    if (perRun < 1) Left(s"a backfill writes at least one topic a run, not $perRun")
    else {
      val stamp = started.getEpochSecond
      days.foldLeft[Either[String, Vector[Declared[TopicParams]]]](Right(Vector.empty)) {
        (acc, day) =>
          keyed.foldLeft(acc) { case (done, (nightly, l)) =>
            val label = ScheduleKey.value(nightly).stripPrefix(s"${TopicDocuments.Nightly}-")
            for {
              runs <- done
              key <- ScheduleKey.of(s"topics-backfill-$label-$day-$stamp")
            } yield runs :+ Declared(
              key,
              dream,
              SlotRule.Once(started, Grace.Zero),
              TopicParams(perRun, TopicDocuments.Recent, TopicDocuments.Recent, Some(day), None),
              l
            )
          }
      }
    }
}

object TopicDocuments {

  /** What its nightly schedules' keys start with, before the label. */
  private val Nightly: String = "topics-dream"

  /** 500: the most current topics one label's runs keep. */
  val Bound: Int = 500

  /** 10: the most topics one run writes. */
  val PerRun: Int = 10

  /** 30: the most days one `grit backfill topics` reaches back. */
  val BackfillMost: Int = 30

  /** 0.15¢ (in USD): the most one topic a backfilled day writes costs, the run's attach ask
    * shared among its topics. Measured on the dream model (Haiku 5.5, $0.10 / $0.50 per million
    * tokens in / out): a ten-topic night is about 100k tokens in and 10k out, about 1.5¢.
    */
  val TopicCost: BigDecimal = BigDecimal("0.0015")

  /** What `grit backfill topics` asks before it runs: the days `from` to `to`, the `labels`,
    * how many `runs` of at most `perRun` topics, and the most they cost at [[TopicCost]] a
    * topic, in cents.
    */
  def backfillQuestion(
      from: LocalDate,
      to: LocalDate,
      labels: Vector[Label],
      runs: Int,
      perRun: Int
  ): String =
    s"Backfill topics for $from to $to at ${labels.map(Label.shown).mkString(", ")}: " +
      s"$runs runs, at most $perRun topics each, about " +
      s"${Cost.cents(TopicCost * runs * perRun)} at most (${Cost.cents(TopicCost)} a topic); " +
      "nothing caps it but today's cap."

  /** What `grit backfill topics` prints last: `nothing ran` when no run did (its question
    * declined), else `spent {cents}`, what the runs spent in all. `spent` is each run's spend in
    * USD, one per run that ran.
    */
  def backfilled(spent: Vector[BigDecimal]): String =
    if (spent.isEmpty) "nothing ran" else s"spent ${Cost.cents(spent.sum)}"

  /** 2 days: how recently a strand must have been spoken in, or a card written, to be looked
    * at.
    */
  val Recent: FiniteDuration = 2.days

  /** The plugin at `labels`, its runs daily `at` in `zone`, reading `cards`' cards when given,
    * each night's run until `seedUntil` (its day included) seeding ([[TopicParams.seedUntil]]),
    * under the deployment's `visibility`; or why not: no label, one given twice, one holding a
    * compartment `visibility` does not declare, or one whose written form makes no schedule key.
    */
  def of(
      name: PluginName,
      labels: Vector[Label],
      at: LocalTime,
      zone: ZoneId,
      visibility: Visibility,
      cards: Option[Exports[CardShelf]],
      seedUntil: Option[LocalDate]
  ): Either[String, TopicDocuments] =
    for {
      keyed <- DreamSchedules.keyed("topics", Nightly, labels, visibility)
      terms <- DocLabel
        .of("topic")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, Bound * labels.size))
    } yield new TopicDocuments(name, keyed, SlotRule.Daily(at, zone), zone, terms, cards, seedUntil)
}
