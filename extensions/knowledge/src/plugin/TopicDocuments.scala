package grit.knowledge.plugin

import java.time.{Instant, LocalDate, LocalTime, ZoneId}

import scala.concurrent.duration.*

import grit.core.clock.Utc
import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{CallSlot, JobName, PluginName, ScheduleKey}
import grit.core.job.{Declared, Grace, Interval, Job, NotOwn, OwnJobs, ScheduleDesk, SlotRule}
import grit.core.message.Cost
import grit.core.need.{Needs, PluginReads, Unneeded}
import grit.core.place.Place
import grit.core.plugin.{Documents, Exports, Plugin, PluginRun, PluginTool}
import grit.core.store.Reads
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  ToolName,
  ToolSpec
}
import grit.core.visibility.{Label, Visibility}
import grit.knowledge.card.CardShelf
import grit.knowledge.topic.{
  AheadParams,
  MeetingLookup,
  MeetingsAhead,
  Passage,
  TopicDream,
  TopicParams
}

/** The topics plugin: one topic per subject its runs find, by its job [[TopicDream]], daily at
  * `at` in `zone`, on a schedule per label in `labels`, keyed `topics-dream-{label}` with the
  * label's written form, `+` written `-`, and declared at that label, each run writing at most
  * [[TopicDocuments.PerRun]] topics from the strands spoken in and the cards changed within
  * [[TopicDocuments.Recent]]. It needs `cards`, a plugin exporting its cards (the knowledge plugin), when given, and reads no
  * cards otherwise. Its documents: label "topic", weight 1, kept 30 days after a version stops
  * being current, bounded at [[TopicDocuments.Bound]] times its labels; at each start, a
  * document at a label not among them is withdrawn ([[Documents.sweep]]); it posts nothing. Its
  * tool `meetings` lists its meetings a turn reads ([[TopicDocuments.meetings]]).
  */
final class TopicDocuments private (
    val name: PluginName,
    keyed: Vector[(ScheduleKey, Label)],
    rule: SlotRule,
    zone: ZoneId,
    terms: DocumentTerms,
    cards: Option[Exports[CardShelf]],
    seedUntil: Option[LocalDate],
    foldAt: Option[Double],
    ahead: Vector[(ScheduleKey, Label)],
    aheadIn: Vector[Place]
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

  private val meetingsAhead = new MeetingsAhead(zone, TopicDocuments.Bound * labels.size)

  override val jobs: Vector[Job[?]] =
    Vector(dream) ++ Option.when(aheadIn.nonEmpty)(meetingsAhead)

  override val tools: Vector[PluginTool[?]] =
    Vector(TopicDocuments.meetings(TopicDocuments.Bound * labels.size, zone))

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
          seedUntil,
          foldAt = foldAt
        ),
        l
      )
    ) ++ (if (aheadIn.isEmpty) Vector.empty
          else
            ahead.map((key, l) =>
              Declared(key, meetingsAhead, SlotRule.Every(Interval.Hour), AheadParams(aheadIn), l)
            ))

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
              TopicParams(
                perRun,
                TopicDocuments.Recent,
                TopicDocuments.Recent,
                Some(day),
                None,
                foldAt = foldAt
              ),
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

  /** `meetings`: [[MeetingLookup.answer]] over the newest `reading` of its documents that the
    * calling turn reads, its days in `zone` (the days its meetings are reckoned in), which its
    * `from` field and header name. Free, rerun on retry, believed as the answer says; `Failed`,
    * naming the error, when the store cannot be read.
    */
  private[plugin] def meetings(reading: Int, zone: ZoneId): PluginTool[MeetingLookup] =
    new PluginTool[MeetingLookup] {
      private val time = s"${Utc.canonical(zone).getId} time"

      private def day(field: String)(text: String): Either[ArgsError, LocalDate] =
        Passage
          .day(text)
          .toRight(ArgsError.Invalid(field, "a date as yyyy-mm-dd", ujson.Str(text).render()))

      val described: Hosted[MeetingLookup] =
        new Hosted(
          ToolSpec(
            ToolName("meetings"),
            "List the meetings this conversation may see, as grit writes them up each day from " +
              "the team's talk: each with where it stands (proposed, planned or held), its day and " +
              "start, who is in it, what it is about and what was said of it. For \"next meeting\" " +
              "or \"this week\", give `from` as today. For a meeting already held, give `from` " +
              "some weeks back, `until` today, and `latest_first`. Meetings proposed with no day yet, " +
              "or whose proposed day has passed, that were talked about lately follow the dated " +
              "ones. Talk since the last write-up is not in it yet; say so when it matters.",
            Args
              .of(
                (
                  from = Field.text(s"The first day to list, yyyy-mm-dd, in $time."),
                  until = Field
                    .text(
                      s"The last day to list, yyyy-mm-dd; ${MeetingLookup.Span} days after `from` " +
                        "when not given."
                    )
                    .optional,
                  about = Field
                    .text(
                      "Words each meeting listed mentions anywhere in its write-up, such as a " +
                        "person's name or a subject; every meeting when not given."
                    )
                    .optional,
                  most = Field
                    .count(
                      s"How many meetings at most; ${MeetingLookup.Shown} when not given.",
                      1,
                      MeetingLookup.Most
                    )
                    .optional,
                  latest_first = Field
                    .flag(
                      "List the latest first, for a meeting already held; soonest first when " +
                        "not given."
                    )
                    .optional
                )
              )
              .refine(a =>
                for {
                  from <- day("from")(a.from)
                  until <- a.until.fold(Right(None))(t => day("until")(t).map(Some(_)))
                  lookup <- MeetingLookup
                    .of(from, until, zone, a.about, a.most, a.latest_first.getOrElse(false))
                    .left
                    .map(ArgsError.Refused("until", _))
                } yield lookup
              ),
            // It only reads.
            retry = Retry.Rerun
          ),
          Gate.Free,
          l =>
            s"${l.from} to ${l.until}" + (if (l.about.isEmpty) ""
                                          else s" about ${l.about.mkString(" ")}"),
          Believed.Own
        )

      def bind(
          own: PluginReads,
          needs: Needs,
          jobs: OwnJobs
      ): Either[Unneeded | NotOwn, PluginRun[MeetingLookup]] =
        Right(new PluginRun[MeetingLookup] {
          def run(l: MeetingLookup, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome =
            reads.read(own.documents.newest(reading)) match {
              case Left(error) => Outcome.Failed(s"the meetings could not be read: $error")
              case Right(documents) =>
                val answer = l.answer(documents)
                Outcome.Done(answer.text, answer.believed)
            }
        })
    }

  /** 10: the most topics one run writes. */
  val PerRun: Int = 10

  /** 90: the most days one `grit backfill topics` reaches back, as long as a passage is kept
    * ([[TopicParams.KeepFor]]): a day further back writes passages the next nightly run drops.
    */
  val BackfillMost: Int = 90

  /** 0.15¢ (in USD): the most one topic a backfilled day writes costs, its write ask and its
    * share of the run's sorts. Measured, when a model ask placed what a sort now does, on the
    * dream model (Haiku 5.5, $0.10 / $0.50 per million
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
    * each nightly and backfill run folding the merges it proposes at `foldAt` or more
    * ([[TopicParams.foldAt]]; none folded when `None`), under the deployment's `visibility`; or
    * why not: no label, one given twice, one holding a compartment `visibility` does not
    * declare, one whose written form makes no schedule key, or `foldAt` not from
    * [[grit.knowledge.topic.TopicDream.FoldAt]] to 1 ([[TopicParams.folding]]). With rooms
    * `aheadIn`, each label also gets an hourly `meetings-ahead-{label}` schedule raising
    * occasions in those rooms alone.
    */
  def of(
      name: PluginName,
      labels: Vector[Label],
      at: LocalTime,
      zone: ZoneId,
      visibility: Visibility,
      cards: Option[Exports[CardShelf]],
      seedUntil: Option[LocalDate],
      foldAt: Option[Double],
      aheadIn: Vector[Place] = Vector.empty
  ): Either[String, TopicDocuments] =
    for {
      keyed <- DreamSchedules.keyed("topics", Nightly, labels, visibility)
      ahead <- DreamSchedules.keyed("topics", JobName.value(MeetingsAhead.Name), labels, visibility)
      terms <- DocLabel
        .of("topic")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, Bound * labels.size))
      fold <- foldAt.fold(Right(None))(TopicParams.folding(_).map(Some(_)))
    } yield new TopicDocuments(
      name,
      keyed,
      SlotRule.Daily(at, zone),
      zone,
      terms,
      cards,
      seedUntil,
      fold,
      ahead,
      aheadIn
    )
}
