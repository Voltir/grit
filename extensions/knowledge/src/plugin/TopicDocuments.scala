package grit.knowledge.plugin

import java.time.{LocalTime, ZoneId}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Declared, Job, SlotRule}
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
    cards: Option[Exports[CardShelf]]
) extends Plugin {

  private val labels: Vector[Label] = keyed.map(_._2)

  val version: Int = 1

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
        TopicParams(TopicDocuments.PerRun, TopicDocuments.Recent, TopicDocuments.Recent, None),
        l
      )
    )
}

object TopicDocuments {

  /** 500: the most current topics one label's runs keep. */
  val Bound: Int = 500

  /** 10: the most topics one run writes. */
  val PerRun: Int = 10

  /** 2 days: how recently a strand must have been spoken in, or a card written, to be looked
    * at.
    */
  val Recent: FiniteDuration = 2.days

  /** The plugin at `labels`, its runs daily `at` in `zone`, reading `cards`' cards when given,
    * under the deployment's `visibility`; or why not: no label, one given twice, one holding a
    * compartment `visibility` does not declare, or one whose written form makes no schedule key.
    */
  def of(
      name: PluginName,
      labels: Vector[Label],
      at: LocalTime,
      zone: ZoneId,
      visibility: Visibility,
      cards: Option[Exports[CardShelf]]
  ): Either[String, TopicDocuments] =
    for {
      keyed <- DreamSchedules.keyed("topics", "topics-dream", labels, visibility)
      terms <- DocLabel
        .of("topic")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, Bound * labels.size))
    } yield new TopicDocuments(name, keyed, SlotRule.Daily(at, zone), zone, terms, cards)
}
