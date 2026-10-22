package grit.app.main

import java.time.{Duration, Instant}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{Declarer, PluginName, ScheduleId, ScheduleKey}
import grit.core.job.{Declared, Grace, Slot, SlotRule}
import grit.core.message.{AssistantBlock, Message}
import grit.core.need.PluginReads
import grit.core.plugin.{DocumentPosting, Documents, Exports, Plugin}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.Engine
import grit.knowledge.card.CardShelf
import grit.knowledge.plugin.TopicDocuments
import grit.knowledge.topic.{TopicDream, TopicParams}

/** What a topics run by hand shares ([[TopicRoom]], [[TopicBackfill]]): the topics plugin with
  * one once schedule, and a cards plugin exporting its cards.
  */
private[main] object TopicRuns {

  import RoomRun.right

  /** The knowledge plugin's cards' terms, as it declares them. */
  val CardTerms: DocumentTerms = DocLabel
    .of("knowledge card")
    .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 100))
    .fold(sys.error, identity)

  /** The topics plugin `topics`, its one schedule `key`, once `at`, with `params`, at `label`. */
  final class DayRun(
      topics: TopicDocuments,
      dream: TopicDream,
      key: ScheduleKey,
      at: Instant,
      params: TopicParams,
      label: Label
  ) extends Plugin {
    val name: PluginName = topics.name
    val version: Int = topics.version
    override val needs = topics.needs
    override val documents: Option[Documents] = topics.documents
    override val jobs = Vector(dream)
    override val schedules: Vector[Declared[?]] = Vector(
      Declared(
        key,
        dream,
        SlotRule.Once(at, Grace.of(10.minutes).getOrElse(Grace.Zero)),
        params,
        label
      )
    )
  }

  /** The cards' plugin `name`: its documents under `terms`, exported as the knowledge plugin
    * exports its own.
    */
  final class Cards(val name: PluginName, terms: DocumentTerms) extends Exports[CardShelf] {
    val version: Int = 1
    override val documents: Option[Documents] = Some(TopicRuns.documents(terms))
    def service(own: PluginReads): CardShelf = CardShelf.over(own.documents)
  }

  /** A plugin `name` that only keeps its documents under `terms`, so a launch keeps them
    * declared: no job, nothing posted.
    */
  final class Keeping(val name: PluginName, terms: DocumentTerms) extends Plugin {
    val version: Int = 1
    override val documents: Option[Documents] = Some(TopicRuns.documents(terms))
  }

  private def documents(t: DocumentTerms): Documents = new Documents {
    val terms: DocumentTerms = t
    val posting: Option[DocumentPosting] = None
  }
}
