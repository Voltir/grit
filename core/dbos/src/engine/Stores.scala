package grit.dbos.engine

import grit.core.document.{DocumentKeeper, DocumentStore, DocumentTerms}
import grit.core.edge.{EdgeDirectory, ToolRequests}
import grit.core.id.{PluginName, TurnRef}
import grit.core.identity.Principal
import grit.core.job.ScheduleStore
import grit.core.plugin.{CacheDocs, PluginCursors, PluginReads}
import grit.core.speech.SpeechStore
import grit.core.spend.Spending
import grit.core.store.{
  Askers,
  ClosedPeriod,
  ConversationStore,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  ModelProfileStore,
  ModelSettingStore,
  PeriodStore,
  Principals,
  PromptStore,
  Savepoints,
  StoreError,
  Tombstones,
  Tx,
  UsageLedger,
  VoiceStore
}
import grit.core.tool.ToolSets
import grit.core.triage.{TriageShadows, TriageStore}
import grit.core.visibility.Visibility
import grit.dbos.sql.{
  Opener,
  SqlCacheDocs,
  SqlConversationStore,
  SqlDocuments,
  SqlEdgeDirectory,
  SqlEntrySearch,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlModelProfileStore,
  SqlModelSettingStore,
  SqlPeriodStore,
  SqlPluginCursors,
  SqlPluginDocs,
  SqlPrincipals,
  SqlPromptStore,
  SqlSavepoints,
  SqlSchedules,
  SqlSpeechStore,
  SqlTombstones,
  SqlToolRequests,
  SqlToolSets,
  SqlTriageShadows,
  SqlTriageStore,
  SqlUsageLedger,
  SqlVoiceStore
}

/** The stores over grit's schema, each stateless: every read and write runs in the `Tx` it is
  * given, and a transaction opened under `visibility` resolves its clearance as [[opener]]
  * does. Built once here, so an [[Engine]] and a [[Rehearsing]] hold the same stores.
  */
class Stores private[engine] (visibility: Visibility) {

  /** How each transaction over these stores gets its clearance. */
  private[engine] val opener: Opener = new Opener(visibility)

  val conversations: ConversationStore = new SqlConversationStore()

  val entries: EntryStore = new SqlEntryStore()

  val search: EntrySearch = new SqlEntrySearch()

  private[engine] val sqlLedger = new SqlUsageLedger()

  val ledger: UsageLedger = sqlLedger

  val spending: Spending = sqlLedger

  /** Which profile each turn's model calls were made under. */
  val profiles: ModelProfileStore = new SqlModelProfileStore()

  /** The system prompt each turn was sent, its fragments kept by id (ADR 0016). */
  val prompts: PromptStore = new SqlPromptStore()

  /** Every tool set a turn was offered, by id. */
  val toolSets: ToolSets = new SqlToolSets()

  /** The requests hosted tool calls become (ADR 0017). */
  val requests: ToolRequests = new SqlToolRequests()

  /** Which live edge serves each place, and what it offers there. */
  val edgeDirectory: EdgeDirectory = new SqlEdgeDirectory()

  /** Settings of model pairs approved while grit runs, over the seed catalog. */
  val modelSettings: ModelSettingStore = new SqlModelSettingStore()

  /** Each conversation's periods, and their closing entries. */
  val periods: PeriodStore = new SqlPeriodStore(entries)

  /** The lifecycle's settings in force. */
  val lifecycle: LifecycleStore = new SqlLifecycleStore()

  /** The voice grit talks to the person in. */
  val voices: VoiceStore = new SqlVoiceStore()

  val principals: Principals = new SqlPrincipals()

  val deliveries: grit.core.edge.Deliveries = new grit.dbos.sql.SqlDeliveries()

  /** The messages an edge marks as being answered while their turns run. */
  val acknowledgements: grit.core.edge.Acknowledgements = new grit.dbos.sql.SqlAcknowledgements()

  /** What grit has decided to delete, and when. */
  val tombstones: Tombstones = new SqlTombstones

  /** What triage made of each heard message. */
  val triage: TriageStore = new SqlTriageStore

  /** What each declared shadow variant made of heard messages already triaged. */
  val shadows: TriageShadows = new SqlTriageShadows

  /** Where each heard message could be answered, and grit's decisions on each. */
  val speech: SpeechStore = new SqlSpeechStore

  /** Which heard messages a review considered, their prompts, and the labels standing on
    * them.
    */
  val reviews: grit.core.review.ReviewStore = new grit.dbos.sql.SqlReviews

  /** Where each thread's first message was placed among its room's exchanges (ADR 0023). */
  val stitches: grit.core.stitch.StitchStore = new grit.dbos.sql.SqlStitchStore

  /** What each room said before a time, as a pool reads it. */
  val rooms: grit.core.recipe.RoomReads = new grit.dbos.sql.SqlRoomReads

  /** Who each turn answers, as these stores' transactions resolve it. */
  val askers: Askers = {
    val resolving = opener
    new Askers {
      def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
        resolving.asker(turn)
    }
  }

  private[engine] val sqlSchedules = new SqlSchedules(tombstones)

  /** Every job's schedules (ADR 0029). */
  val schedules: ScheduleStore = sqlSchedules

  /** Each plugin's cursor. */
  val cursors: PluginCursors = new SqlPluginCursors(tombstones)

  private val sqlDocuments = new SqlDocuments(tombstones)

  /** Every plugin's documents (ADR 0028), as windows draw on them and as the engine itself
    * writes them.
    */
  val documents: DocumentStore = sqlDocuments

  /** The reviews of the documents plugins put up, as an edge posts them and the collector
    * forgets them.
    */
  val documentReviews: grit.core.review.DocumentReviews = new grit.dbos.sql.SqlDocumentReviews

  /** Each plugin's own documents as its tools and its service read them: given a plugin's
    * name, its own, and no other plugin's.
    */
  val reads: PluginName -> PluginReads =
    plugin => PluginReads(new SqlPluginDocs(plugin), sqlDocuments.shelf(plugin))

  /** Given a plugin and its terms, its documents as its posting writes them. */
  val keeper: (PluginName, DocumentTerms) -> DocumentKeeper =
    (plugin, terms) => sqlDocuments.keeper(plugin, terms)

  /** What undoes part of one of its transactions. */
  val savepoints: Savepoints = SqlSavepoints

  /** Given a plugin and the closed period it is posting, where it keeps what it makes of it. */
  val cache: (PluginName, ClosedPeriod) -> CacheDocs =
    (plugin, closed) => new SqlCacheDocs(plugin, closed.order)
}
