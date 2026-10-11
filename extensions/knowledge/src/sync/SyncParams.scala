package grit.knowledge.sync

import grit.core.id.CorpusName

/** What a sync run is told: its source, and at most how many summaries it asks for. */
final case class SyncParams(source: CorpusName, perRun: Int) extends caps.Pure
