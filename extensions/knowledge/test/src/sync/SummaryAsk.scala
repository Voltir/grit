package grit.knowledge.sync

import grit.core.act.Posed
import grit.knowledge.card.CardSummary

/** The sync's summary ask, for a live probe outside this package. */
object SummaryAsk {

  def of(subject: String, head: String): Posed.Json[CardSummary] =
    Sync.summarising(subject, head)
}
