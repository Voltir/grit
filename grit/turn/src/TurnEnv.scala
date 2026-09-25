package grit.turn

import grit.core.classify.Classifier
import grit.core.context.ContextAssembler
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.store.{Db, EntryStore, UsageLedger}

/** What a turn works with besides its `Durable`: its system prompt and [[TurnRecords]],
  * then the capabilities it calls. `assembler` builds its window, `classifier` places its
  * message among the topics, `provider` answers, `summarizer` summarises, and `db` reads
  * the store outside a transaction.
  */
final case class TurnEnv(
    system: String,
    records: TurnRecords,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    provider: Provider^,
    summarizer: Provider^,
    db: Db^
)

/** Where a turn's entries and their costs are written, and how a request is priced. */
final case class TurnRecords(entries: EntryStore, ledger: UsageLedger, estimator: TokenEstimator)
