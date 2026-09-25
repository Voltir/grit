package grit.turn

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.store.{Db, EntryStore, UsageLedger}

/** What a turn works with besides its `Durable`: its system prompt and [[TurnRecords]],
  * then the capabilities it calls. `assembler` builds its window, `classifier` places its
  * message among the topics, `provider` answers, `summarizer` summarises, `db` reads the
  * store outside a transaction, `clock` dates entries and paces the reply's stream, and
  * `fresh` tags each attempt at a model call ([[TurnStream]]).
  */
final case class TurnEnv(
    system: String,
    records: TurnRecords,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    provider: Provider^,
    summarizer: Provider^,
    db: Db^,
    clock: Clock^,
    fresh: Fresh^
)

/** Where a turn's entries and their costs are written, and how a request is priced. */
final case class TurnRecords(entries: EntryStore, ledger: UsageLedger, estimator: TokenEstimator)
