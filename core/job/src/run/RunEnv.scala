package grit.job.run

import grit.act.moves.MovesEnv
import grit.core.clock.Clock
import grit.core.document.{DocumentKeeper, DocumentTerms}
import grit.core.edge.Deliveries
import grit.core.id.PluginName
import grit.core.job.ScheduleStore
import grit.core.need.Needs
import grit.core.spend.Budget
import grit.core.stitch.{StrandExcerpts, StrandShelf}
import grit.core.store.{ConversationStore, Db, EntryStore, Jot}

/** Where a run reads its slot and writes its reply: its turn's entries and conversation, the
  * schedules, and the replies an edge awaits.
  */
final case class RunRecords(
    entries: EntryStore,
    conversations: ConversationStore,
    schedules: ScheduleStore,
    deliveries: Deliveries
)

/** What a run works with besides its `Durable`: its [[RunRecords]]; `db`, which reads its slot
  * outside a transaction; `jot`, whose one transaction holds its reply, the reply's await and
  * its slot marked replied; `clock`, which dates them; `moves`, what its job's moves are made
  * with; `budget`, which admits each of its asks against the day's spend; `keepers`, given
  * a plugin and its terms, its documents as its keeping jobs write them; `strands` and, given
  * a plugin, its `needs`, what its dreaming jobs' scans read; and `excerpts`, what their
  * summaries are asked of.
  */
final case class RunEnv(
    records: RunRecords,
    db: Db^,
    jot: Jot^,
    clock: Clock^,
    moves: MovesEnv^,
    budget: Budget,
    keepers: (PluginName, DocumentTerms) -> DocumentKeeper,
    strands: StrandShelf,
    needs: PluginName -> Needs,
    excerpts: StrandExcerpts
)
