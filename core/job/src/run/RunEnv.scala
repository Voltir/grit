package grit.job.run

import grit.act.moves.MovesEnv
import grit.core.clock.Clock
import grit.core.edge.Deliveries
import grit.core.job.ScheduleStore
import grit.core.spend.Budget
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
  * with; and `budget`, which admits each of its asks against the day's spend.
  */
final case class RunEnv(
    records: RunRecords,
    db: Db^,
    jot: Jot^,
    clock: Clock^,
    moves: MovesEnv^,
    budget: Budget
)
