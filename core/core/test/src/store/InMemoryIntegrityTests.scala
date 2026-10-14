package grit.core.store

import java.time.Instant

import grit.core.id.ConversationId
import grit.core.identity.Account
import grit.core.visibility.Label
import grit.dbos.sql.TestTx

/** The integrity contract, kept by the in-memory fakes. */
object InMemoryIntegrityTests extends IntegrityContract {

  /** A conversation's id holds its origin's task name and run, so the fakes can find its room. */
  private def originOf(c: ConversationId): Origin =
    ConversationId.value(c).split('|') match {
      case Array(name, run) => Origin.Task(name, run)
      case _ => Origin.Task("unknown", ConversationId.value(c))
    }

  private val store = new InMemoryEntryStore(c =>
    Some(Conversation(c, originOf(c), Account.Local, Instant.EPOCH, Label.Public))
  )

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new InMemoryPeriodStore(store, originOf)

  protected def transaction[A](body: (Tx^) ?=> A): A =
    body(using TestTx.inForce(IntegrityContract.Capping))

  protected def conversation(origin: Origin): ConversationId = origin match {
    case Origin.Task(name, run) => ConversationId(s"$name|$run")
    case other => ConversationId(other.toString)
  }
}
