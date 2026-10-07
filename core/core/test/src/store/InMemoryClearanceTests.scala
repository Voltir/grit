package grit.core.store

import grit.core.id.ConversationId
import grit.core.recipe.{InMemoryRoomReads, RoomReads}
import grit.core.stitch.{InMemoryStitchStore, StitchStore}
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.TestTx

/** The clearance contract, kept by the in-memory fakes, over one conversation store that
  * each of them reads conversations' labels, rooms and origins from.
  */
object InMemoryClearanceTests extends ClearanceContract {

  private val conversations = new InMemoryConversationStore

  private def origin(c: ConversationId): Origin =
    conversations.all
      .find(_.id == c)
      .fold(Origin.Task("unknown", ConversationId.value(c)))(_.origin)

  private val store = new InMemoryEntryStore(c => conversations.all.find(_.id == c))

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new InMemoryPeriodStore(store, origin)
  protected val stitches: StitchStore = new InMemoryStitchStore(store, origin)
  protected val rooms: RoomReads = new InMemoryRoomReads(store, origin, new InMemoryPrincipals)

  protected def transaction[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))

  protected def conversation(origin: Origin, label: Label): ConversationId =
    conversations
      .findOrCreate(origin, grit.core.id.PrincipalId.Local, label)(using TestTx.fake)
      .fold(e => throw new java.lang.AssertionError(s"$e"), _.id)
}
