package grit.core.edge

import grit.core.id.ConversationId
import grit.core.identity.TestAccounts
import grit.core.store.{InMemoryConversationStore, Origin, Tx}
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.TestTx

/** The origins contract, kept by the in-memory fake. */
object InMemoryOriginsTests extends OriginsContract {

  private val conversations = new InMemoryConversationStore

  protected val origins: Origins = new InMemoryOrigins(conversations)

  protected def conversation(origin: Origin, label: Label): ConversationId =
    conversations
      .findOrCreate(origin, TestAccounts.account("slack:T1/U1"), label)(using TestTx.fake)
      .fold(e => throw new java.lang.AssertionError(e), _.id)

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))
}
