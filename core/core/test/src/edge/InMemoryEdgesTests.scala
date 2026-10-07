package grit.core.edge

import grit.core.id.ConversationId
import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.{Clearance, Visibility}
import grit.dbos.sql.TestTx

/** The edges contract, kept by the in-memory fake. */
object InMemoryEdgesTests extends EdgesContract {

  private val edges = new InMemoryEdges

  protected val requests: ToolRequests = edges
  protected val directory: EdgeDirectory = edges

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def transaction[A](clearance: Clearance, visibility: Visibility)(
      body: (Tx^) ?=> A
  ): A = body(using TestTx.fake(clearance, visibility))

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected def desk(places: Set[Place]): Desk^ = edges.desk(places)

  protected def kill(desk: Desk^): Unit = edges.kill(desk.registration.edge)
}
