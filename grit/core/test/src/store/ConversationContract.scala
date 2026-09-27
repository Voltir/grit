package grit.core.store

import grit.core.id.{ConversationId, PrincipalId}
import grit.core.place.Directory

import utest.*

/** The contract every [[ConversationStore]] keeps, run against the in-memory fake in core and
  * the SQL store in grit.dbos. Tests share the store, so each names its own origins.
  */
abstract class ConversationContract extends TestSuite {

  protected def conversations: ConversationStore

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation id no conversation has. */
  protected def unknown: ConversationId

  private val someone = PrincipalId.Grit

  private def tui(session: String): Origin =
    Origin.Tui(
      Directory.of("/contract/dir").getOrElse(throw new java.lang.AssertionError()),
      session
    )

  val tests = Tests {
    test("an origin found again is the same conversation, still created by its first finder") {
      val first = transaction(conversations.findOrCreate(tui("again"), PrincipalId.Local))
      val second = transaction(conversations.findOrCreate(tui("again"), someone))
      second.map(c => (c.id, c.createdBy)) ==> first.map(c => (c.id, PrincipalId.Local))
    }

    test("get finds a conversation by its id, with its origin and creator") {
      val created = transaction(conversations.findOrCreate(tui("got"), PrincipalId.Local))
      val found = created.flatMap(c => transaction(conversations.get(c.id)))
      found.map(_.map(c => (c.origin, c.createdBy))) ==> Right(
        Some((tui("got"), PrincipalId.Local))
      )
    }

    test("get of an id no conversation has is None") {
      transaction(conversations.get(unknown)) ==> Right(None)
    }
  }
}
