package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, PrincipalId, TurnSeq}
import grit.core.message.Message
import grit.core.place.Directory

import utest.*

/** The contract every [[ConversationStore]] keeps, run against the in-memory fake in core and
  * the SQL store in grit.dbos. Tests share the store, so each names its own origins.
  */
abstract class ConversationContract extends TestSuite {

  protected def conversations: ConversationStore

  /** The entry store [[conversations]]' entries are in. */
  protected def entries: EntryStore

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

    test("find: the conversation an origin was created as, and none for one never used") {
      val created = transaction(conversations.findOrCreate(tui("found"), PrincipalId.Local))
      transaction(conversations.find(tui("found"))) ==> created.map(Some(_))
      transaction(conversations.find(tui("never"))) ==> Right(None)
      transaction(conversations.find(tui("never"))) ==> Right(None) // and find never creates
    }

    test("a removed conversation's entries go with it, and its next positions start over") {
      val c = transaction(conversations.findOrCreate(tui("removed-entries"), PrincipalId.Local))
        .fold(e => throw new java.lang.AssertionError(e.toString), _.id)
      val hi = EntryId(s"${ConversationId.value(c)}:hi")
      transaction {
        entries.lockNext(c).flatMap { next =>
          entries.insert(
            Entry(
              hi,
              c,
              next.turnSeq,
              None,
              next.seq,
              Payload.Message(Message.User("hi")),
              Instant.EPOCH
            )
          )
        }
      } ==> Right(())
      transaction(entries.lockNext(c)) ==> Right(EntryStore.Next(TurnSeq(1), EntrySeq(1)))
      transaction(conversations.remove(c)) ==> Right(())
      transaction(entries.list(c)) ==> Right(Vector())
      transaction(entries.lockNext(c)) ==> Right(EntryStore.Next(TurnSeq.First, EntrySeq.First))
    }

    test("get of an id no conversation has is None") {
      transaction(conversations.get(unknown)) ==> Right(None)
    }

    test("a removed conversation is gone, its origin free, and removing it again is harmless") {
      val removed = transaction(conversations.findOrCreate(tui("removed"), PrincipalId.Local))
      removed.map(c => transaction(conversations.remove(c.id))) ==> Right(Right(()))
      removed.map(c => transaction(conversations.remove(c.id))) ==> Right(Right(()))
      removed.map(c => transaction(conversations.get(c.id))) ==> Right(Right(None))
      transaction(conversations.find(tui("removed"))) ==> Right(None)
      val again = transaction(conversations.findOrCreate(tui("removed"), someone))
      again.map(_.createdBy) ==> Right(someone)
    }

    test("a conversation made after a removal takes no live conversation's id") {
      val gone = transaction(conversations.findOrCreate(tui("gone"), PrincipalId.Local))
      val kept = transaction(conversations.findOrCreate(tui("kept"), PrincipalId.Local))
      gone.map(c => transaction(conversations.remove(c.id))) ==> Right(Right(()))
      val next = transaction(conversations.findOrCreate(tui("next"), PrincipalId.Local))
      (
        kept.flatMap(c => transaction(conversations.get(c.id))).map(_.map(_.origin)),
        next.flatMap(c => transaction(conversations.get(c.id))).map(_.map(_.origin))
      ) ==> (Right(Some(tui("kept"))), Right(Some(tui("next"))))
    }
  }
}
