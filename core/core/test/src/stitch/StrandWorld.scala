package grit.core.stitch

import java.time.Instant

import grit.core.id.{ConversationId, EntryId}
import grit.core.identity.TestAccounts
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.store.{
  Conversation,
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx

/** Rooms of Slack threads in memory, for a strand's readers' tests: threads begun, messages
  * said `seconds` after `start`, and a thread's first message placed as following
  * a root, read through [[shelf]] and [[excerpts]].
  */
final class StrandWorld(start: Instant = StrandWorld.At) {
  import StrandWorld.right

  private def at(seconds: Long): Instant = start.plusSeconds(seconds)

  val entries = new InMemoryEntryStore
  val conversations = new InMemoryConversationStore(Some(entries))
  private def originOf(c: ConversationId): Origin =
    conversations.all
      .find(_.id == c)
      .fold(Origin.Task("none", ConversationId.value(c)))(_.origin)
  val periods = new InMemoryPeriodStore(entries, originOf)
  val stitches = new InMemoryStitchStore(entries, originOf)
  val shelf: StrandShelf = StrandShelf.over(stitches, conversations)
  val excerpts: StrandExcerpts = StrandExcerpts.over(stitches, new InMemoryPrincipals)

  def thread(channel: String, ts: String): Conversation = right(
    conversations.findOrCreate(
      Origin.Slack("T", channel, ts),
      TestAccounts.account("slack:T/U1"),
      Label.Public
    )(using TestTx.fake)
  )

  def say(
      c: Conversation,
      text: String,
      seconds: Long,
      integrity: Integrity = Integrity.Member
  ): Entry = {
    given Tx = TestTx.fake
    val next = right(entries.lockNext(c.id))
    right(periods.openFor(c.id, next.turnSeq, at(seconds)))
    val e = Entry(
      EntryId(s"${ConversationId.value(c.id)}:${next.seq}"),
      c.id,
      next.turnSeq,
      None,
      next.seq,
      Payload.Heard(text),
      at(seconds),
      integrity
    )
    right(entries.insert(e))
    e
  }

  def follow(first: Entry, root: Conversation): Unit = {
    val placed = Placed.Follows(
      root.id,
      Probability.clamped(0.9),
      Seen(ujson.Obj(), Vector.empty, Tuning.Default),
      "jev",
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None)
    )
    right(stitches.record(first.id, placed, first.createdAt)(using TestTx.fake))
  }

  def of(root: Conversation, asOf: Instant)(using Tx): Vector[Strand.Digested] =
    right(shelf.of(Vector(root.id), asOf))
}

object StrandWorld {

  val At: Instant = Instant.parse("2026-10-12T09:00:00Z")

  def at(seconds: Long): Instant = At.plusSeconds(seconds)

  def right[A](e: Either[StoreError, A]): A =
    e.fold(x => throw new java.lang.AssertionError(s"store failed: $x"), identity)
}
