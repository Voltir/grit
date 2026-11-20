package grit.core.stitch

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, SourceId, TurnRef}
import grit.core.identity.{Account, TestAccounts}
import grit.core.inbox.InboundId
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
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

  // Each entry is labelled its conversation's label, as the SQL store labels it.
  val entries: InMemoryEntryStore =
    new InMemoryEntryStore(c => conversations.all.find(_.id == c))
  val conversations = new InMemoryConversationStore(Some(entries))
  private def originOf(c: ConversationId): Origin =
    conversations.all
      .find(_.id == c)
      .fold(Origin.Task("none", ConversationId.value(c)))(_.origin)
  val periods = new InMemoryPeriodStore(entries, originOf)
  val stitches = new InMemoryStitchStore(entries, originOf)
  val principals = new InMemoryPrincipals
  val shelf: StrandShelf = StrandShelf.over(stitches, conversations, principals)
  val excerpts: StrandExcerpts =
    StrandExcerpts.over(stitches, conversations, principals)

  def thread(channel: String, ts: String): Conversation = right(
    conversations.findOrCreate(
      Origin.Slack("T", channel, ts),
      TestAccounts.account("slack:T/U1"),
      Label.Public
    )(using TestTx.fake)
  )

  /** Channel `channel`'s top level. */
  def room(channel: String, label: Label = Label.Public): Conversation = right(
    conversations.findOrCreate(
      Origin.Room("T", channel),
      TestAccounts.account("slack:T/U1"),
      label
    )(using TestTx.fake)
  )

  /** The thread under the message `ts` of channel `channel`'s top level. */
  def under(channel: String, ts: String): Conversation = right(
    conversations.findOrCreate(
      Origin.Thread("T", channel, ts),
      TestAccounts.account("slack:T/U1"),
      Label.Public
    )(using TestTx.fake)
  )

  /** `text` heard in `c` `seconds` after `start` (posted by grit when `posted`), kept under the
    * inbound id of `source` when it has one, and written through `by` when it is given.
    */
  def say(
      c: Conversation,
      text: String,
      seconds: Long,
      integrity: Integrity = Integrity.Member,
      source: Option[String] = None,
      posted: Boolean = false,
      by: Option[Account] = None
  ): Entry = {
    given Tx = TestTx.fake
    val next = right(entries.lockNext(c.id))
    right(periods.openFor(c.id, next.turnSeq, at(seconds)))
    val e = Entry(
      source.fold(EntryId(s"${ConversationId.value(c.id)}:${next.seq}"))(s =>
        InboundId.of(c.id, SourceId(s))
      ),
      c.id,
      next.turnSeq,
      None,
      next.seq,
      if (posted) Payload.Posted(text) else Payload.Heard(text),
      at(seconds),
      integrity
    )
    right(entries.insert(e))
    by.foreach(a => principals.authored(e.id, a))
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

  /** The period `said` is in sealed and purged `seconds` after `start`, as raw retention purges
    * it: its messages gone.
    */
  def purge(said: Entry, seconds: Long): Unit = {
    seal(said, seconds)
    given Tx = TestTx.fake
    val _ = right(periods.purge(period(said), at(seconds)))
  }

  /** The period `said` is in sealed `seconds` after `start`, so the next message opens another. */
  def seal(said: Entry, seconds: Long): Unit = {
    given Tx = TestTx.fake
    val _ = right(
      periods.seal(
        CloseRef(period(said), said.turnSeq, at(seconds)),
        CloseReason.Lapsed,
        TestClosings.prose("purged"),
        at(seconds)
      )
    )
  }

  /** The period `said` is in, sealed already ([[seal]]), purged `seconds` after `start`. */
  def purgeSealed(said: Entry, seconds: Long): Unit = {
    given Tx = TestTx.fake
    val _ = right(periods.purge(period(said), at(seconds)))
  }

  private def period(said: Entry)(using Tx) =
    right(periods.of(TurnRef(said.conversationId, said.turnSeq)))
      .map(_.ref)
      .getOrElse(throw new java.lang.AssertionError(s"no period holds ${said.id}"))

  def of(root: Conversation, asOf: Instant)(using Tx): Vector[Strand.Digested] =
    right(shelf.of(Vector(root.id), asOf))
}

object StrandWorld {

  val At: Instant = Instant.parse("2026-10-12T09:00:00Z")

  def at(seconds: Long): Instant = At.plusSeconds(seconds)

  /** Runs of messages less than 6 hours apart, at most 4, settled in 6 hours. */
  val sixHours: Strand.Cut = Strand.Cut
    .of(java.time.Duration.ofHours(6), 4, java.time.Duration.ofHours(6))
    .fold(e => throw new java.lang.AssertionError(e), identity)

  def right[A](e: Either[StoreError, A]): A =
    e.fold(x => throw new java.lang.AssertionError(s"store failed: $x"), identity)
}
