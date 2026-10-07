package grit.core.recipe

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, EntryId, PrincipalId}
import grit.core.identity.{Account, TestAccounts}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.place.{Namespace, Place, Prefix, Scope}
import grit.core.stitch.{
  InMemoryStitchStore,
  Link,
  Offered as Why,
  Placed,
  Seen,
  StitchFixtures,
  StitchStore,
  Strand,
  Tuning
}
import grit.core.store.{
  Conversation,
  Entry,
  InMemoryEntryStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.Label
import grit.dbos.sql.TestTx

import utest.*

/** [[Pool.read]]: what each source finds in a room's stores for a heard message, end to end. */
object PoolReadTests extends TestSuite {

  private val T = Instant.parse("2026-10-02T12:00:00Z")

  private val ana = TestAccounts.account("slack:T/UA")
  private val ben = TestAccounts.account("slack:T/UB")
  private val cy = TestAccounts.account("slack:T/UC")

  private val usage = Usage(Tokens(900), Tokens.Zero, Tokens.Zero, None)

  private def right[A](e: Either[StoreError, A]): A =
    e.fold(x => throw new java.lang.AssertionError(s"store failed: $x"), identity)

  private final class World {
    given Tx = TestTx.fake
    val entries = new InMemoryEntryStore
    val principals = new InMemoryPrincipals
    @caps.unsafe.untrackedCaptures
    var origins = Map.empty[ConversationId, Origin]
    private def originOf(c: ConversationId): Origin =
      origins.getOrElse(c, Origin.Task("unknown", ConversationId.value(c)))
    val stitches = new InMemoryStitchStore(entries, originOf)
    val rooms = new InMemoryRoomReads(entries, originOf, principals)

    right(principals.name(ana, "Ana"))
    right(principals.name(ben, "Ben"))
    right(principals.name(cy, "Cy"))

    /** Thread `ts` of `channel`. */
    def thread(ts: String, channel: String = "C"): Conversation = {
      val origin = Origin.Slack("T", channel, ts)
      val id = ConversationId(origin.place.written)
      origins = origins.updated(id, origin)
      Conversation(id, origin, Account.Local, T.minusSeconds(86_400), Label.Public)
    }

    /** `payload` as `c`'s next entry, said `at`, written by `by` when given. */
    def say(c: Conversation, payload: Payload, at: Instant, by: Option[Account]): Entry = {
      val next = right(entries.lockNext(c.id))
      val e = Entry(
        EntryId(s"${ConversationId.value(c.id)}:${next.seq}"),
        c.id,
        next.turnSeq,
        None,
        next.seq,
        payload,
        at
      )
      right(entries.insert(e))
      by.foreach(principals.authored(e.id, _))
      e
    }

    def heard(c: Conversation, text: String, at: Instant, by: Account): Entry =
      say(c, Payload.Heard(text), at, Some(by))

    /** What `pool` shows for `heard` in `c`, its strand `strand`, by section key. */
    def shown(
        pool: Pool,
        c: Conversation,
        heard: Entry,
        strand: Strand.Read = Strand.Read.empty,
        scope: Scope = Scope.Everywhere
    ) =
      right(Pool.read(pool, scope, rooms, stitches, principals, c, strand, heard)).toVector
        .map((s, text) => (s.key, text))
  }

  private def minutes(n: Long) = T.minusSeconds(n * 60)

  val tests = Tests {
    test("Channel: the room's messages in [t − within, t), none its thread shows, named") {
      val w = new World
      val own = w.thread("1.0")
      val member = w.thread("2.0")
      val other = w.thread("3.0")
      val elsewhere = w.thread("1.0", channel = "D")
      w.heard(own, "an earlier one of mine", minutes(4), ana)
      val heard = w.heard(own, "standup?", T, ana)
      w.heard(other, "too old", minutes(11), ben)
      w.heard(other, "lunch?", minutes(5), ben)
      val reply =
        Message.Assistant(Vector(AssistantBlock.Text("it moved")), StopReason.EndTurn, usage, "m")
      w.say(other, Payload.Message(reply), minutes(3), None)
      w.heard(member, "in the strand", minutes(2), ben)
      w.heard(elsewhere, "in another channel", minutes(2), ben)
      w.heard(other, "who's asking?", T.minusMillis(1), TestAccounts.account("slack:T/UX"))
      w.heard(other, "at t", T, ben)
      w.heard(other, "after t", T.plusMillis(1), ben)
      val strand = Strand.Read(None, Vector.empty, Vector.empty, Set(member.id))
      w.shown(Pool(Vector(Source.Channel(10.minutes, 5)), 1_000), own, heard, strand) ==> Vector(
        "nearby_in_channel" -> Vector(
          "Ben, 5 minutes before: lunch?",
          "Assistant, 3 minutes before: it moved",
          "Someone, 0 seconds before: who's asking?"
        ).mkString("\n")
      )
    }

    test("Author: what the heard message's author said; none when it has no author") {
      val w = new World
      val own = w.thread("1.0")
      val other = w.thread("2.0")
      w.heard(other, "the contract term?", minutes(2), ana)
      w.heard(other, "lunch?", minutes(1), ben)
      w.heard(own, "my own thread", minutes(1), ana)
      val heard = w.heard(own, "standup?", T, ana)
      val pool = Pool(Vector(Source.Author(1.hour, 5)), 1_000)
      w.shown(pool, own, heard) ==>
        Vector("nearby_in_channel" -> "Ana, 2 minutes before: the contract term?")
      val toGrit = w.say(own, Payload.Message(Message.User("and you?")), T, None)
      w.shown(pool, own, toGrit) ==> Vector.empty
    }

    test("Exchanges: those offered, as kept, aged from t; the strand's its record alone") {
      val w = new World
      val a = w.thread("1.0")
      val b = w.thread("2.0")
      val gone = w.thread("3.0")
      val late = w.thread("4.0")
      val own = w.thread("5.0")
      w.heard(a, "the contract term?", minutes(120), ben)
      w.heard(b, "lunch?", minutes(40), cy)
      w.heard(late, "at t", T, cy)
      val opening = w.heard(own, "is this real?", minutes(30), ana)
      val heard = w.heard(own, "and another", T, ana)
      // As stitching showed them when the opening was placed: ages from its time, not t.
      def exchange(key: String, from: String, text: String, record: Option[String]) = ujson.Obj(
        "key" -> key,
        "opening" -> ujson.Obj("from" -> from, "text" -> text, "ago" -> "as placed"),
        "latest" -> ujson.Arr(),
        "record" -> record.fold[ujson.Value](ujson.Null)(ujson.Str(_))
      )
      val seen = Seen(
        ujson.Obj(
          "exchanges" -> ujson.Arr(
            exchange("exchange 1", "Ben", "the contract term?", Some("term: 3 years")),
            exchange("exchange 2", "Cy", "lunch?", None),
            exchange("exchange 3", "Cy", "never kept", None),
            exchange("exchange 4", "Cy", "at t", None)
          )
        ),
        Vector(a, b, gone, late).map(c => Seen.Offer(c.id, Why.Recent(1), None)),
        Tuning.Default
      )
      val placed: Placed = StitchFixtures.placed(Some(a.id), seen)
      right(w.stitches.record(opening.id, placed, minutes(29))(using TestTx.fake)) ==> true
      val strand = Strand.Read(None, Vector.empty, Vector.empty, Set(a.id))
      val pool = Pool(Vector(Source.Exchanges), 1_000)
      w.shown(pool, own, heard, strand) ==> Vector(
        "exchanges_in_channel" -> Vector(
          "Record of the exchange this thread continues: term: 3 years",
          "Exchange opened 40 minutes before by Cy: lunch?"
        ).mkString("\n")
      )
      val unplaced = w.thread("6.0")
      w.shown(pool, unplaced, w.heard(unplaced, "hi", T, ana)) ==> Vector.empty
    }

    test("a pool without sources reads nothing; one with them fails when its store does") {
      val w = new World
      val own = w.thread("1.0")
      val heard = w.heard(own, "standup?", T, ana)
      val down = StoreError.DatabaseError("down")
      def read(pool: Pool) =
        Pool.read(
          pool,
          Scope.Everywhere,
          Down.rooms(down),
          Down.stitches(down),
          w.principals,
          own,
          Strand.Read.empty,
          heard
        )(using
          TestTx.fake
        )
      read(Pool.empty) ==> Right(scala.collection.immutable.VectorMap.empty)
      read(Pool(Vector(Source.Channel(1.hour, 5)), 100)) ==> Left(down)
      read(Pool(Vector(Source.Author(1.hour, 5)), 100)) ==> Left(down)
      read(Pool(Vector(Source.Exchanges), 100)) ==> Left(down)
    }

    test("a scope that does not hold the heard message's room shows nothing, reading nothing") {
      val w = new World
      val own = w.thread("1.0")
      val other = w.thread("2.0")
      w.heard(other, "lunch?", minutes(2), ana)
      val heard = w.heard(own, "standup?", T, ana)
      val pool = Pool(Vector(Source.Channel(1.hour, 5), Source.Author(1.hour, 5)), 1_000)
      val nearby = Vector("nearby_in_channel" -> "Ana, 2 minutes before: lunch?")
      def at(channel: String) =
        Scope(Vector(Prefix.At(Place.under(Namespace.Slack, Vector("T", channel)))))
      w.shown(pool, own, heard, scope = Scope.Room) ==> nearby
      w.shown(pool, own, heard, scope = at("C")) ==> nearby
      w.shown(pool, own, heard, scope = at("D")) ==> Vector.empty
      w.shown(pool, own, heard, scope = Scope.Off) ==> Vector.empty
      val down = StoreError.DatabaseError("down")
      Pool.read(
        Pool(Vector(Source.Exchanges), 100),
        Scope.Off,
        Down.rooms(down),
        Down.stitches(down),
        w.principals,
        own,
        Strand.Read.empty,
        heard
      )(using TestTx.fake) ==> Right(scala.collection.immutable.VectorMap.empty)
    }
  }

  /** Stores that fail every read with `error`. */
  private object Down {
    def rooms(error: StoreError): RoomReads = new RoomReads {
      def said(room: Place, from: Instant, until: Instant, outside: Set[ConversationId], most: Int)(
          using Tx^
      ) = Left(error)
      def saidBy(
          room: Place,
          author: PrincipalId,
          from: Instant,
          until: Instant,
          outside: Set[ConversationId],
          most: Int
      )(using Tx^) = Left(error)
      def author(entry: EntryId)(using Tx^) = Left(error)
    }

    def stitches(error: StoreError): StitchStore = new StitchStore {
      def record(root: EntryId, placed: Placed, at: Instant)(using Tx^) = Left(error)
      def placed(root: EntryId)(using Tx^) = Left(error)
      def spokenIn(room: Place, from: Instant, until: Instant)(using Tx^) = Left(error)
      def said(conversations: Vector[ConversationId], from: Instant, until: Instant)(using Tx^) =
        Left(error)
      def openings(conversations: Vector[ConversationId])(using Tx^) = Left(error)
      def links(conversations: Vector[ConversationId])(using
          Tx^
      ): Either[StoreError, Vector[Link]] =
        Left(error)
    }
  }
}
