package grit.core.stitch

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.store.{Entry, EntryStore, Origin, Payload, PeriodStore, StoreError, Tx}

import utest.*

/** The contract every [[StitchStore]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store's database, so each names its own channel.
  */
abstract class StitchContract extends TestSuite {

  protected def entries: EntryStore

  /** The periods of those entries, whose purge deletes them. */
  protected def periods: PeriodStore

  /** The stitch store under test, over [[entries]]. */
  protected def stitches: StitchStore

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** The conversation at `origin`, the same one for the same origin. */
  protected def conversation(origin: Origin): ConversationId

  private val At = Instant.parse("2026-09-30T22:00:00Z")

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private val usage = Usage(Tokens(900), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.00004")))

  private val tuning = Tuning.Default

  private def seen(root: ConversationId) = Seen(
    ujson.Obj("new_message" -> "real?"),
    Vector(Seen.Offer(root, Offered.Lexical(2.5), Some(Probability.clamped(0.8)))),
    tuning
  )

  private def follows(root: ConversationId): Placed =
    Placed.Follows(root, Probability.clamped(0.8), seen(root), "jev", usage)

  private def begins(root: ConversationId): Placed =
    Placed.Begins(Probability.clamped(0.2), seen(root), "jev", usage)

  private val reply: Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text("it moved")), StopReason.EndTurn, usage, "m")

  private def thread(channel: String, ts: String): ConversationId =
    conversation(Origin.Slack("T", channel, ts))

  private def room(channel: String) = Place.under(Namespace.Slack, Vector("T", channel))

  /** `payload` as `c`'s next turn, said `seconds` after `At`, its period opened as the inbox
    * opens one.
    */
  private def say(c: ConversationId, payload: Payload, seconds: Long): Entry = transaction {
    val next = right(entries.lockNext(c))
    right(periods.openFor(c, next.turnSeq, At.plusSeconds(seconds)))
    val e = Entry(
      EntryId(s"${ConversationId.value(c)}:${next.seq}"),
      c,
      next.turnSeq,
      None,
      next.seq,
      payload,
      At.plusSeconds(seconds)
    )
    right(entries.insert(e))
    e
  }

  val tests = Tests {
    test("a placement is kept once, for a conversation's first entry alone") {
      val root = thread("once", "1.0")
      val c = thread("once", "2.0")
      val first = say(c, Payload.Heard("real?"), 10)
      val second = say(c, Payload.Heard("lol"), 20)
      transaction(stitches.record(first.id, follows(root), At)) ==> Right(true)
      transaction(stitches.record(first.id, begins(root), At)) ==> Right(false)
      transaction(stitches.placed(first.id)) ==> Right(Some(follows(root)))
      transaction(stitches.record(second.id, begins(root), At)) match {
        case Left(StoreError.Invalid(_)) => ()
        case other => throw new java.lang.AssertionError(s"not refused: $other")
      }
      transaction(stitches.placed(second.id)) ==> Right(None)
    }

    test("a room speaks its messages and closings in range; never a draft, nor another room's") {
      val a = thread("spoken", "1.0")
      val b = thread("spoken", "2.0")
      val elsewhere = thread("spoken-other", "1.0")
      val heard = say(a, Payload.Heard("the Engine contract term?"), 10)
      val asked = say(a, Payload.Message(Message.User("and the date?")), 20)
      val replied = say(a, Payload.Message(reply), 30)
      say(a, Payload.Draft(reply), 40)
      say(a, Payload.Summary("a summary"), 45)
      val late = say(b, Payload.Heard("lunch?"), 60)
      say(elsewhere, Payload.Heard("elsewhere"), 30)
      val period = right(transaction(periods.of(TurnRef(b, late.turnSeq))))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      right(
        transaction(
          periods.seal(
            CloseRef(period, late.turnSeq, At.plusSeconds(70)),
            CloseReason.Lapsed,
            TestClosings.prose("Someone asked about lunch."),
            At.plusSeconds(70)
          )
        )
      )
      val spoken = right(transaction(stitches.spokenIn(room("spoken"), At, At.plusSeconds(3_600))))
      spoken.filter(_.conversation == a).map(_.entry) ==> Vector(heard, asked, replied)
      spoken
        .filter(_.conversation == b)
        .map(_.entry.payload match {
          case Payload.Closed(_, _, closing) => closing.headline
          case p => p.said.getOrElse("")
        }) ==> Vector("lunch?", "Someone asked about lunch.")
      spoken.map(_.place).distinct.sortBy(_.written) ==>
        Vector(Origin.Slack("T", "spoken", "1.0").place, Origin.Slack("T", "spoken", "2.0").place)
      right(transaction(stitches.spokenIn(room("spoken"), At.plusSeconds(20), At.plusSeconds(30))))
        .map(_.entry) ==> Vector(asked)
    }

    test("grit's post a thread begins with is its opening, said there and in its room") {
      val c = thread("posted", "1.0")
      val post = say(c, Payload.Posted("The engine's open issues."), 10)
      val reply = say(c, Payload.Heard("why this?"), 20)
      right(transaction(stitches.openings(Vector(c)))).map(_.entry) ==> Vector(post)
      right(transaction(stitches.said(Vector(c), At, At.plusSeconds(3_600)))).map(_.entry) ==>
        Vector(post, reply)
      right(transaction(stitches.spokenIn(room("posted"), At, At.plusSeconds(3_600))))
        .map(_.entry) ==> Vector(post, reply)
    }

    test("conversations say their messages, never their closings; openings are first messages") {
      val a = thread("said", "1.0")
      val b = thread("said", "2.0")
      val opening = say(a, Payload.Heard("the Engine contract term?"), 10)
      val answer = say(a, Payload.Message(reply), 20)
      val other = say(b, Payload.Heard("lunch?"), 30)
      val period = right(transaction(periods.of(TurnRef(a, answer.turnSeq))))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      // Sealed inside the range, so its closing entry is there to be left out.
      right(
        transaction(
          periods.seal(
            CloseRef(period, answer.turnSeq, At.plusSeconds(25)),
            CloseReason.Lapsed,
            TestClosings.prose("Someone asked about the contract term."),
            At.plusSeconds(25)
          )
        )
      )
      right(transaction(stitches.said(Vector(a), At, At.plusSeconds(3_600)))).map(_.entry) ==>
        Vector(opening, answer)
      right(transaction(stitches.openings(Vector(a, b)))).map(s => (s.conversation, s.entry)) ==>
        Vector((a, opening), (b, other))
    }

    test("a placement that follows is a link, found from either end; one that begins is none") {
      val root = thread("links", "1.0")
      val b = thread("links", "2.0")
      val c = thread("links", "3.0")
      say(root, Payload.Heard("the Engine contract term?"), 10)
      val bFirst = say(b, Payload.Heard("real?"), 20)
      val cFirst = say(c, Payload.Heard("lunch?"), 30)
      right(transaction(stitches.record(bFirst.id, follows(root), At)))
      right(transaction(stitches.record(cFirst.id, begins(root), At)))
      transaction(stitches.links(Vector(root))) ==> Right(Vector(Link(b, root)))
      transaction(stitches.links(Vector(b))) ==> Right(Vector(Link(b, root)))
      transaction(stitches.links(Vector(c))) ==> Right(Vector.empty)
    }

    test("a placement goes with its root entry: a purged one is neither kept nor a link") {
      val root = thread("purged", "1.0")
      val b = thread("purged", "2.0")
      say(root, Payload.Heard("the Engine contract term?"), 10)
      val first = say(b, Payload.Heard("real?"), 20)
      right(transaction(stitches.record(first.id, follows(root), At)))
      val period = right(transaction(periods.of(TurnRef(b, first.turnSeq))))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      transaction {
        for {
          _ <- periods.seal(
            CloseRef(period, first.turnSeq, At),
            CloseReason.Lapsed,
            TestClosings.prose("heard", None),
            At
          )
          _ <- periods.purge(period, At)
        } yield ()
      }
      transaction(stitches.placed(first.id)) ==> Right(None)
      transaction(stitches.links(Vector(root))) ==> Right(Vector.empty)
      transaction(stitches.record(first.id, follows(root), At)) ==> Right(false)
    }
  }
}
