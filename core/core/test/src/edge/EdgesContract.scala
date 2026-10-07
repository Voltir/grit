package grit.core.edge

import scala.concurrent.duration.*

import grit.core.id.{CallSlot, ConversationId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.prompt.{Fragment, Layer}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}

import utest.*

/** The contract every [[ToolRequests]], [[EdgeDirectory]] and [[Desk]] keeps together, run
  * against the in-memory fake in core and the SQL implementations in grit.dbos. Tests share
  * the database, so each uses places of its own.
  */
abstract class EdgesContract extends TestSuite {

  protected def requests: ToolRequests

  protected def directory: EdgeDirectory

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation requests may name, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  /** A live edge for [[PrincipalId.Local]] hosting `places`, with a desk of its own. */
  protected def desk(places: Set[Place]): Desk^

  /** Ends `desk`'s edge as a crash would: it is no longer live. */
  protected def kill(desk: Desk^): Unit

  private def place(name: String): Place =
    Place.of(Directory.of(s"/contract/$name").getOrElse(throw new java.lang.AssertionError(name)))

  private def request(
      name: String,
      index: Int,
      at: Place,
      retry: Retry = Retry.Rerun,
      permit: Permit = Permit.Free
  ): ToolRequest = {
    val turn = TurnRef(conversation(name), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, index).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      at,
      PrincipalId.Local,
      if (permit == Permit.Free) ToolName("read") else ToolName("write"),
      permit,
      retry,
      ujson.Obj("path" -> "a.txt"),
      Set.empty,
      None
    )
  }

  private def dispatched(rs: ToolRequest*): Unit =
    transaction(requests.dispatch(rs.toVector)) ==> Right(())

  private def keys(
      listed: Either[DeskError, Vector[ToolRequest]]
  ): Either[DeskError, Vector[String]] =
    listed.map(_.map(_.slot.key))

  val tests = Tests {
    test("a dispatched request is open to a desk hosting its place, and to no other") {
      val here = place("open-here")
      val r = request("open", 0, here)
      val hosting = desk(Set(here))
      val elsewhere = desk(Set(place("open-elsewhere")))
      dispatched(r)
      (keys(hosting.open()), keys(elsewhere.open())) ==> (
        Right(Vector(r.slot.key)),
        Right(Vector())
      )
    }

    test("the first claim wins, and a second claim of it is refused") {
      val here = place("claim")
      val r = request("claim", 0, here)
      val one = desk(Set(here))
      val two = desk(Set(here))
      dispatched(r)
      (one.claim(r), two.claim(r)) ==> (Right(true), Right(false))
      keys(one.open()) ==> Right(Vector())
    }

    test("dispatching a key again keeps the first request") {
      val here = place("again")
      val r = request("again", 0, here)
      dispatched(r, r.copy(arguments = ujson.Obj("path" -> "b.txt")))
      desk(Set(here)).open().map(_.map(_.arguments)) ==> Right(Vector(ujson.Obj("path" -> "a.txt")))
    }

    test("settle expires an unclaimed request, which can then not be claimed") {
      val here = place("expire")
      val r = request("expire", 0, here)
      val d = desk(Set(here))
      dispatched(r)
      transaction(requests.settle(r.slot)) ==> Right(RequestState.Expired)
      d.claim(r) ==> Right(false)
    }

    test("settle leaves a claimed request claimed, and gives an answered one's outcome") {
      val here = place("settle")
      val claimed = request("settle", 0, here)
      val answered = request("settle", 1, here)
      val d = desk(Set(here))
      dispatched(claimed, answered)
      (d.claim(claimed), d.claim(answered)) ==> (Right(true), Right(true))
      d.answer(answered.slot, Outcome.Done("text")) ==> Right(true)
      transaction(requests.settle(claimed.slot)) ==> Right(RequestState.Claimed)
      transaction(requests.settle(answered.slot)) ==> Right(
        RequestState.Answered(Outcome.Done("text"))
      )
    }

    test("abandon expires a claimed request, and its edge's answer then fails") {
      val here = place("abandon")
      val r = request("abandon", 0, here)
      val d = desk(Set(here))
      dispatched(r)
      d.claim(r) ==> Right(true)
      transaction(requests.abandon(r.slot)) ==> Right(RequestState.Expired)
      d.answer(r.slot, Outcome.Done("late")) ==> Right(false)
      transaction(requests.settle(r.slot)) ==> Right(RequestState.Expired)
    }

    test("abandon keeps an answer that came first") {
      val here = place("abandon-answered")
      val r = request("abandon-answered", 0, here)
      val d = desk(Set(here))
      dispatched(r)
      d.claim(r) ==> Right(true)
      d.answer(r.slot, Outcome.Done("first")) ==> Right(true)
      transaction(requests.abandon(r.slot)) ==> Right(RequestState.Answered(Outcome.Done("first")))
    }

    test("a key no request has is Invalid to settle and to answered, naming it") {
      val slot = request("none", 9, place("none")).slot
      transaction(requests.settle(slot)) ==>
        Left(StoreError.Invalid(s"no tool request ${slot.key}"))
      transaction(requests.answered(slot)) ==>
        Left(StoreError.Invalid(s"no tool request ${slot.key}"))
    }

    test("answered is an answered request's outcome, and None while open, claimed or expired") {
      val here = place("answered")
      val open = request("answered", 0, here)
      val claimed = request("answered", 1, here)
      val answered = request("answered", 2, here)
      val expired = request("answered", 3, here)
      val d = desk(Set(here))
      dispatched(open, claimed, answered, expired)
      (d.claim(claimed), d.claim(answered)) ==> (Right(true), Right(true))
      d.answer(answered.slot, Outcome.Done("text")) ==> Right(true)
      transaction(requests.settle(expired.slot)) ==> Right(RequestState.Expired)
      Vector(open, claimed, answered, expired).map(r => transaction(requests.answered(r.slot))) ==>
        Vector(Right(None), Right(None), Right(Some(Outcome.Done("text"))), Right(None))
    }

    test("an orphan declared Interrupt is answered Interrupted, and never run again") {
      val here = place("orphan-interrupt")
      // A free tool that interrupts: a rule read from the permit, not the retry, fails here.
      val r = request("orphan-interrupt", 0, here, Retry.Interrupt, Permit.Free)
      val dead = desk(Set(here))
      dispatched(r)
      dead.claim(r) ==> Right(true)
      kill(dead)
      val next = desk(Set(here))
      keys(next.orphans()) ==> Right(Vector())
      transaction(requests.settle(r.slot)) ==> Right(RequestState.Answered(Outcome.Interrupted))
    }

    test(
      "an orphan declared Rerun is claimed again by the edge that finds it, never open between"
    ) {
      val here = place("orphan-rerun")
      // A gated tool that reruns: a rule read from the permit, not the retry, fails here.
      val r = request("orphan-rerun", 0, here, Retry.Rerun, Permit.Approved)
      val dead = desk(Set(here))
      dispatched(r)
      dead.claim(r) ==> Right(true)
      kill(dead)
      val next = desk(Set(here))
      val bystander = desk(Set(here))
      keys(next.orphans()) ==> Right(Vector(r.slot.key))
      keys(bystander.open()) ==> Right(Vector())
      transaction(requests.settle(r.slot)) ==> Right(RequestState.Claimed)
      next.answer(r.slot, Outcome.Done("again")) ==> Right(true)
    }

    test("a live edge's own claims are not orphans") {
      val here = place("not-orphan")
      val r = request("not-orphan", 0, here, Retry.Interrupt)
      val live = desk(Set(here))
      dispatched(r)
      live.claim(r) ==> Right(true)
      keys(desk(Set(here)).orphans()) ==> Right(Vector())
      transaction(requests.settle(r.slot)) ==> Right(RequestState.Claimed)
    }

    // Pins a known gap, not a wanted behaviour (SqlDesk.claim's note): once a claim requires
    // its edge's lock, this turns into a killed desk's claim refused.
    test("a desk's claim does not ask whether its edge is live: a killed desk's claim wins") {
      val here = place("claim-killed")
      val r = request("claim-killed", 0, here)
      val gone = desk(Set(here))
      dispatched(r)
      kill(gone)
      gone.claim(r) ==> Right(true)
      transaction(requests.settle(r.slot)) ==> Right(RequestState.Claimed)
    }

    test("serving names what the live edge offers in a place, and nothing once it is gone") {
      val here = place("serving")
      val tools = ToolSet
        .of(Vector(ToolSet.Entry(ToolName("read"), "Reads.", ujson.Obj(), false, Retry.Rerun)))
        .getOrElse(ToolSet.Empty)
      val file = Fragment(
        Layer.Place,
        "/contract/serving/AGENTS.md",
        "Instructions from /contract/serving/AGENTS.md:\n\nBe brief."
      )
      val d = desk(Set(here))
      d.advertise(here, tools, Vector(file)) ==> Right(())
      transaction(directory.serving(here)).map(_.map(a => (a.edge, a.tools, a.instructions))) ==>
        Right(Some((d.registration.edge, tools.id, Vector(file.id))))
      kill(d)
      transaction(directory.serving(here)) ==> Right(None)
    }

    test(
      "await is false when nothing arrives within its wait, and true once a request is dispatched to its place"
    ) {
      val here = place("await")
      val r = request("await", 0, here)
      val d = desk(Set(here))
      d.await(200.millis) ==> false
      dispatched(r)
      d.await(10.seconds) ==> true
    }
  }
}
