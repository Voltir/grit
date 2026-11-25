package grit.core.act

import scala.concurrent.duration.*

import grit.core.agent.SubagentFixtures.{
  coder,
  coding,
  ok,
  scholar,
  scratch,
  services,
  spendCap,
  subagent
}
import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.message.Cost
import grit.core.place.{Repository, WorkName}
import grit.core.spend.SpendCap
import grit.core.store.Origin
import grit.core.visibility.{Compartment, Integrity, Label, Level}

import utest.*

/** A child's acting narrowed from its parent's, and each refusal of it. */
object DelegationTests extends TestSuite {

  private val parent = Acting(
    TurnRef(ConversationId("parent"), TurnSeq.First),
    ActsFor.Asker,
    Allowance.Admitted,
    Gates.Asker(1.minute),
    Integrity.Authority
  )
  private val child = TurnRef(ConversationId("child"), TurnSeq.First)
  private val working = Origin.Work(scratch, ok(WorkName.of("fix")))
  private val slack = Origin.Slack("t", "c", "1.0")
  private val scholarType = ok(subagent(scholar, services, Integrity.Outside))
  private val coderType = ok(subagent(coder, coding, Integrity.Member))
  private val internal = Label.at(Level.Internal)

  private def capped(usd: String): Acting =
    parent.copy(allowance = Allowance.Capped(ok(SpendCap.of(BigDecimal(usd)))))

  private def usd(s: String): Cost = Cost.Exact(BigDecimal(s))

  /** `as` narrowed from `from`, briefed at `brief`, the parent at `parentAt` and its room
    * receiving `room`, from `origin`, having spent `spent`.
    */
  private def narrowed(
      as: grit.core.agent.Subagent,
      from: Acting = parent,
      brief: Integrity = Integrity.Member,
      parentAt: Label = Label.Public,
      room: Label = internal,
      origin: Origin = working,
      spent: Cost = Cost.Zero
  ): Either[Delegation.Refused, Acting] =
    Delegation.narrowed(from, as, brief, parentAt, room, origin, spent).map(_(child))

  val tests = Tests {
    test(
      "a child of a services type, from any origin, acts as its parent acts for and at its gates, believed at the meet of its brief, held to its type's spend cap"
    ) {
      narrowed(scholarType, brief = Integrity.Outside, origin = slack) ==> Right(
        Acting(
          child,
          parent.actsFor,
          Allowance.Capped(spendCap),
          parent.gates,
          Integrity.Outside
        )
      )
    }

    test("a type requiring Member, briefed from outside, is Below") {
      narrowed(coderType, brief = Integrity.Outside) ==>
        Left(Delegation.Refused.Below(Integrity.Member, Integrity.Outside))
    }

    test("a parent whose integrity is below the type's needs is Below, whatever its brief") {
      narrowed(coderType, from = parent.copy(integrity = Integrity.Outside)) ==>
        Left(Delegation.Refused.Below(Integrity.Member, Integrity.Outside))
    }

    test("a parent classified above what the child's room may receive is Above") {
      narrowed(scholarType, parentAt = Label.at(Level.Confidential)) ==>
        Left(Delegation.Refused.Above(Label.at(Level.Confidential), internal))
    }

    test("a parent's label the room's neither dominates nor is dominated by is Above") {
      val client = Label.at(Level.Public, ok(Compartment.of("client")))
      narrowed(scholarType, parentAt = client) ==> Left(Delegation.Refused.Above(client, internal))
    }

    test("a coding type is NotWorking unless its parent is a run of work in its repository") {
      val elsewhere = Origin.Work(ok(Repository.of("other")), ok(WorkName.of("fix")))
      (
        narrowed(coderType, origin = slack),
        narrowed(coderType, origin = elsewhere),
        narrowed(coderType).map(_.allowance)
      ) ==> (
        Left(Delegation.Refused.NotWorking(scratch)),
        Left(Delegation.Refused.NotWorking(scratch)),
        Right(Allowance.Capped(spendCap))
      )
    }

    test("a capped parent with less left than the type's spend cap is OverCap; with as much, not") {
      val cap = ok(SpendCap.of(BigDecimal("0.10")))
      (
        narrowed(scholarType, from = capped("0.10"), spent = usd("0.06")),
        narrowed(scholarType, from = capped("0.10"), spent = usd("0.05")).map(_.turn)
      ) ==> (Left(Delegation.Refused.OverCap(spendCap, cap, usd("0.06"))), Right(child))
    }

    test("a capped parent whose spend was not all priced is OverCap, however little it shows") {
      val cap = ok(SpendCap.of(BigDecimal("1")))
      val unpriced = Cost.AtLeast(BigDecimal(0))
      narrowed(scholarType, from = capped("1"), spent = unpriced) ==>
        Left(Delegation.Refused.OverCap(spendCap, cap, unpriced))
    }

    test("a parent not held to a spend cap is not refused for what it spent") {
      narrowed(scholarType, spent = usd("100")).map(_.turn) ==> Right(child)
    }

    test("refusals come in order: Below before Above before NotWorking before OverCap") {
      narrowed(
        coderType,
        from = capped("0.01"),
        brief = Integrity.Outside,
        parentAt = Label.at(Level.Restricted),
        origin = slack,
        spent = usd("1")
      ) ==> Left(Delegation.Refused.Below(Integrity.Member, Integrity.Outside))
      narrowed(
        coderType,
        from = capped("0.01"),
        parentAt = Label.at(Level.Restricted),
        origin = slack,
        spent = usd("1")
      ) ==> Left(Delegation.Refused.Above(Label.at(Level.Restricted), internal))
      narrowed(coderType, from = capped("0.01"), origin = slack, spent = usd("1")) ==>
        Left(Delegation.Refused.NotWorking(scratch))
    }
  }
}
