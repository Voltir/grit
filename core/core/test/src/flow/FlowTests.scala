package grit.core.flow

import grit.core.durable.InMemoryDurable
import grit.core.id.WorkflowId
import grit.core.visibility.Integrity.Level
import grit.core.visibility.{Compartment, Integrity}

import utest.*

object FlowTests extends TestSuite {

  private val team: Compartment =
    Compartment.of("team").getOrElse(throw new java.lang.AssertionError("team is a compartment"))

  /** `taint`'s level after lowering it to each of `to` in turn, outside any step. */
  private def lowered(taint: Taint^, to: Integrity*): Integrity = {
    new InMemoryDurable().run(WorkflowId("flow:1")) { _ =>
      to.foreach(taint.lower(_))
      "lowered"
    }
    taint.level
  }

  val tests = Tests {
    test("zip is believed at the meet of the two, tainted or trusted") {
      val member = Mint.tainted(1, Integrity.Member)
      val outside = Mint.tainted("a", Integrity.Outside)
      (member.zip(outside).integrity, outside.zip(member).integrity) ==>
        (Integrity.Outside, Integrity.Outside)
      Mint.trusted(1, Integrity.Member).zip(Mint.trusted(2, Integrity.Outside)).integrity ==>
        Integrity.Outside
    }

    test("a trusted value made unchecked again keeps its integrity") {
      Mint.trusted(1, Integrity.Member).tainted.integrity ==> Integrity.Member
    }

    test("check trusts a value whose integrity dominates the required one, at its own") {
      Trusted
        .check(Mint.tainted(7, Integrity.Member), Integrity.Outside)
        .map(t => (t.extract, t.integrity)) ==> Right((7, Integrity.Member))
    }

    test("asIs trusts any value at the integrity it is believed at") {
      val low = Trusted.asIs(Mint.tainted(7, Integrity.at(Level.Untrusted, team)))
      (low.extract, low.integrity) ==> (7, Integrity.at(Level.Untrusted, team))
    }

    test("check returns a value below the required integrity unchanged") {
      val low = Mint.tainted(7, Integrity.Outside)
      Trusted.check(low, Integrity.Member).left.map(t => (t.open, t.integrity)) ==>
        Left((7, Integrity.Outside))
    }

    test("endorse trusts at the endorser's level, whatever the value was believed at") {
      given (Endorse.Extraction^) = Mint.extraction(Integrity.Member)
      val endorsed = Trusted.endorse(Mint.tainted(true, Integrity.Untrusted))
      (endorsed.extract, endorsed.integrity) ==> (true, Integrity.Member)
    }

    test("an extraction endorses at its level met with Member, never above") {
      (
        Mint.extraction(Integrity.at(Level.Authority, team)).at,
        Mint.extraction(Integrity.Outside).at
      ) ==> (Integrity.Member, Integrity.Outside)
    }

    test("the sanitiser raises Untrusted to Outside, keeping its compartments, and nothing else") {
      val sanitiser = Mint.sanitiser
      def sanitised(i: Integrity): Integrity = sanitiser.sanitised(Mint.tainted(0, i)).integrity
      (
        sanitised(Integrity.Untrusted),
        sanitised(Integrity.at(Level.Untrusted, team)),
        sanitised(Integrity.Member),
        sanitised(Integrity.at(Level.Outside, team))
      ) ==> (
        Integrity.Outside,
        Integrity.at(Level.Outside, team),
        Integrity.Member,
        Integrity.at(Level.Outside, team)
      )
    }

    test("a taint falls to the meet of its level and each lowering, and never rises") {
      (
        lowered(Mint.taint(Integrity.Member), Integrity.at(Level.Authority, team)),
        lowered(Mint.taint(Integrity.Member), Integrity.Outside, Integrity.Member)
      ) ==> (Integrity.Member, Integrity.Outside)
    }

    test("a stamp is at the taint's level when stamped, not as it later falls") {
      val taint = Mint.taint(Integrity.Member)
      val before = taint.stamp("a")
      lowered(taint, Integrity.Outside)
      (before.integrity, taint.stamp("b").integrity) ==> (Integrity.Member, Integrity.Outside)
    }

    test("a literal is trusted at Authority with no compartment") {
      val grit = Trusted.literal("grit")
      (grit.extract, grit.integrity) ==> ("grit", Integrity.at(Level.Authority))
    }
  }
}
