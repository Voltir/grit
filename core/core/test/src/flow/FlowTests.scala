package grit.core.flow

import grit.core.visibility.Integrity.Level
import grit.core.visibility.{Compartment, Integrity}

import utest.*

object FlowTests extends TestSuite {

  private val team: Compartment =
    Compartment.of("team").getOrElse(throw new java.lang.AssertionError("team is a compartment"))

  private val acme: Compartment =
    Compartment.of("acme").getOrElse(throw new java.lang.AssertionError("acme is a compartment"))

  val tests = Tests {
    test("zip is believed at the meet of the two, tainted or trusted") {
      // Neither dominates the other: the meet is below both.
      val inTeam = Integrity.at(Level.Authority, team)
      val inAcme = Integrity.at(Level.Member, acme)
      (
        Mint.tainted(1, inTeam).zip(Mint.tainted("a", inAcme)).integrity,
        Mint.tainted("a", inAcme).zip(Mint.tainted(1, inTeam)).integrity
      ) ==> (Integrity.Member, Integrity.Member)
      Mint.trusted(1, inTeam).zip(Mint.trusted(2, inAcme)).integrity ==> Integrity.Member
    }

    test("a trusted value made unchecked again keeps its integrity") {
      Mint.trusted(1, Integrity.Member).tainted.integrity ==> Integrity.Member
    }

    test(
      "check trusts a value whose integrity dominates the required one, at its own, an equal one too"
    ) {
      Vector(Integrity.Outside, Integrity.Member).map(required =>
        Trusted
          .check(Mint.tainted(7, Integrity.Member), required)
          .map(t => (t.extract, t.integrity))
      ) ==> Vector(Right((7, Integrity.Member)), Right((7, Integrity.Member)))
    }

    test("asIs trusts any value at the integrity it is believed at") {
      val low = Trusted.asIs(Mint.tainted(7, Integrity.at(Level.Untrusted, team)))
      (low.extract, low.integrity) ==> (7, Integrity.at(Level.Untrusted, team))
    }

    test(
      "check returns a value below the required integrity unchanged, and one beside it, neither dominating"
    ) {
      val low = Mint.tainted(7, Integrity.Outside)
      Trusted.check(low, Integrity.Member).left.map(t => (t.open, t.integrity)) ==>
        Left((7, Integrity.Outside))
      val inTeam = Integrity.at(Level.Authority, team)
      Trusted
        .check(Mint.tainted(7, inTeam), Integrity.at(Level.Member, acme))
        .left
        .map(t => (t.open, t.integrity)) ==> Left((7, inTeam))
    }

    test("a literal is trusted at Authority with no compartment") {
      val grit = Trusted.literal("grit")
      (grit.extract, grit.integrity) ==> ("grit", Integrity.at(Level.Authority))
    }
  }
}
