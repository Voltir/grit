package grit.core.visibility

import utest.*
import TestLabels.compartment

object CompartmentsTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")
  private val ops = compartment("ops")
  private val unmapped = Label.at(Level.Public, Compartment.Unmapped)

  private def declared(cs: Compartment*): Compartments =
    Compartments.of(cs.toVector).fold(c => throw new java.lang.AssertionError(c), identity)

  val tests = Tests {
    test("a deployment's compartments always hold unmapped, and refuse one named twice") {
      declared(trial).declared ==> Set(trial, Compartment.Unmapped)
      Compartments.Shipped.declared ==> Set(Compartment.Unmapped)
      Compartments.of(Vector(trial, acme, trial)) ==> Left(trial)
    }

    test("admitting a label keeps its declared compartments and turns the rest into unmapped") {
      val cs = declared(trial)
      cs.admit(Label.at(Level.Internal, trial)) ==> Label.at(Level.Internal, trial)
      cs.admit(Label.at(Level.Confidential, trial, acme, ops)) ==>
        Label.at(Level.Confidential, trial, Compartment.Unmapped)
      cs.admit(Label.at(Level.Public, acme)) ==> unmapped
    }

    test("the first undeclared compartment of a label is named, in order") {
      val cs = declared(trial)
      cs.undeclared(Label.at(Level.Internal, trial, ops, acme)) ==> Some(acme)
      cs.undeclared(Label.at(Level.Restricted, trial, Compartment.Unmapped)) ==> None
    }

    test("top is restricted in every declared compartment, unmapped included") {
      declared(trial, acme).top ==>
        Label.at(Level.Restricted, trial, acme, Compartment.Unmapped)
    }

    test("compartments keep an earlier set when they only add to it, never when one is dropped") {
      assert(
        declared(trial, acme).keeps(declared(trial)),
        declared(trial).keeps(declared(trial)),
        declared(trial).keeps(Compartments.Shipped),
        !declared(trial).keeps(declared(trial, acme)),
        !declared(acme).keeps(declared(trial))
      )
    }
  }
}
