package grit.core.visibility

import utest.*
import TestLabels.compartment
import Integrity.Level

object IntegrityTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")

  private val declared =
    Compartments.of(Vector(trial, acme)).fold(c => throw new java.lang.AssertionError(c), identity)

  /* Every integrity over the four levels and the declared compartments, unmapped among them: the
   * laws are checked on all of them, each pair and each triple. */
  private val all: Vector[Integrity] =
    for {
      level <- Level.values.toVector
      mask <- (0 until 8).toVector
    } yield Integrity.at(
      level,
      Vector(trial, acme, Compartment.Unmapped).zipWithIndex.collect {
        case (c, i) if (mask & (1 << i)) != 0 => c
      }*
    )

  /* The first counterexample, or `None`. */
  private def all1(p: Integrity => Boolean): Option[Integrity] = all.find(!p(_))

  private def all2(p: (Integrity, Integrity) => Boolean): Option[(Integrity, Integrity)] =
    all.iterator.flatMap(a => all.iterator.map((a, _))).find(p.tupled.andThen(!_))

  private def all3(
      p: (Integrity, Integrity, Integrity) => Boolean
  ): Option[(Integrity, Integrity, Integrity)] =
    all.iterator
      .flatMap(a => all.iterator.flatMap(b => all.iterator.map((a, b, _))))
      .find(p.tupled.andThen(!_))

  val tests = Tests {
    test("dominance is reflexive, antisymmetric and transitive over every integrity") {
      all1(a => a.dominates(a)) ==> None
      all2((a, b) => !(a.dominates(b) && b.dominates(a)) || a == b) ==> None
      all3((a, b, c) => !(a.dominates(b) && b.dominates(c)) || a.dominates(c)) ==> None
    }

    test(
      "meet is the greatest integrity both dominate, and join the least dominating both, over every pair"
    ) {
      all3 { (a, b, c) =>
        val m = a.meet(b)
        a.dominates(m) && b.dominates(m) && (!(a.dominates(c) && b.dominates(c)) || m.dominates(c))
      } ==> None
      all3 { (a, b, c) =>
        val j = a.join(b)
        j.dominates(a) && j.dominates(b) && (!(c.dominates(a) && c.dominates(b)) || c.dominates(j))
      } ==> None
    }

    test("meet is commutative, associative and idempotent over every integrity") {
      all2((a, b) => a.meet(b) == b.meet(a)) ==> None
      all3((a, b, c) => a.meet(b).meet(c) == a.meet(b.meet(c))) ==> None
      all1(a => a.meet(a) == a) ==> None
    }

    test(
      "top over the declared compartments is the meet's identity, and dominates every integrity"
    ) {
      val top = Integrity.top(declared)
      all1(a => top.meet(a) == a && a.meet(top) == a && top.dominates(a)) ==> None
      top ==> Integrity.at(Level.Authority, trial, acme, Compartment.Unmapped)
    }

    test(
      "levels rise untrusted, outside, member, authority, and a compartment is held only where endorsed"
    ) {
      Vector(Integrity.Untrusted, Integrity.Outside, Integrity.Member)
        .map(_.meet(Integrity.at(Level.Authority, trial))) ==>
        Vector(Integrity.Untrusted, Integrity.Outside, Integrity.Member)
      Integrity.at(Level.Authority, trial).meet(Integrity.at(Level.Authority, acme)) ==>
        Integrity.at(Level.Authority)
      (
        Integrity.at(Level.Authority, trial).dominates(Integrity.Member),
        Integrity.at(Level.Authority).dominates(Integrity.at(Level.Member, trial)),
        Integrity.Untrusted.dominates(Integrity.Outside)
      ) ==> (true, false, false)
    }

    test("an integrity is below outside exactly when it is untrusted, in any compartments") {
      all1(a => a.belowOutside == (Integrity.level(a) == Level.Untrusted)) ==> None
    }

    test(
      "an integrity's written form reads back as it, and its shown form names its level; a text naming no level first, or a compartment by no compartment's name, is refused saying which"
    ) {
      all1(a => Integrity.read(Integrity.written(a)) == Right(a)) ==> None
      // The written form other files and recorded outputs hold (a tool's `requires`): pinned.
      Integrity.written(Integrity.at(Level.Authority, trial, acme)) ==> "authority+acme+trial"
      (
        Integrity.shown(Integrity.Member),
        Integrity.shown(Integrity.at(Level.Authority, trial))
      ) ==> ("[integrity: member]", "[integrity: authority, in: {trial}]")
      Vector("public", "member+Not-A-Name").map(Integrity.read) ==> Vector(
        Left("public names no integrity level first: public"),
        Left(
          "Not-A-Name is not a compartment's name: lowercase letters, digits and -, 1 to 32 of them"
        )
      )
    }
  }
}
