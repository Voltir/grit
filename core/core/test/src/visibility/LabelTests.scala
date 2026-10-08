package grit.core.visibility

import utest.*
import TestLabels.compartment

object LabelTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")
  private val ops = compartment("ops")

  /* Every label over the four levels and three compartments: the laws are checked on all of
   * them, each pair and each triple. */
  private val labels: Vector[Label] =
    for {
      level <- Level.values.toVector
      mask <- (0 until 8).toVector
    } yield Label.at(
      level,
      Vector(trial, acme, ops).zipWithIndex.collect {
        case (c, i) if (mask & (1 << i)) != 0 => c
      }*
    )

  /* The first counterexample, or `None`. */
  private def all2(p: (Label, Label) => Boolean): Option[(Label, Label)] =
    labels.iterator.flatMap(a => labels.iterator.map((a, _))).find(p.tupled.andThen(!_))

  private def all3(p: (Label, Label, Label) => Boolean): Option[(Label, Label, Label)] =
    labels.iterator
      .flatMap(a => labels.iterator.flatMap(b => labels.iterator.map((a, b, _))))
      .find(p.tupled.andThen(!_))

  val tests = Tests {
    test("join and meet are commutative, associative and absorptive over every label") {
      all2((a, b) => a.join(b) == b.join(a) && a.meet(b) == b.meet(a)) ==> None
      all3((a, b, c) =>
        a.join(b).join(c) == a.join(b.join(c)) && a.meet(b).meet(c) == a.meet(b.meet(c))
      ) ==> None
      all2((a, b) => a.join(a.meet(b)) == a && a.meet(a.join(b)) == a) ==> None
    }

    test("join is the least label dominating both, and meet the greatest both dominate") {
      all3((a, b, c) =>
        a.join(b).dominates(a) && a.join(b).dominates(b) &&
          (!(c.dominates(a) && c.dominates(b)) || c.dominates(a.join(b))) &&
          a.dominates(a.meet(b)) && b.dominates(a.meet(b)) &&
          (!(a.dominates(c) && b.dominates(c)) || a.meet(b).dominates(c))
      ) ==> None
    }

    test("dominance is a partial order with Public at the bottom") {
      labels.find(a => !(a.dominates(a) && a.dominates(Label.Public))) ==> None
      all2((a, b) => !(a.dominates(b) && b.dominates(a)) || a == b) ==> None
      all3((a, b, c) => !(a.dominates(b) && b.dominates(c)) || a.dominates(c)) ==> None
    }

    test("a higher level dominates a lower one only when it holds every compartment too") {
      assert(
        Label.at(Level.Confidential).dominates(Label.at(Level.Internal)),
        !Label.at(Level.Internal).dominates(Label.at(Level.Confidential)),
        !Label.at(Level.Restricted).dominates(Label.at(Level.Public, trial)),
        Label.at(Level.Internal, trial, acme).dominates(Label.at(Level.Public, trial))
      )
      Label.at(Level.Confidential, trial).meet(Label.at(Level.Internal, acme)) ==>
        Label.at(Level.Internal)
    }

    test("a label's stored parts make the same label again") {
      labels.find(l => LabelParts.of(LabelParts.rank(l), LabelParts.compartments(l)) != l) ==> None
    }

    test("a label is stored as its level's rank from public's 0 and its compartments, sorted") {
      // The stored form (grit.labels): pinned, since a change re-reads every stored label.
      val stored = Label.at(Level.Confidential, trial, acme)
      (LabelParts.rank(stored), LabelParts.compartments(stored)) ==> (2, Vector("acme", "trial"))
      (LabelParts.rank(Label.Public), LabelParts.compartments(Label.Public)) ==> (0, Vector.empty)
    }

    test("a label's written form reads back to the same label") {
      labels.find(l => Label.read(Label.written(l)) != Right(l)) ==> None
      // The written form other files and recorded outputs hold: pinned.
      Label.written(Label.at(Level.Confidential, trial, acme)) ==> "confidential+acme+trial"
      Label.written(Label.Public) ==> "public"
    }

    test(
      "a label is shown to people as its level, then its compartments in order, comma-separated in braces, and with none no compartments at all"
    ) {
      (
        Label.shown(Label.at(Level.Confidential, trial, acme)),
        Label.shown(Label.at(Level.Internal)),
        Label.shown(Label.Public)
      ) ==> (
        "[level: confidential, in: {acme, trial}]",
        "[level: internal]",
        "[level: public]"
      )
    }

    test("a written form naming no level first, or a level's name as a compartment, is refused") {
      Label.read("trial") ==> Left("trial names no level first: trial")
      Label.read("internal+confidential") ==>
        Left("confidential is a level's name, never a compartment's")
    }

    test("stored parts naming no compartment, or a rank naming no level, read high and unmapped") {
      LabelParts.of(1, Vector("trial", "Not-A-Name")) ==>
        Label.at(Level.Internal, trial, Compartment.Unmapped)
      LabelParts.of(0, Vector("confidential")) ==> Label.at(Level.Public, Compartment.Unmapped)
      LabelParts.of(4, Vector("trial")) ==> Label.at(Level.Restricted, trial, Compartment.Unmapped)
      LabelParts.of(-1, Vector.empty) ==> Label.at(Level.Restricted, Compartment.Unmapped)
      // A clearance at the top level, with every other compartment, reads none of them.
      val cleared = Label.at(Level.Restricted, trial, acme, ops)
      assert(
        !cleared.dominates(LabelParts.of(0, Vector("Not-A-Name"))),
        !cleared.dominates(LabelParts.of(4, Vector.empty))
      )
    }

    test("a compartment is a short lowercase name and never a level's") {
      Compartment.of("confidential") ==> Left(
        "confidential is a level's name, never a compartment's"
      )
      Compartment.of("public") ==> Left("public is a level's name, never a compartment's")
      Compartment.of("Trial") ==>
        Left("Trial is not a compartment's name: lowercase letters, digits and -, 1 to 32 of them")
      Compartment.of("a" * 33).left.map(_.take(36)) ==> Left("a" * 33 + " is")
      Compartment.of("client-7").map(c => Label.written(Label.at(Level.Public, c))) ==>
        Right("public+client-7")
    }
  }
}
