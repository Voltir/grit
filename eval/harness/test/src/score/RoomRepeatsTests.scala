package grit.eval.harness.score

import utest.*
import RoomFixtures.*

/** [[RoomRepeats]]: a room's scores over repeats as each measure's mean and range. */
object RoomRepeatsTests extends TestSuite {

  val tests = Tests {
    test("each measure is its mean, least and most over the repeats, a proportion's out of its n") {
      // Over `later`'s room: the second repeat follows m5 to m2 (its topic, csv).
      val (room, first) = later
      val second = first.updated(
        4,
        RoomPlacing("m5", RoomPlacing.Made.Follows("m2", 0.7), Vector(offer("m2", 0.7)))
      )
      def measures(against: Against) = RoomRepeats
        .of(Vector(first, second).map(RoomScore.of(room, _, strands, against)), against)
        .map(m => (m.name, m.count, m.of))
      // Right 3/6 then 4/6 (m6 begun, m5 then right); strict misses m6, offered only m3.
      measures(Against.Placings) ==> Vector(
        ("right", Ranged(3.5, 3, 4), Some(Ranged(6, 6, 6))),
        ("offer recall", Ranged(3, 3, 3), Some(Ranged(3, 3, 3))),
        ("strict recall", Ranged(2, 2, 2), Some(Ranged(3, 3, 3))),
        ("Jev given offered", Ranged(1.5, 1, 2), Some(Ranged(3, 3, 3))),
        ("wrong follows", Ranged(1, 1, 1), None),
        ("wrong follows downstream of an ambiguous", Ranged(0, 0, 0), None),
        ("topics split", Ranged(2, 2, 2), None),
        ("strands mixed", Ranged(1, 1, 1), None),
        ("bursts Jev right", Ranged(1, 1, 1), Some(Ranged(2, 2, 2)))
      )
    }

    test("against the truth, the downstream, split and mixed measures are left out") {
      val (room, placings) = later
      RoomRepeats
        .of(Vector(RoomScore.of(room, placings, strands, Against.Truth)), Against.Truth)
        .map(_.name) ==> Vector(
        "right",
        "offer recall",
        "strict recall",
        "Jev given offered",
        "wrong follows",
        "bursts Jev right"
      )
    }

    test("no repeats have no measures") {
      RoomRepeats.of(Vector.empty, Against.Placings) ==> Vector.empty
    }
  }
}
