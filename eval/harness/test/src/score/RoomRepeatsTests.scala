package grit.eval.harness.score

import utest.*
import RoomFixtures.*

/** [[RoomRepeats]]: a room's scores over repeats as each measure's mean and range. */
object RoomRepeatsTests extends TestSuite {

  val tests = Tests {
    test("each measure is its mean, least and most over the repeats, a proportion's out of its n") {
      // The second repeat follows m5 to m2 (its topic, csv): right 4/5, Jev given offered 2/2.
      val second = placings.dropRight(1) :+
        RoomPlacing("m5", RoomPlacing.Made.Follows("m2", 0.7), Vector(offer("m2", 0.7)))
      val measures = RoomRepeats.of(
        Vector(
          RoomScore.of(room, placings, strands, Against.Placings),
          RoomScore.of(room, second, strands, Against.Placings)
        ),
        Against.Placings
      )
      measures.map(m => (m.name, m.count, m.of)).take(5) ==> Vector(
        ("right", Ranged(3.5, 3, 4), Some(Ranged(5, 5, 5))),
        ("offer recall", Ranged(2, 2, 2), Some(Ranged(2, 2, 2))),
        ("strict recall", Ranged(2, 2, 2), Some(Ranged(2, 2, 2))),
        ("Jev given offered", Ranged(1.5, 1, 2), Some(Ranged(2, 2, 2))),
        ("wrong follows", Ranged(1, 1, 1), None)
      )
    }

    test("no repeats have no measures") {
      RoomRepeats.of(Vector.empty, Against.Placings) ==> Vector.empty
    }
  }
}
