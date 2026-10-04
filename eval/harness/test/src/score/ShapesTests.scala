package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.triage.Weighing

import utest.*

/** How a corpus's turns recorded their offers and weighing, counted. */
object ShapesTests extends TestSuite {

  val tests = Tests {
    test(
      "turns are counted by whether their offer recorded a shape, and by what their weigh step recorded, failures by kind"
    ) {
      // w2 shaped and kept; w1 and w4 offered before shapes, no step; w3 no offer, weighed
      // nothing; w5 no offer, its weighing late.
      Shapes.of(TurnFixtures.turns) ==>
        Shapes(1, 2, 2, 1, 0, VectorMap(Weighing.Unweighed.Late -> 1), 1, 2)
    }
  }
}
