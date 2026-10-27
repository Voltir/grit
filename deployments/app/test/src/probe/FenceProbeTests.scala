package grit.app.probe

import utest.*

/** [[FenceProbe]]'s requests, without a model: its before and after arms. */
object FenceProbeTests extends TestSuite {

  val tests = Tests {
    test("each sample's request before fencing has its request's shape, and differs from it") {
      val literals = FenceProbe.literals
      FenceRequests.all.map(b =>
        (b.name, FenceProbe.before(b.request, literals(b.name)).map(_ == b.request))
      ) ==> FenceRequests.all.map(b => (b.name, Right(false)))
    }

    test("each turn arm's request differs from its sample's request after fencing") {
      val turns = FenceRequests.all.filter(_.name.startsWith("turn-"))
      turns.map(b =>
        (b.name, FenceProbe.turnArms(b.request).map((arm, r) => (arm, r != b.request)))
      ) ==>
        turns.map(b =>
          (b.name, Vector("reminder" -> true, "user-role" -> true, "material" -> true))
        )
    }
  }
}
