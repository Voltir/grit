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
  }
}
