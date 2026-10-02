package grit.eval.harness.score

import grit.eval.harness.log.Suite
import grit.eval.harness.score.Fixtures.*

import utest.*

/** Jev's repeat noise, and the cases a replica says moved on. Every id here is synthetic. */
object MovedOnTests extends TestSuite {

  private def near(a: Double, b: Double) = math.abs(a - b) < 1e-12

  val tests = Tests {
    test(
      "the noise on each question is its pooled standard deviation over the cases answered more than once"
    ) {
      val (one, two, once) = (id("C1/1"), id("C1/2"), id("C1/3"))
      val rows = Vector(
        // waiting 0.1 and 0.3: squares 0.02 on one degree of freedom.
        row(Suite.Triage, one, 0, triage(Vector(0.8, 0.2, 0, 0, 0), 0.1, 0.5, 0.5)),
        row(Suite.Triage, one, 1, triage(Vector(0.6, 0.4, 0, 0, 0), 0.3, 0.5, 0.5)),
        // waiting 0.5 three times: no squares, two degrees of freedom.
        row(Suite.Triage, two, 0, triage(Vector(0, 1, 0, 0, 0), 0.5, 0.5, 0.5)),
        row(Suite.Triage, two, 1, triage(Vector(0, 1, 0, 0, 0), 0.5, 0.5, 0.5)),
        row(Suite.Triage, two, 2, triage(Vector(0, 1, 0, 0, 0), 0.5, 0.5, 0.5)),
        // Once: no spread to measure.
        row(Suite.Triage, once, 0, triage(Vector(0, 0, 1, 0, 0), 0.9, 0.1, 0.1))
      )
      val noise = MovedOn.noise(rows)
      // Kind: one's likeliest is the first, at 0.8 and 0.6; two's never moves.
      assert(
        noise.exists(n =>
          near(n.waiting, math.sqrt(0.02 / 3)) && near(n.kind, math.sqrt(0.02 / 3)) &&
            n.durable == 0 && n.helps == 0
        )
      )
      MovedOn.noise(rows.drop(5)) ==> None
    }

    test(
      "a case moved on only where the replica stands from live by more than the multiple of the question's noise"
    ) {
      val noise = Noise(kind = 0.01, waiting = 0.01, durable = 0.01, helps = 0.01)
      // Tolerance 0.1 on every question.
      val live = Answers.of(
        Vector("C1/1", "C1/2", "C1/3", "C1/4").map(c =>
          row(Suite.Triage, id(c), 0, triage(Vector(0.9, 0.1, 0, 0, 0), 0.5, 0.5, 0.5))
        )
      )
      val replica = Answers.of(
        Vector(
          // Within: 0.09 on waiting.
          row(Suite.Triage, id("C1/1"), 0, triage(Vector(0.9, 0.1, 0, 0, 0), 0.59, 0.5, 0.5)),
          // Beyond: 0.11 on helps.
          row(Suite.Triage, id("C1/2"), 0, triage(Vector(0.9, 0.1, 0, 0, 0), 0.5, 0.5, 0.39)),
          // Beyond on kind: live's likeliest given 0.7, not 0.9.
          row(Suite.Triage, id("C1/3"), 0, triage(Vector(0.7, 0.3, 0, 0, 0), 0.5, 0.5, 0.5))
          // C1/4: no replica answer, so nothing to compare.
        )
      )
      MovedOn.of(replica, live, noise) ==> Moved(noise, Vector(id("C1/2"), id("C1/3")))
    }
  }
}
