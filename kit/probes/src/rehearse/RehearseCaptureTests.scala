package grit.kit.rehearse

import grit.core.durable.Probes

import utest.*

/** What a dry rehearsal's answers may hold: nothing that reaches a model. [[StandIns]] is
  * sealed, so no answers but its own are made, and pure, so a capability, a provider among
  * them, can neither be held in one nor have its capture set added to one's type. What it
  * showed ([[Rehearsed]]) is pure too: it outlives the rehearsal's block, so it holds no tape,
  * tap or session.
  */
object RehearseCaptureTests extends TestSuite {

  private val prelude =
    """package grit.probe
      |import grit.core.provider.Provider
      |import grit.kit.rehearse.{StandIns, Rehearsed, Tape}
      |""".stripMargin

  /** The control: answers handed on beside a provider, holding none of it. */
  private val beside =
    "def f(p: Provider^): (StandIns, Provider^{p}) = (StandIns.Stubbed, p)"

  /** StandIns typed as holding a provider. */
  private val holding = "def f(p: Provider^): StandIns^{p} = StandIns.Stubbed"

  /** A pure class, as answers are, holding a provider: what a case of [[StandIns]] holding one
    * would be, which only its own file could declare.
    */
  private val pureHolding = "final class Live(p: Provider^) extends caps.Pure"

  /** The control: what a rehearsal showed, handed on beside its tape. */
  private val shownBeside =
    "def g(t: Tape^): (Rehearsed, Tape^{t}) = (Rehearsed(Vector.empty), t)"

  /** What a rehearsal showed, typed as holding its tape. */
  private val shownHolding = "def g(t: Tape^): Rehearsed^{t} = Rehearsed(Vector.empty)"

  private val escapes = List(holding, pureHolding, shownHolding)

  private lazy val compiled = Probes.batch(prelude, beside :: shownBeside :: escapes)

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.classpath.nonEmpty)
    }

    test("answers handed on beside a provider compile") {
      assert(compiled.errors(beside).isEmpty)
    }

    test("answers typed as holding a provider are rejected") {
      assert(compiled.errors(holding).exists(_.contains("StandIns is a pure type")))
    }

    test("a pure class holding a provider, as a case of answers would, is rejected") {
      assert(
        compiled
          .errors(pureHolding)
          .exists(_.contains("is not included in the allowed capture set {}"))
      )
    }

    test("what a rehearsal showed, handed on beside its tape, compiles") {
      assert(compiled.errors(shownBeside).isEmpty)
    }

    test("what a rehearsal showed, typed as holding its tape, is rejected") {
      assert(compiled.errors(shownHolding).exists(_.contains("Rehearsed is a pure type")))
    }

    test("capture checking is what rejects them") {
      val flags = Probes.options.filterNot(f =>
        f == "-language:experimental.captureChecking" ||
          f == "-language:experimental.separationChecking"
      )
      val unchecked = Probes.batch(Probes.erased(prelude), escapes.map(Probes.erased), flags)
      assert(escapes.map(Probes.erased).forall(p => unchecked.errors(p).isEmpty))
    }
  }
}
