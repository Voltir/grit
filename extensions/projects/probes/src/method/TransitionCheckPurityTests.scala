package grit.projects.method

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, heldImpure, options}

import utest.*

/** What the compiler rejects about a transition check: it is pure, so it reaches no store, and a
  * bench's replay of a move sees what the live move sees. Pinned by compiling probe sources
  * against projects with its own flags; `assertCompileError` cannot see capture errors
  * (docs/capture-checking.md).
  */
object TransitionCheckPurityTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.store.Reads
      |import grit.projects.artifact.*
      |import grit.projects.method.*
      |import grit.projects.name.*
      |""".stripMargin

  /** A check holding the store's reads, so its answer could depend on what it reads. */
  private val checkHoldsReads =
    """abstract class Reading(reads: Reads^) extends TransitionCheck
      |""".stripMargin

  private val checkIsPure =
    """abstract class Plain(named: CheckName) extends TransitionCheck
      |def compiled(c: Plain): Vector[TransitionCheck] = Vector(c)
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(checkIsPure, checkHoldsReads))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }
    test("a pure check is accepted as a method's compiled check") {
      assert(compiled.errors(checkIsPure).isEmpty)
    }
    test("a check holding the store's reads is rejected") {
      assert(heldImpure(compiled.errors(checkHoldsReads)))
    }
  }
}
