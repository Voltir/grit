package grit.projects.artifact

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, flowsInto, options}

import utest.*

/** What the compiler rejects about a sweep's decision: what it is told is ended is pure, so a
  * sweep reaches no store while it decides. Pinned by compiling probe sources against projects
  * with its own flags; `assertCompileError` cannot see capture errors (docs/capture-checking.md).
  */
object SweepPurityTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.store.Reads
      |import grit.projects.artifact.*
      |import grit.projects.name.*
      |""".stripMargin

  /** An `ended` reading the store, so the decision could depend on what it reads. */
  private val endedReadsStore =
    """def swept(p: Project, reads: Reads^): Project.Swept =
      |  Project.swept(p, Set.empty, h => reads.hashCode > 0, Map.empty)
      |""".stripMargin

  private val endedIsPure =
    """def swept(p: Project, ends: Set[StateName]): Project.Swept =
      |  Project.swept(p, Set.empty, h => ends(h.state), Map.empty)
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(endedIsPure, endedReadsStore))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }
    test("an ended that holds only data is accepted") {
      assert(compiled.errors(endedIsPure).isEmpty)
    }
    test("an ended that holds the store's reads is rejected") {
      val errs = compiled.errors(endedReadsStore)
      assert(flowsInto("{}")(errs))
    }
  }
}
