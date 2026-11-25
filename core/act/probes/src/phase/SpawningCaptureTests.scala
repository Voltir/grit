package grit.act.phase

import grit.core.durable.Probes
import grit.core.durable.Probes.classpath

import utest.*

/** What capture checking rejects about [[Spawning.spawn]]: its input crosses every step on the
  * wire and is replayed from the record, so it can carry no capability, a transaction least of
  * all. Pinned by compiling probe sources against act with act's own flags;
  * `assertCompileError` cannot see capture errors (docs/capture-checking.md).
  */
object SpawningCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.act.phase.*
      |import grit.core.durable.{Flow, StepName}
      |import grit.core.store.Tx
      |""".stripMargin

  private val overRequests =
    """def spawning(steps: SpawnSteps[Spawning.Request]): Flow[SpawnEnv, Spawning.Request, Nothing, Delegated] =
      |  Spawning.spawn(steps, identity)""".stripMargin

  private val overTransactions =
    """def spawning(steps: SpawnSteps[Tx^], r: Spawning.Request) =
      |  Spawning.spawn[Tx^](steps, _ => r)""".stripMargin

  private val overHolders =
    """final case class Holding(tx: Tx^, r: Spawning.Request)
      |def spawning(steps: SpawnSteps[Holding^]) =
      |  Spawning.spawn[Holding^](steps, h => h.r)""".stripMargin

  private lazy val compiled =
    Probes.batch(prelude, List(overRequests, overTransactions, overHolders))

  val tests = Tests {
    test("a spawn over pure requests compiles") {
      val errs = compiled.errors(overRequests)
      assert(classpath.nonEmpty, errs.isEmpty)
    }

    test("a spawn whose input is a transaction, or holds one, is rejected") {
      val flows = "Note that capability `any` cannot flow into capture set {}"
      val (bare, held) = (compiled.errors(overTransactions), compiled.errors(overHolders))
      assert(bare.exists(_.contains(flows)), held.exists(_.contains(flows)))
    }
  }
}
