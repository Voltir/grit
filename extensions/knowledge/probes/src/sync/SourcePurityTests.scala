package grit.knowledge.sync

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, heldImpure, options}

import utest.*

/** What the compiler rejects about a card source: it is data, so it holds no capability, and
  * neither does what it reads a listing as. Pinned by compiling probe sources against knowledge
  * with its own flags; `assertCompileError` cannot see capture errors
  * (docs/capture-checking.md).
  */
object SourcePurityTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.act.*
      |import grit.knowledge.sync.*
      |final case class Names(paths: Vector[String]) extends caps.Pure
      |final class Leaky(val moves: Keeping^)
      |""".stripMargin

  /** A source reading its listings as a class that holds a run's moves. */
  private val listingHoldsMoves =
    """abstract class Reads extends Source[Leaky]
      |""".stripMargin

  /** A source holding a run's moves as a field. */
  private val sourceHoldsMoves =
    """abstract class Holds(moves: Keeping^) extends Source[Names]
      |""".stripMargin

  private val sourceIsPure =
    """def pure[A <: caps.Pure](a: A): A = a
      |abstract class Plain(name: String) extends Source[Names]
      |def source(s: Plain): Source[Names] = pure(s)
      |""".stripMargin

  private lazy val compiled =
    Probes.batch(prelude, List(sourceIsPure, sourceHoldsMoves, listingHoldsMoves))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("a pure source is accepted where caps.Pure is asked for") {
      assert(compiled.errors(sourceIsPure).isEmpty)
    }

    test("a source holding its run's moves is rejected") {
      assert(heldImpure(compiled.errors(sourceHoldsMoves)))
    }

    test("a source whose listings hold its run's moves is rejected") {
      val errors = compiled.errors(listingHoldsMoves)
      assert(errors.exists(_.contains("does not conform to upper bound scala.caps.Pure")))
    }
  }
}
