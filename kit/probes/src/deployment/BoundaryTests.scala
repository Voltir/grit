package grit.kit.deployment

import grit.core.durable.Probes

import utest.*

/** What a deployment outside `grit` may name, pinned by compiling probe sources against the
  * kit with its own flags: the engine's internals are `private[grit]`, the kit's surface is
  * not. Package-private is source-level, so code declaring itself inside `grit` still reaches
  * in; the second control shows that limit.
  */
object BoundaryTests extends TestSuite {

  private val outside =
    """package slim.probe
      |import grit.core.clock.Clock
      |import grit.core.edge.Console
      |import grit.core.spend.Budget
      |import grit.core.visibility.Visibility
      |import grit.core.host.ProcessIdentity
      |import grit.dbos.engine.{Engine, EngineLock, Link}
      |import grit.dbos.sql.DbConfig
      |import grit.kit.deployment.*
      |import grit.kit.run.Kit
      |""".stripMargin

  private val inside = outside.replace("package slim.probe", "package grit.probe")

  private val engineOpen =
    """def f(c: DbConfig, i: ProcessIdentity, b: Budget, v: Visibility, k: Clock^): Boolean =
      |  Engine.open(c, "e", i, b, v, k).isRight
      |""".stripMargin

  private val engineStart =
    """def f(c: DbConfig, l: EngineLock^, i: ProcessIdentity, b: Budget, v: Visibility, k: Clock^): Boolean =
      |  Engine.start(c, l, "e", i, b, v, k).isRight
      |""".stripMargin

  private val linkAttach =
    """def f(c: DbConfig, i: ProcessIdentity, b: Budget, v: Visibility, k: Clock^): String =
      |  Link.attach(c, "e", i, b, v, k).toString
      |""".stripMargin

  private val lockTake =
    """def f(c: DbConfig): Boolean = EngineLock.take(c).isRight
      |""".stripMargin

  private val workflow =
    """def f(w: grit.dbos.workflow.DurableWorkflow): String = w.run()
      |""".stripMargin

  private val surface =
    """def f(d: Deployment, env: Map[String, String]) = Kit.serve(d, env)
      |val rounds: Either[String, Rounds] = Rounds.of(4)
      |val asked: TriageQuestions = TriageQuestions.V1
      |val shadows: Vector[ShadowVariant] = Vector.empty
      |val seeded = Seed.catalog
      |val jev = Placer.JevService
      |def console(
      |    d: Deployment,
      |    s: grit.kit.environment.Secrets,
      |    k: Clock^,
      |    e: Console.Edge^
      |): Either[grit.kit.run.KitFailure, Console.Opener^] =
      |  Kit.console(d, s, scala.concurrent.duration.FiniteDuration(0, "ms"), k, e)
      |def here(o: Console.Opener^): Boolean = o.runner == Console.Runner.Here
      |def opened(o: Console.Opener^): Either[String, Console^] = o.open()
      |""".stripMargin

  private val internals = List(engineOpen, engineStart, linkAttach, lockTake, workflow)

  private lazy val fromOutside = Probes.batch(outside, surface :: internals)

  /** Whether `errs` refuse a name for being lent to `grit` alone. */
  private def refused(errs: List[String]): Boolean =
    errs.exists(_.contains("can only be accessed from package grit."))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.classpath.nonEmpty)
    }

    test("outside grit, the kit's surface and what it exports compile") {
      val errs = fromOutside.errors(surface)
      assert(errs.isEmpty)
    }

    test("outside grit, Engine.open is not accessible") {
      val errs = fromOutside.errors(engineOpen)
      assert(refused(errs))
    }

    test("outside grit, Engine.start is not accessible") {
      val errs = fromOutside.errors(engineStart)
      assert(refused(errs))
    }

    test("outside grit, Link.attach is not accessible") {
      val errs = fromOutside.errors(linkAttach)
      assert(refused(errs))
    }

    test("outside grit, EngineLock.take is not accessible") {
      val errs = fromOutside.errors(lockTake)
      assert(refused(errs))
    }

    test("outside grit, DurableWorkflow is not accessible") {
      val errs = fromOutside.errors(workflow)
      assert(refused(errs))
    }

    test("code declaring itself inside grit still opens an engine: the boundary is source-level") {
      val errs = Probes.batch(inside, List(engineOpen)).errors(engineOpen)
      assert(errs.isEmpty)
    }
  }
}
