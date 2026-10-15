package grit.core.durable

import utest.*
import Probes.classpath

/** What capture and separation checking reject about a [[Flow]]'s steps and glue, pinned by
  * compiling probe sources against core with core's own flags: a body acts only through the
  * environment a run hands it, never the `Durable` or a capability in scope; glue is pure; a
  * transaction's `Tx` and a program's `Steps` never leave their bodies; and `over` finds its
  * environment only in the larger one (docs/capture-checking.md).
  */
object DurableFlowCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.durable.*
      |import grit.core.store.Tx
      |import grit.core.visibility.Subject
      |trait Provider extends caps.SharedCapability { def call(): String }
      |trait Db extends caps.SharedCapability { def read(): String }
      |final class Env(val provider: Provider^)
      |final class Wide(val env: Env^, val db: Db^)
      |val n: StepName[Any] = StepName("n")
      |val plain: Step[Env, String, String] = Step[Env, String, String](n)(r => in => in)
      |""".stripMargin

  // F1: a body naming the Durable in scope, and the controls that compile.
  private val bodyStepsOnDurable =
    """def f(using d: Durable^): Step[Env, String, String] =
      |  Step[Env, String, String](n)(r => in => d.step("inner")(() => in))
      |""".stripMargin

  private val bodyReadsPatch =
    """def f(using d: Durable^): Step[Env, String, String] =
      |  Step[Env, String, String](n)(r => in => if (d.patch("p")) "new" else in)
      |""".stripMargin

  private val patchTaken =
    """def f(env: Env^)(using d: Durable^): Either[Nothing, String] =
      |  Flows.run(Flow.patch("p")(Flow.step(plain), Flow.step(plain).map(_ + "!")), env, "x")
      |""".stripMargin

  private val patchReadBefore =
    """def f(env: Env^)(using d: Durable^): Either[Nothing, String] = {
      |  val fresh = d.patch("p")
      |  Flows.run(Flow.step(Step[Env, String, String](n)(r => in => if (fresh) "new" else in)), env, "x")
      |}
      |""".stripMargin

  // F2: a transaction body returning its Tx.
  private val txReturned =
    """def f(using Journaled[Option[Tx^]]): Step[Env, String, Option[Tx^]] =
      |  Step.transact[Env, String, Option[Tx^]](n, _ => Subject.Public)(r => in => Option(summon[Tx^]))
      |""".stripMargin

  private val txUsed =
    """def f: Step[Env, String, String] =
      |  Step.transact[Env, String, String](n, _ => Subject.Public)(r => in => { val _ = summon[Tx^]; in })
      |""".stripMargin

  // F3: a body capturing a provider from scope, and the provider reached through the env.
  private val providerFromScope =
    """def f(p: Provider^): Step[Env, String, String] =
      |  Step[Env, String, String](n)(r => in => p.call())
      |""".stripMargin

  private val providerThroughEnv =
    """val f: Step[Env, String, String] = Step[Env, String, String](n)(r => in => r.provider.call())
      |""".stripMargin

  // F4: glue using a capability: map, decide, each's items, a step name's key.
  private val glue: Vector[String] = Vector(
    "def f(p: Provider^): Flow[Env, String, Nothing, String] = Flow.step(plain).map(o => o + p.call())",
    """def f(p: Provider^): Flow[Env, String, String, String] =
      |  Flow.decide(plain)((i, o) => if (o == p.call()) Left(i) else Right(o))""".stripMargin,
    """def f(p: Provider^): Flow[Env, String, Nothing, Vector[String]] =
      |  Flow.each[Env, String, Nothing, String, String](in => Vector(p.call()))(Flow.step(plain))""".stripMargin,
    "def f(p: Provider^): StepName[String] = StepName.keyed[String](\"k\")(in => p.call())"
  )

  // F5: a capability on the wire, used by a body.
  private val dbOnTheWire =
    """val f: Step[Env, Db^, String] = Step[Env, Db^, String](n)(r => db => db.read())
      |""".stripMargin

  // F6: a program returning or keeping its Steps, or naming the Durable.
  private val stepsReturned =
    """val f: Flow[Env, String, Nothing, Steps[Env]^] =
      |  Flow.program[Env, String, Steps[Env]^]("p")(steps => in => steps)
      |""".stripMargin

  private val stepsKept =
    """def f(): Flow[Env, String, Nothing, String] = {
      |  var kept: Option[Steps[Env]^] = None
      |  Flow.program[Env, String, String]("p")(steps => in => { kept = Some(steps); in })
      |}
      |""".stripMargin

  private val programOnDurable =
    """def f(using d: Durable^): Flow[Env, String, Nothing, String] =
      |  Flow.program[Env, String, String]("p")(steps => in => d.step("x")(() => in))
      |""".stripMargin

  private val programSteps =
    """val f: Flow[Env, String, Nothing, String] =
      |  Flow.program[Env, String, String]("p")(steps => in => steps.run(Flow.step(plain), in).merge)
      |""".stripMargin

  // F7: over's get returning a capability from scope, and a projection of its argument.
  private val overFromScope =
    """def f(other: Env^): Flow[Wide, String, Nothing, String] =
      |  Flow.step(plain).over[Wide](w => other)
      |""".stripMargin

  private val overProjected =
    """val f: Flow[Wide, String, Nothing, String] = Flow.step(plain).over[Wide](w => w.env)
      |""".stripMargin

  private val controls: List[String] = List(
    patchTaken,
    patchReadBefore,
    txUsed,
    providerThroughEnv,
    programSteps,
    overProjected
  )

  private val breaches: List[String] = List(
    bodyStepsOnDurable,
    bodyReadsPatch,
    txReturned,
    providerFromScope,
    dbOnTheWire,
    stepsReturned,
    stepsKept,
    programOnDurable,
    overFromScope
  ) ++ glue

  private lazy val compiled = Probes.batch(prelude, controls ++ breaches)

  private def errors(body: String): List[String] = compiled.errors(body)

  /** Whether `errs` reject a closure found capturing `name`, which its expected type excludes. */
  private def charged(name: String)(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains(s"->{$name}"))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(classpath.nonEmpty, compiled.runs == 1)
    }

    test(
      "controls: a patch the flow takes or read before it, a Tx used, the env's provider, a program's steps and a projected env compile"
    ) {
      val errs = controls.map(errors)
      errs ==> controls.map(_ => Nil)
    }

    test("F1: a body naming the Durable in scope is rejected, a patch read included") {
      val (stepped, patched) = (errors(bodyStepsOnDurable), errors(bodyReadsPatch))
      assert(charged("d")(stepped), charged("d")(patched))
    }

    test("F2: a transaction body returning its Tx is rejected") {
      val errs = errors(txReturned)
      assert(errs.exists(e => e.contains("Found:") && e.contains("Option[grit.core.store.Tx^")))
    }

    test("F3: a body capturing a provider in scope is rejected") {
      val errs = errors(providerFromScope)
      assert(charged("p")(errs))
    }

    test("F4: glue using a capability is rejected: map, decide, each's items and a key") {
      val rejected = glue.map(g => charged("p")(errors(g)))
      rejected ==> Vector(true, true, true, true)
    }

    test("F5: a body using a capability its input carries is rejected") {
      val errs = errors(dbOnTheWire)
      assert(errs.exists(_.contains("->{any} String")))
    }

    test("F6: a program returning or keeping its Steps, or naming the Durable, is rejected") {
      val (returned, kept, durable) =
        (errors(stepsReturned), errors(stepsKept), errors(programOnDurable))
      assert(
        returned.exists(e => e.contains("String ->{steps}") && e.contains("Steps[probe.Env")),
        charged("kept")(kept),
        charged("d")(durable)
      )
    }

    test("F7: an over whose get returns a capability in scope is rejected") {
      val errs = errors(overFromScope)
      assert(charged("other")(errs))
    }
  }
}
