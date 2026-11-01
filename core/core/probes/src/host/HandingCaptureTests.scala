package grit.core.host

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about [[Handing]]: it is tracked, so a closure that uses one shows
  * it in its capture set, and the argv it is given is pure. These claim no more: what keeps a
  * tool from holding a Handing is the law (no-tool-names-secret), not these.
  */
object HandingCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.host.*
      |import grit.core.secret.Secret
      |""".stripMargin

  // No `^` on either: that Handing is tracked by its own declaration is the claim.
  private val params = "(p: Processes, h: Handing, s: Secret, v: Variables)"

  private def hand(argv: String, body: String): String =
    s"h.started(RelPath.Root, s, v)($argv)($body)"

  private val pureArgv = "path => Argv.of(\"cat\", path)"

  private val control =
    s"""def f$params: () ->{p, h} Unit = () => {
       |  val _ = ${hand(pureArgv, "_ => ()")}
       |}
       |""".stripMargin

  private val hidden =
    s"""def f$params: () ->{p} Unit = () => {
       |  val _ = ${hand(pureArgv, "_ => ()")}
       |}
       |""".stripMargin

  private val argvRuns =
    s"""def f$params: Unit = {
       |  val _ = ${hand(
        "path => { val _ = p.run(Argv.of(\"true\").toOption.get, RelPath.Root, scala.concurrent.duration.Duration(1, \"s\")); Argv.of(\"cat\", path) }",
        "_ => ()"
      )}
       |}
       |""".stripMargin

  private val returned =
    s"""def f$params: Unit = {
       |  val t = ${hand(pureArgv, "started => started")}
       |}
       |""".stripMargin

  private val probes = List(control, hidden, argvRuns, returned)

  private lazy val compiled = Probes.batch(prelude, probes)

  private def errors(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("control: a closure that hands a secret on captures the Handing") {
      errors(control) ==> Nil
    }

    test("a closure that hands a secret on, typed as capturing only Processes, is rejected") {
      assert(
        errors(hidden).exists(
          _.contains("Reference `h` is not included in the allowed capture set {p}")
        )
      )
    }

    test("an argv that runs a program is rejected: it is pure") {
      assert(errors(argvRuns).exists(_.contains("capability `p` cannot flow into capture set {}")))
    }

    test("a handed program's handle returned from its block is rejected") {
      assert(errors(returned).exists(_.contains("Capability `started` outlives its scope")))
    }
  }
}
