package grit.core.host

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about [[Allowlisted]]: nothing outside its file implements it, so
  * every one checks its [[Allowlist]] as [[Allowlisted.over]] does, and it is tracked, so a closure
  * that runs through one shows it. An [[Allowlist]] is pure.
  */
object AllowlistedCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import scala.concurrent.duration.FiniteDuration
      |import grit.core.host.*
      |""".stripMargin

  private val control =
    """def f(p: Runs^, l: Allowlist): Allowlisted^ = Allowlisted.over(p, l)
      |def g(l: Allowlist): () -> Boolean = () => l.rules.isEmpty
      |""".stripMargin

  private val implemented =
    """def f(p: Runs^, l: Allowlist): Allowlisted^ = new Allowlisted {
      |  def allowlist: Allowlist = l
      |  def start(argv: Argv, timeout: FiniteDuration, wait: FiniteDuration) = p.start(argv, timeout, wait)
      |  def tail(id: RunId, wait: FiniteDuration) = p.tail(id, wait)
      |  def stop(id: RunId) = p.stop(id)
      |}
      |""".stripMargin

  private val hidden =
    """def f(r: Allowlisted, a: Argv, t: FiniteDuration): () -> Unit = () => {
      |  val _ = r.start(a, t, t)
      |}
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(control, implemented, hidden))

  val tests = Tests {
    test("control: over gives an Allowlisted, and a closure over an Allowlist is pure") {
      compiled.errors(control) ==> Nil
    }

    test("an Allowlisted implemented outside its file is rejected: the trait is sealed") {
      assert(compiled.errors(implemented).exists(_.contains("sealed trait Allowlisted")))
    }

    test("a closure that runs through an Allowlisted, typed as pure, is rejected") {
      assert(compiled.errors(hidden).exists(_.contains("cannot flow into capture set {}")))
    }
  }
}
