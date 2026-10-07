package grit.core.store

import grit.core.durable.Probes

import Probes.options
import utest.*

/** What capture checking rejects about a [[Tx]]: one kept past the opener that handed it out.
  * Pinned by compiling probe sources against core with core's own flags, since
  * `assertCompileError` never reports a capture-checking error (docs/capture-checking.md).
  */
object TxCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.store.*
      |object Probe {
      |""".stripMargin

  private def errors(body: String, flags: List[String] = options): List[String] =
    Probes.errors(prelude + body + "\n}\n", flags)

  private val withoutSeparation =
    options.filterNot(_ == "-language:experimental.separationChecking")

  private val keptInVar =
    """def f(db: Db^): Unit = {
      |  var kept: Option[Tx^] = None
      |  val _ = db.read { kept = Some(summon[Tx^]); Right(()) }
      |}
      |""".stripMargin

  private val returned =
    """def f(db: Db^): Unit = {
      |  val t = db.read(Right(summon[Tx^]))
      |}
      |""".stripMargin

  /** Whether `errs` reject a transaction's `Tx` for flowing into the variable `kept`. */
  private def kept(errs: List[String]): Boolean =
    errs.exists(e =>
      e.contains("grit.core.store.Tx^") && e.contains("cannot flow into capture set {any}") &&
        e.contains("variable kept")
    )

  /** Whether `errs` reject a transaction's `Tx` for outliving the read that opened it. */
  private def outlives(errs: List[String]): Boolean =
    errs.exists(e => e.contains("outlives its scope") && e.contains("grit.core.store.Tx^"))

  val tests = Tests {
    test("control: a read that keeps only what it read compiles") {
      errors(
        """def f(db: Db^): Either[StoreError, Int] =
          |  db.read { val _ = summon[Tx^]; Right(1) }
          |""".stripMargin
      ) ==> Nil
    }

    test("a Tx kept in a variable outside its read is rejected") {
      val errs = errors(keptInVar)
      assert(kept(errs))
    }

    test("a Tx returned from its read is rejected") {
      val errs = errors(returned)
      assert(outlives(errs))
    }

    test("capture checking, not separation checking, rejects a kept Tx") {
      assert(
        kept(errors(keptInVar, withoutSeparation)),
        outlives(errors(returned, withoutSeparation))
      )
    }
  }
}
