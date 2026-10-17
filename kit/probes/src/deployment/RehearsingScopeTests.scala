package grit.kit.deployment

import grit.core.durable.Probes

import utest.*

/** A rehearsal's session lives only inside its block (`Rehearsing.apply`): capture checking
  * rejects the session, its reads or its writes carried out of it, so nothing can reach the
  * session after its transaction is rolled back and its connection closed.
  */
object RehearsingScopeTests extends TestSuite {

  private val prelude =
    """package grit.probe
      |import grit.core.clock.Clock
      |import grit.core.spend.Budget
      |import grit.core.store.{Db, Jot, StoreError}
      |import grit.core.visibility.{Subject, Visibility}
      |import grit.dbos.engine.Rehearsing
      |import grit.dbos.sql.DbConfig
      |""".stripMargin

  /** A block answering `body`, of type `out`, out of a session. */
  private def within(out: String, body: String): String =
    s"""def f(c: DbConfig, b: Budget, v: Visibility, k: Clock^): Either[Rehearsing.Unrehearsed, $out] =
       |  Rehearsing(c, b, v, k) { r => $body }
       |""".stripMargin

  private val readOut = within("Either[StoreError, Int]", "r.db.read(Subject.Public)(Right(1))")

  private val session = within("Rehearsing^", "r")

  private val reads = within("Db^", "r.db")

  private val writes = within("Jot^", "r.jot")

  private val writing =
    within("() => Either[StoreError, Int]", "() => r.jot.write(Subject.Public)(Right(1))")

  private val escapes = List(session, reads, writes, writing)

  private lazy val compiled = Probes.batch(prelude, readOut :: escapes)

  /** Whether `errs` reject a capability for leaving the block it was given to: as a value it
    * outlives its scope; captured by a closure, it is not visible where the closure goes.
    */
  private def leaves(errs: List[String]): Boolean =
    errs.exists(e => e.contains("outlives its scope") || e.contains("is not visible from"))

  val tests = Tests {
    test("the probe environment is set") {
      assert(Probes.classpath.nonEmpty)
    }

    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("a value read inside the block is carried out of it") {
      assert(compiled.errors(readOut).isEmpty)
    }

    test("the session carried out of its block is rejected") {
      assert(leaves(compiled.errors(session)))
    }

    test("the session's reads carried out of its block are rejected") {
      assert(leaves(compiled.errors(reads)))
    }

    test("the session's writes carried out of its block are rejected") {
      assert(leaves(compiled.errors(writes)))
    }

    test("a closure writing through the session, carried out of its block, is rejected") {
      assert(leaves(compiled.errors(writing)))
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
