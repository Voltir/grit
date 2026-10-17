package grit.kit.deployment

import grit.core.durable.Probes

import utest.*

/** A rehearsal's session lives only inside its block (`Rehearsing.apply`): capture checking
  * rejects the session, its reads, its writes or a body over them carried out of it, as a
  * session or as the `Runtime` a launch sees, and a runner keeping a body it was handed, so
  * nothing can reach the session after its transaction is rolled back and its connection
  * closed.
  */
object RehearsingScopeTests extends TestSuite {

  private val prelude =
    """package grit.probe
      |import grit.core.clock.Clock
      |import grit.core.durable.Durable
      |import grit.core.id.{TurnRef, WorkflowId}
      |import grit.core.inbox.InboxError
      |import grit.core.spend.Budget
      |import grit.core.stitch.Placements
      |import grit.core.store.{Db, Jot, StoreError}
      |import grit.core.visibility.{Subject, Visibility}
      |import grit.dbos.engine.{Rehearsing, Runtime}
      |import grit.dbos.sql.DbConfig
      |""".stripMargin

  /** A block answering `body`, of type `out`, out of a session. */
  private def within(out: String, body: String): String =
    s"""def f(c: DbConfig, b: Budget, v: Visibility, k: Clock^, n: Rehearsing.Runner^): Either[Rehearsing.Unrehearsed, $out] =
       |  Rehearsing(c, b, v, k, n) { r => $body }
       |""".stripMargin

  private val readOut = within("Either[StoreError, Int]", "r.db.read(Subject.Public)(Right(1))")

  /** The control for the runtime view: a pure value read through a `Runtime`'s reads. */
  private val runtimeReadOut =
    within(
      "Either[StoreError, Int]",
      "{ val rt: Runtime^ = r; rt.db.read(Subject.Public)(Right(1)) }"
    )

  private val session = within("Rehearsing^", "r")

  private val reads = within("Db^", "r.db")

  private val writes = within("Jot^", "r.jot")

  private val writing =
    within("() => Either[StoreError, Int]", "() => r.jot.write(Subject.Public)(Right(1))")

  private val runtime = within("Runtime^", "{ val rt: Runtime^ = r; rt }")

  private val placements = within("Placements^", "r.placements")

  private val body =
    within(
      "WorkflowId => Durable^ ?=> String",
      "(id: WorkflowId) => (_: Durable^) ?=> r.db.read(Subject.Public)(Right(1)).toString"
    )

  /** A runner that keeps the turn body it is handed in a `var` declared outside the block. */
  private val kept =
    """def f(c: DbConfig, b: Budget, v: Visibility, k: Clock^, p: Placements^): Unit = {
      |  var turnBody: WorkflowId => Durable^ ?=> String = Runtime.Unrun
      |  val n = new Rehearsing.Runner {
      |    def launched(
      |        session: Rehearsing^,
      |        turn: WorkflowId => Durable^ ?=> String,
      |        close: WorkflowId => Durable^ ?=> String,
      |        settle: WorkflowId => Durable^ ?=> String,
      |        post: WorkflowId => Durable^ ?=> String,
      |        triage: WorkflowId => Durable^ ?=> String,
      |        stitch: WorkflowId => Durable^ ?=> String,
      |        shadow: WorkflowId => Durable^ ?=> String,
      |        run: WorkflowId => Durable^ ?=> String
      |    ): Unit = { turnBody = turn }
      |    def started(turn: TurnRef): Either[InboxError, Unit] = Right(())
      |    val placements: Placements^{p} = p
      |  }
      |  Rehearsing(c, b, v, k, n) { r =>
      |    r.launch(
      |      id => (_: Durable^) ?=> r.db.read(Subject.Public)(Right(1)).toString,
      |      Runtime.Unrun, Runtime.Unrun, Runtime.Unrun, Runtime.Unrun, Runtime.Unrun,
      |      Vector.empty
      |    )
      |  }
      |  ()
      |}
      |""".stripMargin

  private val escapes = List(session, reads, writes, writing, runtime, placements, body, kept)

  private lazy val compiled = Probes.batch(prelude, readOut :: runtimeReadOut :: escapes)

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

    test("a pure value read through the session as a runtime is carried out of it") {
      assert(compiled.errors(runtimeReadOut).isEmpty)
    }

    test("the session carried out of its block as a runtime is rejected") {
      assert(leaves(compiled.errors(runtime)))
    }

    test("the session's placements carried out of its block are rejected") {
      assert(leaves(compiled.errors(placements)))
    }

    test("a body over the session's reads, carried out of its block, is rejected") {
      assert(leaves(compiled.errors(body)))
    }

    test("a runner keeping the body it is handed in a var outside the block is rejected") {
      assert(leaves(compiled.errors(kept)))
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
