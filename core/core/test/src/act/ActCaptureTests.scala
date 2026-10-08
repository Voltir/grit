package grit.core.act

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, flowsInto, heldImpure, options}

import utest.*

/** What capture checking rejects about acting (ADR 0034): the acting value is data; a job is
  * handed its moves for one run and keeps none, and makes no model call but through them; a
  * keep's body reaches its keeper and transaction alone, and returns nothing that holds either;
  * a run's body makes no step beside its moves, and what it returns cannot carry them out, so
  * the run's next step may use the `Durable` they were made over. Pinned by compiling probe sources against core with core's own
  * flags, as `grit.core.job.JobCaptureTests` does; `assertCompileError` cannot see capture
  * errors (docs/capture-checking.md).
  *
  * `PlainJob` and `Run.plain` here stand in for the job a run is made of and the function that
  * hands it moves over the run's `Durable`, with the bounds those must keep.
  */
object ActCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import java.time.Instant
      |import scala.concurrent.duration.*
      |import grit.core.act.*
      |import grit.core.document.*
      |import grit.core.durable.*
      |import grit.core.id.*
      |import grit.core.message.*
      |import grit.core.provider.*
      |import grit.core.store.*
      |import grit.core.visibility.*
      |
      |final case class Count(n: Int) extends caps.Pure
      |object Count {
      |  given Journaled[Count] =
      |    Journaled.json(c => ujson.Num(c.n), v => v.numOpt.map(d => Count(d.toInt)).toRight("no count"))
      |}
      |final case class Reply(text: String) extends caps.Pure
      |trait PlainJob extends caps.Pure {
      |  def run(input: String, moves: Moves^): Reply
      |}
      |object Run {
      |  def plain[A <: caps.Pure](acting: Acting)(body: Moves^ => A)(using d: Durable^): A = {
      |    val moves = new Moves {
      |      def ask(name: MoveName, request: ModelRequest): Either[MoveError, Asked] =
      |        Left(MoveError.Model(d.step(s"move:${MoveName.value(name)}") { () => "no model" }))
      |      def call(name: MoveName, service: grit.core.place.Service, tool: grit.core.tool.ToolName, arguments: ujson.Obj): Either[MoveError, Called] =
      |        Right(Called.Failed(d.step(s"move:${MoveName.value(name)}") { () => "no edge" }))
      |    }
      |    body(moves)
      |  }
      |}
      |object Names {
      |  def of(text: String): MoveName = MoveName.of(text).fold(sys.error, identity)
      |  val request: ModelRequest = ModelRequest("", Vector.empty)
      |}
      |""".stripMargin

  /** The error messages from compiling `body` after the prelude. */
  private def errors(body: String, flags: List[String] = options): List[String] =
    Probes.errors(prelude + body + "\n", flags)

  /** 1. A job that keeps the moves it is handed in a field. */
  private val jobKeepsMoves =
    """final class Keeps extends PlainJob {
      |  var kept: Option[Moves^] = None
      |  def run(input: String, moves: Moves^): Reply = { kept = Some(moves); Reply(input) }
      |}
      |""".stripMargin

  /** 2. A job that stashes the moves it is handed in a mutable collection. */
  private val jobStashesMoves =
    """final class Stashes extends PlainJob {
      |  val kept: scala.collection.mutable.ArrayBuffer[Moves^] = scala.collection.mutable.ArrayBuffer.empty
      |  def run(input: String, moves: Moves^): Reply = { kept += moves; Reply(input) }
      |}
      |""".stripMargin

  /** 3. A keep whose body makes a move. */
  private val keepAsks =
    """def f(moves: Keeping^): Either[MoveError, Count] =
      |  moves.keep(Names.of("count")) { (keeper, at) =>
      |    val _ = moves.ask(Names.of("inner"), Names.request)
      |    Right(Count(1))
      |  }
      |""".stripMargin

  /** 4. A job holding a provider it was built with. */
  private val jobHoldsProvider =
    """final class Calls(provider: Provider^) extends PlainJob {
      |  def run(input: String, moves: Moves^): Reply = { val _ = provider; Reply(input) }
      |}
      |""".stripMargin

  /** 5. A keep whose body returns a read to run after its transaction is gone. */
  private val keepReturnsRead =
    """given anyJournaled[A]: Journaled[A] = new Journaled[A] {
      |  def encode(a: A): String = ""
      |  def decode(s: String): Either[String, A] = Left("never read")
      |}
      |def f(moves: Keeping^, key: DocKey, label: Label) =
      |  moves.keep(Names.of("later")) { (keeper, at) => Right(() => keeper.current(key, label)) }
      |""".stripMargin

  /** 6. A run's body that returns the moves it is handed. */
  private val bodyReturnsMoves =
    """def f(acting: Acting)(using d: Durable^): Moves^ = Run.plain[Moves^](acting)(moves => moves)
      |""".stripMargin

  /** 7. An acting value that holds a store. */
  private val actingHoldsDb =
    """final case class Acting2(turn: TurnRef, actsFor: ActsFor, allowance: Allowance, gates: Gates, db: Db^) extends caps.Pure
      |""".stripMargin

  /** 8b. A run's body that makes a step of its own while its moves, made over the same
    * `Durable`, are live.
    */
  private val bodySteps =
    """def f(job: PlainJob, acting: Acting)(using d: Durable^): Reply =
      |  Run.plain(acting)(moves => { val seen = d.step("peek") { () => "x" }; job.run(seen, moves) })
      |""".stripMargin

  /** Every breach capture checking rejects; [[keepReturnsRead]] and [[bodyReturnsMoves]] are
    * the bound on what a keep and a run's body return, rejected with the checker off too.
    */
  private val breaches: Vector[String] =
    Vector(jobKeepsMoves, jobStashesMoves, keepAsks, jobHoldsProvider, actingHoldsDb, bodySteps)

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test(
      "a job using its moves within its run, a keep over its keeper alone, and a run's body then its next step, compile"
    ) {
      val errs = errors(
        """object Counter extends PlainJob {
          |  def run(input: String, moves: Moves^): Reply =
          |    moves.ask(Names.of("count"), Names.request).fold(e => Reply(e.toString), a => Reply(a.message.toString))
          |}
          |def kept(moves: Keeping^, key: DocKey, label: Label): Either[MoveError, Count] =
          |  moves.keep(Names.of("count")) { (keeper, at) => keeper.current(key, label).map(d => Count(d.fold(0)(_ => 1))) }
          |def runBody(job: PlainJob, turn: TurnRef)(using d: Durable^): String = {
          |  val acting = Acting(turn, ActsFor.Asker, Allowance.Admitted, Gates.Closed)
          |  val reply = Run.plain(acting)(moves => job.run("input", moves))
          |  d.step("reply") { () => reply.text }
          |}
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("the acting value is pure data: a bound asking for caps.Pure takes it") {
      val errs = errors(
        """final class Child(val parent: Acting) extends caps.Pure {
          |  def closed: Acting = parent.copy(gates = Gates.Closed)
          |}
          |def pure[A <: caps.Pure](a: A): A = a
          |def child(turn: TurnRef): Acting = pure(Acting(turn, ActsFor.Asker, Allowance.Admitted, Gates.Asker(1.minute)))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a job that keeps its moves in a field is rejected") {
      assert(flowsInto("{any}")(errors(jobKeepsMoves)))
    }

    test("a job that stashes its moves in a collection field is rejected") {
      assert(flowsInto("{any}")(errors(jobStashesMoves)))
    }

    test("a keep whose body makes a move is rejected") {
      assert(flowsInto("{}")(errors(keepAsks)))
    }

    test("a job holding a provider is rejected") {
      assert(heldImpure(errors(jobHoldsProvider)))
    }

    test("a keep whose body returns a read for later is rejected by its bound") {
      val errs = errors(keepReturnsRead)
      assert(
        errs.exists(e => e.startsWith("Found:    () ->") && e.endsWith("Required: scala.caps.Pure"))
      )
    }

    test("a run's body that returns its moves is rejected by its bound") {
      val errs = errors(bodyReturnsMoves)
      assert(
        errs.contains(
          "Type argument grit.core.act.Moves^ does not conform to upper bound scala.caps.Pure"
        )
      )
    }

    test("a run's body that makes a step while its moves are live is rejected") {
      assert(errors(bodySteps).exists(_.startsWith("Separation failure")))
    }

    test("an acting value holding a store is rejected") {
      assert(heldImpure(errors(actingHoldsDb)))
    }

    test("capture checking is what rejects each breach but the bounds") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = Probes.errors(erased(prelude + breaches.mkString("\n") + "\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
