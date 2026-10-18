package grit.core.act

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, flowsInto, heldImpure, options}

import utest.*

/** What capture checking rejects about acting (ADR 0034): the acting value is data; a job is
  * handed its moves for one run and keeps none, and makes no model call but through them; a
  * keep's body reaches its keeper and transaction alone, and returns nothing that holds either;
  * a reply's reader (`grit.core.schema.Typed`'s `read`) makes no move.
  * Pinned by compiling probe sources against core with core's own flags, as
  * `grit.core.job.JobCaptureTests` does; `assertCompileError` cannot see capture errors
  * (docs/capture-checking.md). The jobs are core's own, `PlainJob` and `KeepingJob`, and the
  * keeps `Keeping.keep`. What a run's body may do with the moves `grit.act.moves.DurableMoves`
  * hands it is `grit.act.moves.MovesCaptureTests`'.
  */
object ActCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import java.time.Instant
      |import scala.concurrent.duration.*
      |import grit.core.act.*
      |import grit.core.classify.{Ask, Classifier, Level, StateJson}
      |import grit.core.document.*
      |import grit.core.durable.*
      |import grit.core.id.*
      |import grit.core.flow.{Tainted, Trusted}
      |import grit.core.job.{DreamingJob, JobRun, KeepingJob, PlainJob}
      |import grit.core.message.*
      |import grit.core.provider.*
      |import grit.core.stitch.*
      |import grit.core.store.*
      |import grit.core.visibility.*
      |
      |final case class Count(n: Int) extends caps.Pure
      |object Count {
      |  given Journaled[Count] =
      |    Journaled.json(c => ujson.Num(c.n), v => v.numOpt.map(d => Count(d.toInt)).toRight("no count"))
      |}
      |trait Counts {
      |  val name: JobName = JobName.of("count").fold(sys.error, identity)
      |  val version: Int = 1
      |  def write(params: Count): ujson.Value = ujson.Num(params.n)
      |  def read(params: ujson.Value): Either[String, Count] = params.numOpt.map(d => Count(d.toInt)).toRight("no count")
      |}
      |abstract class Counted extends PlainJob[Count], Counts
      |abstract class Tallied extends KeepingJob[Count], Counts
      |abstract class Dreamt extends DreamingJob[Count], Counts
      |object Text {
      |  given StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))
      |}
      |object Names {
      |  def of(text: String): MoveName = MoveName.of(text).fold(sys.error, identity)
      |  val request: ModelRequest = ModelRequest("", Vector.empty)
      |  def open[A](t: Tainted[A]): A = Trusted.asIs(t).extract
      |}
      |""".stripMargin

  /** The error messages from compiling `body` after the prelude, with every probe below. */
  private def errors(body: String): List[String] = compiled.errors(body)

  /** 1. A job that keeps the moves it is handed in a field. */
  private val jobKeepsMoves =
    """final class Keeps extends Counted {
      |  var kept: Option[Moves^] = None
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = { kept = Some(moves); "kept" }
      |}
      |""".stripMargin

  /** 2. A job that stashes the moves it is handed in a mutable collection. */
  private val jobStashesMoves =
    """final class Stashes extends Counted {
      |  val kept: scala.collection.mutable.ArrayBuffer[Moves^] = scala.collection.mutable.ArrayBuffer.empty
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = { kept += moves; "stashed" }
      |}
      |""".stripMargin

  /** 3. A keeping job whose keep's body makes a move. */
  private val keepAsks =
    """final class Asks extends Tallied {
      |  def run(run: Trusted[JobRun[Count]], moves: Keeping^): String =
      |    moves.keep(Names.of("count"), run.tainted) { (_, keeper, at) =>
      |      val _ = moves.ask(Names.of("inner"), run.tainted.map(_ => Posed.Text(Names.request)))
      |      Right(Count(1))
      |    }.fold(_.toString, _.toString)
      |}
      |""".stripMargin

  /** 4. A job holding a provider it was built with. */
  private val jobHoldsProvider =
    """final class Calls(provider: Provider) extends Counted {
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = { val _ = provider; "called" }
      |}
      |""".stripMargin

  /** 5. A keeping job whose keep's body returns a read to run after its transaction is gone. */
  private val keepReturnsRead =
    """given anyJournaled[A]: Journaled[A] = new Journaled[A] {
      |  def encode(a: A): String = ""
      |  def decode(s: String): Either[String, A] = Left("never read")
      |}
      |final class Later(key: DocKey, label: Label) extends Tallied {
      |  def run(run: Trusted[JobRun[Count]], moves: Keeping^): String =
      |    moves.keep(Names.of("later"), run.tainted) { (_, keeper, at) => Right(() => keeper.current(key, label)) }
      |      .fold(_.toString, _ => "later")
      |}
      |""".stripMargin

  /** 6. A job whose reply's reader makes a move: a JSON ask whose `Typed`'s `read` asks. */
  private val readerAsks =
    """final class Reads(schema: grit.core.schema.JsonSchema) extends Counted {
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = {
      |    val typed = grit.core.schema.Typed[String](schema, c =>
      |      moves.ask(Names.of("inner"), run.tainted.map(_ => Posed.Text(Names.request))).left.map(_.toString).map(_ => "read")
      |    )
      |    moves.ask(Names.of("count"), run.tainted.map(_ => Posed.Json("", Vector.empty, typed))).fold(_.toString, a => Names.open(a.reply))
      |  }
      |}
      |""".stripMargin

  /** 7. A judgment's reader that asks a classifier: an `Ask` mapped through a function that
    * calls one.
    */
  private val judgmentReaderClassifies =
    """final class Maps(classifier: Classifier) {
      |  import Text.given
      |  def asked: Ask[String, Double] =
      |    Ask.yesNo[String]("Is `text` urgent?", None, None).map { p =>
      |      val _ = classifier.ask("x", Ask.yesNo[String]("Is `text` late?", None, None))
      |      p
      |    }
      |}
      |""".stripMargin

  /** 8. A job holding a classifier it was built with. */
  private val jobHoldsClassifier =
    """final class Judges(classifier: Classifier) extends Counted {
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = { val _ = classifier; "judged" }
      |}
      |""".stripMargin

  /** 9. A dreaming job whose scan's body makes a move. */
  private val scanAsks =
    """final class ScanAsks extends Dreamt {
      |  def run(run: Trusted[JobRun[Count]], moves: Dreaming^): String =
      |    moves.scan(Names.of("scan"), run.tainted) { (_, keeper, strands, at) =>
      |      val _ = moves.ask(Names.of("inner"), run.tainted.map(_ => Posed.Text(Names.request)))
      |      Right(Count(1))
      |    }.fold(_.toString, _.toString)
      |}
      |""".stripMargin

  /** 10. A dreaming job whose scan's body returns a read of its strand shelf for later. */
  private val scanReturnsShelf =
    """given anyJournaled[A]: Journaled[A] = new Journaled[A] {
      |  def encode(a: A): String = ""
      |  def decode(s: String): Either[String, A] = Left("never read")
      |}
      |final class Later(roots: Vector[ConversationId]) extends Dreamt {
      |  def run(run: Trusted[JobRun[Count]], moves: Dreaming^): String =
      |    moves.scan(Names.of("later"), run.tainted) { (_, keeper, strands, at) => Right(() => strands.of(roots, at)) }
      |      .fold(_.toString, _ => "later")
      |}
      |""".stripMargin

  /** 11. A dreaming job that keeps the moves it is handed in a field. */
  private val dreamKeepsMoves =
    """final class DreamKeeps extends Dreamt {
      |  var kept: Option[Dreaming^] = None
      |  def run(run: Trusted[JobRun[Count]], moves: Dreaming^): String = { kept = Some(moves); "kept" }
      |}
      |""".stripMargin

  /** 12. A keeping job asking its moves for a strand read: a plain type error, so that widening
    * [[Keeping]] to read strands is a decision, not an accident.
    */
  private val keepScans =
    """final class Scans extends Tallied {
      |  def run(run: Trusted[JobRun[Count]], moves: Keeping^): String =
      |    moves.scan(Names.of("scan"), run.tainted) { (_, keeper, strands, at) => Right(Count(1)) }
      |      .fold(_.toString, _.toString)
      |}
      |""".stripMargin

  /** Every breach capture checking rejects; [[keepReturnsRead]] is the bound on what a keep
    * returns, rejected with the checker off too.
    */
  private val breaches: Vector[String] =
    Vector(
      jobKeepsMoves,
      jobStashesMoves,
      keepAsks,
      jobHoldsProvider,
      readerAsks,
      judgmentReaderClassifies,
      jobHoldsClassifier,
      scanAsks,
      dreamKeepsMoves
    )

  private val usesMoves =
    """object Counter extends Counted {
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String =
      |    moves.ask(Names.of("count"), run.tainted.map(_ => Posed.Text(Names.request))).fold(_.toString, a => Names.open(a.reply).toString)
      |}
      |final class Tally(key: DocKey, label: Label) extends Tallied {
      |  def run(run: Trusted[JobRun[Count]], moves: Keeping^): String =
      |    moves.keep(Names.of("count"), run.tainted) { (_, keeper, at) => keeper.current(key, label).map(d => Count(d.fold(0)(_ => 1))) }
      |      .fold(_.toString, _.toString)
      |}
      |""".stripMargin

  private val dreams =
    """final class Dreams(summary: Dreaming.Summary[String]) extends Dreamt {
      |  def run(run: Trusted[JobRun[Count]], moves: Dreaming^): String =
      |    moves.scan(Names.of("scan"), run.tainted) { (_, keeper, strands, at) => strands.atFloor(at, at).map(s => Count(s.size)) }
      |      .fold(_.toString, c => moves.summarise(Names.of("one"), ???, run.tainted.map(_ => summary)).fold(_.toString, _.fold("none")(a => Names.open(a.reply))))
      |}
      |""".stripMargin

  private val readsJson =
    """final class Reads(schema: grit.core.schema.JsonSchema) extends Counted {
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = {
      |    val typed = grit.core.schema.Typed[String](schema, c => Right("read"))
      |    val shaped = moves.ask(Names.of("count"), run.tainted.map(_ => Posed.Json("", Vector.empty, typed))).fold(_.toString, a => Names.open(a.reply))
      |    val json = moves.ask(Names.of("json"), run.tainted.map(_ => Posed.json("", Vector.empty, schema))).fold(_.toString, a => Names.open(a.reply).text)
      |    val asked = moves.ask(Names.of("text"), run.tainted.map(_ => Posed.Text(Names.request))).fold(_.toString, a => Names.open(a.reply).toString)
      |    s"$shaped $json $asked"
      |  }
      |}
      |""".stripMargin

  private val judges =
    """final class Weighs(schema: grit.core.schema.JsonSchema) extends Counted {
      |  import Text.given
      |  def run(run: Trusted[JobRun[Count]], moves: Moves^): String = {
      |    val judged = Ask.score[String, Int]("How urgent is `text`?", Level(0, "not"), Level(1, "very"))
      |      .fold(_.toString, ask =>
      |        moves.ask(Names.of("urgency"), run.tainted.map(_ => Posed.judge("the build is red", ask))).fold(_.toString, a => Names.open(a.reply).likeliest.toString)
      |      )
      |    val typed = grit.core.schema.Typed[String](schema, c => Right("read"))
      |    val shaped = moves.ask(Names.of("count"), run.tainted.map(_ => Posed.Json("", Vector.empty, typed))).fold(_.toString, a => Names.open(a.reply))
      |    s"$judged $shaped"
      |  }
      |}
      |""".stripMargin

  private val actingPure =
    """final class Child(val parent: Acting) extends caps.Pure {
      |  def closed: Acting = parent.copy(gates = Gates.Closed)
      |}
      |def pure[A <: caps.Pure](a: A): A = a
      |def child(turn: TurnRef): Acting = pure(Acting(turn, ActsFor.Asker, Allowance.Admitted, Gates.Asker(1.minute), Integrity.Authority))
      |""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    breaches.toList ++ List(
      keepReturnsRead,
      scanReturnsShelf,
      keepScans,
      usesMoves,
      dreams,
      readsJson,
      judges,
      actingPure
    )
  )

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("two compiles answer every probe: the typer's refusal of a keep's bound, then the rest") {
      compiled.runs ==> 2
    }

    test(
      "a job using its moves within its run, and a keeping job's keep over its keeper alone, compile"
    ) {
      val errs = errors(usesMoves)
      assert(errs.isEmpty)
    }

    test("a dreaming job scanning its strands, then summarising one, compiles") {
      val errs = errors(dreams)
      assert(errs.isEmpty)
    }

    test("a dreaming job whose scan's body makes a move is rejected") {
      assert(flowsInto("{}")(errors(scanAsks)))
    }

    test(
      "a dreaming job whose scan's body returns its strand shelf for later is rejected by its bound"
    ) {
      val errs = errors(scanReturnsShelf)
      assert(
        errs.exists(e => e.startsWith("Found:    () ->") && e.endsWith("Required: scala.caps.Pure"))
      )
    }

    test("a dreaming job that keeps its moves in a field is rejected") {
      assert(flowsInto("{any}")(errors(dreamKeepsMoves)))
    }

    test("a keeping job's moves offer no strand read") {
      val errs = errors(keepScans)
      assert(errs.exists(_.contains("value scan is not a member of grit.core.act.Keeping")))
    }

    test("a job asking for JSON its pure reader reads, beside asks it makes itself, compiles") {
      val errs = errors(readsJson)
      assert(errs.isEmpty)
    }

    test("a job judging with its pure reader, beside a JSON ask, compiles") {
      val errs = errors(judges)
      assert(errs.isEmpty)
    }

    test("a judgment whose reader asks a classifier is rejected") {
      assert(flowsInto("{}")(errors(judgmentReaderClassifies)))
    }

    test("a job holding a classifier is rejected") {
      assert(heldImpure(errors(jobHoldsClassifier)))
    }

    test("a job whose reply's reader makes a move is rejected") {
      assert(flowsInto("{}")(errors(readerAsks)))
    }

    test("the acting value is pure data: a bound asking for caps.Pure takes it") {
      val errs = errors(actingPure)
      assert(errs.isEmpty)
    }

    test("a job that keeps its moves in a field is rejected") {
      assert(flowsInto("{any}")(errors(jobKeepsMoves)))
    }

    test("a job that stashes its moves in a collection field is rejected") {
      assert(flowsInto("{any}")(errors(jobStashesMoves)))
    }

    test("a keeping job whose keep's body makes a move is rejected") {
      assert(flowsInto("{}")(errors(keepAsks)))
    }

    test("a job holding a provider is rejected") {
      assert(heldImpure(errors(jobHoldsProvider)))
    }

    test("a keeping job whose keep's body returns a read for later is rejected by its bound") {
      val errs = errors(keepReturnsRead)
      assert(
        errs.exists(e => e.startsWith("Found:    () ->") && e.endsWith("Required: scala.caps.Pure"))
      )
    }

    test("capture checking is what rejects each breach but the bounds") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      val errs = Probes.errors(erased(prelude + breaches.mkString("\n") + "\n"), flags)
      assert(errs.isEmpty)
    }
  }
}
