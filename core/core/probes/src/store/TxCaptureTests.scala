package grit.core.store

import grit.core.durable.Probes

import Probes.options
import utest.*

/** What the compiler rejects about opening a transaction: a [[Tx]] kept past the opener that
  * handed it out, a step body that reads its `Durable` to choose whom it reads for, and an
  * opener called without saying whom for. Pinned by compiling probe sources against core with
  * core's own flags, since `assertCompileError` never reports a capture-checking error
  * (docs/capture-checking.md).
  */
object TxCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.durable.*
      |import grit.core.id.*
      |import grit.core.store.*
      |import grit.core.visibility.Subject
      |""".stripMargin

  private val withoutSeparation =
    options.filterNot(_ == "-language:experimental.separationChecking")

  /** One way to open a transaction: the parameters that hold it, and a call opening one for a
    * body that does `effect` and returns `result`, an `Option`.
    */
  private final case class Opener(params: String, open: (String, String) => String)

  private val openers: Vector[Opener] = Vector(
    Opener("(db: Db^)", (effect, result) => s"db.read(Subject.Public) { $effect; Right($result) }"),
    Opener("(reads: Reads^)", (effect, result) => s"reads.read { $effect; Right($result) }"),
    Opener(
      "(jot: Jot^)",
      (effect, result) => s"jot.write(Subject.Public) { $effect; Right($result) }"
    ),
    // A step's output needs a Journaled instance, so one is given for the Option a probe returns.
    Opener(
      "(d: Durable^)(using Journaled[Option[Tx^]], Journaled[Option[String]])",
      (effect, result) => s"""d.transact("t", Subject.Public) { $effect; $result }"""
    )
  )

  private def keptInVar(o: Opener): String =
    s"""def f${o.params}: Unit = {
       |  var kept: Option[Tx^] = None
       |  val _ = ${o.open("kept = Some(summon[Tx^])", "Option.empty[Tx^]")}
       |}
       |""".stripMargin

  private def returned(o: Opener): String =
    s"""def f${o.params}: Unit = {
       |  val t = ${o.open("()", "Option(summon[Tx^])")}
       |}
       |""".stripMargin

  /** Whether `errs` reject a transaction's `Tx` for flowing into the variable `kept`. */
  private def kept(errs: List[String]): Boolean =
    errs.exists(e =>
      e.contains("grit.core.store.Tx^") && e.contains("cannot flow into capture set {any}") &&
        e.contains("variable kept")
    )

  /** Whether `errs` reject a transaction's `Tx` for outliving the opener that opened it. */
  private def outlives(errs: List[String]): Boolean =
    errs.exists(e => e.contains("outlives its scope") && e.contains("grit.core.store.Tx^"))

  private val usesTx: Vector[String] = openers.map(o =>
    s"def f${o.params}: Any =\n  ${o.open("val _ = summon[Tx^]", "Option.empty[String]")}"
  )

  private val subjectComputed =
    """def f(turn: TurnRef)(using d: Durable^): String = {
      |  val subject = Subject.Turn(turn)
      |  d.transact("t", subject) { val _ = summon[Tx^]; "read" }
      |}
      |""".stripMargin

  private val readsDurable =
    """def f(using d: Durable^): String =
      |  d.transact("t", Subject.Public) { if (d.patch("p")) "new" else "old" }
      |""".stripMargin

  private val openedForNoOne: Vector[String] = Vector(
    "def f(db: Db^): Any = db.read { Right(1) }",
    "def f(jot: Jot^): Any = jot.write { Right(1) }",
    """def f(using d: Durable^): String = d.transact("t") { "read" }"""
  )

  private val leaks: Vector[String] = openers.map(keptInVar) ++ openers.map(returned)

  private lazy val compiled = Probes.batch(
    prelude,
    (usesTx ++ leaks ++ Vector(subjectComputed, readsDurable) ++ openedForNoOne).toList
  )

  private lazy val unseparated = Probes.batch(prelude, leaks.toList, withoutSeparation)

  private def errors(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test(
      "two compiles answer every probe: the typer's refusals of a subjectless opening, then the rest"
    ) {
      compiled.runs ==> 2
    }

    test("one compile answers every probe with separation checking off") {
      unseparated.runs ==> 1
    }

    test("control: each opener's body may use its Tx and return what it read") {
      val errs = usesTx.flatMap(errors)
      errs ==> Vector.empty
    }

    test("a Tx kept in a variable outside its opener is rejected, for each opener") {
      val rejected = openers.map(o => kept(errors(keptInVar(o))))
      rejected ==> Vector(true, true, true, true)
    }

    test("a Tx returned from its opener is rejected, for each opener") {
      val rejected = openers.map(o => outlives(errors(returned(o))))
      rejected ==> Vector(true, true, true, true)
    }

    test("capture checking, not separation checking, rejects a kept Tx") {
      val rejected = openers.map(o =>
        kept(unseparated.errors(keptInVar(o))) &&
          outlives(unseparated.errors(returned(o)))
      )
      rejected ==> Vector(true, true, true, true)
    }

    test("control: a step body may capture a subject computed before it") {
      errors(subjectComputed) ==> Nil
    }

    test("a step body that reads its Durable to choose whom it reads for is rejected") {
      val errs = errors(readsDurable)
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("no transaction opens without saying whom it is for") {
      val errs = openedForNoOne.map(errors)
      // A body where the subject goes is read as the subject: a typer error either way.
      val subjectless = (e: String) =>
        e.contains("Required: grit.core.visibility.Subject") ||
          e.contains("missing argument for parameter subject")
      errs.map(_.exists(subjectless)) ==> Vector(true, true, true)
    }
  }
}
