package grit.core.files

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about the file area: an [[OpenFile]] kept past the block of
  * [[Files.open]] or [[Intake.put]] that opened it, and an [[Admitted]] made anywhere but by
  * [[Files.admit]], or recorded by a step. Pinned by compiling probe sources against core, as
  * `TxCaptureTests` does.
  */
object OpenFileCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import java.time.Instant
      |import grit.core.durable.*
      |import grit.core.files.*
      |import grit.core.id.*
      |""".stripMargin

  /** One way to open a file: the parameters that hold it, and a call opening it for `body`. */
  private final case class Opener(params: String, open: String => String)

  private val openers: Vector[Opener] = Vector(
    Opener("(files: Files^, admitted: Admitted)", body => s"files.open(admitted)($body)"),
    Opener(
      "(intake: Intake^, key: PutKey, origin: FileOrigin, item: FileItem, in: Incoming^)",
      body => s"""intake.put(key, "f", None, origin, item, in)($body)"""
    )
  )

  private def uses(o: Opener): String =
    s"def f${o.params}: Any = ${o.open("file => file.bytes")}\n"

  private def keptInVar(o: Opener): String =
    s"""def f${o.params}: Unit = {
       |  var kept: Option[OpenFile^] = None
       |  val _ = ${o.open("file => { kept = Some(file) }")}
       |}
       |""".stripMargin

  private def returned(o: Opener): String =
    s"""def f${o.params}: Unit = {
       |  val t = ${o.open("file => file")}
       |}
       |""".stripMargin

  private def closure(o: Opener): String =
    s"""def f${o.params}: Unit = {
       |  val t = ${o.open("file => () => file.bytes")}
       |}
       |""".stripMargin

  private val madeOutside =
    "def f(record: FileRecord): Admitted = new Admitted(record)\n"

  private val copiedOutside =
    "def f(admitted: Admitted, record: FileRecord): Admitted = admitted.copy(record = record)\n"

  private val journaled = "def f: Any = summon[Journaled[Admitted]]\n"

  private val probes: Vector[String] =
    openers.map(uses) ++ openers.map(keptInVar) ++ openers.map(returned) ++
      openers.map(closure) ++ Vector(madeOutside, copiedOutside, journaled)

  private lazy val compiled = Probes.batch(prelude, probes.toList)

  private def errors(body: String): List[String] = compiled.errors(body)

  /** Whether `errs` reject an open file for flowing into the variable `kept`. */
  private def kept(errs: List[String]): Boolean =
    errs.exists(e =>
      e.contains("OpenFile^") && e.contains("cannot flow into capture set {any}") &&
        e.contains("variable kept")
    )

  /** Whether `errs` reject an open file for outliving the block that opened it. */
  private def outlives(errs: List[String]): Boolean =
    errs.exists(e => e.contains("outlives its scope") && e.contains("OpenFile^"))

  val tests = Tests {
    test(
      "two compiles answer every probe: the typer's refusals of an Admitted made, then the rest"
    ) {
      compiled.runs ==> 2
    }

    test("control: each opener's body may read its file and return what it read") {
      openers.flatMap(o => errors(uses(o))) ==> Vector.empty
    }

    test("an open file kept in a variable outside its block is rejected, for each opener") {
      openers.map(o => kept(errors(keptInVar(o)))) ==> Vector(true, true)
    }

    test("an open file returned from its block is rejected, for each opener") {
      openers.map(o => outlives(errors(returned(o)))) ==> Vector(true, true)
    }

    test("an open file captured in a closure its block returns is rejected, for each opener") {
      openers.map(o => outlives(errors(closure(o)))) ==> Vector(true, true)
    }

    test("an Admitted is made by admit alone: built or copied outside grit, it is rejected") {
      Vector(madeOutside, copiedOutside).map(p =>
        errors(p).exists(e => e.contains("Admitted") && e.contains("cannot be accessed"))
      ) ==> Vector(true, true)
    }

    test("no step records an Admitted: it has no stored form") {
      assert(
        errors(journaled).exists(_.contains("grit.core.files.Admitted needs a Journaled instance"))
      )
    }
  }
}
