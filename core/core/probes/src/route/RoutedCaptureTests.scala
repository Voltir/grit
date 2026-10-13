package grit.core.route

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about a routing: one made other than from a transaction (typer
  * errors), and the reads [[Routed.reading]] hands its body kept past it (capture errors).
  * Compiled in a probe package of its own against core.
  */
object RoutedCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.model.Pins
      |import grit.core.route.Routed
      |import grit.core.store.*
      |import grit.core.visibility.*
      |""".stripMargin

  private val read =
    """def read(db: Db^, s: Subject, pins: Pins): Either[StoreError, Either[StoreError, Boolean]] =
      |  Routed.reading(db, s)((r, reads) => reads.read(Right(r.cleared(pins).nonEmpty)))""".stripMargin
  private val applied =
    "def f(using Tx^): Routed = Routed(Tx.classification, Tx.visibility(summon[Tx^]).clearances)"
  private val constructed =
    "def f(using Tx^): Routed = new Routed(Tx.classification, Tx.visibility(summon[Tx^]).clearances)"
  private val copied =
    "def f(r: Routed)(using Tx^): Routed = r.copy(classification = Tx.classification)"
  private val keptInVar =
    """def f(db: Db^, s: Subject): Unit = {
      |  var kept: Option[Reads^] = None
      |  val _ = Routed.reading(db, s)((_, reads) => kept = Some(reads))
      |}""".stripMargin
  private val returned =
    """def f(db: Db^, s: Subject): Unit = {
      |  val _ = Routed.reading(db, s)((_, reads) => reads)
      |}""".stripMargin

  private val keptFromReads =
    """def f(handed: Reads^): Unit = {
      |  var kept: Option[Reads^] = None
      |  val _ = Routed.reading(handed)((_, reads) => kept = Some(reads))
      |}""".stripMargin
  private val returnedFromReads =
    """def f(handed: Reads^): Unit = {
      |  val _ = Routed.reading(handed)((_, reads) => reads)
      |}""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    List(read, applied, constructed, copied, keptInVar, returned, keptFromReads, returnedFromReads)
  )

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("two compiles answer every probe: the typer's refusals, then the rest") {
      compiled.runs ==> 2
    }

    test("control: a site routes one read, then reads bounded by it") {
      probe(read) ==> Nil
    }

    test("a routing is not made outside core but from a transaction") {
      Vector(applied, constructed, copied).map(probe(_).map(_.linesIterator.next())) ==> Vector(
        List("object Routed in package grit.core.route does not take parameters"),
        List(
          "constructor Routed cannot be accessed as a member of grit.core.route.Routed from object Probe2."
        ),
        List(
          "method copy cannot be accessed as a member of (r : grit.core.route.Routed) from object Probe3."
        )
      )
    }

    test(
      "the reads a routed body is handed are not kept in a variable outside it, through a db or reads"
    ) {
      Vector(keptInVar, keptFromReads).map(p =>
        probe(p).exists(e => e.contains("Reads^") && e.contains("variable kept"))
      ) ==> Vector(true, true)
    }

    test("the reads a routed body is handed are not returned from it, through a db or reads") {
      Vector(returned, returnedFromReads).map(p =>
        probe(p).exists(e => e.contains("Reads^") && e.contains("outlives its scope"))
      ) ==> Vector(true, true)
    }
  }
}
