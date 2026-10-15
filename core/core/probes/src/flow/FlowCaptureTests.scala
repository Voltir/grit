package grit.core.flow

import grit.core.durable.Probes

import utest.*

/** What capture and separation checking reject about the flow types, from code outside their
  * package. Each probe holds one breach; the controls must compile.
  *
  * Expires: delete once capture and separation checking are no longer experimental. Until then
  * an upgrade can change what they reject silently; this suite is re-read at each Scala bump.
  */
object FlowCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.durable.*
      |import grit.core.flow.*
      |import grit.core.visibility.Integrity
      |trait Provider extends caps.SharedCapability { def send(s: String): Int }
      |trait Conn extends caps.ExclusiveCapability { def write(s: String): Int }
      |""".stripMargin

  // Controls: each type's ordinary use, from outside the package.
  private val taintedControl =
    """def f(a: Tainted[String], b: Trusted[Int]): Tainted[(Int, Int)] =
      |  a.map(_.length).zip(b.tainted)
      |""".stripMargin
  private val trustedControl =
    """def f(t: Trusted[String]): (String, Int) =
      |  (Trusted.literal("grit").extract, t.coflatMap(_.extract.length).map(_ + 1).extract)
      |""".stripMargin
  private val checkControl =
    """def f(t: Tainted[String]): Either[Tainted[String], Trusted[String]] =
      |  Trusted.check(t, Integrity.Member)
      |""".stripMargin
  private val taintControl =
    """def f(t: Taint^)(using d: Durable^): Tainted[String] = {
      |  val read = d.step("gate") { () => "outside" }
      |  t.lower(Integrity.Outside)
      |  t.stamp(read)
      |}
      |""".stripMargin

  // A pure combinator given an impure function.
  private val taintedProvider =
    """def f(t: Tainted[String], p: Provider^): Tainted[Int] =
      |  t.map(s => p.send(s))
      |""".stripMargin
  private val taintedConn =
    """def f(t: Tainted[String], tx: Conn^): Tainted[Int] =
      |  t.map(s => tx.write(s))
      |""".stripMargin
  private val taintedVar =
    """def f(t: Tainted[String]): String = {
      |  var out = ""
      |  t.map { s => out = s; 0 }
      |  out
      |}
      |""".stripMargin
  private val trustedProvider =
    """def f(t: Trusted[String], p: Provider^): Trusted[Int] =
      |  t.map(s => p.send(s))
      |""".stripMargin
  private val trustedCoflatProvider =
    """def f(t: Trusted[String], p: Provider^): Trusted[Int] =
      |  t.coflatMap(w => p.send(w.extract))
      |""".stripMargin

  // Minting outside Mint.
  private val taintedNew = "def f(s: String): Tainted[String] = new Tainted(s, Integrity.Member)"
  private val taintedOpen = "def f(t: Tainted[String]): String = t.open"
  private val trustedNew = "def f(s: String): Trusted[String] = new Trusted(s, Integrity.Member)"
  private val trustedAt = "def f(s: String): Trusted[String] = Trusted.at(s, Integrity.Member)"
  private val trustedLiteral = "def f(s: String): Trusted[String] = Trusted.literal(s)"
  private val taintNew = "def f(): Taint^ = new Taint(Integrity.Member)"
  private val downgradeNew =
    "def f(d: Downgrade.Described[?]): Downgrade[Int, Int] = new Downgrade(d, i => i)"
  private val boundedForged = "given Bounded[String, 1] = new Bounded[String, 1] {}"

  // The cell: exclusive, lowered only with the Durable, never inside a step's body.
  private val taintInMap =
    """def f(t: Taint^, x: Tainted[Int]): Tainted[Int] =
      |  x.map(n => { t.stamp(n); n })
      |""".stripMargin
  private val taintInPure =
    "def f(t: Taint^): () -> Integrity = () => t.level"
  private val taintInStep =
    """def f(t: Taint^)(using d: Durable^): String =
      |  d.step("gate") { () => t.lower(Integrity.Outside); "lowered" }
      |""".stripMargin
  private val taintReadOnly =
    """def f(t: Taint^{caps.any.rd})(using Durable^): Unit = t.lower(Integrity.Outside)
      |""".stripMargin
  private val taintRaise = "def f(t: Taint^): Unit = t.current = Integrity.Member"
  private val taintTwice =
    """def both(a: Taint^, b: Taint^)(using Durable^): Unit = {
      |  a.lower(Integrity.Outside)
      |  b.lower(Integrity.Member)
      |}
      |def f(t: Taint^)(using Durable^): Unit = both(t, t)
      |""".stripMargin

  // Holes, pinned so that a compiler which starts rejecting one is noticed.
  private val leakBuffer =
    """def f(t: Tainted[String]): String = {
      |  val out = scala.collection.mutable.ArrayBuffer.empty[String]
      |  t.map { s => out += s; 0 }
      |  out.mkString
      |}
      |""".stripMargin
  private val leakThrow =
    """def f(t: Tainted[String]): String =
      |  try { t.map(s => throw new RuntimeException(s)); "" }
      |  catch { case e: RuntimeException => e.getMessage }
      |""".stripMargin
  private val trustedLaundered =
    """def f(t: Trusted[Unit], raw: String): Trusted[String] =
      |  t.coflatMap(_ => raw)
      |""".stripMargin
  private val endorseSplit =
    """type In = (value: Tainted[Option[Extracted]], level: Integrity)
      |def f(a: Tainted[Option[Extracted]], b: Tainted[Option[Extracted]])(
      |    r: Raise[In, Trusted[Option[Extracted]]]^
      |): Trusted[(Option[Extracted], Option[Extracted])] =
      |  r((value = a, level = Integrity.Member)).zip(r((value = b, level = Integrity.Member)))
      |""".stripMargin
  private val capabilityInside =
    """def f(t: Tainted[Provider^]): Tainted[Int] =
      |  t.map(p => p.send("raw"))
      |""".stripMargin

  private val controls =
    List(taintedControl, trustedControl, checkControl, taintControl)
  private val holes = List(leakBuffer, leakThrow, trustedLaundered, endorseSplit)

  private val all: List[String] = controls ++ List(
    taintedProvider,
    taintedConn,
    taintedVar,
    trustedProvider,
    trustedCoflatProvider,
    taintedNew,
    taintedOpen,
    trustedNew,
    trustedAt,
    taintNew,
    downgradeNew,
    boundedForged,
    taintInMap,
    taintInPure,
    taintInStep,
    taintReadOnly,
    taintRaise,
    taintTwice,
    capabilityInside
  ) ++ holes

  private val noSeparation =
    Probes.options.filterNot(_ == "-language:experimental.separationChecking")
  private val noCapture =
    noSeparation.filterNot(_ == "-language:experimental.captureChecking")

  private lazy val compiled = Probes.batch(prelude, all)

  private def errs(probe: String): List[String] = compiled.errors(probe)
  private def says(text: String)(found: List[String]): Boolean = found.exists(_.contains(text))
  private def pure(name: String): String = s"capability `$name` cannot flow into capture set {}"

  val tests = Tests {
    test("the probes reach capture checking in two runs: the typer's refusals, then the rest") {
      compiled.runs ==> 2
    }

    test("controls: each type's ordinary use compiles outside the package") {
      controls.map(errs) ==> controls.map(_ => Nil)
    }

    test("map and coflatMap reject a function that captures a capability or a local var") {
      assert(
        says(pure("p"))(errs(taintedProvider)),
        says(pure("tx"))(errs(taintedConn)),
        says(pure("out"))(errs(taintedVar)),
        says(pure("p"))(errs(trustedProvider)),
        says(pure("p"))(errs(trustedCoflatProvider))
      )
    }

    test("capture checking is what rejects the capturing function") {
      val off =
        Probes.batch(Probes.erased(prelude), List(Probes.erased(taintedProvider)), noCapture)
      off.errors(Probes.erased(taintedProvider)) ==> Nil
    }

    test("nothing outside the package makes a tainted value or opens one") {
      assert(
        says("constructor Tainted cannot be accessed")(errs(taintedNew)),
        says("method open cannot be accessed")(errs(taintedOpen))
      )
    }

    test("nothing outside the package makes a trusted value from raw data") {
      assert(
        says("constructor Trusted cannot be accessed")(errs(trustedNew)),
        says("method at cannot be accessed")(errs(trustedAt)),
        says("expected a constant value but found: s")(Probes.errors(prelude + trustedLiteral))
      )
    }

    test("nothing outside the package makes a cell, a downgrade or a bound") {
      assert(
        says("constructor Taint cannot be accessed")(errs(taintNew)),
        says("constructor Downgrade cannot be accessed")(errs(downgradeNew)),
        says("Cannot extend sealed class Bounded")(errs(boundedForged))
      )
    }

    test("the cell does not escape into a map or a pure function") {
      assert(says(pure("t.rd"))(errs(taintInMap)), says(pure("t.rd"))(errs(taintInPure)))
    }

    test("the cell is not lowered inside a step's body") {
      assert(says("Separation failure")(errs(taintInStep)))
    }

    test("separation checking is what rejects lowering inside a step's body") {
      Probes.batch(prelude, List(taintInStep), noSeparation).errors(taintInStep) ==> Nil
    }

    test("a read-only alias cannot lower the cell, nothing outside can raise it") {
      assert(
        says("Cannot call update method lower of t")(errs(taintReadOnly)),
        says("variable current cannot be accessed")(errs(taintRaise))
      )
    }

    test("the cell is exclusive: one cell is not passed as two parameters") {
      assert(says("Separation failure")(errs(taintTwice)))
      Probes.batch(prelude, List(taintTwice), noSeparation).errors(taintTwice) ==> Nil
    }

    // `Tainted`'s `T` is unbounded, since neither `ujson.Obj` nor `Int` nor a tuple conforms to
    // `caps.Pure`; a capability it holds is still not called by a pure function, through its
    // reach capability.
    test("a capability inside a tainted value is not called in its map") {
      assert(says(pure("t*"))(errs(capabilityInside)))
    }

    test("holes: what grit's flags accept that leaks or launders") {
      holes.map(errs) ==> holes.map(_ => Nil)
    }
  }
}
