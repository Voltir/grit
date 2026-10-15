package grit.core.flow

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about downgrades, from code outside their package: a node made
  * after glue, free text endorsed, a description or raise forged, a raise returned past the
  * body or program it was handed to.
  * Each probe holds one breach; the controls must compile.
  *
  * Expires: delete once capture and separation checking are no longer experimental. Until then
  * an upgrade can change what they reject silently; this suite is re-read at each Scala bump.
  */
object DowngradeCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.durable.*
      |import grit.core.flow.*
      |import grit.core.visibility.Integrity
      |val s: Step[Any, String, String] = Step[Any, String, String](StepName("s"))(_ => in => in)
      |val node = Downgrade.extraction[Boolean, 1]
      |def of(o: String): Option[(answer: Option[Boolean], level: Integrity)] = None
      |type Ra = Raise[(answer: Option[Boolean], level: Integrity), Trusted[Option[Boolean]]]
      |""".stripMargin

  private val onControl = "val f = node.on(s)(of)"
  private val afterMap = "val f = node.on(Flow.step(s).map(_ + \"!\"))(of)"
  private val boundedControl = "val n = Downgrade.extraction[Extracted, 256]"
  private val freeText = "val n = Downgrade.extraction[String, 256]"
  private val tooWide =
    """val n: Downgrade[(answer: Option[Token[43]], level: Integrity), Trusted[Option[Token[43]]]] =
      |  Downgrade.extraction""".stripMargin
  private val wideEnough =
    """val n: Downgrade[(answer: Option[Token[42]], level: Integrity), Trusted[Option[Token[42]]]] =
      |  Downgrade.extraction""".stripMargin
  private val raiseInPure =
    """def f(r: Ra): Option[Boolean] -> Trusted[Option[Boolean]] =
      |  t => r((answer = t, level = Integrity.Member))
      |""".stripMargin
  private val raiseInMap =
    """def f(t: Tainted[Option[Boolean]], r: Ra): Tainted[Boolean] =
      |  t.map(a => r((answer = a, level = Integrity.Member)).extract.isDefined)
      |""".stripMargin
  private val textRule =
    """val d = Downgrade.Described("x", Downgrade.Axis.Integrity,
      |  Downgrade.Authority.Rule(Integrity.Member), Downgrade.Capacity.Text)""".stripMargin
  private val sanitiserOutside =
    """val n = Downgrade.sanitiser[Unit, Unit]("x")((_, _, _) => ((), true))"""
  private val ruleOutside =
    "def f(t: Option[Boolean]) = node.rule((answer = t, level = Integrity.Member))"
  private val raiseMinted = "val r = new Raise.Of[Int, Int](i => i)"
  private val raiseForged =
    "final class Mine extends Raise[Int, Int] { def apply(a: Int): Int = a }"
  private val raiseControl =
    """val st = node.step[Any, String, String](StepName("t"))(_ => raise => in =>
      |  raise((answer = Some(in == "x"), level = Integrity.Member)).extract.toString)
      |""".stripMargin
  private val raiseKept =
    """type Held = Option[Raise[(answer: Option[Boolean], level: Integrity), Trusted[Option[Boolean]]]^]
      |val st = node.step[Any, String, Held](StepName("t"))(_ => raise => in => Some(raise))(
      |  using Journaled.json[Held](_ => ujson.Null, _ => Right(None)))
      |""".stripMargin
  private val programControl =
    """val p = Flow.program[Any, String, String]("p", Vector(node))(steps => in =>
      |  node.raise(using steps) match {
      |    case Some(r) =>
      |      r((answer = Some(in == "x"), level = Integrity.Member)).extract.toString
      |    case None => in
      |  })
      |""".stripMargin
  private val programKept =
    """type Held = Option[Raise[(answer: Option[Boolean], level: Integrity), Trusted[Option[Boolean]]]^]
      |val p = Flow.program[Any, String, Held]("p", Vector(node))(steps => in => node.raise(using steps))
      |""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    List(
      onControl,
      afterMap,
      boundedControl,
      freeText,
      tooWide,
      wideEnough,
      raiseInPure,
      raiseInMap,
      textRule,
      sanitiserOutside,
      ruleOutside,
      raiseMinted,
      raiseForged,
      raiseControl,
      raiseKept,
      programControl,
      programKept
    )
  )

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("controls: a node on a step, a bounded value, a raise used in its body or program") {
      (probe(onControl), probe(boundedControl), probe(raiseControl), probe(programControl)) ==>
        (Nil, Nil, Nil, Nil)
    }

    test("a node made after map, on glue rather than a step's output, is refused") {
      probe(afterMap).exists(_.contains("Step")) ==> true
    }

    test("free text has no bound, so is never endorsed") {
      probe(freeText).exists(_.contains("Bounded")) ==> true
    }

    test("a value of more than K bits is never endorsed") {
      (
        probe(wideEnough),
        probe(tooWide).exists(_.contains("Cannot prove that B <= grit.core.flow.Bounded.K"))
      ) ==> (Nil, true)
    }

    test("a raise does not escape into a pure function, nor into a map") {
      val pure = "capability `r` cannot flow into capture set {}"
      (probe(raiseInPure).exists(_.contains(pure)), probe(raiseInMap).exists(_.contains(pure))) ==>
        (true, true)
    }

    test("a description of free text by a code rule cannot be built outside the package") {
      probe(textRule).exists(_.contains("Described")) ==> true
    }

    test("a sanitiser cannot be made, nor a node's rule run, outside grit.core") {
      (
        probe(sanitiserOutside).exists(_.contains("sanitiser")),
        probe(ruleOutside).exists(_.contains("rule"))
      ) ==>
        (true, true)
    }

    test("a raise is neither made nor forged outside the package") {
      (
        probe(raiseMinted).exists(_.contains("Of")),
        probe(raiseForged).exists(_.contains("Raise"))
      ) ==> (true, true)
    }

    test("a raise returned past its step's body is refused") {
      probe(raiseKept).exists(_.contains("outlives its scope")) ==> true
    }

    test("a program's raise returned past the program is refused") {
      probe(programKept).exists(_.contains("is not visible from")) ==> true
    }
  }
}
