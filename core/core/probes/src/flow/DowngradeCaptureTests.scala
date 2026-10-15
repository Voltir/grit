package grit.core.flow

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about downgrades, from code outside their package: a node made
  * after glue, free text endorsed, a description or raise forged, a raise kept past its body.
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
      |def of(o: String): Option[(value: Tainted[Boolean], level: Integrity)] = None
      |""".stripMargin

  private val onControl = "val f = node.on(s)(of)"
  private val afterMap = "val f = node.on(Flow.step(s).map(_ + \"!\"))(of)"
  private val boundedControl = "val n = Downgrade.extraction[Extracted, 256]"
  private val freeText = "val n = Downgrade.extraction[String, 256]"
  private val textRule =
    """val d = Downgrade.Described("x", Downgrade.Axis.Integrity,
      |  Downgrade.Authority.Rule(Integrity.Member), Downgrade.Capacity.Text)""".stripMargin
  private val sanitiserOutside =
    """val n = Downgrade.sanitiser[Unit, Unit]("x")((_, _, _) => ((), true))"""
  private val ruleOutside =
    "def f(t: Tainted[Boolean]) = node.rule((value = t, level = Integrity.Member))"
  private val raiseMinted = "val r = new Raise.Of[Int, Int](i => i)"
  private val raiseForged =
    "final class Mine extends Raise[Int, Int] { def apply(a: Int): Int = a }"
  private val raiseControl =
    """val st = node.step[Any, String, String](StepName("t"))(_ => raise => in =>
      |  raise((value = Trusted.literal("x").tainted.map(_ == in), level = Integrity.Member)).extract.toString)
      |""".stripMargin
  private val raiseKept =
    """var held: Option[Raise[(value: Tainted[Boolean], level: Integrity), Trusted[Boolean]]^] = None
      |val st = node.step[Any, String, String](StepName("t"))(_ => raise => in => {
      |  held = Some(raise)
      |  in
      |})""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    List(
      onControl,
      afterMap,
      boundedControl,
      freeText,
      textRule,
      sanitiserOutside,
      ruleOutside,
      raiseMinted,
      raiseForged,
      raiseControl,
      raiseKept
    )
  )

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("controls: a node on a step, a bounded value, a raise used in its body") {
      (probe(onControl), probe(boundedControl), probe(raiseControl)) ==> (Nil, Nil, Nil)
    }

    test("a node made after map, on glue rather than a step's output, is refused") {
      probe(afterMap).exists(_.contains("Step")) ==> true
    }

    test("free text has no bound, so is never endorsed") {
      probe(freeText).exists(_.contains("Bounded")) ==> true
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

    test("a raise kept past its step's body is refused") {
      probe(raiseKept).exists(_.contains("raise")) ==> true
    }
  }
}
