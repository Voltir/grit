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

  private val downgradeMade =
    """val n = new Downgrade[Int, Int](Downgrade.Described("x", Downgrade.Axis.Classification,
      |  Downgrade.Authority.Policy, Downgrade.Capacity.Text), (i: Int) => i)""".stripMargin
  private val declassificationOutside =
    "val n = grit.core.document.Declassified.declassification"
  private val declassifyingNodeOn =
    "val f = grit.core.act.Declassifying.Node.on(s)(of)"
  private val stepsForged =
    """final class Mine extends Steps[Any] {
      |  def run[I, E, O](flow: Flow[Any, I, E, O], in: I): Either[E, O] = Left(???)
      |  def declares(node: Flow.Node): Boolean = true
      |}""".stripMargin
  private val declassifiedMade =
    """import grit.core.document.*
      |def f(t: DocText, s: Declassified.Source, a: PolicyApplication,
      |  p: grit.core.period.Probability, l: grit.core.visibility.Label) =
      |  new Declassified(t, s, a, p, l)""".stripMargin
  private val rowMade =
    """import grit.core.document.*
      |def f(d: Declassified, k: grit.core.id.DocKey, p: grit.core.place.Place, t: DocText) =
      |  new Declassified.Row(k, d.label, Integrity.Member, p, t, ujson.Obj(), d.source, d.applied,
      |    d.judged)""".stripMargin

  /** A program declaring the declassification, whose body raises it: what a plugin may build. */
  private val declassifying =
    """import grit.core.document.*
      |def program(t: DocText, s: Declassified.Source, a: PolicyApplication) =
      |  Flow.program[Any, String, String]("p", Vector(grit.core.act.Declassifying.Node))(steps => in =>
      |    grit.core.act.Declassifying.raise(using steps) match {
      |      case Some(r) =>
      |        r((rewrite = t, source = s, applied = a,
      |          leak = grit.core.period.Probability.Zero)).toString
      |      case None => in
      |    })
      |""".stripMargin
  private val declassifyingRun =
    declassifying +
      """def f(t: DocText, s: Declassified.Source, a: PolicyApplication, d: Durable^) =
        |  program(t, s, a).run((), "x")(using d)""".stripMargin
  private val declassifyingWalked =
    declassifying +
      """def f(t: DocText, s: Declassified.Source, a: PolicyApplication) =
        |  program(t, s, a).walk("x", ???, Vector())""".stripMargin

  /** Inside grit.core.document, where the declassification is made: its rule is pure. */
  private val inside =
    """package grit.core.document
      |import grit.core.flow.*
      |import grit.core.store.Tx
      |val described = Downgrade.Described("x", Downgrade.Axis.Classification,
      |  Downgrade.Authority.Policy, Downgrade.Capacity.Text)
      |""".stripMargin
  private val pureRule = "val n = new Downgrade[Int, Int](described, (i: Int) => i)"
  private val txRule =
    """def n(using tx: Tx^) =
      |  new Downgrade[Int, Int](described, (i: Int) => { val _ = Tx.clearance(tx); i })""".stripMargin

  private lazy val insideCompiled = Probes.batch(inside, List(pureRule, txRule))

  /** In grit but outside grit.core, where a plugin's job reads the application in force. */
  private val plugin =
    """package grit.outside
      |import grit.core.document.PolicyApplication
      |""".stripMargin
  private val inForceApplied = "def f(i: PolicyApplication.InForce) = i.applied"
  private val inForceRead = "def f(i: PolicyApplication.InForce) = i.policy"

  private lazy val pluginCompiled = Probes.batch(plugin, List(inForceApplied, inForceRead))

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
      programKept,
      downgradeMade,
      declassificationOutside,
      declassifiedMade,
      rowMade,
      declassifying,
      declassifyingRun,
      declassifyingWalked,
      declassifyingNodeOn,
      stepsForged
    )
  )

  private def probe(body: String): List[String] = compiled.errors(body)
  private def says(text: String)(found: List[String]): Boolean = found.exists(_.contains(text))

  val tests = Tests {
    test("controls: a node on a step, a bounded value, a raise used in its body or program") {
      (probe(onControl), probe(boundedControl), probe(raiseControl), probe(programControl)) ==>
        (Nil, Nil, Nil, Nil)
    }

    test("a node made after map, on glue rather than a step's output, is refused") {
      assert(
        says(
          """Found:    grit.core.durable.Flow[Any, String, Nothing, String]
            |Required: grit.core.durable.Step[Nothing, Nothing, Any]""".stripMargin
        )(probe(afterMap))
      )
    }

    test("free text has no bound, so is never endorsed") {
      assert(
        says(
          "No given instance of type grit.core.flow.Bounded[String, (256 : Int)] was found " +
            "for parameter x$1 of method extraction in object Downgrade"
        )(probe(freeText))
      )
    }

    test("a value of more than K bits is never endorsed") {
      probe(wideEnough) ==> Nil
      assert(
        says("Cannot prove that B <= grit.core.flow.Bounded.K =:= (true : Boolean).")(
          probe(tooWide)
        )
      )
    }

    test("a raise does not escape into a pure function, nor into a map") {
      assert(
        says(
          """Found:    (t: Option[Boolean]) ->{r} grit.core.flow.Trusted[Option[Boolean]]
            |Required: Option[Boolean] -> grit.core.flow.Trusted[Option[Boolean]]
            |
            |Note that capability `r` cannot flow into capture set {}.""".stripMargin
        )(probe(raiseInPure)),
        says(
          """Found:    (a: Option[Boolean]) ->{r} Boolean
            |Required: Option[Boolean] -> Boolean
            |
            |Note that capability `r` cannot flow into capture set {}.""".stripMargin
        )(probe(raiseInMap))
      )
    }

    test("a description of free text by a code rule cannot be built outside grit.core") {
      assert(says("object Described in object Downgrade does not take parameters")(probe(textRule)))
    }

    test("a sanitiser cannot be made, nor a node's rule run, outside grit.core") {
      assert(
        says(
          "private[core] method sanitiser can only be accessed from package grit.core in package grit."
        )(probe(sanitiserOutside)),
        says(
          "private[flow] value rule can only be accessed from package grit.core.flow in package grit.core."
        )(probe(ruleOutside))
      )
    }

    test("a raise is neither made nor forged outside the package") {
      assert(
        says(
          "private[flow] class Of can only be accessed from package grit.core.flow in package grit.core."
        )(probe(raiseMinted)),
        says("Cannot extend sealed class Raise in a different source file")(probe(raiseForged))
      )
    }

    test("a raise returned past its step's body is refused") {
      assert(
        says("Capability `any` outlives its scope: it leaks into outer capture set")(
          probe(raiseKept)
        )
      )
    }

    test("a program's raise returned past the program is refused") {
      assert(says("is not visible from any in value p.")(probe(programKept)))
    }

    test("nothing outside grit.core makes a downgrade, nor reaches the declassification") {
      assert(
        says(
          "private[core] constructor Downgrade can only be accessed from package grit.core in package grit."
        )(probe(downgradeMade)),
        says(
          "private[core] value declassification can only be accessed from package grit.core in package grit."
        )(probe(declassificationOutside))
      )
    }

    test("a Declassified, or the row a keeper keeps of one, is made only in grit.core.document") {
      assert(
        says(
          "private[document] constructor Declassified can only be accessed from package grit.core.document in package grit.core."
        )(probe(declassifiedMade)),
        says(
          "private[document] constructor Row can only be accessed from package grit.core.document in package grit.core."
        )(probe(rowMade))
      )
    }

    test(
      "an application in force is read outside grit.core, and its policy's text and checks are not"
    ) {
      pluginCompiled.errors(inForceApplied) ==> Nil
      assert(
        says(
          "private[core] value policy can only be accessed from package grit.core in package grit."
        )(pluginCompiled.errors(inForceRead))
      )
    }

    test(
      "a program raising the declassification is built outside grit.core, and neither run nor walked there"
    ) {
      probe(declassifying) ==> Nil
      assert(
        says(
          "private[durable] method run can only be accessed from package grit.core.durable in package grit.core."
        )(probe(declassifyingRun)),
        says(
          "private[durable] method walk can only be accessed from package grit.core.durable in package grit.core."
        )(probe(declassifyingWalked))
      )
    }

    test(
      "the declassification's node is no downgrade outside grit.core, and no Steps is forged to declare it"
    ) {
      assert(
        says("value on is not a member of grit.core.durable.Flow.Node")(probe(declassifyingNodeOn)),
        says("Cannot extend sealed trait Steps in a different source file")(probe(stepsForged))
      )
    }

    test("a declassification's rule capturing a Tx is refused where downgrades are made") {
      insideCompiled.errors(pureRule) ==> Nil
      assert(
        says(
          """Found:    (i: Int) ->{tx} Int
            |Required: Int -> Int
            |
            |Note that capability `tx` cannot flow into capture set {}.""".stripMargin
        )(insideCompiled.errors(txRule))
      )
    }
  }
}
