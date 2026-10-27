package grit.core.screen

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about a screen's values, from code in core outside their package:
  * answers or a verdict made, or remade, other than by their readers, and checks made other
  * than by their read. Typer errors, compiled in a probe package of their own against core; the
  * controls must compile.
  */
object ScreenCaptureTests extends TestSuite {

  private val prelude =
    """package grit.core.act
      |import grit.core.classify.Answer
      |import grit.core.period.Probability
      |import grit.core.screen.*
      |val shape = Screen.Shape(0, Vector(Must.Absent))
      |val p = Probability.Zero
      |""".stripMargin

  private val readControl =
    """def f(as: Vector[Answer]) =
      |  Screen.read(shape, as).map(s => Bench.verdict(Vector((Benched.Kind.Accept, s))).line)
      |def b = Screen.readBaseline(1, Vector(Answer.YesNo(0.1)))
      |""".stripMargin
  private val screenedMade = "def s = Screened(shape, Vector.empty, Vector(p))"
  private val screenedNew = "def s = new Screened(shape, Vector.empty, Vector(p))"
  private val screenedCopied = "def f(s: Screened) = s.copy(signals = Vector(p))"
  private val baselineMade = "def b = Screen.Baseline(Vector(p))"
  private val baselineCopied = "def f(b: Screen.Baseline) = b.copy(floor = Vector(p))"
  private val firedMade = "def f = Fired(Vector.empty, Vector.empty, Vector.empty)"
  private val firedCopied = "def f(x: Fired) = x.copy(absent = Vector.empty)"
  private val benchedMade = "def b = Benched(Vector.empty)"
  private val benchedCopied = "def f(b: Benched) = b.copy(examples = Vector.empty)"
  private val lineMade = "def f(e: Benched.Example) = Benched.Line(e)"

  private val checksControl = "def c = Checks.read(None).map(c => (c.canonical, c.signals.all))"
  private val checksMade = "def f(s: Signals, e: Examples) = Checks(s, e)"
  private val checksCopied = "def f(c: Checks) = c.copy(examples = c.examples)"
  private val signalMade = "def s = Signal(\"the codename\", Must.Absent)"
  private val signalCopied = "def f(s: Signal) = s.copy(infer = \"the codename\")"
  private val signalsMade = "def f(s: Signal) = Signals(s, Vector.empty)"
  private val examplesMade = "def e = Examples(Vector(\"a\"), Vector(\"r\"))"

  private val made = List(
    screenedMade,
    screenedNew,
    screenedCopied,
    baselineMade,
    baselineCopied,
    firedMade,
    firedCopied,
    benchedMade,
    benchedCopied,
    lineMade
  )
  private val checksMadeOutside =
    List(checksMade, checksCopied, signalMade, signalCopied, signalsMade, examplesMade)

  private lazy val compiled =
    Probes.batch(prelude, readControl :: checksControl :: made ++ checksMadeOutside)

  /** The probes reaching a constructor through a case class's `apply`. */
  private val applied = Set(
    screenedMade,
    baselineMade,
    firedMade,
    benchedMade,
    lineMade,
    checksMade,
    signalMade,
    signalsMade,
    examplesMade
  )

  /** Whether `probe` is rejected for reaching a constructor private to `grit.core.screen`:
    * through `new` or `copy`, named; through a case class's `apply` ([[applied]]), which is as
    * private, as its object taking no parameters.
    */
  private def private_(probe: String): Boolean =
    compiled.errors(probe).exists { e =>
      e.contains("can only be accessed from package grit.core.screen in package grit.core.") ||
      (applied(probe) && e.contains("does not take parameters"))
    }

  val tests = Tests {
    test("the controls compile: answers read into a Screened, a Baseline and a Benched") {
      (compiled.errors(readControl), compiled.errors(checksControl)) ==> (Nil, Nil)
    }

    test(
      "a Screened, its Fired, a Baseline, a Benched and its Line are made only by their readers"
    ) {
      made.filterNot(private_) ==> Nil
    }

    test("checks are made only by their read") {
      checksMadeOutside.filterNot(private_) ==> Nil
    }
  }
}
