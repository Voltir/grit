package grit.tui.runtime.app

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, options}

import utest.*

/** What capture checking rejects in a view tree's handlers and apps, pinned by compiling
  * probe sources against tui with tui's own flags -- `assertCompileError` cannot see
  * capture errors (docs/capture-checking.md). The pattern is `grit.core.durable.SeparationTests`.
  */
object AppCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.tui.components.tree.{Node, PaneKey}
      |import grit.tui.runtime.app.App
      |import grit.tui.components.pane.Anchor
      |import grit.tui.model.select.Doc
      |import grit.tui.model.input.Input
      |import grit.tui.wire.term.Terminal
      |import grit.tui.runtime.app.Effect
      |final case class N(n: Int) extends caps.Pure
      |""".stripMargin

  /** The error messages from compiling `body` after the prelude, with every probe below. */
  private def errors(body: String): List[String] = compiled.errors(body)

  /** A copy handler that writes to the clipboard itself instead of asking for an Effect. */
  private val copies =
    """def pane(term: Terminal): Node[Int] =
      |  Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => { term.copyOut(t); t.length })
      |""".stripMargin

  private val CaptureChecking = "-language:experimental.captureChecking"

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("term") && (e.contains("capture") || e.contains("Found:")))

  /** Rejected with an error naming `term`, and compiled cleanly with capture checking off:
    * refused for what it captures, not for a mistake of its own.
    */
  private def rejectedForCapture(body: String): Boolean =
    rejected(errors(body)) && unchecked.errors(body).isEmpty

  private val pureHandlers =
    """enum Msg { case Copied(n: Int); case Key(i: Input) }
    |val view: Int -> Node[Msg] = s =>
    |  Node.column(
    |    Node.fixed(1) -> Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => t.length + s).map(Msg.Copied(_)),
    |    Node.flex() -> Node.doc(PaneKey.of("j"), Doc.empty, Anchor.Bottom).onScroll(_ => Msg.Copied(0))
    |  ).onKey(i => Some(Msg.Key(i)))
    |""".stripMargin

  private val keyCaptures =
    """def screen(term: Terminal): Node[Int] =
    |  Node.column(Node.fixed(1) -> Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom))
    |    .onKey(_ => { term.flush(); None })
    |""".stripMargin

  private val mapCaptures =
    """def screen(term: Terminal, n: Node[Int]): Node[Int] = n.map(i => { term.flush(); i })
    |""".stripMargin

  private val smugglesThunk =
    """enum Msg { case Run(f: () => Unit) }
    |def pane(term: Terminal): Node[Msg] =
    |  Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => Msg.Run(() => term.copyOut(t)))
    |""".stripMargin

  private val viewCloses =
    """def app(term: Terminal): Int -> Node[Int] = s =>
    |  Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onScroll(_ => { term.flush(); s })
    |""".stripMargin

  private val appMethods =
    """object Counter extends App[Int, N] {
    |  def init: (Int, Effect[N]) = (0, Effect.NoOp)
    |  def update(m: N, s: Int): (Int, Effect[N]) = (s + m.n, Effect.NoOp)
    |  private def bump(s: Int): Int -> N = n => N(n + s)
    |  def view(s: Int): Node[N] =
    |    Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => bump(s)(t.length))
    |}
    |""".stripMargin

  private val appHolds =
    """final class Leaky(term: Terminal) extends App[Int, N] {
    |  def init: (Int, Effect[N]) = (0, Effect.NoOp)
    |  def update(m: N, s: Int): (Int, Effect[N]) = { term.flush(); (s + m.n, Effect.NoOp) }
    |  def view(s: Int): Node[N] = Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom)
    |}
    |""".stripMargin

  private val carriesPure =
    """enum Carry extends caps.Pure {
    |  case Term(t: Terminal^)
    |  case Plain
    |}
    |""".stripMargin

  private val carriesOpen =
    """enum Carry {
    |  case Term(t: Terminal^)
    |  case Plain
    |}
    |""".stripMargin

  private val openMessage =
    """enum Open { case Plain }
    |object Opened extends App[Int, Open] {
    |  def init: (Int, Effect[Open]) = (0, Effect.NoOp)
    |  def update(m: Open, s: Int): (Int, Effect[Open]) = (s, Effect.NoOp)
    |  def view(s: Int): Node[Open] = Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom)
    |}
    |""".stripMargin

  /** The probes `rejectedForCapture` reads. */
  private val forCapture = List(copies, keyCaptures, mapCaptures, smugglesThunk, appHolds)

  /* An app's undeclared message type is refused by a bound, the typer's: a second compile
   * answers the rest. */
  private lazy val compiled = Probes.batch(
    prelude,
    forCapture ++ List(pureHandlers, viewCloses, appMethods, carriesPure, carriesOpen, openMessage)
  )

  private lazy val unchecked =
    Probes.batch(prelude, forCapture, options.filterNot(_ == CaptureChecking))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains(CaptureChecking))
    }

    test("two compiles answer every probe, and one with capture checking off") {
      (compiled.runs, unchecked.runs) ==> (2, 1)
    }

    test("pure handlers, a mapped subtree and a view value compile") {
      val errs = errors(
        pureHandlers
      )
      assert(errs.isEmpty)
    }

    test("a handler that captures the terminal is rejected") {
      assert(rejectedForCapture(copies))
    }

    test("a key handler that captures the terminal is rejected") {
      assert(
        rejectedForCapture(
          keyCaptures
        )
      )
    }

    test("Node.map with a capturing function is rejected") {
      assert(
        rejectedForCapture(
          mapCaptures
        )
      )
    }

    test("a message that smuggles a thunk over the terminal is rejected at the handler") {
      assert(
        rejectedForCapture(
          smugglesThunk
        )
      )
    }

    test("a view value that closes over the terminal is rejected") {
      // No control without capture checking: `->` itself needs it.
      assert(
        rejected(
          errors(
            viewCloses
          )
        )
      )
    }

    test("an app's handlers built in its own methods are pure with no ceremony") {
      val errs = errors(
        appMethods
      )
      assert(errs.isEmpty)
    }

    test("an app that holds the terminal is rejected where it is defined") {
      assert(
        rejectedForCapture(
          appHolds
        )
      )
    }

    test("a message that carries the terminal is rejected where it is declared") {
      val errs = errors(
        carriesPure
      )
      assert(errs.exists(_.contains("Term.this.t")))
    }

    test("without caps.Pure, a message may carry the terminal") {
      val errs = errors(
        carriesOpen
      )
      assert(errs.isEmpty)
    }

    test("an app whose message type is not declared pure is refused") {
      val errs = errors(
        openMessage
      )
      assert(errs.exists(_.contains("does not conform to upper bound scala.caps.Pure")))
    }
  }
}
