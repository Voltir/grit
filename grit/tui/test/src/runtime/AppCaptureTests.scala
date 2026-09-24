package grit.tui.runtime

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import java.nio.file.Files
import utest.*

/** What capture checking rejects in a view tree's handlers and apps, pinned by compiling
  * probe sources against tui with tui's own flags -- `assertCompileError` cannot see
  * capture errors (docs/capture-checking.md). The pattern is `grit.core.SeparationTests`.
  */
object AppCaptureTests extends TestSuite {

  private val classpath = sys.env.getOrElse("GRIT_PROBE_CLASSPATH", "")
  private val options = sys.env.getOrElse("GRIT_PROBE_OPTIONS", "").split(" ").toList

  private val prelude =
    """package probe
      |import grit.tui.components.{Node, PaneKey}
      |import grit.tui.runtime.App
      |import grit.tui.components.pane.Anchor
      |import grit.tui.model.select.Doc
      |import grit.tui.model.input.Input
      |import grit.tui.wire.term.Terminal
      |import grit.tui.runtime.Effect
      |final case class N(n: Int) extends caps.Pure
      |object Probe {
      |""".stripMargin

  private def errors(body: String, flags: List[String] = options): List[String] = {
    val dir = Files.createTempDirectory("grit-node-probe")
    try {
      val source = Files.writeString(dir.resolve("Probe.scala"), prelude + body + "\n}\n")
      val args = flags ++ List("-classpath", classpath, "-d", dir.toString, source.toString)
      new Driver().process(args.toArray, StoreReporter(), null).allErrors.map(_.message)
    } finally {
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }
  }

  /** A copy handler that writes to the clipboard itself instead of asking for an Effect. */
  private val copies =
    """def pane(term: Terminal): Node[Int] =
      |  Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => { term.copyOut(t); t.length })
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("term") && (e.contains("capture") || e.contains("Found:")))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("pure handlers, a mapped subtree and a view value compile") {
      val errs = errors(
        """enum Msg { case Copied(n: Int); case Key(i: Input) }
          |val view: Int -> Node[Msg] = s =>
          |  Node.column(
          |    Node.fixed(1) -> Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => t.length + s).map(Msg.Copied(_)),
          |    Node.flex() -> Node.doc(PaneKey.of("j"), Doc.empty, Anchor.Bottom).onScroll(_ => Msg.Copied(0))
          |  ).onKey(i => Some(Msg.Key(i)))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a handler that captures the terminal is rejected") {
      val errs = errors(copies)
      assert(rejected(errs))
    }

    test("a key handler that captures the terminal is rejected") {
      val errs = errors(
        """def screen(term: Terminal): Node[Int] =
          |  Node.column(Node.fixed(1) -> Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom))
          |    .onKey(_ => { term.flush(); None })
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("Node.map with a capturing function is rejected") {
      val errs = errors(
        """def screen(term: Terminal, n: Node[Int]): Node[Int] = n.map(i => { term.flush(); i })
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("a message that smuggles a thunk over the terminal is rejected at the handler") {
      val errs = errors(
        """enum Msg { case Run(f: () => Unit) }
          |def pane(term: Terminal): Node[Msg] =
          |  Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => Msg.Run(() => term.copyOut(t)))
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("a view value that closes over the terminal is rejected") {
      val errs = errors(
        """def app(term: Terminal): Int -> Node[Int] = s =>
          |  Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onScroll(_ => { term.flush(); s })
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("an app's handlers built in its own methods are pure with no ceremony") {
      val errs = errors(
        """object Counter extends App[Int, N] {
          |  def init: (Int, Effect[N]) = (0, Effect.NoOp)
          |  def update(m: N, s: Int): (Int, Effect[N]) = (s + m.n, Effect.NoOp)
          |  private def bump(s: Int): Int -> N = n => N(n + s)
          |  def view(s: Int): Node[N] =
          |    Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom).onCopy((t, _) => bump(s)(t.length))
          |}
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("an app that holds the terminal is rejected where it is defined") {
      val errs = errors(
        """final class Leaky(term: Terminal) extends App[Int, N] {
          |  def init: (Int, Effect[N]) = (0, Effect.NoOp)
          |  def update(m: N, s: Int): (Int, Effect[N]) = { term.flush(); (s + m.n, Effect.NoOp) }
          |  def view(s: Int): Node[N] = Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom)
          |}
          |""".stripMargin
      )
      assert(rejected(errs))
    }

    test("a message that carries the terminal is rejected where it is declared") {
      val errs = errors(
        """enum Carry extends caps.Pure {
          |  case Term(t: Terminal^)
          |  case Plain
          |}
          |""".stripMargin
      )
      assert(errs.exists(_.contains("Term.this.t")))
    }

    test("without caps.Pure, a message may carry the terminal") {
      val errs = errors(
        """enum Carry {
          |  case Term(t: Terminal^)
          |  case Plain
          |}
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("an app whose message type is not declared pure is refused") {
      val errs = errors(
        """enum Open { case Plain }
          |object Opened extends App[Int, Open] {
          |  def init: (Int, Effect[Open]) = (0, Effect.NoOp)
          |  def update(m: Open, s: Int): (Int, Effect[Open]) = (s, Effect.NoOp)
          |  def view(s: Int): Node[Open] = Node.doc(PaneKey.of("k"), Doc.empty, Anchor.Bottom)
          |}
          |""".stripMargin
      )
      assert(errs.exists(_.contains("does not conform to upper bound scala.caps.Pure")))
    }

    test("capture checking is what rejects the capturing handler") {
      val errs = errors(copies, options.filterNot(_ == "-language:experimental.captureChecking"))
      assert(errs.isEmpty)
    }
  }
}
