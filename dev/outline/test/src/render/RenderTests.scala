package grit.outline.render

import grit.outline.locate.{Locate, Root}
import grit.outline.model.Defn
import grit.outline.read.Read
import grit.outline.testing.OutlineLayout
import grit.outline.trace.{Trace, Traced}

import utest.*

object RenderTests extends TestSuite {

  /** The worktree root: the nearest directory at or above the test's working directory that holds `build.mill`. */
  private def repoRoot: os.Path = {
    def up(dir: os.Path): os.Path =
      if (os.exists(dir / "build.mill")) dir
      else {
        val parent = dir / os.up
        if (parent == dir) throw new Exception("no build.mill above the test's working directory")
        else up(parent)
      }
    up(os.pwd)
  }

  private lazy val root: Root = Root(repoRoot)

  private lazy val read: Vector[Defn] = {
    val fixtureClasses = sys.env.get("OUTLINE_FIXTURE_CLASSES") match {
      case Some(path) => path
      case None => throw new Exception("OUTLINE_FIXTURE_CLASSES env var not set")
    }
    val tastyFiles = os
      .walk(os.Path(fixtureClasses))
      .filter(p => p.ext == "tasty")
      .toVector
    Read.defns(root, OutlineLayout, tastyFiles) match {
      case Right(defns) => defns
      case Left(message) => throw new Exception(message)
    }
  }

  /** Every definition with this full name, at any depth. */
  private def named(defns: Vector[Defn], fullName: String): Vector[Defn] =
    defns.flatMap { d =>
      (if (d.fullName == fullName) Vector(d) else Vector.empty) ++ named(d.members, fullName)
    }

  private def load(topLevel: String): Either[String, Vector[Defn]] =
    Read.defns(root, OutlineLayout, Locate.forTopLevel(root, OutlineLayout, topLevel))

  private def noTraced: Traced = Traced(Vector.empty, Vector.empty, Vector.empty)

  private val sample = "dev/outline/fixture/src/Sample.scala"

  val tests = Tests {
    test("show prints A.keep verbatim, then its traced types as one-liners in line order") {
      val traced = Trace.trace(named(read, "grit.outline.fixture.A.keep"), 1, load)
      val out = Render.show(
        named(read, "grit.outline.fixture.A.keep"),
        traced,
        read,
        Set.empty,
        Set.empty,
        false,
        80000
      )
      val before = out.split("\n").takeWhile(!_.startsWith("-- library")).mkString("\n")
      val expected = Vector(
        s"== $sample  grit.outline.fixture",
        "9-10 def A.keep  [0.2 KB]",
        "  /** `b` kept, in `tx`. */",
        "  def keep(b: B)(using Tx^): Either[String, Int]",
        "→ 3-4 trait Tx extends caps.SharedCapability  [0.2 KB]",
        "→ 16-17 final case class B(c: C, note: String)  [0.2 KB]"
      ).mkString("\n")
      assert(before == expected)
      assert(out.split("\n").lastOption.exists(_.matches("""\[\d+\.\d KB\]""")))
    }
    test("oneLine of an enum lists its cases with their lines") {
      val colour = named(read, "grit.outline.fixture.Colour")
      assert(
        colour.map(Render.oneLine(_, read)) == Vector(
          "→ 38-46 enum Colour: Red :41 | Mix(weight: Double) :44  [0.2 KB]"
        )
      )
    }
    test("oneLine of an enum whose cases share a line lists each case once") {
      val suit = named(read, "grit.outline.fixture.Suit")
      assert(
        suit.map(Render.oneLine(_, read)) == Vector(
          "→ 93-96 enum Suit: Hearts :95 | Spades :95 | Clubs :95  [0.2 KB]"
        )
      )
    }
    test("a named three-argument Limits.of prints its body when its name is in bodies") {
      val of3 = named(read, "grit.outline.fixture.Limits.of").filter(_.signature.contains("extra"))
      val withBody = Render.show(of3, noTraced, read, Set.empty, Set("of"), false, 80000)
      val withoutBody = Render.show(of3, noTraced, read, Set.empty, Set.empty, false, 80000)
      assert(withBody.contains("if (asks < 0 || calls < 0 || extra < 0)"))
      assert(withBody.contains("else { Right(new Limits(asks, calls + hidden)) }"))
      assert(!withoutBody.contains("if (asks < 0"))
    }
    test("a cap cuts whole entries, says so, and shortens the answer") {
      val a = named(read, "grit.outline.fixture.A")
      val full = Render.show(a, noTraced, read, Set.empty, Set.empty, false, 80000)
      val capped = Render.show(a, noTraced, read, Set.empty, Set.empty, false, 200)
      assert(capped.contains("[truncated: "))
      assert(capped.length < full.length)
    }
    test("a stale file's group header is flagged") {
      val c = named(read, "grit.outline.fixture.C")
      val out = Render.show(c, noTraced, read, Set(sample), Set.empty, false, 80000)
      assert(
        out
          .split("\n")
          .contains(s"== $sample  grit.outline.fixture  [stale: source newer than .tasty]")
      )
    }

    // The lines printed under the entry whose header starts with `header`, up to the next header or the KB line.
    def under(out: String, header: String): Vector[String] = {
      val lines = out.split("\n", -1).toVector
      lines
        .dropWhile(!_.startsWith(header))
        .drop(1)
        .takeWhile(l => !l.startsWith("[") && !l.matches("""\d+-\d+ .*"""))
    }

    test("show prints a member's head and body as the file's own lines") {
      val of3 = named(read, "grit.outline.fixture.Limits.of").filter(_.signature.contains("extra"))
      val out = Render.show(of3, noTraced, read, Set.empty, Set("of"), false, 80000)
      val file = os.read.lines(root.dir / os.RelPath(sample)).toVector
      assert(of3.map(_.lines) == Vector(grit.outline.model.Lines(30, 33)))
      assert(under(out, "30-33 ") == file.slice(29, 33))
    }

    test("a member nested four spaces deep prints its head and body as the file's own lines") {
      val deep = named(read, "grit.outline.fixture.Outer.Inner.deep")
      val out = Render.show(deep, noTraced, read, Set.empty, Set("deep"), false, 80000)
      val file = os.read.lines(root.dir / os.RelPath(sample)).toVector
      assert(deep.map(_.lines) == Vector(grit.outline.model.Lines(69, 73)))
      assert(under(out, "69-73 ") == file.slice(68, 73))
    }

    test(
      "a definition's header line ends with its size in KB to one decimal, at least 0.2, and with none when its size is unknown"
    ) {
      val keep = named(read, "grit.outline.fixture.A.keep")
      def header(size: Option[Int]): Option[String] =
        Render
          .show(keep.map(_.copy(size = size)), noTraced, read, Set.empty, Set.empty, false, 80000)
          .split("\n")
          .find(_.startsWith("9-10 "))
      assert(header(Some(1946)) == Some("9-10 def A.keep  [1.9 KB]"))
      assert(header(Some(10)) == Some("9-10 def A.keep  [0.2 KB]"))
      assert(header(None) == Some("9-10 def A.keep"))
    }

    test(
      "a definition's head without --body is the file's lines from its doc to its signature's end"
    ) {
      val of3 = named(read, "grit.outline.fixture.Limits.of").filter(_.signature.contains("extra"))
      val out = Render.show(of3, noTraced, read, Set.empty, Set.empty, false, 80000)
      val file = os.read.lines(root.dir / os.RelPath(sample)).toVector
      assert(under(out, "30-33 ") == file.slice(29, 31))
    }

    test(
      "a definition shown without its body ends with its last line under an end-of header: Limits.of's line 33"
    ) {
      val of3 = named(read, "grit.outline.fixture.Limits.of").filter(_.signature.contains("extra"))
      val out = Render.show(of3, noTraced, read, Set.empty, Set.empty, false, 80000)
      val file = os.read.lines(root.dir / os.RelPath(sample)).toVector
      assert(under(out, "33-33 end of Limits.of") == file.slice(32, 33))
    }

    test(
      "a last line of closing brackets alone comes with the line before it: Outer.Inner.deep's 72-73"
    ) {
      val deep = named(read, "grit.outline.fixture.Outer.Inner.deep")
      val out = Render.show(deep, noTraced, read, Set.empty, Set.empty, false, 80000)
      val file = os.read.lines(root.dir / os.RelPath(sample)).toVector
      assert(under(out, "72-73 end of Outer.Inner.deep") == file.slice(71, 73))
    }

    test("a type's closing lines follow its members' entries: object Limits ends with its 35-36") {
      val limits =
        named(read, "grit.outline.fixture.Limits").filter(_.kind == grit.outline.model.Kind.Object)
      val out = Render.show(limits, noTraced, read, Set.empty, Set.empty, false, 80000)
      val lines = out.split("\n").toVector
      val file = os.read.lines(root.dir / os.RelPath(sample)).toVector
      assert(under(out, "35-36 end of Limits") == file.slice(34, 36))
      assert(
        lines.indexWhere(_.startsWith("35-36 end of Limits")) >
          lines.indexWhere(_.startsWith("33-33 end of Limits.of"))
      )
    }

    test(
      "a definition whose head reaches its last line, or shown with its body, has no closing lines: A.keep, and Limits.of with --body"
    ) {
      val keep = named(read, "grit.outline.fixture.A.keep")
      val of3 = named(read, "grit.outline.fixture.Limits.of").filter(_.signature.contains("extra"))
      val keepOut = Render.show(keep, noTraced, read, Set.empty, Set.empty, false, 80000)
      val ofOut = Render.show(of3, noTraced, read, Set.empty, Set("of"), false, 80000)
      assert(!keepOut.contains(" end of "))
      assert(!ofOut.contains(" end of "))
    }
  }
}
