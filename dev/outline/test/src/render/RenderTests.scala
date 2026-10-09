package grit.outline.render

import grit.outline.locate.{Locate, Root}
import grit.outline.model.Defn
import grit.outline.read.Read
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
    Read.defns(root, tastyFiles) match {
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
    Read.defns(root, Locate.forTopLevel(root, topLevel))

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
        "9-10 def A.keep",
        "  /** `b` kept, in `tx`. */",
        "  def keep(b: B)(using Tx^): Either[String, Int]",
        "→ 3-4 trait Tx extends caps.SharedCapability",
        "→ 16-17 final case class B(c: C, note: String)"
      ).mkString("\n")
      assert(before == expected)
      assert(out.split("\n").lastOption.exists(_.matches("""\[\d+\.\d KB\]""")))
    }
    test("oneLine of an enum lists its cases with their lines") {
      val colour = named(read, "grit.outline.fixture.Colour")
      assert(
        colour.map(Render.oneLine(_, read)) == Vector(
          "→ 38-46 enum Colour: Red :41 | Mix(weight: Double) :44"
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
  }
}
