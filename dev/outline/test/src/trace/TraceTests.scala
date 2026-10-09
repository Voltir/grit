package grit.outline.trace

import grit.outline.locate.{Locate, Root}
import grit.outline.model.Defn
import grit.outline.read.Read

import utest.*

object TraceTests extends TestSuite {

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

  private val keep = "grit.outline.fixture.A.keep"
  private val run = "grit.outline.fixture.A.run"
  private val b = "grit.outline.fixture.B"

  val tests = Tests {
    test("depth 1 from A.keep reaches exactly B and Tx, and names scala.util.Either as library") {
      val traced = Trace.trace(named(read, keep), 1, load)
      assert(traced.types.map(_.fullName).toSet == Set(b, "grit.outline.fixture.Tx"))
      assert(traced.library.contains("scala.util.Either"))
    }
    test("depth 2 from A.keep also reaches C, through B") {
      val traced = Trace.trace(named(read, keep), 2, load)
      assert(traced.types.map(_.fullName).contains("grit.outline.fixture.C"))
    }
    test("roots A.keep and A.run name B once") {
      val traced = Trace.trace(named(read, keep) ++ named(read, run), 1, load)
      assert(traced.types.count(_.fullName == b) == 1)
    }
    test("depth 0 traces no types") {
      val traced = Trace.trace(named(read, keep), 0, load)
      assert(traced.types.isEmpty)
    }
  }
}
