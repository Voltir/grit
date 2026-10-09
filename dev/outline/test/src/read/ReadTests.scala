package grit.outline.read

import grit.outline.locate.Root
import grit.outline.model.{Defn, Kind, Lines}

import utest.*

object ReadTests extends TestSuite {

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

  private lazy val read: Vector[Defn] = {
    val fixtureClasses = sys.env.get("OUTLINE_FIXTURE_CLASSES") match {
      case Some(path) => path
      case None => throw new Exception("OUTLINE_FIXTURE_CLASSES env var not set")
    }
    val tastyFiles = os
      .walk(os.Path(fixtureClasses))
      .filter(p => p.ext == "tasty")
      .toVector
    Read.defns(Root(repoRoot), tastyFiles) match {
      case Right(defns) => defns
      case Left(message) => throw new Exception(message)
    }
  }

  /** Every definition with this full name, at any depth. */
  private def named(defns: Vector[Defn], fullName: String): Vector[Defn] =
    defns.flatMap { d =>
      (if (d.fullName == fullName) Vector(d) else Vector.empty) ++ named(d.members, fullName)
    }

  private def only(found: Vector[Defn], what: String): Defn = found match {
    case Vector(d) => d
    case other => throw new Exception(s"expected exactly one $what, found ${other.size}")
  }

  def tests = Tests {
    test("A.keep is an abstract def with its signature, doc, lines, body and file") {
      val keep = only(named(read, "grit.outline.fixture.A.keep"), "A.keep")
      assert(keep.kind == Kind.Def)
      assert(keep.signature == "  def keep(b: B)(using Tx^): Either[String, Int]")
      assert(keep.doc == Some("  /** `b` kept, in `tx`. */"))
      assert(keep.lines == Lines(9, 10))
      assert(keep.body.isEmpty)
      assert(keep.isAbstract)
      assert(keep.file == "dev/outline/fixture/src/Sample.scala")
    }

    test("A.run keeps its function parameter's type verbatim") {
      val run = only(named(read, "grit.outline.fixture.A.run"), "A.run")
      assert(run.signature.contains("Int -> Tx^ ?-> Int"))
    }

    test("Limits.of's two overloads keep their own lines, body and signature") {
      val ofs = named(read, "grit.outline.fixture.Limits.of")
      assert(ofs.size == 2)
      val first = only(ofs.filter(_.lines == Lines(27, 28)), "Limits.of at lines 27-28")
      val second = only(ofs.filter(_.lines == Lines(30, 33)), "Limits.of at lines 30-33")
      assert(first.body == Some("of(asks, calls, 0)"))
      assert(
        second.signature == "  def of(asks: Int, calls: Int, extra: Int): Either[String, Limits]"
      )
    }

    test(
      "Limits.hidden is private, A is a trait with members keep and run, Limits is an object without a constructor"
    ) {
      val hidden = only(named(read, "grit.outline.fixture.Limits.hidden"), "Limits.hidden")
      assert(hidden.isPrivate)
      val a = only(named(read, "grit.outline.fixture.A"), "A")
      assert(a.kind == Kind.Trait)
      assert(a.members.map(_.name) == Vector("keep", "run"))
      val limits = only(
        named(read, "grit.outline.fixture.Limits").filter(_.kind == Kind.Object),
        "the Limits object"
      )
      assert(!limits.members.exists(_.name == "<init>"))
    }

    test("Ids.Id is an opaque type with its signature and lines") {
      val id = only(named(read, "grit.outline.fixture.Ids.Id"), "Ids.Id")
      assert(id.kind == Kind.Opaque)
      assert(id.signature == "  opaque type Id = String")
      assert(id.lines == Lines(50, 51))
    }
  }
}
