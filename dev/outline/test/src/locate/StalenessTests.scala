package grit.outline.locate

import grit.outline.model.Staleness

import utest.*

object StalenessTests extends TestSuite {

  /** The fixture's classes dir, which the build compiles before the tests run. */
  private lazy val fixtureClasses: os.Path = sys.env.get("OUTLINE_FIXTURE_CLASSES") match {
    case Some(path) => os.Path(path)
    case None => throw new Exception("OUTLINE_FIXTURE_CLASSES env var not set")
  }

  /** The worktree: the nearest directory at or above the test's working directory that holds `build.mill`. */
  private lazy val worktree: os.Path = {
    def up(dir: os.Path): os.Path =
      if (os.exists(dir / "build.mill")) dir
      else {
        val parent = dir / os.up
        if (parent == dir) throw new Exception("no build.mill above the test's working directory")
        else up(parent)
      }
    up(os.pwd)
  }

  /** `Sample.scala`, which compiles to several `.tasty` files, `Almanac.tasty` not among them. */
  private val a = os.RelPath("dev/outline/fixture/src/Sample.scala")

  /** `Docs.scala`, in `Sample.scala`'s package, which compiles to `Almanac.tasty`. */
  private val b = os.RelPath("dev/outline/fixture/src/Docs.scala")

  private val pkg = os.RelPath("grit/outline/fixture")

  /** A temp root holding the fixture's two sources as compiled and their package's `.tasty` files, every source an hour
    * older than every `.tasty`.
    */
  private def fresh(): Root = {
    val d = os.temp.dir(prefix = "outline-staleness-")
    Vector(a, b).foreach(f => os.copy(worktree / f, d / f, createFolders = true))
    os.list(fixtureClasses / pkg)
      .filter(_.ext == "tasty")
      .foreach(p => os.copy.into(p, d / "classes" / pkg, createFolders = true))
    val now = System.currentTimeMillis()
    os.walk(d).filter(os.isFile).foreach { p =>
      os.mtime.set(p, if (p.ext == "scala") now - 3600000L else now)
    }
    Root(d)
  }

  /** `file`'s first line given to `edit`, the file written over, its mtime an hour after every `.tasty`. */
  private def rewrite(root: Root, file: os.RelPath, edit: String => String): Unit = {
    val lines = os.read(root.dir / file).split("\n", -1).toVector
    os.write.over(
      root.dir / file,
      lines.updated(0, edit(lines.headOption.getOrElse(""))).mkString("\n")
    )
    os.mtime.set(root.dir / file, System.currentTimeMillis() + 3600000L)
  }

  private def tastyOf(root: Root): Vector[os.Path] =
    os.list(root.dir / "classes" / pkg).filter(_.ext == "tasty").sorted.toVector

  private def staleness(root: Root, file: os.RelPath): Staleness =
    Locate.staleness(root, Vector(file -> tastyOf(root))).headOption.getOrElse(Staleness.NoTasty)

  def tests = Tests {
    test("a source no newer than its tasty is Fresh") {
      assert(staleness(fresh(), a) == Staleness.Fresh)
    }

    test("a source rewritten with the same content after its compile is Fresh") {
      val root = fresh()
      rewrite(root, a, identity)
      assert(staleness(root, a) == Staleness.Fresh)
    }

    test("an edit that changes a line's length is Stale") {
      val root = fresh()
      rewrite(root, a, _ + " ")
      assert(staleness(root, a) match {
        case Staleness.Stale(_, _) => true
        case _ => false
      })
    }

    test(
      "an edit that keeps every line's length reads Fresh: the known gap of comparing line sizes"
    ) {
      val root = fresh()
      rewrite(root, a, _.replace("package", "pockage"))
      assert(staleness(root, a) == Staleness.Fresh)
    }

    test(
      "a source is judged against the tasty compiled from it: a later tasty of another file in its package does not make an edit Fresh"
    ) {
      val root = fresh()
      rewrite(root, a, _ + " ")
      os.mtime.set(
        root.dir / "classes" / pkg / "Almanac.tasty",
        System.currentTimeMillis() + 7200000L
      )
      assert(staleness(root, a) match {
        case Staleness.Stale(_, _) => true
        case _ => false
      })
      assert(staleness(root, b) == Staleness.Fresh)
    }
  }
}
