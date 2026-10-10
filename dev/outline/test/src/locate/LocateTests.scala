package grit.outline.locate

import java.time.Instant

import grit.outline.model.Staleness

import utest.*

object LocateTests extends TestSuite {

  /** A fresh checkout with an empty file at each relative path. */
  private def tree(files: String*): Root = {
    val dir = os.temp.dir()
    files.foreach(f => os.write(dir / os.RelPath(f), "", createFolders = true))
    Root(dir)
  }

  private def at(root: Root, p: String): os.Path = root.dir / os.RelPath(p)

  def tests = Tests {
    test(
      "forTopLevel with a package reads only that package's dir; without one it searches every classes dir"
    ) {
      val pkgRoot = tree(
        "out/a/compile.dest/classes/grit/core/act/Moves.tasty",
        "out/b/test/compile.dest/classes/grit/core/act/Moves.tasty"
      )
      assert(
        Locate.forTopLevel(pkgRoot, MillLayout, "grit.core.act.Moves") == Vector(
          at(pkgRoot, "out/a/compile.dest/classes/grit/core/act/Moves.tasty"),
          at(pkgRoot, "out/b/test/compile.dest/classes/grit/core/act/Moves.tasty")
        )
      )

      val noPkgRoot = tree(
        "out/a/compile.dest/classes/grit/core/act/Moves.tasty",
        "out/b/test/compile.dest/classes/grit/core/act/Moves.tasty"
      )
      assert(
        Locate.forTopLevel(noPkgRoot, MillLayout, "Moves.ask") == Vector(
          at(noPkgRoot, "out/a/compile.dest/classes/grit/core/act/Moves.tasty"),
          at(noPkgRoot, "out/b/test/compile.dest/classes/grit/core/act/Moves.tasty")
        )
      )

      val otherRoot = tree(
        "out/a/compile.dest/classes/grit/other/Moves.tasty",
        "out/a/compile.dest/classes/grit/core/act/Moves.tasty"
      )
      assert(
        Locate.forTopLevel(otherRoot, MillLayout, "grit.other.Moves") == Vector(
          at(otherRoot, "out/a/compile.dest/classes/grit/other/Moves.tasty")
        )
      )

      val packageObjectRoot =
        tree("out/a/compile.dest/classes/grit/core/act/MoveName$package.tasty")
      assert(
        Locate.forTopLevel(packageObjectRoot, MillLayout, "MoveName") == Vector(
          at(packageObjectRoot, "out/a/compile.dest/classes/grit/core/act/MoveName$package.tasty")
        )
      )
    }

    test(
      "forTopLevel finds a top-level's tasty under whatever classes dir a layout other than Mill's names"
    ) {
      val compiled = os.temp.dir()
      os.write(compiled / "p" / "Moves.tasty", "", createFolders = true)
      val fake = new Layout {
        def isTestSource(file: os.RelPath): Boolean = false
        def classesDirs(root: Root): Vector[os.Path] = Vector(compiled)
        def libraryJars(root: Root): Vector[os.Path] = Vector.empty
        def notCompiled(root: Root): String = "not compiled"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val root = Root(os.temp.dir())
      assert(
        Locate.forTopLevel(root, fake, "Moves") == Vector(compiled / "p" / "Moves.tasty")
      )
    }

    test(
      "inPackageOf reads the .tasty files directly in the source's package dir from every classes dir"
    ) {
      val root = tree(
        "core/act/src/Moves.scala",
        "out/a/compile.dest/classes/grit/core/act/X.tasty",
        "out/b/x/compile.dest/classes/grit/core/act/Y.tasty",
        "out/a/compile.dest/classes/grit/core/act/sub/Z.tasty"
      )
      os.write.over(
        root.dir / "core/act/src/Moves.scala",
        "package grit.core.act\n\nobject Moves\n"
      )
      assert(
        Locate.inPackageOf(root, MillLayout, os.RelPath("core/act/src/Moves.scala")) == Vector(
          at(root, "out/a/compile.dest/classes/grit/core/act/X.tasty"),
          at(root, "out/b/x/compile.dest/classes/grit/core/act/Y.tasty")
        )
      )
    }

    test(
      "mentioning finds the source that uses the name as a whole word, and not build output or a longer name"
    ) {
      val root = tree("core/x/src/A.scala", "core/x/src/B.scala", "out/x/src/C.scala")
      os.write.over(root.dir / "core/x/src/A.scala", "object A { Inbox.hear }\n")
      os.write.over(root.dir / "core/x/src/B.scala", "object B { InboxError }\n")
      os.write.over(root.dir / "out/x/src/C.scala", "object C { Inbox }\n")
      assert(Locate.mentioning(root, "Inbox") == Vector(os.RelPath("core/x/src/A.scala")))
    }

    test(
      "staleness is Fresh while the tasty is newer, Stale once the source is later, and NoTasty with none"
    ) {
      val t0 = 1700000000000L
      val root = tree("core/x/src/A.scala", "out/a/compile.dest/classes/grit/core/x/A.tasty")
      val file = os.RelPath("core/x/src/A.scala")
      val tastyPath = at(root, "out/a/compile.dest/classes/grit/core/x/A.tasty")
      os.mtime.set(at(root, "core/x/src/A.scala"), t0)
      os.mtime.set(tastyPath, t0 + 1000)
      assert(Locate.staleness(root, file, Vector(tastyPath)) == Staleness.Fresh)
      os.mtime.set(at(root, "core/x/src/A.scala"), t0 + 5000)
      assert(
        Locate.staleness(root, file, Vector(tastyPath)) == Staleness.Stale(
          Instant.ofEpochMilli(t0 + 5000),
          Instant.ofEpochMilli(t0 + 1000)
        )
      )
      assert(Locate.staleness(root, file, Vector.empty) == Staleness.NoTasty)
    }

    test("a source file that is missing is NoSource, not an error") {
      val root = tree("out/a/compile.dest/classes/grit/core/x/A.tasty")
      val file = os.RelPath("core/x/src/A.scala")
      val tastyPath = at(root, "out/a/compile.dest/classes/grit/core/x/A.tasty")
      assert(Locate.staleness(root, file, Vector(tastyPath)) == Staleness.NoSource)
    }
  }
}
