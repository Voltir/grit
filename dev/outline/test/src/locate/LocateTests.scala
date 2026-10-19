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
    test("forTopLevelIn finds nothing in a classes dir a compile removed before its walk") {
      val root = tree("out/a/compile.dest/classes/p/Moves.tasty")
      scala.util
        .Try(Locate.forTopLevelIn(Vector(at(root, "out/gone/compile.dest/classes")), "Moves"))
        .toEither
        .left
        .map(_.getClass.getSimpleName) ==> Right(Vector())
    }

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
        def moduleOf(root: Root, classesDir: os.Path): String = "m"
        def compile(modules: Vector[String]): String = "compile"
        def isTest(classesDir: os.Path): Boolean = false
      }
      val root = Root(os.temp.dir())
      assert(
        Locate.forTopLevel(root, fake, "Moves") == Vector(compiled / "p" / "Moves.tasty")
      )
    }

    test(
      "inPackageIn reads the .tasty files directly in the source's package dir from every classes dir"
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
        Locate.inPackageIn(
          root,
          MillLayout.classesDirs(root),
          os.RelPath("core/act/src/Moves.scala")
        ) == Vector(
          at(root, "out/a/compile.dest/classes/grit/core/act/X.tasty"),
          at(root, "out/b/x/compile.dest/classes/grit/core/act/Y.tasty")
        )
      )
    }

    test(
      "inPackagesIn gives each file its own package's .tasty files, as inPackageIn does, though two files in one dir declare two packages"
    ) {
      val root = tree(
        "core/act/src/Moves.scala",
        "core/act/src/Acts.scala",
        "core/act/src/sub/Sub.scala",
        "out/a/compile.dest/classes/grit/core/act/X.tasty",
        "out/a/compile.dest/classes/grit/core/act/X.class",
        "out/a/compile.dest/classes/grit/core/act/sub/Z.tasty"
      )
      os.write.over(
        root.dir / "core/act/src/Moves.scala",
        "package grit.core.act\n\nobject Moves\n"
      )
      os.write.over(
        root.dir / "core/act/src/Acts.scala",
        "package grit.core\npackage act.sub\n\nobject Acts\n"
      )
      os.write.over(
        root.dir / "core/act/src/sub/Sub.scala",
        "package grit.core.act.sub\n\nobject Sub\n"
      )
      val classes = MillLayout.classesDirs(root)
      val files =
        Vector("core/act/src/sub/Sub.scala", "core/act/src/Moves.scala", "core/act/src/Acts.scala")
          .map(os.RelPath(_))
      val inPackages = Locate.inPackagesIn(root, classes)
      assert(files.map(inPackages) == files.map(Locate.inPackageIn(root, classes, _)))
      assert(
        files.map(inPackages) == Vector(
          Vector(at(root, "out/a/compile.dest/classes/grit/core/act/sub/Z.tasty")),
          Vector(at(root, "out/a/compile.dest/classes/grit/core/act/X.tasty")),
          Vector(at(root, "out/a/compile.dest/classes/grit/core/act/sub/Z.tasty"))
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
      assert(
        Locate.mentioning(root, "Inbox", SourceWords.empty)._1 == Vector(
          os.RelPath("core/x/src/A.scala")
        )
      )
    }

    test(
      "mentioning finds a name beside a `$`, as in an interpolation, which a glob of a definition's name does not"
    ) {
      val root = tree("core/x/src/A.scala", "core/x/src/B.scala")
      os.write.over(root.dir / "core/x/src/A.scala", "object A { val s = s\"$Inbox\" }\n")
      os.write.over(root.dir / "core/x/src/B.scala", "object B { Inbox$1 }\n")
      assert(
        Locate.mentioning(root, "Inbox", SourceWords.empty)._1 == Vector(
          os.RelPath("core/x/src/A.scala"),
          os.RelPath("core/x/src/B.scala")
        ),
        !WordGlob("Inbox").foundIn("s\"$Inbox\""),
        !WordGlob("Inbox").foundIn("Inbox$1")
      )
    }

    test(
      "mentioningEach over the words it last read answers from a file's text now, though its size and mtime are unchanged"
    ) {
      val root = tree("core/x/src/A.scala")
      val a = root.dir / "core/x/src/A.scala"
      os.write.over(a, "object A { Alpha }\n")
      val (first, words) = Locate.mentioningEach(root, Vector("Alpha", "Bravo"), SourceWords.empty)
      val stamp = os.mtime(a)
      os.write.over(a, "object A { Bravo }\n")
      os.mtime.set(a, stamp)
      val (again, _) = Locate.mentioningEach(root, Vector("Alpha", "Bravo"), words)
      val rel = os.RelPath("core/x/src/A.scala")
      assert(first == Map("Alpha" -> Vector(rel), "Bravo" -> Vector.empty))
      assert(again == Map("Alpha" -> Vector.empty, "Bravo" -> Vector(rel)))
    }

    test(
      "mentioningEach forgets the words of a file deleted since the words it was given: written back unchanged, it is scanned again"
    ) {
      val root = tree("core/x/src/A.scala", "core/x/src/B.scala")
      val b = root.dir / "core/x/src/B.scala"
      os.write.over(root.dir / "core/x/src/A.scala", "object A { Alpha }\n")
      os.write.over(b, "object B { Bravo }\n")
      val (_, both) = Locate.mentioningEach(root, Vector("Alpha"), SourceWords.empty)
      os.remove(b)
      val (_, afterDelete) = Locate.mentioningEach(root, Vector("Alpha"), both)
      os.write(b, "object B { Bravo }\n")
      // `read` answers with the same words, unchanged, only for a file whose bytes it holds.
      assert(both.read(b)._3 eq both)
      assert(!(afterDelete.read(b)._3 eq afterDelete))
    }

    test(
      "mentioningEach gives each word the files holding it whole as \\b reads it: not inside a longer name, beside a letter \\w lacks, or with a dot"
    ) {
      val root = tree("core/x/src/A.scala", "core/x/src/B.scala", "core/x/src/C.scala")
      os.write.over(root.dir / "core/x/src/A.scala", "object A { Inbox.hear; Error }\n")
      os.write.over(root.dir / "core/x/src/B.scala", "object B { InboxError; Inbox.hear }\n")
      os.write.over(root.dir / "core/x/src/C.scala", "object C { Inboxes; Outboxé }\n")
      val a = os.RelPath("core/x/src/A.scala")
      val b = os.RelPath("core/x/src/B.scala")
      val c = os.RelPath("core/x/src/C.scala")
      assert(
        Locate
          .mentioningEach(
            root,
            Vector("Inbox", "InboxError", "Error", "Inbox.hear", "hear", "Outbox", "Spare"),
            SourceWords.empty
          )
          ._1 == Map(
          "Inbox" -> Vector(a, b),
          "InboxError" -> Vector(b),
          "Error" -> Vector(a),
          "Inbox.hear" -> Vector(a, b),
          "hear" -> Vector(a, b),
          "Outbox" -> Vector(c),
          "Spare" -> Vector.empty
        )
      )
    }

    test(
      "a source no tasty records as its own is Fresh while the newest tasty is newer, Stale once the source is later, and NoTasty with none"
    ) {
      val t0 = 1700000000000L
      val root = tree("core/x/src/A.scala", "out/a/compile.dest/classes/grit/core/x/A.tasty")
      val file = os.RelPath("core/x/src/A.scala")
      val tastyPath = at(root, "out/a/compile.dest/classes/grit/core/x/A.tasty")
      os.mtime.set(at(root, "core/x/src/A.scala"), t0)
      os.mtime.set(tastyPath, t0 + 1000)
      assert(
        Locate.staleness(root, Vector(file -> Vector(tastyPath))).headOption == Some(
          Staleness.Fresh
        )
      )
      os.mtime.set(at(root, "core/x/src/A.scala"), t0 + 5000)
      assert(
        Locate.staleness(root, Vector(file -> Vector(tastyPath))).headOption == Some(
          Staleness.Stale(Instant.ofEpochMilli(t0 + 5000), Instant.ofEpochMilli(t0 + 1000))
        )
      )
      assert(
        Locate.staleness(root, Vector(file -> Vector.empty)).headOption == Some(Staleness.NoTasty)
      )
    }

    test("a source file that is missing is NoSource, not an error") {
      val root = tree("out/a/compile.dest/classes/grit/core/x/A.tasty")
      val file = os.RelPath("core/x/src/A.scala")
      val tastyPath = at(root, "out/a/compile.dest/classes/grit/core/x/A.tasty")
      assert(
        Locate.staleness(root, Vector(file -> Vector(tastyPath))).headOption == Some(
          Staleness.NoSource
        )
      )
    }

    test("a .tasty deleted since it was read is left out of the judging, not an error") {
      val t0 = 1700000000000L
      val root = tree("core/x/src/A.scala", "out/a/compile.dest/classes/grit/core/x/A.tasty")
      val file = os.RelPath("core/x/src/A.scala")
      val kept = at(root, "out/a/compile.dest/classes/grit/core/x/A.tasty")
      val deleted = at(root, "out/a/compile.dest/classes/grit/core/x/A$.tasty")
      os.mtime.set(at(root, "core/x/src/A.scala"), t0 + 5000)
      os.mtime.set(kept, t0)
      assert(
        Locate.staleness(root, Vector(file -> Vector(kept, deleted))).headOption == Some(
          Staleness.Stale(Instant.ofEpochMilli(t0 + 5000), Instant.ofEpochMilli(t0))
        )
      )
      assert(
        Locate.staleness(root, Vector(file -> Vector(deleted))).headOption == Some(
          Staleness.NoTasty
        )
      )
    }
  }
}
