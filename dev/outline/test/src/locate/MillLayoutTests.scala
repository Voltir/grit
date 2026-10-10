package grit.outline.locate

import utest.*

object MillLayoutTests extends TestSuite {

  /** A fresh checkout with an empty file at each relative path. */
  private def tree(files: String*): Root = {
    val dir = os.temp.dir()
    files.foreach(f => os.write(dir / os.RelPath(f), "", createFolders = true))
    Root(dir)
  }

  private def at(root: Root, p: String): os.Path = root.dir / os.RelPath(p)

  def tests = Tests {
    test(
      "classesDirs finds each compile.dest/classes and nothing inside a classes dir or another .dest"
    ) {
      val root = tree(
        "out/a/compile.dest/classes/grit/A.tasty",
        "out/b/test/compile.dest/classes/grit/B.tasty",
        "out/a/zinc.dest/classes/grit/Z.tasty",
        "out/a/compile.dest/classes/x/compile.dest/classes/grit/N.tasty"
      )
      assert(
        MillLayout.classesDirs(root) == Vector(
          at(root, "out/a/compile.dest/classes"),
          at(root, "out/b/test/compile.dest/classes")
        )
      )
    }

    test(
      "classesDirs skips a nested Mill output directory (one holding a mill-out-lock) and any mill-build"
    ) {
      val root = tree(
        "out/m/compile.dest/classes/grit/M.tasty",
        "out/lint/mill-out-lock",
        "out/lint/m/compile.dest/classes/grit/M.tasty",
        "out/mill-build/compile.dest/classes/build/B.tasty"
      )
      assert(
        MillLayout.classesDirs(root) == Vector(at(root, "out/m/compile.dest/classes"))
      )
    }

    test(
      "libraryJars skips a jar named only under a nested Mill output directory"
    ) {
      val root = tree(
        "out/m/compile.dest/classes/x/A.tasty",
        "out/lint/mill-out-lock",
        "out/lint/m/resolvedMvnDeps.json"
      )
      val jar = at(root, "lib/a.jar")
      os.write(jar, "", createFolders = true)
      os.write.over(
        at(root, "out/lint/m/resolvedMvnDeps.json"),
        ujson.write(ujson.Obj("value" -> ujson.Arr(jar.toString)))
      )
      assert(MillLayout.libraryJars(root) == Vector.empty[os.Path])
    }

    test(
      "libraryJars is the existing .jar files a resolvedMvnDeps.json names, with the coursier prefix stripped"
    ) {
      val root = tree("out/m/compile.dest/classes/x/A.tasty", "lib/b.pom")
      val jar = at(root, "lib/a.jar")
      os.write(jar, "", createFolders = true)
      val missing = at(root, "lib/missing.jar")
      val pom = at(root, "lib/b.pom")
      val deps = ujson.Obj(
        "value" -> ujson.Arr(
          s"qref:v1:abc123:$jar",
          s"ref:v1:abc123:$missing",
          s"qref:v1:abc123:$pom"
        )
      )
      os.write(at(root, "out/m/resolvedMvnDeps.json"), ujson.write(deps))
      assert(MillLayout.libraryJars(root) == Vector(jar))
    }

    test(
      "isTestSource is true for a source under a test or it module's src, and false under a main one"
    ) {
      assert(MillLayout.isTestSource(os.RelPath("core/core/test/src/inbox/InboxTests.scala")))
      assert(MillLayout.isTestSource(os.RelPath("core/dbos/it/src/Engine.scala")))
      assert(!MillLayout.isTestSource(os.RelPath("core/core/src/inbox/Inbox.scala")))
      assert(!MillLayout.isTestSource(os.RelPath("core/core/src/test/Helper.scala")))
    }

    test("isTest is true for a classes dir under a test or it module, and false under a main one") {
      val root = tree(
        "out/x/test/compile.dest/classes/A.tasty",
        "out/x/compile.dest/classes/A.tasty"
      )
      assert(MillLayout.isTest(at(root, "out/x/test/compile.dest/classes")))
      assert(!MillLayout.isTest(at(root, "out/x/compile.dest/classes")))
    }
  }
}
