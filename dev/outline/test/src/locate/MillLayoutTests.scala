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
  }
}
