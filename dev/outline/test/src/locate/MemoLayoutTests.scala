package grit.outline.locate

import utest.*

object MemoLayoutTests extends TestSuite {

  /** A layout that counts each call to its two walks. */
  private final class Counting extends Layout {
    var classesWalks = 0
    var jarWalks = 0
    def classesDirs(root: Root): Vector[os.Path] = {
      classesWalks += 1
      Vector(root.dir / "out" / "classes")
    }
    def libraryJars(root: Root): Vector[os.Path] = {
      jarWalks += 1
      Vector(root.dir / "lib.jar")
    }
    def notCompiled(root: Root): String = "not compiled"
    def isTest(classesDir: os.Path): Boolean = false
    def isTestSource(file: os.RelPath): Boolean = false
  }

  def tests = Tests {
    test(
      "each root's classesDirs and libraryJars are walked once, however often they are asked for"
    ) {
      val counting = new Counting
      val memo = new MemoLayout(counting)
      val a = Root(os.root / "a")
      val b = Root(os.root / "b")
      val answers =
        Vector(
          memo.classesDirs(a),
          memo.classesDirs(a),
          memo.libraryJars(a),
          memo.libraryJars(a),
          memo.classesDirs(b)
        )
      assert(
        answers == Vector(
          Vector(a.dir / "out" / "classes"),
          Vector(a.dir / "out" / "classes"),
          Vector(a.dir / "lib.jar"),
          Vector(a.dir / "lib.jar"),
          Vector(b.dir / "out" / "classes")
        )
      )
      assert((counting.classesWalks, counting.jarWalks) == (2, 1))
    }
  }
}
