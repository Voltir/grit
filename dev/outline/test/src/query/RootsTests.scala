package grit.outline.query

import grit.outline.locate.{MillLayout, Root}
import grit.outline.read.Read

import utest.*

object RootsTests extends TestSuite {

  /** A checkout under a fresh temp dir: an empty `build.mill`, `src/p/S.scala` holding `source`, compiled into its `out`. */
  private def checkout(name: String, source: String): Root = {
    val dir = os.temp.dir(prefix = s"outline-$name-")
    os.write(dir / "build.mill", "")
    val src = dir / "src" / "p" / "S.scala"
    os.write(src, source, createFolders = true)
    val classes = dir / "out" / "m" / "compile.dest" / "classes"
    os.makeDir.all(classes)
    val reporter = dotty.tools.dotc.Main.process(
      Array(
        "-d",
        classes.toString,
        "-classpath",
        System.getProperty("java.class.path"),
        src.toString
      )
    )
    assert(!reporter.hasErrors)
    Root(dir)
  }

  private val one = "package p\n/** One. */\nobject S { def f(a: Int): Int = a }\n"
  private val two = "package p\n/** Two. */\nobject S { def f(a: String, b: Int): String = a }\n"

  def tests = Tests {
    test(
      "alternating roots: each answer holds its own root's signature and header, never the other's"
    ) {
      val r1 = checkout("one", one)
      val r2 = checkout("two", two)
      val (a1, s1) =
        Query.show(r1, MillLayout, Roots.empty(6000), Vector("S.f"), 1, Set.empty, false, 80000)
      val (a2, s2) = Query.show(r2, MillLayout, s1, Vector("S.f"), 1, Set.empty, false, 80000)
      val (a3, _) = Query.show(r1, MillLayout, s2, Vector("S.f"), 1, Set.empty, false, 80000)
      assert(a1.text.startsWith(s"## root ${r1.dir} ("))
      assert(a1.text.contains("def f(a: Int): Int"))
      assert(a2.text.startsWith(s"## root ${r2.dir} ("))
      assert(a2.text.contains("def f(a: String, b: Int): String"))
      assert(a3.text == a1.text)
    }

    test("Read.classpath is the root's own classes directory, every entry under the root") {
      val r1 = checkout("classpath", one)
      val entries = Read.classpath(r1, MillLayout)
      assert(entries.nonEmpty)
      assert(entries.forall(_.toString.startsWith(r1.dir.toString + "/")))
    }

    test(
      "a root whose out/ was built in another checkout is Failed, saying the source is outside it"
    ) {
      val r1 = checkout("foreign", one)
      val r3 = Root(os.temp.dir(prefix = "outline-foreign-"))
      os.copy(r1.dir / "out", r3.dir / "out")
      os.copy(r1.dir / "src", r3.dir / "src")
      os.write(r3.dir / "build.mill", "")
      val (answer, _) =
        Query.show(r3, MillLayout, Roots.empty(6000), Vector("S.f"), 1, Set.empty, false, 80000)
      assert(answer.status == Status.Failed)
      assert(answer.text.contains("outside"))
    }

    test("a ref to a top-level opaque type is traced to its definition, not reported not loaded") {
      val r = checkout(
        "opaque",
        "package p\nopaque type Tag = Int\nobject S { def f(a: Tag): Tag = a }\n"
      )
      val (answer, _) =
        Query.show(r, MillLayout, Roots.empty(6000), Vector("S.f"), 1, Set.empty, false, 80000)
      assert(answer.status == Status.Found)
      assert(answer.text.contains("opaque type Tag"))
      assert(!answer.text.contains("not loaded"))
    }

    test("Roots.put with room for one file keeps only the root put last") {
      val a = Root(os.temp.dir(prefix = "outline-cache-a-"))
      val b = Root(os.temp.dir(prefix = "outline-cache-b-"))
      val loadedA = Loaded(Map(a.dir / "A.tasty" -> Cached(0L, Vector.empty)))
      val loadedB = Loaded(Map(b.dir / "B.tasty" -> Cached(0L, Vector.empty)))
      val roots = Roots.put(Roots.put(Roots.empty(1), a, loadedA), b, loadedB)
      assert(roots.byRoot.map(_._1) == Vector(b))
    }
  }
}
