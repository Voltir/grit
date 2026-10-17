package grit.outline.query

import java.nio.file.Files
import java.nio.file.attribute.FileTime

import grit.outline.locate.{MillLayout, Root}
import grit.outline.render.TestsShown
import grit.outline.testing.Dotc

import utest.*

object StaleLineTests extends TestSuite {

  /** A temp root with three Mill modules: `m`, whose package `p` holds objects A to D each with a member `f`, `n`,
    * whose package `q` holds E with an `f`, and `k`, whose package `r` holds a trait T and its implementation U, each in
    * its own file; compiled once, the checkout untouched.
    */
  private lazy val dir: os.Path = {
    val d = Dotc.millRoot("outline-stale-line-")
    def module(name: String, pkg: String, objects: Vector[String]): Unit =
      compiled(name, pkg, objects.map(o => o -> s"object $o { def f: Int = 1 }"))
    // Each source is its package line and one line of `text`.
    def compiled(name: String, pkg: String, texts: Vector[(String, String)]): Unit = {
      val sources = texts.map { case (file, text) =>
        val src = d / name / "src" / pkg / s"$file.scala"
        os.write(src, s"package $pkg\n$text\n", createFolders = true)
        src
      }
      Dotc.compile(Dotc.classesOf(d, os.sub / name), sources)
    }
    module("m", "p", Vector("A", "B", "C", "D"))
    module("n", "q", Vector("E"))
    compiled(
      "k",
      "r",
      Vector("T" -> "trait T { def g: Int }", "U" -> "object U extends T { def g: Int = 2 }")
    )
    d
  }

  /** Every source of the root as compiled and set an hour before its compile, or (`stale`) given one more line and set an
    * hour after it.
    */
  private def touch(stale: Boolean): Unit = {
    val at = System.currentTimeMillis() + (if (stale) 3600000L else -3600000L)
    os.walk(dir)
      .filter(_.ext == "scala")
      .foreach { p =>
        val compiled = os.read(p).linesIterator.take(2).mkString("", "\n", "\n")
        os.write.over(p, if (stale) compiled + "// edited\n" else compiled)
        Files.setLastModifiedTime(p.toNIO, FileTime.fromMillis(at))
      }
  }

  private def find(pattern: String): Answer =
    Query.find(Root(dir), MillLayout, Config.empty, Roots.empty(6000), pattern, false, 80000)._1

  def tests = Tests {
    test("an answer read from tasty newer than its sources has no stale line") {
      touch(stale = false)
      val lines = find("f").text.linesIterator.toVector
      assert(lines.lift(1).exists(_.startsWith("p.A.f  ")))
      assert(!lines.exists(_.startsWith("-- stale")))
    }

    test(
      "a find reading stale files of two modules names each after the root line, three files a module then +N, and the command compiling both"
    ) {
      touch(stale = true)
      val lines = find("f").text.linesIterator.toVector
      assert(
        lines.lift(1) == Some(
          "-- stale since the last compile: m (A.scala, B.scala, C.scala +1), n (E.scala): ./mill m.compile + n.compile"
        )
      )
    }

    test("a stale line names only the files the query read: show p.A names A.scala alone") {
      touch(stale = true)
      val (answer, _) = Query.show(
        Root(dir),
        MillLayout,
        Config.empty,
        Roots.empty(6000),
        Vector("p.A"),
        depth = 0,
        bodies = Set.empty,
        withPrivate = false,
        cap = 80000,
        tests = TestsShown.Dropped
      )
      assert(
        answer.text.linesIterator.toVector.lift(1) ==
          Some("-- stale since the last compile: m (A.scala): ./mill m.compile")
      )
    }

    test(
      "the stale line and the file tags judge alike: an edited file is on both though another file's tasty in its package is later"
    ) {
      touch(stale = true)
      val laterTasty = dir / "out" / "m" / "compile.dest" / "classes" / "p" / "B.tasty"
      val compiledAt = os.mtime(laterTasty)
      os.mtime.set(laterTasty, System.currentTimeMillis() + 7200000L)
      try {
        val (answer, _) = Query.show(
          Root(dir),
          MillLayout,
          Config.empty,
          Roots.empty(6000),
          Vector("p.A"),
          depth = 0,
          bodies = Set.empty,
          withPrivate = false,
          cap = 80000,
          tests = TestsShown.Dropped
        )
        val lines = answer.text.linesIterator.toVector
        assert(
          lines.lift(1) == Some("-- stale since the last compile: m (A.scala): ./mill m.compile")
        )
        assert(lines.contains("== m/src/p/A.scala  p  [stale: source newer than .tasty]"))
      } finally os.mtime.set(laterTasty, compiledAt)
    }

    test("a family reading an implementation from a stale file tags that file's == line") {
      touch(stale = true)
      val (answer, _) = Query.family(
        Root(dir),
        MillLayout,
        Config.empty,
        Roots.empty(6000),
        "r.T",
        member = None,
        withBody = false,
        cap = 80000
      )
      assert(
        answer.text.linesIterator.contains(
          "== k/src/r/U.scala  r  [stale: source newer than .tasty]"
        )
      )
    }

    test("a family of a trait on one line prints that line once") {
      touch(stale = false)
      val (answer, _) = Query.family(
        Root(dir),
        MillLayout,
        Config.empty,
        Roots.empty(6000),
        "r.T",
        member = None,
        withBody = false,
        cap = 80000
      )
      val printed = answer.text.linesIterator.toVector
      assert(printed.count(_ == "trait T { def g: Int }") == 1)
    }

    test("a show of a trait on one line prints that line once") {
      touch(stale = false)
      val (answer, _) = Query.show(
        Root(dir),
        MillLayout,
        Config.empty,
        Roots.empty(6000),
        Vector("r.T"),
        depth = 0,
        bodies = Set.empty,
        withPrivate = false,
        cap = 80000,
        tests = TestsShown.Dropped
      )
      val printed = answer.text.linesIterator.toVector
      assert(printed.count(_ == "trait T { def g: Int }") == 1)
    }

    test("a show of a trait on one line with --body naming its member prints the member's entry") {
      touch(stale = false)
      val (answer, _) = Query.show(
        Root(dir),
        MillLayout,
        Config.empty,
        Roots.empty(6000),
        Vector("r.T"),
        depth = 0,
        bodies = Set("g"),
        withPrivate = false,
        cap = 80000,
        tests = TestsShown.Dropped
      )
      assert(
        answer.text.linesIterator.drop(1).toVector == Vector(
          "== k/src/r/T.scala  r",
          "2-2  [0.2 KB]",
          "trait T { def g: Int }",
          "2-2  [0.2 KB]",
          "trait T { def g: Int }",
          "[0.1 KB]"
        )
      )
    }
  }
}
