package grit.outline.query

import java.nio.file.Files
import java.nio.file.attribute.FileTime

import grit.outline.locate.{MillLayout, Root}

import utest.*

object StaleLineTests extends TestSuite {

  /** A temp root with two Mill modules: `m`, whose package `p` holds objects A to D each with a member `f`, and `n`,
    * whose package `q` holds E with an `f`; compiled once, the checkout untouched.
    */
  private lazy val dir: os.Path = {
    val d = os.temp.dir(prefix = "outline-stale-line-")
    os.write(d / "build.mill", "")
    def module(name: String, pkg: String, objects: Vector[String]): Unit = {
      val sources = objects.map { o =>
        val src = d / name / "src" / pkg / s"$o.scala"
        os.write(src, s"package $pkg\nobject $o { def f: Int = 1 }\n", createFolders = true)
        src
      }
      val classes = d / "out" / name / "compile.dest" / "classes"
      os.makeDir.all(classes)
      val reporter = dotty.tools.dotc.Main.process(
        Array("-d", classes.toString, "-classpath", System.getProperty("java.class.path")) ++
          sources.map(_.toString)
      )
      assert(!reporter.hasErrors)
    }
    module("m", "p", Vector("A", "B", "C", "D"))
    module("n", "q", Vector("E"))
    d
  }

  /** Every source of the root set an hour after (`stale`) or before its compile. */
  private def touch(stale: Boolean): Unit = {
    val at = System.currentTimeMillis() + (if (stale) 3600000L else -3600000L)
    os.walk(dir)
      .filter(_.ext == "scala")
      .foreach(p => Files.setLastModifiedTime(p.toNIO, FileTime.fromMillis(at)))
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
        withTests = false
      )
      assert(
        answer.text.linesIterator.toVector.lift(1) ==
          Some("-- stale since the last compile: m (A.scala): ./mill m.compile")
      )
    }
  }
}
