package grit.outline.query

import java.nio.file.Files
import java.nio.file.attribute.FileTime

import grit.outline.locate.{MillLayout, Root}
import grit.outline.testing.FixtureLayout

import utest.*

object FileTests extends TestSuite {

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

  private lazy val root: Root = Root(repoRoot)

  private val matches = "dev/outline/fixture/src/Matches.scala"
  private val sample = "dev/outline/fixture/src/Sample.scala"

  private def file(target: String): Answer =
    Query.file(root, FixtureLayout, Config.empty, Roots.empty(6000), target, 80000)._1

  private def linesOf(target: String): Vector[String] = file(target).text.linesIterator.toVector

  private def source(path: String): Vector[String] =
    os.read(repoRoot / os.RelPath(path)).split("\n").toVector

  /** A temp root with two Mill modules, `m` and `n`, each holding a `p/A.scala` compiled once; `m` also holds
    * `p/Z.scala`, never compiled.
    */
  private lazy val dir: os.Path = {
    val d = os.temp.dir(prefix = "outline-file-")
    os.write(d / "build.mill", "")
    def module(name: String, pkg: String): Unit = {
      val src = d / name / "src" / pkg / "A.scala"
      os.write(src, s"package $pkg\nobject A { def f: Int = 1 }\n", createFolders = true)
      val classes = d / "out" / name / "compile.dest" / "classes"
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
    }
    module("m", "p")
    module("n", "q")
    os.write(d / "m" / "src" / "p" / "Z.scala", "package p\nobject Z\n")
    d
  }

  private def fileIn(target: String): Answer =
    Query.file(Root(dir), MillLayout, Config.empty, Roots.empty(6000), target, 80000)._1

  val tests = Tests {
    test(
      "a file's package and imports print first under their line range, as the file's own lines"
    ) {
      val lines = linesOf(matches)
      assert(lines.lift(1).exists(_.startsWith(s"== $matches  grit.outline.fixture  46 lines  [")))
      assert(lines.lift(2) == Some("1-4"))
      assert(lines.slice(3, 7) == source(matches).take(4))
    }
    test(
      "each top-level definition is a header line of its range, kind and name, in order, and no doc prints"
    ) {
      val heads =
        linesOf(matches).filter(l => l.matches("""\d+-\d+ \w.*""") && !l.endsWith("end of file"))
      assert(
        heads.map(_.takeWhile(_ != '[').trim) == Vector(
          "6-34 object Matches",
          "38-46 object Parsed"
        )
      )
      assert(!linesOf(matches).exists(_.contains("/**")))
    }
    test(
      "a type's members print one level down, and their members do not: Outer's Inner, not Inner's deep"
    ) {
      val lines = linesOf(sample)
      assert(lines.exists(_.matches("""  \d+-\d+ object Inner .*""")))
      assert(!lines.exists(_.contains("deep")))
      assert(linesOf(matches).exists(_.matches("""  41-42 def number .*""")))
    }
    test(
      "lines outside every top-level definition print where they stand, as the file's own lines"
    ) {
      val lines = linesOf(matches)
      val at = lines.indexOf("36-36")
      assert(at > 0)
      assert(lines.lift(at + 1) == Some("// A comment between two top-level definitions."))
      assert(lines.lift(at + 2).exists(_.startsWith("38-46 object Parsed")))
    }
    test("a file ends with its last lines under an end of file line, as the file's own lines") {
      val lines = linesOf(matches).dropRight(1)
      assert(lines.takeRight(3) == Vector("45-46 end of file") ++ source(matches).takeRight(2))
    }
    test("a definition's name, or a unique path suffix, finds its file") {
      val byName = file("Holder")
      assert(byName.status == Status.Found)
      assert(byName.text.linesIterator.exists(_.startsWith(s"== $sample  ")))
      val bySuffix = file("fixture/src/Sample.scala")
      assert(bySuffix.text == byName.text)
    }
    test("a suffix two files end with is NoMatch naming both") {
      val answer = fileIn("A.scala")
      assert(answer.status == Status.NoMatch)
      assert(answer.text.contains("m/src/p/A.scala"), answer.text.contains("n/src/q/A.scala"))
    }
    test("a file with no tasty fails, naming the command that compiles it") {
      val answer = fileIn("m/src/p/Z.scala")
      assert(answer.status == Status.Failed)
      assert(answer.text.contains(MillLayout.compile(Vector("m"))))
    }
    test("a source edited since its tasty is tagged stale") {
      val src = dir / "n" / "src" / "q" / "A.scala"
      val compiled = os.read(src)
      os.write.over(src, compiled + "// edited\n")
      Files.setLastModifiedTime(
        src.toNIO,
        FileTime.fromMillis(System.currentTimeMillis() + 3600000L)
      )
      try {
        val lines = fileIn("n/src/q/A.scala").text.linesIterator.toVector
        assert(lines.exists(l => l.startsWith("== n/src/q/A.scala  q") && l.contains("[stale: ")))
      } finally os.write.over(src, compiled)
    }
  }
}
