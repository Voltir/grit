package grit.eval.harness.bench

import utest.*

object ChangedTests extends TestSuite {

  /** A diff of `files`, each a path and its hunks' changed lines, as `git diff -U0` prints it. */
  private def diff(files: (String, Seq[String])*): String =
    files
      .map { (path, changed) =>
        (Seq(
          s"diff --git a/$path b/$path",
          "index 1111111..2222222 100644",
          s"--- a/$path",
          s"+++ b/$path",
          "@@ -1,0 +1,2 @@"
        ) ++ changed).mkString("\n")
      }
      .mkString("", "\n", "\n")

  private val known = diff(
    "core/src/A.scala" -> Seq("-  def a: Int = 1", "+  def a: Int = 2", "+  def b: Int = 3")
  )

  val tests = Tests {
    test("a diff's changed lines are counted by file, sign and text, its headers not among them") {
      Changed.of(known).lines ==> Map(
        ("core/src/A.scala", '-', "  def a: Int = 1") -> 1,
        ("core/src/A.scala", '+', "  def a: Int = 2") -> 1,
        ("core/src/A.scala", '+', "  def b: Int = 3") -> 1
      )
    }

    test("only source files count: Scala, build.mill and schema.sql, not docs or data") {
      Changed
        .of(
          diff(
            "build.mill" -> Seq("+  grit.host,"),
            "core/dbos/src/schema.sql" -> Seq("+CREATE TABLE t ();"),
            "docs/README.md" -> Seq("+A line."),
            "eval/bench/tasks/x/task.json" -> Seq("+{}"),
            "x.mill.bak" -> Seq("+no")
          )
        )
        .lines
        .keySet
        .map(_._1) ==> Set("build.mill", "core/dbos/src/schema.sql")
    }

    test("the same changed lines are distance 0") {
      Changed.distance(Changed.of(known), Changed.of(known)) ==> 0L
    }

    test("a line one diff changes and the other does not counts one, each way") {
      val coder = diff(
        "core/src/A.scala" -> Seq("-  def a: Int = 1", "+  def a: Int = 2", "+  def c: Int = 4")
      )
      Changed.distance(Changed.of(known), Changed.of(coder)) ==> 2L
    }

    test("one line changed in two files counts once per file") {
      val line = Seq("+  val x = 1")
      Changed.distance(
        Changed.of(diff("A.scala" -> line)),
        Changed.of(diff("A.scala" -> line, "B.scala" -> line))
      ) ==> 1L
    }

    test("a line changed twice in one diff and once in the other counts one") {
      Changed.distance(
        Changed.of(diff("A.scala" -> Seq("+  }", "+  }"))),
        Changed.of(diff("A.scala" -> Seq("+  }")))
      ) ==> 1L
    }
  }
}
