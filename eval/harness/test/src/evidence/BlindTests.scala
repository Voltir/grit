package grit.eval.harness.evidence

import grit.core.either.Got.got
import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.BenchTask

import utest.*

/** A task's diffs packed for a blind reviewer ([[Blind.packed]]). */
object BlindTests extends TestSuite {

  private val task: BlindTask = BlindTask.of(
    got(
      BenchTask.of(
        got(TrialRows.TaskName.of("commands-as-tools")),
        """{"base":"c905203b2f09fb367b42e0476b609fb458ec2bb4",
          |"commits":["8c5f8a48114526adebfbbbabe7217d2ec5478bc9"],
          |"checks":[{"runner":"check","args":["grit.models"]}]}""".stripMargin,
        "Serve the commands as tools.",
        Map.empty
      )
    )
  )

  private def section(file: String, added: String): String =
    s"""diff --git a/$file b/$file
       |index 1111111..2222222 100644
       |--- a/$file
       |+++ b/$file
       |@@ -1,2 +1,2 @@
       | kept
       |-gone
       |+$added
       |""".stripMargin

  private val diffs: Vector[(String, String)] =
    Vector("opus" -> section("a.scala", "one"), "glm" -> section("b.scala", "two")) ++
      Vector("original" -> section("c.scala", "three"))

  private def packed(seed: Long): Blind.Packed = got(Blind.packed(task, "Rank them.", diffs, seed))

  /** Each label's diff as its letter's file holds it. */
  private def shown(p: Blind.Packed): Map[String, String] = {
    val files = p.files.toMap
    p.key.flatMap((letter, label) => files.get(s"diff-$letter.patch").map(label -> _))
  }

  /** The labels in the order of their letters, comma-joined. */
  private def order(p: Blind.Packed): String =
    p.key.toVector.sortBy(_._1).map(_._2).mkString(",")

  val tests = Tests {
    test("the seed alone decides the order: one seed packs alike twice, and seeds differ") {
      assert(packed(7L) == packed(7L))
      val orders = (0L until 16L).map(s => order(packed(s))).toSet
      assert(orders.size > 1)
    }
    test("each letter's file holds the diff its label gave, beside the brief and the rubric") {
      val p = packed(3L)
      assert(p.key.keySet == Set("A", "B", "C"))
      assert(
        p.files.map(_._1).sorted ==
          Vector("brief.md", "diff-A.patch", "diff-B.patch", "diff-C.patch", "rubric.md")
      )
      assert(p.files.toMap.get("brief.md").contains(task.brief))
      assert(p.files.toMap.get("rubric.md").contains("Rank them."))
      val unindexed = diffs.toMap.map((label, d) =>
        label -> d.linesIterator.filterNot(_.startsWith("index ")).mkString("", "\n", "\n")
      )
      val mismatched = (0L until 16L).filter(s => shown(packed(s)) != unindexed)
      assert(mismatched.isEmpty)
    }
    test(
      "a patch's commit messages, authors, trailers and blob ids are dropped, its file sections kept"
    ) {
      val mailed =
        """From 8c5f8a48114526adebfbbbabe7217d2ec5478bc9 Mon Sep 17 00:00:00 2001
          |From: Opus Coder <opus@example.com>
          |Subject: [PATCH 1/2] tools: the commands, served
          |
          |Co-Authored-By: Claude Opus <noreply@example.com>
          |---
          | a.scala | 2 +-
          |
          |diff --git a/a.scala b/a.scala
          |index 1111111..2222222 100644
          |--- a/a.scala
          |+++ b/a.scala
          |@@ -1,2 +1,2 @@
          | kept
          |-gone
          |+one
          |--
          |2.43.0
          |
          |From 9d6e7f0011223344556677889900aabbccddeeff Mon Sep 17 00:00:00 2001
          |From: Opus Coder <opus@example.com>
          |Subject: [PATCH 2/2] tools: a second, by opus
          |
          |diff --git a/d.scala b/d.scala
          |new file mode 100644
          |index 0000000..3333333
          |--- /dev/null
          |+++ b/d.scala
          |@@ -0,0 +1 @@
          |+-- a line that starts like a signature
          |\ No newline at end of file
          |--
          |2.43.0
          |""".stripMargin
      val p = got(
        Blind.packed(task, "Rank them.", Vector("opus" -> mailed, "glm" -> diffs(1)._2), 0L)
      )
      val kept = shown(p).get("opus")
      val expected =
        """diff --git a/a.scala b/a.scala
          |--- a/a.scala
          |+++ b/a.scala
          |@@ -1,2 +1,2 @@
          | kept
          |-gone
          |+one
          |diff --git a/d.scala b/d.scala
          |new file mode 100644
          |--- /dev/null
          |+++ b/d.scala
          |@@ -0,0 +1 @@
          |+-- a line that starts like a signature
          |\ No newline at end of file
          |""".stripMargin
      assert(kept.contains(expected))
      assert(!p.files.exists((_, text) => text.toLowerCase.contains("opus")))
    }
    test("refused: one diff, a label given twice, or a diff with no file section") {
      val one = Blind.packed(task, "", diffs.take(1), 0L)
      val twice = Blind.packed(task, "", Vector("opus" -> diffs(0)._2, "opus" -> diffs(1)._2), 0L)
      val empty = Blind.packed(task, "", Vector("opus" -> diffs(0)._2, "glm" -> "no diff\n"), 0L)
      assert(one == Left("2 to 26 diffs are packed, not 1"))
      assert(twice == Left("the label opus is given twice"))
      assert(empty == Left("the diff glm holds no file section"))
    }
  }
}
