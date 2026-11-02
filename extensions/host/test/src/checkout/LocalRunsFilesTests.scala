package grit.host.checkout

import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.{Clock, Fresh}
import grit.core.host.{Argv, RunStatus}

import utest.*

/** [[LocalRuns]]' files: where a run's output is kept, and what an earlier process left. */
object LocalRunsFilesTests extends TestSuite {

  /** `script` run by `sh -c`. */
  def sh(script: String): Argv =
    Argv.of("sh", "-c", script).getOrElse(throw new IllegalArgumentException(script))

  /** A fresh directory, given to `body`, deleted after. */
  def inCheckout[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-runs")
    try body(dir)
    finally { val _ = LocalTree.removed(dir) }
  }

  /** Runs over `root`, with the system's clock and random ids. */
  def over(root: Path): LocalRuns^ =
    new LocalRuns(root, sys.env, Redaction.of(Map.empty), Clock.system(), Fresh.random())

  /** Every path under `dir`, relative to it, sorted. */
  private def listed(dir: Path): Vector[String] = {
    val walk = Files.walk(dir)
    try walk.iterator.asScala.map(p => dir.relativize(p).toString).toVector.sorted
    finally walk.close()
  }

  /** A run an earlier process `stem` left: a `sleep 60`, recorded in `stem/aaaaaaaa.pid` with
    * the start `start` gives, beside `stem.lock`, which nobody holds. Its process.
    */
  private def left(runs: Path, stem: String, start: Process => String): Process = {
    val process = new ProcessBuilder("sleep", "60").start()
    val dir = Files.createDirectories(runs.resolve(stem))
    Files.writeString(dir.resolve("aaaaaaaa.out"), "half a build\n")
    Files.writeString(dir.resolve("aaaaaaaa.pid"), s"${process.pid()} ${start(process)}")
    Files.writeString(runs.resolve(s"$stem.lock"), "")
    process
  }

  private def started(p: Process): String =
    p.toHandle.info().startInstant().map(_.toEpochMilli.toString).orElse("-")

  private val quick = sh("echo hi")

  val tests: Tests = Tests {
    test("a run's output is kept under .grit/runs, which git ignores, and close removes it") {
      inCheckout { root =>
        val runs = over(root)
        val id = runs.start(quick, 10.seconds, 5.seconds).map(_.id)
        val kept = listed(root).filter(_.endsWith(".out"))
        val text = kept.headOption.map(p => Files.readString(root.resolve(p)))
        val ignore = Files.readString(root.resolve(".grit/runs/.gitignore"))
        runs.close()
        (
          id.isRight,
          kept.map(_.split('/').toList.lastOption),
          text,
          ignore,
          listed(root.resolve(".grit/runs"))
        ) ==> (
          true,
          Vector(id.toOption.map(i => s"${i.written}.out")),
          Some("hi\n"),
          "*\n",
          Vector("", ".gitignore")
        )
      }
    }

    test("close kills a run still going") {
      inCheckout { root =>
        val runs = over(root)
        val got = runs.start(sh("exec sleep 30"), 10.seconds, Duration.Zero).map(_.status)
        val pids =
          listed(root).filter(_.endsWith(".pid")).map(p => Files.readString(root.resolve(p)))
        runs.close()
        val alive = pids.flatMap(_.split(" ").headOption.flatMap(_.toLongOption)).map { pid =>
          java.lang.ProcessHandle.of(pid).map(_.isAlive).orElse(false)
        }
        (got.isRight && got.exists(_.isInstanceOf[RunStatus.Going]), alive) ==> (
          true,
          Vector(false)
        )
      }
    }

    test("a run whose lock no process holds is killed, and its files removed, at the first start") {
      inCheckout { root =>
        val runs = root.resolve(".grit/runs")
        val process = left(runs, "1-deadbeef", started)
        try {
          val made = over(root)
          try {
            val ran = made.start(quick, 10.seconds, 5.seconds).isRight
            val ended = process.waitFor(5, TimeUnit.SECONDS)
            val leftovers = listed(runs).filter(_.startsWith("1-deadbeef"))
            (ran, ended, leftovers) ==> (true, true, Vector.empty)
          } finally made.close()
        } finally { val _ = process.destroyForcibly() }
      }
    }

    test("a dead process that had this one's pid is swept: its run killed, its files removed") {
      inCheckout { root =>
        val runs = root.resolve(".grit/runs")
        val stem = s"${ProcessHandle.current().pid()}-12345-deadbeef"
        val process = left(runs, stem, started)
        try {
          val made = over(root)
          try {
            val ran = made.start(quick, 10.seconds, 5.seconds).isRight
            val ended = process.waitFor(5, TimeUnit.SECONDS)
            (ran, ended, listed(runs).filter(_.startsWith(stem))) ==> (true, true, Vector.empty)
          } finally made.close()
        } finally { val _ = process.destroyForcibly() }
      }
    }

    test("a run whose recorded start is not its process's is not killed, though its files go") {
      inCheckout { root =>
        val runs = root.resolve(".grit/runs")
        val process = left(runs, "1-deadbeef", _ => "12345")
        try {
          val made = over(root)
          try {
            val ran = made.start(quick, 10.seconds, 5.seconds).isRight
            (ran, process.isAlive, listed(runs).filter(_.startsWith("1-deadbeef"))) ==>
              (true, true, Vector.empty)
          } finally made.close()
        } finally { val _ = process.destroyForcibly() }
      }
    }

    test("a run whose lock is held is left as it is") {
      inCheckout { root =>
        val runs = root.resolve(".grit/runs")
        val process = left(runs, "1-deadbeef", started)
        val channel = FileChannel.open(runs.resolve("1-deadbeef.lock"), StandardOpenOption.WRITE)
        try {
          val _ = channel.lock()
          val made = over(root)
          try {
            val ran = made.start(quick, 10.seconds, 5.seconds).isRight
            (ran, process.isAlive, listed(runs).filter(_.startsWith("1-deadbeef"))) ==>
              (
                true,
                true,
                Vector(
                  "1-deadbeef",
                  "1-deadbeef.lock",
                  "1-deadbeef/aaaaaaaa.out",
                  "1-deadbeef/aaaaaaaa.pid"
                )
              )
          } finally made.close()
        } finally {
          channel.close()
          val _ = process.destroyForcibly()
        }
      }
    }
  }
}
