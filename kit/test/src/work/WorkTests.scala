package grit.kit.work

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.{Clock, Fresh}
import grit.core.id.SourceId
import grit.core.inbox.TurnEnding
import grit.core.store.{TestWentOn, WentOn}
import grit.host.checkout.{LocalRuns, Redaction}

import utest.*

/** What `grit work` decides with no database: its arguments, its text's source, what follows
  * a turn, and its task's check.
  */
object WorkTests extends TestSuite {

  /** Runs over `root`, as `grit work` checks its task: the system's clock and random ids. */
  private def over(root: Path): LocalRuns^ =
    new LocalRuns(root, sys.env, Redaction.of(Map.empty), Clock.system(), Fresh.random())

  /** `dir` and everything under it, deleted. */
  private def removed(dir: Path): Unit = {
    val walk = Files.walk(dir)
    try walk.iterator.asScala.toVector.reverse.foreach(p => { val _ = Files.deleteIfExists(p) })
    finally walk.close()
  }

  val tests: Tests = Tests {

    test("a text is said under work: and its SHA-256, so the same text names one source") {
      (Work.source("hi"), Work.source("hi"), Work.source("hi ") == Work.source("hi")) ==> (
        SourceId("work:8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"),
        SourceId("work:8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"),
        false
      )
    }

    test(
      "work takes a file, --check, a command and a file, or --dry and a file; anything else is refused with the form"
    ) {
      (
        Work.asked("grit", Vector("brief.md")),
        Work.asked("grit", Vector("--dry", "brief.md")),
        Work.asked("grit", Vector("--check", "make test", "brief.md")),
        Work.asked("grit", Vector.empty),
        Work.asked("grit", Vector("brief.md", "--dry")),
        Work.asked("grit", Vector("a.md", "b.md")),
        Work.asked("grit", Vector("--check", "make test", "--dry", "brief.md")),
        Work.asked("grit", Vector("--check", "brief.md")),
        Work.asked("grit", Vector("--check", " ", "brief.md"))
      ) ==> (
        Right(Work.Asked(Path.of("brief.md"), dry = false, None)),
        Right(Work.Asked(Path.of("brief.md"), dry = true, None)),
        Right(Work.Asked(Path.of("brief.md"), dry = false, Some(TestWentOn.command("make test")))),
        Left(Work.usage("grit")),
        Left(Work.usage("grit")),
        Left(Work.usage("grit")),
        Left(Work.usage("grit")),
        Left(Work.usage("grit")),
        Left("the check command is empty")
      )
    }

    test("work's form and refusals name the program it runs under") {
      val dir = Files.createTempDirectory("work-named")
      try {
        val missing = dir.resolve("missing.md")
        val blank = Files.writeString(dir.resolve("blank.md"), " \n")
        (
          Work.asked("kalandra", Vector.empty),
          Work.keyed("kalandra"),
          Work.text("kalandra", missing).left.map(_.takeWhile(_ != ':')),
          Work.text("kalandra", blank)
        ) ==> (
          Left(
            "kalandra work takes one file: kalandra work [--check '<command>'] <file>, or " +
              "kalandra work --dry <file>"
          ),
          "kalandra work --dry calls no model, so it runs only with no model key set: " +
            "unset ANTHROPIC_API_KEY and OPENROUTER_API_KEY",
          Left(s"kalandra work cannot read $missing"),
          Left(s"kalandra work was given $blank, which is empty")
        )
      } finally removed(dir)
    }

    test("a task's turn n after its first is said under its text's source and :n") {
      (Work.source("hi", 1), Work.source("hi", 2)) ==> (
        Work.source("hi"),
        SourceId("work:8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4:2")
      )
    }

    test(
      "a turn that ended at its limit goes on to the next until GRIT_WORK_TURNS, said in one line; any other ending stops"
    ) {
      (
        Work.after("w1", TurnEnding.AtLimit, 1, 8, None, None),
        Work.after("w8", TurnEnding.AtLimit, 8, 8, None, None),
        Work.after("w3", TurnEnding.Otherwise, 3, 8, None, None)
      ) ==> (
        Work.After.GoOn(
          2,
          WentOn.AtLimit,
          "[main] w1 reached its limit; going on in a fresh turn (turn 2 of at most 8)"
        ),
        Work.After.Stop(
          Some(
            "[main] w8 reached its limit at turn 8, the most GRIT_WORK_TURNS allows; raise GRIT_WORK_TURNS and rerun"
          )
        ),
        Work.After.Stop(None)
      )
    }

    test(
      "a turn that answered stops unchecked or when its check passes; a failed check goes on with its output's last lines, until the cap or a repeat"
    ) {
      val failed =
        TestWentOn.failed("make test", Some(1), (1 to 30).map(n => s"l$n").mkString("\n"))
      val stopped = TestWentOn.failed("make test", None, "slow")
      val tail = (11 to 30).map(n => s"\n  | l$n").mkString
      (
        Work.after("w2", TurnEnding.Answered, 2, 8, None, None),
        Work.after("w2", TurnEnding.Answered, 2, 8, Some(Work.Verdict.Passed), Some(failed)),
        Work.after("w2", TurnEnding.Answered, 2, 8, Some(Work.Verdict.Failed(failed)), None),
        Work.after(
          "w2",
          TurnEnding.Answered,
          2,
          8,
          Some(Work.Verdict.Failed(stopped)),
          Some(failed)
        ),
        Work.after("w8", TurnEnding.Answered, 8, 8, Some(Work.Verdict.Failed(failed)), None),
        Work.after(
          "w3",
          TurnEnding.Answered,
          3,
          8,
          Some(Work.Verdict.Failed(failed)),
          Some(failed)
        ),
        Work.after("w2", TurnEnding.Answered, 2, 8, Some(Work.Verdict.Unrun("no sh")), None),
        Work.after("w2", TurnEnding.Otherwise, 2, 8, Some(Work.Verdict.Failed(failed)), None)
      ) ==> (
        Work.After.Stop(None),
        Work.After.Stop(Some("[main] w2: the check passed; the task is done")),
        Work.After.GoOn(
          3,
          failed,
          "[main] w2: the check failed (exit 1); going on in a fresh turn (turn 3 of at most 8)" +
            tail
        ),
        Work.After.GoOn(
          3,
          stopped,
          "[main] w2: the check failed (it ran past 30 minutes and was stopped); going on in a " +
            "fresh turn (turn 3 of at most 8)\n  | slow"
        ),
        Work.After.Fail(
          "w8: the check still fails at turn 8, the most GRIT_WORK_TURNS allows; fix it or " +
            "raise GRIT_WORK_TURNS and rerun"
        ),
        Work.After.Fail(
          "w3: the check failed with the same output as last time; stopping, since another " +
            "turn is unlikely to change it"
        ),
        Work.After.Fail("the check could not run: no sh"),
        Work.After.Stop(None)
      )
    }

    test(
      "a check runs in the task's directory: exit 0 passes; another exit, or past its time, fails with the end of its output"
    ) {
      val root = Files.createTempDirectory("grit-check").toRealPath()
      val work = over(root)
      try {
        def check(command: String) = Work.checked(work, TestWentOn.command(command), 1.second)
        (
          check("test -d . && echo fine"),
          check("echo wrong in $(basename $PWD); exit 3"),
          check("echo started; sleep 5"),
          check("seq 1 20000; exit 1")
        ) ==> (
          Work.Verdict.Passed,
          Work.Verdict.Failed(
            TestWentOn.failed(
              "echo wrong in $(basename $PWD); exit 3",
              Some(3),
              s"wrong in ${root.getFileName}"
            )
          ),
          Work.Verdict.Failed(TestWentOn.failed("echo started; sleep 5", None, "started")),
          Work.Verdict.Failed(
            TestWentOn.failed(
              "seq 1 20000; exit 1",
              Some(1),
              (20001 - WentOn.TailLines to 20000).mkString("\n")
            )
          )
        )
      } finally {
        work.close()
        removed(root)
      }
    }

    test(
      "a check is recorded where the next start's sweep in its directory finds it: a .pid under .grit/runs names its shell while it runs"
    ) {
      val root = Files.createTempDirectory("grit-check").toRealPath()
      val shell = root.resolve("shell.pid")
      val work = over(root)
      try {
        val checking = Thread.ofPlatform().start { () =>
          val _ =
            Work.checked(work, TestWentOn.command("echo $$ > shell.pid; sleep 20"), 30.seconds)
        }
        val began = System.nanoTime()
        while (
          (!Files.exists(shell) || Files.readString(shell).isBlank) &&
          System.nanoTime() - began < 3.seconds.toNanos
        ) Thread.sleep(20)
        val pid = Files.readString(shell).trim
        val runs = root.resolve(".grit/runs")
        val recorded =
          if (!Files.isDirectory(runs)) Vector.empty
          else {
            val walk = Files.walk(runs)
            try
              walk.iterator.asScala.toVector
                .filter(_.getFileName.toString.endsWith(".pid"))
                .map(Files.readString(_).trim.takeWhile(_ != ' '))
            finally walk.close()
          }
        work.close()
        checking.join(5000)
        recorded ==> Vector(pid)
      } finally {
        work.close()
        removed(root)
      }
    }

    test("GRIT_WORK_TURNS: unset is 8; under 1 or not a whole number is an error") {
      (
        Work.turns(Map.empty),
        Work.turns(Map("GRIT_WORK_TURNS" -> " 3 ")),
        Work.turns(Map("GRIT_WORK_TURNS" -> "0")),
        Work.turns(Map("GRIT_WORK_TURNS" -> "many"))
      ) ==> (
        Right(8),
        Right(3),
        Left("GRIT_WORK_TURNS is a whole number of at least 1, not 0"),
        Left("GRIT_WORK_TURNS is a whole number of at least 1, not many")
      )
    }
  }
}
