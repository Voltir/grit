package grit.eval.harness.evidence

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.{BenchTask, Sha}

import utest.*

/** A frozen task as a proposer sees it ([[BlindTask]]), over the bench's own frozen tasks. */
object BlindTaskTests extends TestSuite {

  /** Each frozen task under `GRIT_BENCH_TASKS`, read as the bench reads it. */
  private def frozen(): Vector[BenchTask] = {
    val dir = Path.of(sys.env.getOrElse("GRIT_BENCH_TASKS", "")).toAbsolutePath
    Files.list(dir).iterator.asScala.toVector.sortBy(_.getFileName.toString).map { at =>
      val read = for {
        name <- TrialRows.TaskName.of(at.getFileName.toString)
        task <- BenchTask.of(
          name,
          Files.readString(at.resolve("task.json")),
          Files.readString(at.resolve("brief.md"))
        )
      } yield task
      read.fold(why => throw new java.lang.AssertionError(why), identity)
    }
  }

  val tests = Tests {
    test(
      "a frozen task as a proposer sees it keeps its name, base and brief, and names none of its commits"
    ) {
      val tasks = frozen()
      val blind = tasks.map(BlindTask.of)
      // A commit is named by any prefix git would take for it, 7 hex digits or more.
      val named = blind.zip(tasks).map { (b, t) =>
        t.commits.filter(c => b.brief.contains(Sha.value(c).take(7)))
      }
      // The brief compared by equality alone: a frozen brief runs to pages.
      (blind.zip(tasks).map((b, t) => (b.name, b.base, b.brief == t.brief)), named) ==> (
        tasks.map(t => (t.name, t.base, true)),
        Vector.fill(tasks.size)(Vector.empty[Sha])
      )
    }
  }
}
