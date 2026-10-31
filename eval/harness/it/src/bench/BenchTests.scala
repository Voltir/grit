package grit.eval.harness.bench

import java.io.ByteArrayInputStream

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.host.TempBlobs
import grit.core.id.{Harness, SessionId, Sha256}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.LiveEngine
import grit.dbos.internal.BenchRows
import grit.dbos.sql.TestPostgres

import utest.*

/** The bench's keeping against a real Postgres: what is left on disk once the cap is passed. */
object BenchTests extends TestSuite {

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private def trial(local: String): BenchRows.Trial =
    ok(
      for {
        h <- Harness.of("claude-code")
        s <- SessionId.of(h, local)
        t <- BenchRows.TaskName.of("split-reader")
        m <- ModelId.of("anthropic/claude-opus-5-5").toRight("model")
        r <- BenchRows.Trial.of(
          s,
          t,
          m,
          Usage(Tokens(10), Tokens(5), Tokens(0), None),
          calls = 1,
          wall = 1.second,
          readBeforeEdit = None,
          callsBeforeEdit = None,
          passed = false,
          distance = None
        )
      } yield r
    )

  val tests = Tests {
    test("keeping past the cap leaves every kept trajectory's blob and no other") {
      val db = TestPostgres.freshDatabase("bench_keep")
      val engine = LiveEngine.open(db, "test")
      try {
        TempBlobs.around { blobs =>
          val n = BenchRows.Kept + 1
          val kept = (0 until n).map { i =>
            Bench.keep(engine.jot, blobs, Clock.system())(
              Subject.Public,
              trial(s"k-$i"),
              new ByteArrayInputStream(s"transcript $i".getBytes("UTF-8")),
              Label.Public
            )
          }
          assert(
            kept.forall(_.isRight),
            blobs.kept().map(_.toSet) == Right(
              (1 until n).map(i => Sha256.text(s"transcript $i")).toSet
            )
          )
        }
      } finally engine.close()
    }
  }
}
