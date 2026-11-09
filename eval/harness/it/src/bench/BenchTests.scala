package grit.eval.harness.bench

import java.io.ByteArrayInputStream

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.host.TempBlobs
import grit.core.id.{Harness, SessionId, Sha256}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.core.visibility.{Label, Level, Subject}
import grit.dbos.engine.LiveEngine
import grit.dbos.internal.{Reader, TrialRows}
import grit.dbos.sql.TestPostgres

import utest.*

/** The bench's keeping against a real Postgres: what is left on disk once the cap is passed. */
object BenchTests extends TestSuite {

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private def trial(local: String): TrialRows.Trial =
    ok(
      for {
        h <- Harness.of("claude-code")
        s <- SessionId.of(h, local)
        t <- TrialRows.TaskName.of("split-reader")
        m <- ModelId.of("anthropic/claude-opus-5-5").toRight("model")
        r <- TrialRows.Trial.of(
          s,
          t,
          m,
          Usage(Tokens(10), Tokens(5), Tokens(0), None),
          calls = 1,
          wall = 1.second,
          readBeforeEdit = None,
          callsBeforeEdit = None,
          passed = false,
          distance = None,
          standing = None
        )
      } yield r
    )

  val tests = Tests {
    test("keeping past the cap leaves every kept trial's transcript and diff blobs and no other") {
      val db = TestPostgres.freshDatabase("bench_keep")
      val engine = LiveEngine.open(db, "test")
      try {
        TempBlobs.around { blobs =>
          val n = TrialRows.MostTrials + 1
          val kept = (0 until n).map { i =>
            Bench.keep(engine.jot, blobs, Clock.system())(
              Subject.Public,
              trial(s"k-$i"),
              None,
              new ByteArrayInputStream(s"transcript $i".getBytes("UTF-8")),
              new ByteArrayInputStream(s"diff $i".getBytes("UTF-8")),
              Label.Public
            )
          }
          assert(
            kept.forall(_.isRight),
            blobs.kept().map(_.toSet) == Right(
              (1 until n).flatMap(i => Vector(s"transcript $i", s"diff $i")).map(Sha256.text).toSet
            )
          )
        }
      } finally engine.close()
    }

    test(
      "a trial's transcript reads back whole under its row's label; none for a session with no trial or one kept above what is read"
    ) {
      val db = TestPostgres.freshDatabase("bench_trajectory")
      val engine = LiveEngine.open(db, "test")
      try {
        TempBlobs.around { blobs =>
          def keep(local: String, text: String, label: Label) =
            ok(
              Bench.keep(engine.jot, blobs, Clock.system())(
                Subject.Public,
                trial(local),
                None,
                new ByteArrayInputStream(text.getBytes("UTF-8")),
                new ByteArrayInputStream("diff".getBytes("UTF-8")),
                label
              )
            )
          val whole = "[main] a call\n```\nits output, é\n```\n"
          keep("open", whole, Label.Public)
          keep("kept", "not for the public", Label.at(Level.Internal))
          val reader = Reader.open(db)
          try {
            def read(local: String) =
              Bench.trajectory(reader, blobs)(Subject.Public, trial(local).session)
            (read("open"), read("missing"), read("kept")) ==> (
              Right(Some(Bench.Transcript(whole, clipped = false))),
              Right(None),
              Right(None)
            )
          } finally reader.close()
        }
      } finally engine.close()
    }
  }
}
