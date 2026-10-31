package grit.dbos.internal

import java.io.ByteArrayInputStream

import scala.concurrent.duration.*

import grit.core.host.{Blobs, TempBlobs}
import grit.core.id.{Harness, SessionId, Sha256}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.core.store.StoreError
import grit.core.visibility.{Clearance, Label, TestLabels}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{LiveDb, TestPostgres}

import utest.*

/** The bench's rows against a real Postgres: a trial and its trajectory kept and read back, who
  * reads a trajectory, and the cap.
  */
object BenchRowsTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("bench_rows")
    LiveEngine.open(c, "test").close()
    c
  }

  private val Low: Clearance = Clearance.of(Label.Public)
  private val High: Clearance = Clearance.of(TestLabels.Trial)

  private def ok[A](e: Either[Any, A]): A =
    e.fold(x => throw new java.lang.AssertionError(x.toString), identity)

  private def trial(local: String, task: String = "split-reader"): BenchRows.Trial =
    ok(
      for {
        h <- Harness.of("claude-code")
        s <- SessionId.of(h, local)
        t <- BenchRows.TaskName.of(task)
        m <- ModelId.of("anthropic/claude-opus-5-5").toRight("model")
        r <- BenchRows.Trial.of(
          s,
          t,
          m,
          Usage(Tokens(1200), Tokens(300), Tokens(800), Some(BigDecimal("0.0123")), Tokens(100)),
          calls = 7,
          wall = 95.seconds,
          readBeforeEdit = Some(4096L),
          callsBeforeEdit = Some(3),
          passed = true,
          distance = Some(12L)
        )
      } yield r
    )

  private def bytes(text: String) = new ByteArrayInputStream(text.getBytes("UTF-8"))

  /** `trial` recorded at `clearance` with `text` as its transcript, kept in `blobs`. */
  private def withTranscript(
      blobs: Blobs^,
      rows: BenchRows^{blobs},
      t: BenchRows.Trial,
      text: String,
      label: Label = Label.Public,
      clearance: Clearance = Low
  ): Either[StoreError, BenchRows.Recorded] =
    ok(blobs.receive(bytes(text), BenchRows.Longest) { p =>
      LiveDb.transaction(config, clearance)(rows.record(t, p, clipped = false, label))
    })

  private def trials(rows: BenchRows^): Vector[BenchRows.Trial] =
    LiveDb.transaction(config, Low)(ok(rows.trials()))

  private def trajectory(
      rows: BenchRows^,
      t: BenchRows.Trial,
      clearance: Clearance = Low
  ): Option[BenchRows.Trajectory] =
    LiveDb.transaction(config, clearance)(ok(rows.trajectory(t.session)))

  val tests = Tests {
    test("a trial and its trajectory recorded are read back as recorded, the blob kept") {
      TempBlobs.around { blobs =>
        val rows = BenchRows(blobs)
        val t = trial("t-read")
        val recorded = withTranscript(blobs, rows, t, "a transcript")
        val sha = Sha256.text("a transcript")
        assert(
          recorded == Right(BenchRows.Recorded.Fresh(0)),
          trials(rows).filter(_.session == t.session) == Vector(t),
          trajectory(rows, t) == Some(BenchRows.Trajectory(sha, 12L, false, Label.Public)),
          blobs.at(sha).isDefined
        )
      }
    }

    test("recording a session's trial again is a repeat that writes nothing and keeps no blob") {
      TempBlobs.around { blobs =>
        val rows = BenchRows(blobs)
        val first = trial("t-repeat")
        ok(LiveDb.transaction(config, Low)(rows.record(first)))
        val again = trial("t-repeat", task = "another-task")
        val repeat = withTranscript(blobs, rows, again, "never kept")
        assert(
          repeat == Right(BenchRows.Recorded.Repeat),
          trials(rows).filter(_.session == first.session) == Vector(first),
          trajectory(rows, first) == None,
          blobs.kept() == Right(Vector.empty)
        )
      }
    }

    test("a transcript past the longest a trajectory keeps is refused, recording nothing") {
      TempBlobs.around { blobs =>
        val rows = BenchRows(blobs)
        val t = trial("t-long")
        val big = bytes("x".repeat((BenchRows.Longest + 1).toInt))
        val refused = ok(blobs.receive(big, BenchRows.Longest + 1) { p =>
          LiveDb.transaction(config, Low)(rows.record(t, p, clipped = false, Label.Public))
        })
        assert(
          refused == Left(
            StoreError.Invalid(
              s"a trajectory holds at most ${BenchRows.Longest} bytes, not ${BenchRows.Longest + 1}"
            )
          ),
          trials(rows).filter(_.session == t.session) == Vector.empty,
          blobs.kept() == Right(Vector.empty)
        )
      }
    }

    test("a trajectory kept where a transaction does not read is not read back to it") {
      TempBlobs.around { blobs =>
        val rows = BenchRows(blobs)
        val t = trial("t-high")
        ok(withTranscript(blobs, rows, t, "secret", TestLabels.Trial, High))
        assert(
          trajectory(rows, t, Low) == None,
          trajectory(rows, t, High).map(_.label) == Some(TestLabels.Trial)
        )
      }
    }

    test(
      "recording past the cap removes the oldest recorded with its trajectory's row, never its blob"
    ) {
      // A database of its own: the cap counts every trial in it.
      val db = TestPostgres.freshDatabase("bench_cap")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = BenchRows(blobs)
        val all = Vector.tabulate(BenchRows.Kept + 1)(i => trial(s"t-$i"))
        ok(blobs.receive(bytes("the oldest"), BenchRows.Longest) { p =>
          LiveDb.transaction(db, Low)(rows.record(all(0), p, clipped = false, Label.Public))
        })
        val middle =
          all.slice(1, BenchRows.Kept).map(t => LiveDb.transaction(db, Low)(rows.record(t)))
        val last = LiveDb.transaction(db, Low)(rows.record(all(BenchRows.Kept)))
        assert(
          middle.forall(_ == Right(BenchRows.Recorded.Fresh(0))),
          last == Right(BenchRows.Recorded.Fresh(1)),
          LiveDb.transaction(db, Low)(ok(rows.trials())) == all.drop(1),
          LiveDb.transaction(db, Low)(ok(rows.trajectory(all(0).session))) == None,
          blobs.kept() == Right(Vector(Sha256.text("the oldest")))
        )
      }
    }

    test(
      "a prune removes each blob no trajectory names and keeps those named, one two trials share included"
    ) {
      val db = TestPostgres.freshDatabase("bench_prune")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = BenchRows(blobs)
        def kept(t: BenchRows.Trial, text: String) =
          ok(blobs.receive(bytes(text), BenchRows.Longest) { p =>
            LiveDb.transaction(db, Low)(rows.record(t, p, clipped = false, Label.Public))
          })
        kept(trial("p-a"), "shared")
        kept(trial("p-b"), "shared")
        kept(trial("p-c"), "its own")
        ok(blobs.receive(bytes("stray"), 10)(_.keep()))
        val pruned = LiveDb.transaction(db, Low)(rows.prune(java.time.Instant.EPOCH))
        assert(
          pruned == Right(Right(BenchRows.Pruned(1, 0))),
          blobs.kept().map(_.toSet) == Right(Set(Sha256.text("shared"), Sha256.text("its own")))
        )
      }
    }
  }
}
