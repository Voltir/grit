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

/** The bench's rows against a real Postgres: a trial and its transcript kept and read back, who
  * reads a transcript, and the cap.
  */
object TrialRowsTests extends TestSuite {

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

  private def trial(
      local: String,
      task: String = "split-reader",
      thinking: Option[Tokens] = None
  ): TrialRows.Trial =
    ok(
      for {
        h <- Harness.of("claude-code")
        s <- SessionId.of(h, local)
        t <- TrialRows.TaskName.of(task)
        m <- ModelId.of("anthropic/claude-opus-5-5").toRight("model")
        r <- TrialRows.Trial.of(
          s,
          t,
          m,
          Usage(
            Tokens(1200),
            Tokens(300),
            Tokens(800),
            Some(BigDecimal("0.0123")),
            Tokens(100),
            thinking
          ),
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
      rows: TrialRows^{blobs},
      t: TrialRows.Trial,
      text: String,
      label: Label = Label.Public,
      clearance: Clearance = Low
  ): Either[StoreError, TrialRows.Recorded] =
    ok(blobs.receive(bytes(text), TrialRows.Longest) { p =>
      LiveDb.transaction(config, clearance)(rows.record(t, p, clipped = false, label))
    })

  private def trials(): Vector[TrialRows.Trial] =
    LiveDb.transaction(config, Low)(ok(TrialRows.trials()))

  private def transcript(
      rows: TrialRows^,
      t: TrialRows.Trial,
      clearance: Clearance = Low
  ): Option[TrialRows.Transcript] =
    LiveDb.transaction(config, clearance)(ok(rows.transcript(t.session)))

  val tests = Tests {
    test("a trial and its transcript recorded are read back as recorded, the blob kept") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val t = trial("t-read")
        val recorded = withTranscript(blobs, rows, t, "a transcript")
        val sha = Sha256.text("a transcript")
        assert(
          recorded == Right(TrialRows.Recorded.Fresh(0)),
          trials().filter(_.session == t.session) == Vector(t),
          transcript(rows, t) == Some(TrialRows.Transcript(sha, 12L, false, Label.Public)),
          blobs.at(sha).isDefined
        )
      }
    }

    test("recording a session's trial again is a repeat that writes nothing and keeps no blob") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val first = trial("t-repeat")
        ok(LiveDb.transaction(config, Low)(rows.record(first)))
        val again = trial("t-repeat", task = "another-task")
        val repeat = withTranscript(blobs, rows, again, "never kept")
        assert(
          repeat == Right(TrialRows.Recorded.Repeat),
          trials().filter(_.session == first.session) == Vector(first),
          transcript(rows, first) == None,
          blobs.kept() == Right(Vector.empty)
        )
      }
    }

    test("a transcript past the longest a transcript keeps is refused, recording nothing") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val t = trial("t-long")
        val big = bytes("x".repeat((TrialRows.Longest + 1).toInt))
        val refused = ok(blobs.receive(big, TrialRows.Longest + 1) { p =>
          LiveDb.transaction(config, Low)(rows.record(t, p, clipped = false, Label.Public))
        })
        assert(
          refused == Left(
            StoreError.Invalid(
              s"a trajectory holds at most ${TrialRows.Longest} bytes, not ${TrialRows.Longest + 1}"
            )
          ),
          trials().filter(_.session == t.session) == Vector.empty,
          blobs.kept() == Right(Vector.empty)
        )
      }
    }

    test("a transcript kept where a transaction does not read is not read back to it") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val t = trial("t-high")
        ok(withTranscript(blobs, rows, t, "secret", TestLabels.Trial, High))
        assert(
          transcript(rows, t, Low) == None,
          transcript(rows, t, High).map(_.label) == Some(TestLabels.Trial)
        )
      }
    }

    test(
      "recording past the cap removes the oldest recorded with its transcript's row, never its blob"
    ) {
      // A database of its own: the cap counts every trial in it.
      val db = TestPostgres.freshDatabase("bench_cap")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val all = Vector.tabulate(TrialRows.Kept + 1)(i => trial(s"t-$i"))
        ok(blobs.receive(bytes("the oldest"), TrialRows.Longest) { p =>
          LiveDb.transaction(db, Low)(rows.record(all(0), p, clipped = false, Label.Public))
        })
        val middle =
          all.slice(1, TrialRows.Kept).map(t => LiveDb.transaction(db, Low)(rows.record(t)))
        val last = LiveDb.transaction(db, Low)(rows.record(all(TrialRows.Kept)))
        assert(
          middle.forall(_ == Right(TrialRows.Recorded.Fresh(0))),
          last == Right(TrialRows.Recorded.Fresh(1)),
          LiveDb.transaction(db, Low)(ok(TrialRows.trials())) == all.drop(1),
          LiveDb.transaction(db, Low)(ok(rows.transcript(all(0).session))) == None,
          blobs.kept() == Right(Vector(Sha256.text("the oldest")))
        )
      }
    }

    test("a trial's thinking is read back as recorded, reported or not reported") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val reported = trial("t-thinks", thinking = Some(Tokens(250)))
        val silent = trial("t-silent")
        ok(LiveDb.transaction(config, Low)(rows.record(reported)))
        ok(LiveDb.transaction(config, Low)(rows.record(silent)))
        assert(
          trials().filter(_.session == reported.session) == Vector(reported),
          trials().filter(_.session == silent.session) == Vector(silent)
        )
      }
    }

    test(
      "a prune removes each blob no transcript names and keeps those named, one two trials share included"
    ) {
      val db = TestPostgres.freshDatabase("bench_prune")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        def kept(t: TrialRows.Trial, text: String) =
          ok(blobs.receive(bytes(text), TrialRows.Longest) { p =>
            LiveDb.transaction(db, Low)(rows.record(t, p, clipped = false, Label.Public))
          })
        kept(trial("p-a"), "shared")
        kept(trial("p-b"), "shared")
        kept(trial("p-c"), "its own")
        ok(blobs.receive(bytes("stray"), 10)(_.keep()))
        val pruned = LiveDb.transaction(db, Low)(rows.prune(java.time.Instant.EPOCH))
        assert(
          pruned == Right(Right(TrialRows.Pruned(1, 0))),
          blobs.kept().map(_.toSet) == Right(Set(Sha256.text("shared"), Sha256.text("its own")))
        )
      }
    }
  }
}
