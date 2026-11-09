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
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

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
      thinking: Option[Tokens] = None,
      standing: Option[Sha256] = None
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
          distance = Some(12L),
          standing = standing
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
    withParts(blobs, rows, t, text, s"a diff of $text", label = label, clearance = clearance)

  /** `trial` recorded in `db` at `clearance` with `transcript` and `diff` as its parts, neither
    * clipped, kept in `blobs`.
    */
  private def withParts(
      blobs: Blobs^,
      rows: TrialRows^{blobs},
      t: TrialRows.Trial,
      transcript: String,
      diff: String,
      label: Label = Label.Public,
      clearance: Clearance = Low,
      db: DbConfig = config,
      standing: Option[String] = None,
      clipped: Set[TrialRows.Part] = Set.empty
  ): Either[StoreError, TrialRows.Recorded] =
    ok(blobs.receive(bytes(transcript), TrialRows.MostBytes) { p =>
      ok(blobs.receive(bytes(diff), TrialRows.MostBytes) { d =>
        LiveDb.transaction(db, clearance)(rows.record(t, standing, p, d, clipped, label))
      })
    })

  private def trials(): Vector[TrialRows.Trial] =
    LiveDb.transaction(config, Low)(ok(TrialRows.trials()))

  private def transcript(
      rows: TrialRows^,
      t: TrialRows.Trial,
      clearance: Clearance = Low
  ): Option[TrialRows.Kept] =
    LiveDb.transaction(config, clearance)(ok(rows.kept(t.session, TrialRows.Part.Transcript)))

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
          transcript(rows, t) == Some(TrialRows.Kept(sha, 12L, false, Label.Public)),
          blobs.at(sha).isDefined
        )
      }
    }

    test("recording a session's trial again is a repeat that writes nothing and keeps no blob") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val first = trial("t-repeat")
        ok(LiveDb.transaction(config, Low)(rows.record(first, None)))
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
        val big = bytes("x".repeat((TrialRows.MostBytes + 1).toInt))
        val refused = ok(blobs.receive(big, TrialRows.MostBytes + 1) { p =>
          ok(blobs.receive(bytes("a diff"), TrialRows.MostBytes) { d =>
            LiveDb.transaction(config, Low)(rows.record(t, None, p, d, Set.empty, Label.Public))
          })
        })
        assert(
          refused == Left(
            StoreError.Invalid(
              s"a trajectory holds at most ${TrialRows.MostBytes} bytes, not ${TrialRows.MostBytes + 1}"
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
      "recording past the cap removes the oldest recorded with its parts' rows, never their blobs"
    ) {
      // A database of its own: the cap counts every trial in it.
      val db = TestPostgres.freshDatabase("bench_cap")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val all = Vector.tabulate(TrialRows.MostTrials + 1)(i => trial(s"t-$i"))
        ok(withParts(blobs, rows, all(0), "the oldest", "the oldest diff", db = db))
        val middle =
          all
            .slice(1, TrialRows.MostTrials)
            .map(t => LiveDb.transaction(db, Low)(rows.record(t, None)))
        val last = LiveDb.transaction(db, Low)(rows.record(all(TrialRows.MostTrials), None))
        assert(
          middle.forall(_ == Right(TrialRows.Recorded.Fresh(0))),
          last == Right(TrialRows.Recorded.Fresh(1)),
          LiveDb.transaction(db, Low)(ok(TrialRows.trials())) == all.drop(1),
          LiveDb.transaction(db, Low)(ok(rows.kept(all(0).session, TrialRows.Part.Transcript))) ==
            None,
          LiveDb.transaction(db, Low)(ok(rows.kept(all(0).session, TrialRows.Part.Diff))) == None,
          blobs.kept().map(_.toSet) ==
            Right(Set(Sha256.text("the oldest"), Sha256.text("the oldest diff")))
        )
      }
    }

    test("a trial's thinking is read back as recorded, reported or not reported") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val reported = trial("t-thinks", thinking = Some(Tokens(250)))
        val silent = trial("t-silent")
        ok(LiveDb.transaction(config, Low)(rows.record(reported, None)))
        ok(LiveDb.transaction(config, Low)(rows.record(silent, None)))
        assert(
          trials().filter(_.session == reported.session) == Vector(reported),
          trials().filter(_.session == silent.session) == Vector(silent)
        )
      }
    }

    test(
      "a prune removes each blob no transcript or diff names and keeps those named, one two trials share included"
    ) {
      val db = TestPostgres.freshDatabase("bench_prune")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        def kept(t: TrialRows.Trial, transcript: String, diff: String) =
          ok(withParts(blobs, rows, t, transcript, diff, db = db))
        kept(trial("p-a"), "shared", "diff a")
        kept(trial("p-b"), "shared", "diff b")
        kept(trial("p-c"), "its own", "diff c")
        ok(blobs.receive(bytes("stray"), 10)(_.keep()))
        val pruned = LiveDb.transaction(db, Low)(rows.prune(java.time.Instant.EPOCH))
        assert(
          pruned == Right(Right(TrialRows.Pruned(1, 0))),
          blobs.kept().map(_.toSet) == Right(
            Set("shared", "its own", "diff a", "diff b", "diff c").map(Sha256.text)
          )
        )
      }
    }

    test(
      "a trial's standing text is recorded by its digest and kept, through a prune, as its blob"
    ) {
      val db = TestPostgres.freshDatabase("bench_standing")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val text = "You work by these rules."
        // The sha256 of `text`'s UTF-8 bytes, as `sha256sum` prints it.
        val digest = "74f00f5498dbded008ef1327ac055d11587f05959f307a9a24dbc34d21ce0a40"
        val t = trial("s-kept", standing = Sha256.read(digest))
        val recorded = LiveDb.transaction(db, Low)(rows.record(t, Some(text)))
        val pruned = LiveDb.transaction(db, Low)(rows.prune(java.time.Instant.EPOCH))
        assert(
          recorded == Right(TrialRows.Recorded.Fresh(0)),
          pruned == Right(Right(TrialRows.Pruned(0, 0))),
          LiveDb.transaction(db, Low)(ok(TrialRows.trials())).map(_.standing.map(_.hex)) ==
            Vector(Some(digest)),
          Sha256.read(digest).flatMap(blobs.at).map(java.nio.file.Files.readString) == Some(text)
        )
      }
    }

    test("a standing text its trial's digest does not name is refused, recording nothing") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val t = trial("s-other", standing = Some(Sha256.text("the rules")))
        val recorded = LiveDb.transaction(config, Low)(rows.record(t, Some("other rules")))
        assert(
          recorded == Left(
            StoreError.Invalid("a trial's standing text is not the one its digest names")
          ),
          trials().filter(_.session == t.session) == Vector.empty,
          blobs.kept() == Right(Vector.empty)
        )
      }
    }

    test("a trial pruned past the cap takes its standing text's and its parts' blobs with it") {
      val db = TestPostgres.freshDatabase("bench_standing_cap")
      LiveEngine.open(db, "test").close()
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val oldest = trial("s-0", standing = Some(Sha256.text("the oldest rules")))
        val texts = Vector("the oldest rules", "its transcript", "its diff")
        ok(
          withParts(blobs, rows, oldest, texts(1), texts(2), db = db, standing = Some(texts(0)))
        )
        val kept = blobs.kept().map(_.toSet)
        Vector
          .tabulate(TrialRows.MostTrials)(i => trial(s"s-${i + 1}"))
          .foreach(t => ok(LiveDb.transaction(db, Low)(rows.record(t, None))))
        val pruned = LiveDb.transaction(db, Low)(rows.prune(java.time.Instant.EPOCH))
        assert(
          kept == Right(texts.map(Sha256.text).toSet),
          pruned == Right(Right(TrialRows.Pruned(3, 0))),
          blobs.kept() == Right(Vector.empty)
        )
      }
    }

    test("a trial's diff is kept beside its transcript, each read back with its own clip") {
      TempBlobs.around { blobs =>
        val rows = TrialRows(blobs)
        val t = trial("d-kept")
        val diff = "diff --git a/a.scala b/a.scala\n-object A\n+object A { val x = 1 }\n"
        val recorded =
          withParts(blobs, rows, t, "a transcript", diff, clipped = Set(TrialRows.Part.Diff))
        val read = LiveDb.transaction(config, Low)(ok(rows.kept(t.session, TrialRows.Part.Diff)))
        assert(
          recorded == Right(TrialRows.Recorded.Fresh(0)),
          read == Some(TrialRows.Kept(Sha256.text(diff), diff.length.toLong, true, Label.Public)),
          transcript(rows, t).map(_.clipped) == Some(false),
          read.flatMap(k => blobs.at(k.sha256)).map(java.nio.file.Files.readString) == Some(diff)
        )
      }
    }

    test("a part's kind is stored under its recorded name") {
      // Stored in bench_trajectories.kind: renaming one strands every row kept under it.
      TrialRows.Part.values.toVector.map(TrialRows.kind) ==> Vector("transcript", "diff")
    }
  }
}
