package grit.core.retention

import scala.concurrent.duration.*

import grit.core.id.{
  ConversationId,
  DocumentVersionId,
  FileHandle,
  Harness,
  PeriodRef,
  PeriodSeq,
  PieceSetId,
  PluginName,
  ScheduleId,
  SessionId
}
import grit.core.period.{CloseOrdinal, Windows}

import utest.*

object TargetTests extends TestSuite {

  private val p = PeriodRef(ConversationId("c1"), PeriodSeq.of(3).getOrElse(PeriodSeq.First))
  private val digest = PluginName.of("digest").fold(sys.error, identity)
  private val seven = CloseOrdinal.of(7).getOrElse(CloseOrdinal.Start)
  private val version = DocumentVersionId.of(42).getOrElse(sys.error("version"))
  private val schedule = ScheduleId.of("declared:plugin:digest:daily").fold(sys.error, identity)
  private val file = FileHandle.of(41).getOrElse(sys.error("file"))
  private val set = PieceSetId.of(43).getOrElse(sys.error("set"))
  // A harness's name is a slug, so a session's own id may hold the key's separator.
  private val session =
    Harness.of("claude-code").flatMap(SessionId.of(_, "a:b")).fold(sys.error, identity)

  private val all: Vector[Target] = Vector(
    Target.Raw(p),
    Target.Superseded(p),
    Target.Quiet(p),
    Target.PostRuns(digest, 2, seven),
    Target.Restarted(digest),
    Target.Disabled(digest),
    Target.Document(version),
    Target.Schedule(schedule),
    Target.File(file),
    Target.PieceSet(set),
    Target.SessionEvents(session),
    Target.Session(session)
  )

  val tests = Tests {
    // The stored form: tombstones already written must keep reading after any change.
    test("each target is stored as its kind's name and a key") {
      all.map(t => (t.kind.name, Target.key(t))) ==> Vector(
        ("raw", "c1:3"),
        ("superseded", "c1:3"),
        ("quiet", "c1:3"),
        ("post-runs", "digest:2:7"),
        ("restarted", "digest"),
        ("disabled", "digest"),
        ("document", "42"),
        ("schedule", "declared:plugin:digest:daily"),
        ("file", "41"),
        ("piece-set", "43"),
        ("session-events", "claude-code:a:b"),
        ("session", "claude-code:a:b")
      )
    }

    test("a stored target reads back as itself, and a malformed key says why") {
      all.map(t =>
        Target.Kind.named(t.kind.name).toRight("kind").flatMap(Target.read(_, Target.key(t)))
      ) ==>
        all.map(Right(_))
      Target.read(Target.Kind.Raw, "c1:0") ==> Left("raw c1:0: not a period")
      Target.read(Target.Kind.PostRuns, "digest:2") ==> Left("post-runs digest:2: not a run's")
      Target.read(Target.Kind.Document, "0") ==> Left("document 0: not a version")
      Target.read(Target.Kind.Schedule, "asked:7") ==>
        Left("schedule asked:7: asked:7 is no schedule's id")
      Target.read(Target.Kind.File, "f41") ==> Left("file f41: not a file's handle")
      Target.read(Target.Kind.PieceSet, "0") ==> Left("piece-set 0: not a set's number")
      Target.read(Target.Kind.Session, "Claude:a") ==>
        Left(
          "session Claude:a: a harness's name holds 'C', not a lowercase letter, a digit or '-': Claude"
        )
      Target.read(Target.Kind.SessionEvents, "grit") ==> Left(
        "session-events grit: not a session's"
      )
      Target.Kind.named("purge") ==> None
    }

    test(
      "raw, posting runs, restarts and schedules keep the raw window; documents and piece sets their plugin's; files and sessions none, past their own expiry; the rest the ledger's"
    ) {
      val w = Windows.of(1.hour, 2.days, 9.days).fold(sys.error, identity)
      Target.Kind.values.toVector.map(k => (k, k.retention(w))) ==> Vector(
        (Target.Kind.Raw, Retention.For(2.days)),
        (Target.Kind.Superseded, Retention.For(9.days)),
        (Target.Kind.Quiet, Retention.For(9.days)),
        (Target.Kind.PostRuns, Retention.For(2.days)),
        (Target.Kind.Restarted, Retention.For(2.days)),
        (Target.Kind.Disabled, Retention.For(9.days)),
        (Target.Kind.Document, Retention.Declared),
        (Target.Kind.Schedule, Retention.For(2.days)),
        (Target.Kind.File, Retention.For(Duration.Zero)),
        (Target.Kind.PieceSet, Retention.Declared),
        (Target.Kind.SessionEvents, Retention.For(Duration.Zero)),
        (Target.Kind.Session, Retention.For(Duration.Zero))
      )
    }
  }
}
