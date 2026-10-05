package grit.core.retention

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, DocumentVersion, PeriodRef, PeriodSeq, PluginName}
import grit.core.period.{CloseOrdinal, Windows}

import utest.*

object TargetTests extends TestSuite {

  private val p = PeriodRef(ConversationId("c1"), PeriodSeq.of(3).getOrElse(PeriodSeq.First))
  private val digest = PluginName.of("digest").fold(sys.error, identity)
  private val seven = CloseOrdinal.of(7).getOrElse(CloseOrdinal.Start)
  private val version = DocumentVersion.of(42).getOrElse(sys.error("version"))

  private val all: Vector[Target] = Vector(
    Target.Raw(p),
    Target.Superseded(p),
    Target.Quiet(p),
    Target.PostRuns(digest, 2, seven),
    Target.Restarted(digest),
    Target.Disabled(digest),
    Target.Document(version)
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
        ("document", "42")
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
      Target.Kind.named("purge") ==> None
    }

    test(
      "raw, posting runs and restarts keep the raw window; documents their plugin's; the rest the ledger's"
    ) {
      val w = Windows.of(1.hour, 2.days, 9.days).fold(sys.error, identity)
      Target.Kind.values.toVector.map(k => (k, k.retention(w))) ==> Vector(
        (Target.Kind.Raw, Retention.For(2.days)),
        (Target.Kind.Superseded, Retention.For(9.days)),
        (Target.Kind.Quiet, Retention.For(9.days)),
        (Target.Kind.PostRuns, Retention.For(2.days)),
        (Target.Kind.Restarted, Retention.For(2.days)),
        (Target.Kind.Disabled, Retention.For(9.days)),
        (Target.Kind.Document, Retention.Declared)
      )
    }
  }
}
