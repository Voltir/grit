package grit.lifecycle.shadow

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{EntryId, ShadowName, TriageRef, WorkflowId}
import grit.core.stitch.Tuning
import grit.core.triage.{InMemoryTriageShadows, Shadowed}
import grit.dbos.sql.TestTx
import grit.lifecycle.triage.{TriageFixtures, TriageQuestion}

/** The shadow's test world: triage's ([[TriageFixtures.World]]), with what each variant
  * made of its messages kept beside their tags.
  */
object ShadowFixtures {

  def named(s: String): ShadowName =
    ShadowName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  val Words: ShadowName = named("words")

  /** A variant's own words: the shipped ones with the kind question put otherwise. */
  val Reworded: TriageQuestion.Wording =
    TriageQuestion.Wording.Shipped.copy(kind = "What kind of message is new_message?")

  /** A clock stopped at `time` whose milliseconds go on `step` each time they are read. */
  final class Ticking(time: Instant, step: Long) extends Clock {
    @caps.unsafe.untrackedCaptures
    private var read = 0L
    def now(): Instant = time
    def millis(): Long = { read += step; read }
    def sleep(duration: FiniteDuration): Unit = ()
  }

  final class World {
    val triaged = new TriageFixtures.World
    val shadows = new InMemoryTriageShadows(triaged.entries, triaged.triage)

    /** `text` heard from `name` at `minutes`; its triage. */
    def hear(text: String, name: String, minutes: Long): TriageRef =
      triaged.hear(text, name, minutes)

    /** The heard message's entry. */
    def entry(t: TriageRef): Option[EntryId] =
      triaged.entries
        .list(t.period.conversationId)(using TestTx.fake)
        .getOrElse(Vector.empty)
        .find(_.turnSeq == t.turn)
        .map(_.id)

    /** What `name` made of the heard message `t`, if it kept anything. */
    def kept(t: TriageRef, name: ShadowName): Option[Shadowed] =
      entry(t).flatMap(e =>
        shadows.of(name, Vector(e))(using TestTx.fake).toOption.flatMap(_.get(e))
      )

    /** The shadow's body over this world: `classifier` asks variant [[Words]] in
      * [[Reworded]] of model `jev-variant`, at `minutes`, each call taking 250 ms.
      */
    def body(classifier: Classifier^, minutes: Long)(id: WorkflowId)(using Durable^): String =
      Shadow.body(
        ShadowEnv(
          triaged.reads,
          triaged.rooms,
          shadows,
          Map(Words -> ShadowAsking(Reworded, "jev-variant", classifier)),
          TriageFixtures.FakeDb,
          new Ticking(TriageFixtures.at(minutes), 250),
          Tuning.Default
        )
      )(id)
  }
}
