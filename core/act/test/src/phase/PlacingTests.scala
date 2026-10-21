package grit.act.phase

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.{Clock, SetClock}
import grit.core.durable.{Flows, InMemoryDurable, Journaled, StepName}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.stitch.{Opening, Placements}
import grit.core.store.{
  Db,
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.{Integrity, Label, Subject}
import grit.dbos.sql.TestTx

import utest.*

/** The wait for a message's placement and the quiet after it. */
object PlacingTests extends TestSuite {

  private val Said = Instant.parse("2026-10-08T12:00:00Z")
  private val Quiet = 10.seconds

  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Every placement ends at once, placed. */
  private object Placed extends Placements {
    def awaited(opening: Opening): Either[String, String] = Right("placed")
    def awaitedWithin(
        opening: Opening,
        within: FiniteDuration,
        clock: Clock^
    ): Either[Placements.Unplaced, String] = Right("placed")
  }

  private given Journaled[Either[String, Option[String]]] = Journaled.json(
    _.fold(
      why => ujson.Obj("unplaced" -> why),
      p => ujson.Obj("placed" -> p.fold(ujson.Null)(ujson.Str(_)))
    ),
    v =>
      v.objOpt
        .flatMap(o =>
          o.get("unplaced")
            .flatMap(_.strOpt)
            .map(Left(_))
            .orElse(o.get("placed").map(p => Right(p.strOpt)))
        )
        .toRight(s"not a placing: $v")
  )

  private val placing = Placing.placed[TurnRef, String](
    StepName("stitched"),
    identity,
    _ => Subject.Public,
    _.toString,
    Quiet
  )

  /** How long a run of [[placing]] waits on a clock reading `now`, over a message said at
    * [[Said]].
    */
  private def waited(now: Instant): FiniteDuration = {
    val entries = new InMemoryEntryStore
    val conversations = new InMemoryConversationStore(Some(entries))
    given Tx = TestTx.fake
    val turn = (for {
      c <- conversations.findOrCreate(Origin.Task("placing", "c"), Account.Local, Label.Public)
      n <- entries.lockNext(c.id)
      _ <- entries.insert(
        Entry(
          EntryId("h"),
          c.id,
          n.turnSeq,
          None,
          n.seq,
          Payload.Heard("hm"),
          Said,
          Integrity.Member
        )
      )
    } yield TurnRef(c.id, n.turnSeq)).fold(e => throw new java.lang.AssertionError(s"$e"), identity)
    val clock = new SetClock(now)
    val env = Placing.Env(entries, conversations, FakeDb, Placed, clock)
    val _ = new InMemoryDurable().run(WorkflowId("w"))(_ => Flows.run(placing, env, turn).toString)
    (clock.at.toEpochMilli - now.toEpochMilli).millis
  }

  val tests = Tests {
    test(
      "a placing waits Quiet after the message was said; run again within it, only what is left"
    ) {
      Vector(Said, Said.plusSeconds(4), Said.plusSeconds(12)).map(waited) ==>
        Vector(10.seconds, 6.seconds, Duration.Zero)
    }
  }
}
