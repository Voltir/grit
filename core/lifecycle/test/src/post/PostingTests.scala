package grit.lifecycle.post

import java.time.Instant

import grit.core.durable.InMemoryDurable
import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, PluginName}
import grit.core.message.Message
import grit.core.period.{CloseOrdinal, CloseReason, TestClosings}
import grit.core.plugin.{CacheDocs, InMemoryPlugins, Plugin, PostRef}
import grit.core.retention.{Target, Tombstone}
import grit.core.store.{
  ClosedPeriod,
  Entry,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryTombstones,
  Jot,
  Payload,
  StoreError,
  Tx
}
import grit.dbos.sql.TestTx
import grit.lifecycle.close.CloseFixtures.SetClock

import utest.*

object PostingTests extends TestSuite {

  private val c = ConversationId("c1")

  private def name(s: String): PluginName =
    PluginName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def ordinal(n: Long): CloseOrdinal =
    CloseOrdinal.of(n).getOrElse(throw new java.lang.AssertionError(n))

  /** Keeps each closed period's prose under its close ordinal, as of `version`; refuses the
    * periods whose prose is in `refused`.
    */
  private final class Recorder(
      val name: PluginName,
      val version: Int,
      refused: Set[String] = Set.empty
  ) extends Plugin {
    def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit] =
      if (refused.contains(closed.closing.flows.prose))
        Left(StoreError.Invalid(s"refused ${closed.closing.flows.prose}"))
      else
        docs.put(
          CloseOrdinal.value(closed.order).toString,
          ujson.Str(s"v$version ${closed.closing.flows.prose}")
        )
  }

  /** Writes straight through to the in-memory stores, never rolled back. */
  private final class FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  /** `n` periods of `c`, each one turn, closed in order with prose `p1`, `p2`, ... */
  private final class World(n: Int) {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val tombstones = new InMemoryTombstones
    val plugins = new InMemoryPlugins(tombstones)
    locally {
      given Tx = TestTx.fake
      for (i <- 1 to n) {
        val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
        periods.openFor(c, next.turnSeq, Instant.EPOCH)
        entries.insert(
          Entry(
            EntryId(s"m$i"),
            c,
            next.turnSeq,
            None,
            next.seq,
            Payload.Message(Message.User("hi")),
            Instant.EPOCH
          )
        )
        val closing = TestClosings.prose(s"p$i")
        periods.seal(
          CloseRef(
            PeriodRef(c, PeriodSeq.of(i.toLong).getOrElse(sys.error("seq"))),
            next.turnSeq,
            Instant.EPOCH
          ),
          CloseReason.Lapsed,
          closing,
          Instant.EPOCH
        )
      }
    }
    def run(ps: Vector[Plugin], ref: PostRef): String =
      new InMemoryDurable().run(ref.workflowId)(
        Posting.body(
          ps,
          PostEnv(
            periods,
            plugins.cursors,
            plugins.posting,
            tombstones,
            new FakeJot,
            new SetClock(Instant.EPOCH)
          )
        )
      )
    def cursor(p: Plugin): CloseOrdinal =
      plugins.cursors
        .start(p.name, p.version, Instant.EPOCH)(using TestTx.fake)
        .getOrElse(sys.error("cursor"))
    def docs(p: Plugin): Vector[(String, ujson.Value)] =
      plugins.docs(p.name).newest("", 1000)(using TestTx.fake).getOrElse(Vector.empty).reverse
  }

  val tests = Tests {
    test("each closed period is posted once, in close order, the cursor moved with it") {
      val w = new World(3)
      val digest = new Recorder(name("digest"), 1)
      w.run(Vector(digest), PostRef(digest.name, 1, CloseOrdinal.Start, 0)) ==> "posted 3"
      w.docs(digest) ==> Vector(
        "1" -> ujson.Str("v1 p1"),
        "2" -> ujson.Str("v1 p2"),
        "3" -> ujson.Str("v1 p3")
      )
      w.cursor(digest) ==> ordinal(3)
      w.run(Vector(digest), PostRef(digest.name, 1, ordinal(3), 0)) ==> "posted 0"
    }

    test(
      "a run that moves the cursor marks the runs from its start, and one that moves none, none"
    ) {
      val w = new World(3)
      val digest = new Recorder(name("digest"), 1)
      val from = PostRef(digest.name, 1, CloseOrdinal.Start, 0)
      w.run(Vector(digest), from) ==> "posted 3"
      w.tombstones.pending ==>
        Vector(Tombstone(Target.PostRuns(digest.name, 1, CloseOrdinal.Start), Instant.EPOCH))
      w.run(Vector(digest), PostRef(digest.name, 1, ordinal(3), 0)) ==> "posted 0"
      w.tombstones.pending.map(_.target) ==>
        Vector(Target.PostRuns(digest.name, 1, CloseOrdinal.Start))
    }

    test("a Left ends the run, the cursor before the period refused") {
      val w = new World(3)
      val picky = new Recorder(name("picky"), 1, refused = Set("p2"))
      w.run(
        Vector(picky),
        PostRef(picky.name, 1, CloseOrdinal.Start, 0)
      ) ==> "posted 1; stopped: refused p2"
      w.cursor(picky) ==> ordinal(1)
    }

    test("a run posts at most MaxPerRun periods, and the next run goes on from its cursor") {
      val w = new World(Posting.MaxPerRun + 1)
      val digest = new Recorder(name("digest"), 1)
      w.run(Vector(digest), PostRef(digest.name, 1, CloseOrdinal.Start, 0)) ==>
        s"posted ${Posting.MaxPerRun}; more to come"
      w.run(Vector(digest), PostRef(digest.name, 1, w.cursor(digest), 0)) ==> "posted 1"
    }

    test("a new version posts every closed period again") {
      val w = new World(2)
      w.run(
        Vector(new Recorder(name("digest"), 1)),
        PostRef(name("digest"), 1, CloseOrdinal.Start, 0)
      )
      val v2 = new Recorder(name("digest"), 2)
      w.run(Vector(v2), PostRef(v2.name, 2, CloseOrdinal.Start, 0)) ==> "posted 2"
      w.docs(v2) ==> Vector("1" -> ujson.Str("v2 p1"), "2" -> ujson.Str("v2 p2"))
    }

    test("a run of a plugin not enabled, or of another version, posts nothing") {
      val w = new World(1)
      val digest = new Recorder(name("digest"), 2)
      w.run(
        Vector(digest),
        PostRef(name("wiki"), 1, CloseOrdinal.Start, 0)
      ) ==> "no plugin wiki at version 1"
      w.run(
        Vector(digest),
        PostRef(name("digest"), 1, CloseOrdinal.Start, 0)
      ) ==> "no plugin digest at version 1"
      w.docs(digest) ==> Vector()
    }
  }
}
