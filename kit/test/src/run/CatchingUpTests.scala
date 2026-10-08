package grit.kit.run

import java.time.{Instant, LocalDate}

import scala.jdk.CollectionConverters.*

import grit.core.clock.{Clock, SetClock}
import grit.core.edge.{
  Attesting,
  CatchUp,
  EdgeRefusal,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  Unheard,
  Variable
}
import grit.core.id.{
  CloseRef,
  ConversationId,
  EdgeName,
  PeriodRef,
  PeriodSeq,
  PluginName,
  SettleRef,
  TurnSeq
}
import grit.core.inbox.InMemoryInbox
import grit.core.period.CloseOrdinal
import grit.core.place.{Namespace, Place}
import grit.core.plugin.PostRef
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, StoreError, Tx}
import grit.core.visibility.{
  Compartments,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Subject,
  TestLabels,
  Visibility
}
import grit.dbos.engine.Swept
import grit.dbos.sql.TestTx

import utest.*

/** What a catch-up decides without an edge or the database. */
object CatchingUpTests extends TestSuite {

  private val close = CloseRef(
    PeriodRef(ConversationId("c1"), PeriodSeq.First),
    TurnSeq(0),
    Instant.parse("2026-09-27T10:00:00Z")
  )

  private val settle = SettleRef(close.period, TurnSeq(0), close.due)

  private val post = PostRef(
    PluginName.of("digest").getOrElse(throw new java.lang.AssertionError("a plugin name")),
    1,
    CloseOrdinal.of(1).getOrElse(throw new java.lang.AssertionError("an ordinal")),
    0
  )

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private val budget = Budget(java.time.ZoneOffset.UTC, None)
  private val inbox = InMemoryInbox.fresh(budget)
  private val stores =
    EdgeStores(
      inbox,
      inbox.principals,
      new InMemoryDeliveries,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      FakeJot,
      new InMemoryEdges,
      new Attesting(InMemoryVoucher.none(), FakeJot, _ => ())
    )

  private def room(channel: String): Place = Place.under(Namespace.Slack, Vector("T1", channel))

  /** One thread of two messages in a trial channel, and one of one in a general one. */
  private val trial = Unheard("#trial (C1)", room("C1"), Vector(Vector(10, 20)))
  private val general = Unheard("#general (C2)", room("C2"), Vector(Vector(30)))

  private val Confidential = Label.at(Level.Confidential, TestLabels.trial)

  /** Trial's room labelled confidential in trial, every other room internal. */
  private val Declared: Visibility =
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      rooms <- RoomLabels
        .of(Vector(room("C1") -> Confidential), Labelled.Mapped(Label.at(Level.Internal)))
        .left
        .map(_.toString)
      v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** What a catch-up of `unheard`, read since the epoch, says under `visibility`, declined. */
  private def said(visibility: Visibility): Vector[String] =
    declined(visibility, new SetClock(Instant.EPOCH))._1

  /** As [[declined]], each room's label read by `labelled`; and what the catch-up returned. */
  private def declined(
      visibility: Visibility,
      clock: SetClock
  ): (Vector[String], Vector[Instant]) = {
    val (lines, opened, _) = labelledBy(
      place => Right(Tx.roomLabel(place)(using TestTx.inForce(visibility))),
      clock
    )
    (lines, opened)
  }

  /** What a catch-up of `unheard`, read since the epoch, says with each room's label read by
    * `labelled`, declined, its clock `clock`; the time its source was opened at; and what it
    * returned.
    */
  private def labelledBy(
      labelled: Place => Either[String, Label],
      clock: SetClock
  ): (Vector[String], Vector[Instant], Either[KitFailure, Unit]) = {
    val lines = Vector.newBuilder[String]
    val opened = Vector.newBuilder[Instant]
    val catchUp = new CatchUp {
      def name: EdgeName = EdgeName("test")
      def needs: Vector[Variable] = Vector.empty
      def open(
          stores: EdgeStores^,
          env: Map[String, String],
          clock: Clock^,
          log: String => Unit
      ): Either[EdgeRefusal, CatchUp.Open^{stores, clock, log, caps.any}] = {
        opened += clock.now()
        Right(new CatchUp.Open {
          def since: Instant = Instant.EPOCH
          def unheard: Vector[Unheard] = Vector(trial, general)
          def hear(): Either[EdgeRefusal, Unit] = Right(())
          def close(): Unit = ()
        })
      }
    }
    val returned = CatchingUp.run(
      catchUp,
      stores,
      Map.empty,
      budget,
      labelled,
      clock,
      () => Right(BigDecimal(0)),
      () => Right(Swept.nothing),
      () => Right(0),
      _ => false,
      lines += _
    )
    (lines.result(), opened.result(), returned)
  }

  private val epoch = LocalDate.parse("1970-01-01")

  val tests = Tests {
    test(
      "a backfill names the label each source is heard at, and once, before asking, that a label declared later does not relabel it"
    ) {
      said(Declared) ==> Vector(
        CatchingUp.line(trial.source, Confidential, Estimate.of(trial), epoch),
        CatchingUp.line(general.source, Label.at(Level.Internal), Estimate.of(general), epoch),
        CatchingUp.Relabel,
        "backfill: nothing heard"
      )
    }

    test(
      "a room whose label cannot be read refuses the catch-up before anything is said or heard"
    ) {
      val (lines, _, returned) = labelledBy(
        place => if (place == general.place) Left("down") else Right(Label.Public),
        new SetClock(Instant.EPOCH)
      )
      (lines, returned) ==> (Vector(), Left(KitFailure.Store("a room's label is unread: down")))
    }

    test("a catch-up's source is opened at its clock's time") {
      val at = Instant.parse("2031-02-03T04:05:06Z")
      declined(Visibility.Shipped, new SetClock(at))._2 ==> Vector(at)
    }

    test(
      "drain sweeps until a sweep, each made once no workflow is queued or running, enqueues, asks and posts nothing"
    ) {
      val looks = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      var counts = List(2, 0, 1, 0)
      var sweeps = List(
        Swept(enqueued = Vector(close)),
        Swept(asked = Vector(settle)),
        Swept(posted = Vector(post)),
        Swept.nothing
      )
      val made = CatchingUp.drain(
        () => {
          looks.add("sweep")
          val s = sweeps.headOption.getOrElse(Swept.nothing)
          sweeps = sweeps.drop(1)
          Right(s)
        },
        () => {
          val n = counts.headOption.getOrElse(0)
          counts = counts.drop(1)
          looks.add(s"unfinished $n")
          Right(n)
        },
        () => ()
      )
      (made, looks.asScala.toVector) ==> (
        Right(4),
        Vector(
          "unfinished 2",
          "unfinished 0",
          "sweep",
          "unfinished 1",
          "unfinished 0",
          "sweep",
          "unfinished 0",
          "sweep",
          "unfinished 0",
          "sweep"
        )
      )
    }

    test(
      "a channel's line names it, the label it is heard at, its counts, the day it reads from, and each bound rounded up"
    ) {
      CatchingUp.line(
        "#standup (C123ABC456)",
        Label.at(Level.Confidential, TestLabels.trial),
        Estimate(812, 143, BigDecimal("0.0190001"), BigDecimal("0.2145")),
        LocalDate.parse("2026-09-26")
      ) ==> "backfill #standup (C123ABC456), heard at confidential+trial: 812 messages in 143 threads since 2026-09-26; up to $0.2336 (triage $0.0191, closings up to $0.2145)"
    }
  }
}
