package grit.core.route

import grit.core.model.*
import grit.core.store.{Db, Reads, StoreError, Tx}
import grit.core.visibility.*
import grit.dbos.sql.TestTx

import utest.*

/** One routed read per site (ADR 0037): what it was routed for, chosen from and bounded by. */
object RoutedTests extends TestSuite {

  private def model(id: String): ModelId =
    ModelId.of(id).getOrElse(throw new java.lang.AssertionError(id))
  private def upstream(slug: String): Upstream =
    Upstream.of(slug).getOrElse(throw new java.lang.AssertionError(slug))

  private val internal = Label.at(Level.Internal)
  private val restricted = Label.at(Level.Restricted)

  private val open = ModelRef(model("vendor/open"), None)
  private val flash = ModelRef(model("vendor/flash"), Some(upstream("fireworks")))
  private val strong = ModelRef(model("vendor/strong"), Some(upstream("anthropic")))

  private def party(ref: ModelRef): Party =
    Party.of(ref).getOrElse(throw new java.lang.AssertionError(ref.toString))

  // The open pair first, then flash, then the strong pair.
  private val pins: Pins = {
    val ladder = Ladder
      .of(
        Vector(open, flash, strong).map(r => Rung(Grade.Standard, Assignment(r, 100, None)))
      )
      .getOrElse(throw new java.lang.AssertionError())
    Catalog.of(Policy(ladder, ladder, ladder, ladder), Vector.empty).pin.turn
  }

  /** Flash cleared for internal, the strong pair for restricted. */
  private val visibility: Visibility = {
    val trial = Compartment.of("trial").getOrElse(throw new java.lang.AssertionError())
    val rooms: Labeller[Room] = new Labeller[Room] {
      def label(item: Room): Labelled = Labelled.Unmapped(Label.Public)
      def requires: Vector[Compartment] = Vector.empty
    }
    (for {
      compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector.empty,
          Vector.empty,
          Vector(Cleared(party(flash), internal), Cleared(party(strong), restricted))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
  }

  /** Reads at `floor`, under [[visibility]], counting each body it runs; every read fails with
    * `fails` when given.
    */
  private final class At(floor: Label, fails: Option[StoreError] = None) extends Db {
    @caps.unsafe.untrackedCaptures
    var ran = 0
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      fails.toLeft(()).flatMap { _ =>
        ran = ran + 1
        body(using TestTx.fake(Clearance.of(floor), visibility))
      }
  }

  /** Reads at internal first, at restricted after, under [[visibility]]. */
  private final class Rising extends Db {
    @caps.unsafe.untrackedCaptures
    var reads = 0
    def read[A](s: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = {
      val floor = if (reads == 0) internal else restricted
      reads = reads + 1
      body(using TestTx.fake(Clearance.of(floor), visibility))
    }
  }

  private val subject = Subject.Turn(
    grit.core.id.TurnRef(grit.core.id.ConversationId("c"), grit.core.id.TurnSeq.First)
  )

  private def routedAt(floor: Label): Routed =
    new At(floor)
      .read(subject)(Right(Routed.of))
      .fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("a routing is the classification its transaction knew, cleared as it read the parties") {
      val r = routedAt(internal)
      (
        r.classification.label,
        r.cleared(pins).map(_.all.map(_.assignment.ref)),
        Vector(flash, strong).map(f => r.clears(party(f)))
      ) ==> (internal, Some(Vector(flash, strong)), Vector(true, true))
      routedAt(Label.at(Level.Confidential)).cleared(pins).map(_.all.map(_.assignment.ref)) ==>
        Some(Vector(strong))
    }

    test("a read bounded by a routing that knows more than it was routed for runs nothing") {
      val r = routedAt(internal)
      val (above, at) = (new At(restricted), new At(internal))
      @caps.unsafe.untrackedCaptures
      var bodies = 0
      val reads = Vector(
        r.bound(above).read(subject) { bodies = bodies + 1; Right(()) },
        r.bound(above.as(subject)).read { bodies = bodies + 1; Right(()) },
        r.bound(at).read(subject) { bodies = bodies + 1; Right(()) }
      )
      (reads.map(_.left.map(_.toString.contains("ask again"))), bodies) ==>
        (Vector(Left(true), Left(true), Right(())), 1)
    }

    test(
      "reading hands its body the routing of one read and reads bounded by it; a failed first read runs no body"
    ) {
      val db = new At(internal)
      val routedThenRead = Routed.reading(db, subject)((r, reads) =>
        (r.classification.label, reads.read(Right(Tx.floor(summon[Tx]))).map(_ == internal))
      )
      routedThenRead ==> Right((internal, Right(true)))
      db.ran ==> 2
      val failed = new At(internal, Some(StoreError.Invalid("down")))
      @caps.unsafe.untrackedCaptures
      var bodies = 0
      Routed.reading(failed, subject)((_, _) => bodies = bodies + 1) ==>
        Left(StoreError.Invalid("down"))
      Routed.reading(failed.as(subject))((_, _) => bodies = bodies + 1) ==>
        Left(StoreError.Invalid("down"))
      bodies ==> 0
    }

    test("reading bounds the body's reads by the routing it read first, through a db or reads") {
      // Each reads at internal first, then at restricted: what it knows rose after routing.
      val (viaDb, viaReads) = (new Rising, new Rising)
      val read = Vector(
        Routed.reading(viaDb, subject)((_, reads) => reads.read(Right(()))),
        Routed.reading(viaReads.as(subject))((_, reads) => reads.read(Right(())))
      )
      read.map(_.map(_.left.exists(_.toString.contains("ask again")))) ==>
        Vector(Right(true), Right(true))
    }
  }
}
