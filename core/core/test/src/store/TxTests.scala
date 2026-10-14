package grit.core.store

import grit.core.model.{Grade, ModelId, Upstream}
import grit.core.place.{Place, Service}
import grit.core.visibility.*
import grit.dbos.sql.TestTx

import utest.*
import TestLabels.{compartment, place}

/** What a transaction may write or send outside grit, and read from a source, under the
  * deployment's labels (ADR 0031): no write down, no read up.
  */
object TxTests extends TestSuite {

  private val trial = compartment("trial")
  private val finance = compartment("finance")
  private val ops = compartment("ops")

  private def service(name: String): Service =
    Service.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  private val github = service("github")
  private val jira = service("jira")
  private val wiki = service("wiki")

  private val flash = ModelId.of("vendor/flash").getOrElse(throw new java.lang.AssertionError())
  private def upstream(slug: String): Upstream =
    Upstream.of(slug).getOrElse(throw new java.lang.AssertionError(slug))
  private val flashAtA = Party.Model(flash, upstream("a"))
  private val flashAtB = Party.Model(flash, upstream("b"))

  private val confidentialTrial = Label.at(Level.Confidential, trial)
  private val internal = Label.at(Level.Internal)
  private val internalFinance = Label.at(Level.Internal, finance)
  private val unmapped = Label.at(Level.Public, Compartment.Unmapped)

  private val room = place("slack:T/C-trial")
  private val sameLevel = place("slack:T/C-trial-two")
  private val above = place("slack:T/C-board")
  private val public = place("slack:T/C-general")
  private val undeclared = place("slack:T/C-ops")
  private val unplaced = place("slack:T/C-new")

  private val declared: Vector[(Place, Label)] = Vector(
    room -> confidentialTrial,
    sameLevel -> confidentialTrial,
    above -> Label.at(Level.Restricted, trial, finance),
    public -> Label.Public,
    github.place -> internal,
    jira.place -> Label.at(Level.Confidential)
  )

  /* The deployment's rooms: a declared table, but one place returns a compartment the
   * deployment does not declare, and anything else is unplaced. */
  private val rooms: Labeller[Room] = new Labeller[Room] {
    def label(item: Room): Labelled =
      if (item.place == undeclared) Labelled.Mapped(Label.at(Level.Internal, ops))
      else
        declared
          .collectFirst { case (p, l) if p == item.place => Labelled.Mapped(l) }
          .getOrElse(Labelled.Unmapped(Label.Public))
    def requires: Vector[Compartment] = Vector(trial, finance)
  }

  private val visibility: Visibility =
    (for {
      compartments <- Compartments.of(Vector(trial, finance)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector.empty,
          Vector.empty,
          Vector(
            Cleared(Party.Service(github), confidentialTrial),
            Cleared(flashAtA, confidentialTrial)
          )
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  private def at(clearance: Clearance): Tx = TestTx.fake(clearance, visibility)

  /** In [[room]], for an asker cleared at `asker`. */
  private def inRoom(asker: Label): Tx = at(Clearance.inRoom(room, confidentialTrial, asker))

  val tests = Tests {
    test("in a confidential trial room, a place is written to only when its label is as high") {
      given Tx = inRoom(Label.Public)
      Vector(public, sameLevel, above).map(Tx.writesTo(_, Tx.classification)) ==>
        Vector(false, true, true)
    }

    test("an unplaced destination is never written to, whatever the floor") {
      val floors = Vector(Clearance.of(Label.Public), Clearance.of(unmapped))
      val written =
        (floors.map(at) :+ inRoom(Label.Public)).map { tx =>
          given Tx = tx
          Tx.writesTo(unplaced, Tx.classification)
        }
      written ==> Vector(false, false, false)
      Tx.writable(unplaced)(using at(Clearance.of(unmapped))) ==> None
    }

    test("a direct message's room is never written to, though the labeller maps every place") {
      val mapsAll = Visibility
        .of(
          Compartments.of(Vector.empty).fold(c => throw new java.lang.AssertionError(c), identity),
          RoomLabels
            .of(Vector.empty, Labelled.Mapped(internal))
            .fold(p => throw new java.lang.AssertionError(p.written), identity),
          Vector.empty,
          Vector.empty
        )
        .fold(r => throw new java.lang.AssertionError(r.toString), identity)
      given Tx = TestTx.fake(Clearance.of(Label.Public), mapsAll)
      (Tx.writable(place("direct:slack/T/U")), Tx.writable(place("slack:T/C"))) ==>
        (None, Some(internal))
    }

    test("a place labelled with a compartment the deployment does not declare is not writable") {
      given Tx = at(Clearance.of(Label.Public))
      (Tx.writable(undeclared), Tx.writable(public)) ==> (None, Some(Label.Public))
    }

    test("labels in force") {
      /* Declared at its own place, an open room's default, and everywhere else `otherwise`. */
      val labels = RoomLabels
        .of(
          Vector(room -> confidentialTrial),
          Labelled.Mapped(Label.Public),
          Some(Labelled.Mapped(internal))
        )
        .fold(p => throw new java.lang.AssertionError(p.written), identity)
      val inForce = Visibility
        .of(
          Compartments
            .of(Vector(trial, finance))
            .fold(c => throw new java.lang.AssertionError(c), identity),
          labels,
          Vector.empty,
          Vector.empty
        )
        .fold(r => throw new java.lang.AssertionError(r.toString), identity)
      def kept(
          access: Option[RoomAccess] = None,
          label: Option[Label] = None,
          quiet: Boolean = false,
          baseline: Option[Grade] = None
      ) =
        Recorded.Kept(access, label, quiet, baseline)
      def under(rooms: (Place, Recorded.Kept)*): Tx =
        TestTx.fake(Clearance.of(Label.Public), inForce, Recorded(rooms.toMap, Map.empty))

      test(
        "a label set through grit is in force over the one declared at its place and over the access default; a direct message's room stays top"
      ) {
        given Tx = under(
          room -> kept(label = Some(internalFinance)),
          public -> kept(access = Some(RoomAccess.Open), label = Some(confidentialTrial)),
          place("direct:slack/T/U") -> kept(label = Some(Label.Public))
        )
        (Tx.roomLabel(room), Tx.roomLabel(public), Tx.roomLabel(place("direct:slack/T/U"))) ==>
          (
            internalFinance,
            confidentialTrial,
            Label.at(Level.Restricted, trial, finance, Compartment.Unmapped)
          )
        (Tx.writable(room), Tx.writable(public)) ==> (
          Some(internalFinance),
          Some(confidentialTrial)
        )
      }

      test(
        "a set label holding a compartment not declared is in force at unmapped, and so not writable"
      ) {
        given Tx = under(room -> kept(label = Some(Label.at(Level.Internal, ops))))
        (Tx.roomLabel(room), Tx.writable(room)) ==>
          (Label.at(Level.Internal, Compartment.Unmapped), None)
      }

      test(
        "with nothing set or declared at its place, a room's reported access picks its default: open for open, unmapped for invited, otherwise the declared map"
      ) {
        given Tx = under(
          sameLevel -> kept(access = Some(RoomAccess.Open)),
          above -> kept(access = Some(RoomAccess.Invited)),
          room -> kept(access = Some(RoomAccess.Invited))
        )
        (
          Vector(sameLevel, above, unplaced, room).map(Tx.roomLabel),
          Vector(sameLevel, above, unplaced).map(Tx.access)
        ) ==> (
          Vector(internal, unmapped, Label.Public, confidentialTrial),
          Vector(Some(RoomAccess.Open), Some(RoomAccess.Invited), None)
        )
      }

      test(
        "a room's default is its label in force but for one set through grit, and only a set label is said to be set"
      ) {
        given Tx = under(
          room -> kept(label = Some(internalFinance)),
          sameLevel -> kept(access = Some(RoomAccess.Invited), label = Some(internal)),
          public -> kept(access = Some(RoomAccess.Open))
        )
        val direct = place("direct:slack/T/U")
        (
          Vector(room, sameLevel, public, unplaced, direct).map(Tx.defaultLabel),
          Vector(room, sameLevel, public, unplaced, direct).map(Tx.labelSet)
        ) ==> (
          Vector(
            confidentialTrial,
            unmapped,
            internal,
            Label.Public,
            Label.at(Level.Restricted, trial, finance, Compartment.Unmapped)
          ),
          Vector(true, true, false, false, false)
        )
      }

      test(
        "a room's baseline is the one recorded for it; none for a room recorded without one or with nothing recorded"
      ) {
        given Tx =
          under(room -> kept(baseline = Some(Grade.Strong)), sameLevel -> kept(quiet = true))
        Vector(room, sameLevel, public).map(Tx.baseline) ==> Vector(Some(Grade.Strong), None, None)
      }

      test(
        "a room is decided when a person set its label, quiet or baseline, and not for its access"
      ) {
        Vector(
          kept(access = Some(RoomAccess.Open)),
          kept(label = Some(internalFinance)),
          kept(quiet = true),
          kept(baseline = Some(Grade.Light))
        ).map(_.decided) ==> Vector(false, true, true, true)
      }

      test("a quiet room is never writable, though its label is; a room not quiet is") {
        given Tx = under(
          room -> kept(quiet = true),
          sameLevel -> kept(label = Some(confidentialTrial), quiet = true),
          public -> kept()
        )
        (
          Vector(room, sameLevel, public).map(Tx.quiet),
          Vector(room, sameLevel, public).map(Tx.writable)
        ) ==> (Vector(true, true, false), Vector(None, None, Some(Label.Public)))
      }

      test(
        "a source is read from at the label in force: one raised through grit above the reader is not read"
      ) {
        given Tx = TestTx.fake(
          Clearance.inRoom(room, confidentialTrial, internal),
          inForce,
          Recorded(Map(public -> kept(label = Some(Label.at(Level.Restricted)))), Map.empty)
        )
        Tx.readsFrom(public) ==> false
      }
    }

    test("a source is read from up to the room's label in its own room, elsewhere up to the meet") {
      given Tx = inRoom(internal)
      Vector(room, github.place, jira.place, unplaced).map(Tx.readsFrom) ==>
        Vector(true, true, false, false)
    }

    test("a service is sent to only when what it is cleared for is as high as the floor") {
      val floors = Vector(confidentialTrial, internal, internalFinance, Label.Public)
      // In a room at each floor, asked by someone cleared for nothing: the floor, not the read.
      val sends = floors.map { floor =>
        val tx = at(Clearance.inRoom(room, floor, Label.Public))
        given Tx = tx
        (Tx.sendsTo(github, Tx.classification), Tx.sendsTo(wiki, Tx.classification))
      }
      sends ==> Vector((true, false), (true, false), (false, false), (true, true))
    }

    test("a routed classification is held at its label, and never below what is known now") {
      val bounds = Vector(Label.Public, internal, confidentialTrial)
      // Known now: an internal floor, as after a direct message's clearance fell or rose.
      given Tx = at(Clearance.of(internal))
      bounds.map(b => Tx.locked(b).label) ==> Vector(internal, internal, confidentialTrial)
    }

    test(
      "a send and a write out are checked at the classification they are given, not at the floor"
    ) {
      // A public floor, sending what a turn routed at the confidential trial label read.
      given Tx = at(Clearance.of(Label.Public))
      val routed = Tx.locked(confidentialTrial)
      val atRouted = (
        Tx.sendsTo(wiki, routed),
        Tx.sendsTo(github, routed),
        Tx.writesTo(public, routed),
        Tx.writesTo(sameLevel, routed)
      )
      atRouted ==> (false, true, false, true)
    }

    test(
      "a model is sent to only when its pair's clearance is as high as the floor, the " +
        "transaction's classification"
    ) {
      val floors = Vector(confidentialTrial, internal, internalFinance, Label.Public)
      val sends = floors.map { floor =>
        val tx = at(Clearance.inRoom(room, floor, Label.Public))
        (
          Tx.classification(using tx).label,
          Tx.clears(flashAtA, Tx.classification(using tx))(using tx),
          Tx.clears(flashAtB, Tx.classification(using tx))(using tx)
        )
      }
      sends ==> Vector(
        (confidentialTrial, true, false),
        (internal, true, false),
        (internalFinance, false, false),
        (Label.Public, true, true)
      )
    }
  }
}
