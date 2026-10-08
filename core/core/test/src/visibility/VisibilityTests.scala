package grit.core.visibility

import grit.core.identity.{Account, Evidence, Held, Principal, Realm, TestAccounts}
import grit.core.place.Service
import grit.core.store.Tx
import grit.dbos.sql.TestTx

import utest.*
import TestLabels.{compartment, group, place}

object VisibilityTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")
  private val compartments =
    Compartments.of(Vector(trial, acme)).fold(c => throw new java.lang.AssertionError(c), identity)

  private val github =
    Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)
  private val jira = Service.of("jira").fold(e => throw new java.lang.AssertionError(e), identity)

  private val ana = TestAccounts.account("test:ana")
  private val bo = TestAccounts.account("test:bo")
  private val anaAtWork = TestAccounts.account("slack:T1/U-ana")
  private val t1 =
    Realm.of("slack", "T1").fold(e => throw new java.lang.AssertionError(e), identity)

  private val groups: Vector[Group] = Vector(
    Group(group("trial-team"), Set(ana)),
    Group(group("acme-team"), Set(ana, bo)),
    Group(group("ops"), Set(anaAtWork)),
    Group(group("t1-members"), Set.empty, Set(t1))
  )
  private val grants: Vector[Grant] = Vector(
    Grant(group("trial-team"), Label.at(Level.Internal, trial)),
    Grant(group("acme-team"), Label.at(Level.Confidential, acme)),
    Grant(group("ops"), Label.at(Level.Restricted)),
    Grant(group("t1-members"), Label.at(Level.Internal, trial))
  )

  /** A person holding `accounts`, each its home, a full member of its realm when paired `true`. */
  private def person(accounts: (Account, Boolean)*): Principal =
    Principal.Person(
      TestAccounts.principalId(accounts.headOption.fold(ana)(_._1)),
      accounts.map((a, member) => Held(a, Evidence.Home, member)).toSet
    )

  private val visibility =
    Visibility
      .of(compartments, RoomLabels.Public, groups, grants)
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)

  val tests = Tests {
    test(
      "a room's label, a clearance and an explanation are read only on a transaction, never " +
        "from the declaration alone, which knows nothing set through grit"
    ) {
      import scala.compiletime.testing.typeChecks
      assert(
        typeChecks("Visibility.Shipped.trusted(github)"),
        !typeChecks("Visibility.Shipped.roomLabel(place(\"slack:acme/C1\"))"),
        !typeChecks("Visibility.Shipped.cleared(Principal.Grit)"),
        !typeChecks("Visibility.Shipped.explain(None, Label.Public)")
      )
    }

    test("a person is cleared for the join of their groups' grants, and no one else for any") {
      Explained
        .cleared(visibility, person(ana -> false)) ==> Label.at(Level.Confidential, trial, acme)
      Explained.cleared(visibility, person(bo -> false)) ==> Label.at(Level.Confidential, acme)
      Explained.cleared(
        visibility,
        person(TestAccounts.account("test:cy") -> false)
      ) ==> Label.Public
    }

    test("a person holding several accounts is cleared for the join over the groups of each") {
      (
        Explained.cleared(visibility, person(bo -> false, anaAtWork -> false)),
        Explained.cleared(visibility, person(anaAtWork -> false, bo -> false))
      ) ==> (Label.at(Level.Restricted, acme), Label.at(Level.Restricted, acme))
    }

    test(
      "a group naming a realm clears a person holding one of its accounts as a full member, " +
        "and not one holding it otherwise or holding another realm's"
    ) {
      val outsider = TestAccounts.account("slack:T10/U1")
      (
        Explained.cleared(visibility, person(TestAccounts.account("slack:T1/U1") -> true)),
        Explained.cleared(visibility, person(TestAccounts.account("slack:T1/U1") -> false)),
        Explained.cleared(visibility, person(outsider -> true))
      ) ==> (Label.at(Level.Internal, trial), Label.Public, Label.Public)
    }

    test(
      "a person added to a declared group through grit is cleared for its grants as a declared member is; an account added to a group not declared clears for nothing"
    ) {
      val cy = TestAccounts.account("test:cy")
      val recorded = Recorded(
        Map.empty,
        Map(group("trial-team") -> Set(bo), group("not-declared") -> Set(cy))
      )
      given Tx = TestTx.inForce(visibility, recorded)
      (Tx.clearanceOf(person(bo -> false)), Tx.clearanceOf(person(cy -> false))) ==>
        (Label.at(Level.Confidential, trial, acme), Label.Public)
    }

    test("grit is cleared for every declared compartment at the top level") {
      Explained.cleared(visibility, Principal.Grit) ==>
        Label.at(Level.Restricted, trial, acme, Compartment.Unmapped)
    }

    test("the shipped visibility clears no one beyond public and labels every room public") {
      (
        Explained.cleared(Visibility.Shipped, person(ana -> false)),
        Tx.roomLabel(place("slack:acme/C1"))(using TestTx.inForce(Visibility.Shipped))
      ) ==> (Label.Public, Label.Public)
    }

    test(
      "a direct message's room is labelled top, whatever the rooms' labeller says of everywhere else"
    ) {
      val internal = Visibility
        .of(
          compartments,
          RoomLabels
            .of(Vector.empty, Labelled.Mapped(Label.at(Level.Internal)))
            .fold(p => throw new java.lang.AssertionError(p.written), identity),
          groups,
          grants
        )
        .fold(r => throw new java.lang.AssertionError(r.toString), identity)
      {
        given Tx = TestTx.inForce(internal)
        (Tx.roomLabel(place("direct:slack/T/U")), Tx.roomLabel(place("slack:T/C")))
      } ==>
        (compartments.top, Label.at(Level.Internal))
    }

    test("a declared room within direct is refused: a direct message is labelled by its person") {
      val at = place("direct:slack/T")
      val declaring = RoomLabels
        .of(Vector(at -> Label.at(Level.Internal)), Labelled.Mapped(Label.Public))
        .fold(p => throw new java.lang.AssertionError(p.written), identity)
      Visibility.of(compartments, declaring, groups, grants) ==>
        Left(VisibilityRefusal.DirectDeclared(at))
    }

    test("a compartment not declared is refused, saying what names it") {
      val ops = compartment("ops")
      val opsRooms = new Labeller[Room] {
        def label(item: Room): Labelled = Labelled.Mapped(Label.Public)
        def requires: Vector[Compartment] = Vector(ops)
      }
      Visibility.of(compartments, opsRooms, groups, grants) ==>
        Left(VisibilityRefusal.Undeclared(Namer.Rooms, ops))
      val at = place("slack:acme/#ops")
      // Its requires names it too; the declared place is named, being more exact.
      val declaredRooms = RoomLabels
        .of(Vector(at -> Label.at(Level.Internal, ops)), Labelled.Mapped(Label.Public))
        .fold(p => throw new java.lang.AssertionError(p.written), identity)
      Visibility.of(compartments, declaredRooms, groups, grants) ==>
        Left(VisibilityRefusal.Undeclared(Namer.RoomAt(at), ops))
      val otherwise = RoomLabels
        .of(Vector.empty, Labelled.Unmapped(Label.at(Level.Public, ops)))
        .fold(p => throw new java.lang.AssertionError(p.written), identity)
      Visibility.of(compartments, otherwise, groups, grants) ==>
        Left(VisibilityRefusal.Undeclared(Namer.OtherRooms, ops))
      val open = RoomLabels
        .of(
          Vector.empty,
          Labelled.Mapped(Label.Public),
          Some(Labelled.Mapped(Label.at(Level.Internal, ops)))
        )
        .fold(p => throw new java.lang.AssertionError(p.written), identity)
      Visibility.of(compartments, open, groups, grants) ==>
        Left(VisibilityRefusal.Undeclared(Namer.OpenRooms, ops))
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups,
        grants :+ Grant(group("acme-team"), Label.at(Level.Internal, ops))
      ) ==> Left(VisibilityRefusal.Undeclared(Namer.Granted(group("acme-team")), ops))
    }

    test("two groups of one name, or a grant to no declared group, are refused") {
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups :+ Group(group("trial-team"), Set(bo)),
        grants
      ) ==>
        Left(VisibilityRefusal.GroupTwice(group("trial-team")))
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups,
        grants :+ Grant(group("nobody"), Label.at(Level.Internal))
      ) ==> Left(VisibilityRefusal.NoSuchGroup(group("nobody")))
    }

    test("an administrators group not declared is refused") {
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups,
        grants,
        administrators = Some(group("admins"))
      ) ==>
        Left(VisibilityRefusal.NoSuchGroup(group("admins")))
    }

    test(
      "an administrators group named as a declared compartment is refused: clear could add administrators"
    ) {
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups :+ Group(group("trial"), Set(ana)),
        grants,
        administrators = Some(group("trial"))
      ) ==> Left(VisibilityRefusal.AdministersCompartment(group("trial")))
    }

    test("a declared administrators group no compartment is named for is kept") {
      Visibility
        .of(
          compartments,
          RoomLabels.Public,
          groups :+ Group(group("admins"), Set(bo)),
          grants,
          administrators = Some(group("admins"))
        )
        .map(_.administrators) ==> Right(Some(group("admins")))
    }

    test("stewards") {
      /* `trial` and `acme` with their own groups, and a stewards group no compartment is named for. */
      val owned = groups ++ Vector(
        Group(group("trial"), Set(ana)),
        Group(group("acme"), Set(bo)),
        Group(group("stewards"), Set(bo))
      )
      def stewarded(stewards: Steward*): Either[VisibilityRefusal, Vector[Steward]] =
        Visibility
          .of(compartments, RoomLabels.Public, owned, grants, stewards = stewards.toVector)
          .map(_.stewards)

      test(
        "a compartment stewarded through its own group, or a group no compartment is named for, is kept"
      ) {
        stewarded(Steward(trial, group("trial")), Steward(acme, group("stewards"))) ==>
          Right(Vector(Steward(trial, group("trial")), Steward(acme, group("stewards"))))
      }

      test("a compartment stewarded through another compartment's own group is refused") {
        stewarded(Steward(trial, group("acme"))) ==>
          Left(VisibilityRefusal.StewardsThroughCompartment(trial, group("acme")))
      }

      test("a steward of unmapped is refused") {
        stewarded(Steward(Compartment.Unmapped, group("stewards"))) ==>
          Left(VisibilityRefusal.StewardsUnmapped)
      }

      test("a steward of a compartment not declared is refused, saying it is stewarded") {
        val finance = compartment("finance")
        stewarded(Steward(finance, group("stewards"))) ==>
          Left(VisibilityRefusal.Undeclared(Namer.Stewarded(finance), finance))
      }

      test("a steward through a group not declared is refused") {
        stewarded(Steward(trial, group("nobody"))) ==>
          Left(VisibilityRefusal.NoSuchGroup(group("nobody")))
      }

      test("a compartment stewarded twice is refused") {
        stewarded(Steward(trial, group("trial")), Steward(trial, group("stewards"))) ==>
          Left(VisibilityRefusal.StewardedTwice(trial))
      }
    }

    test("a service is trusted with its declared label, and with public when none is declared") {
      val trusted = Visibility
        .of(
          compartments,
          RoomLabels.Public,
          groups,
          grants,
          Vector(Trust(github, Label.at(Level.Confidential, trial)))
        )
        .map(v => (v.trusted(github), v.trusted(jira)))
      trusted ==> Right((Label.at(Level.Confidential, trial), Label.Public))
      Visibility.Shipped.trusted(github) ==> Label.Public
    }

    test("a trust naming an undeclared compartment, or a service trusted twice, is refused") {
      val finance = compartment("finance")
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups,
        grants,
        Vector(Trust(github, Label.at(Level.Internal, trial, finance)))
      ) ==> Left(VisibilityRefusal.Undeclared(Namer.Trusted(github), finance))
      Visibility.of(
        compartments,
        RoomLabels.Public,
        groups,
        grants,
        Vector(Trust(github, Label.at(Level.Internal)), Trust(github, Label.Public))
      ) ==> Left(VisibilityRefusal.TrustedTwice(github))
    }
  }
}
