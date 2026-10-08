package grit.core.visibility

import grit.core.identity.{Account, Evidence, Held, Principal, Realm, TestAccounts}
import grit.core.place.Service

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
    test("a person is cleared for the join of their groups' grants, and no one else for any") {
      visibility.cleared(person(ana -> false)) ==> Label.at(Level.Confidential, trial, acme)
      visibility.cleared(person(bo -> false)) ==> Label.at(Level.Confidential, acme)
      visibility.cleared(person(TestAccounts.account("test:cy") -> false)) ==> Label.Public
    }

    test("a person holding several accounts is cleared for the join over the groups of each") {
      (
        visibility.cleared(person(bo -> false, anaAtWork -> false)),
        visibility.cleared(person(anaAtWork -> false, bo -> false))
      ) ==> (Label.at(Level.Restricted, acme), Label.at(Level.Restricted, acme))
    }

    test(
      "a group naming a realm clears a person holding one of its accounts as a full member, " +
        "and not one holding it otherwise or holding another realm's"
    ) {
      val outsider = TestAccounts.account("slack:T10/U1")
      (
        visibility.cleared(person(TestAccounts.account("slack:T1/U1") -> true)),
        visibility.cleared(person(TestAccounts.account("slack:T1/U1") -> false)),
        visibility.cleared(person(outsider -> true))
      ) ==> (Label.at(Level.Internal, trial), Label.Public, Label.Public)
    }

    test("grit is cleared for every declared compartment at the top level") {
      visibility.cleared(Principal.Grit) ==>
        Label.at(Level.Restricted, trial, acme, Compartment.Unmapped)
    }

    test("the shipped visibility clears no one beyond public and labels every room public") {
      (
        Visibility.Shipped.cleared(person(ana -> false)),
        Visibility.Shipped.roomLabel(place("slack:acme/C1"))
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
      (internal.roomLabel(place("direct:slack/T/U")), internal.roomLabel(place("slack:T/C"))) ==>
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
