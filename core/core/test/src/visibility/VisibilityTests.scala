package grit.core.visibility

import grit.core.id.PrincipalId

import utest.*
import TestLabels.{compartment, group, place}

object VisibilityTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")
  private val compartments =
    Compartments.of(Vector(trial, acme)).fold(c => throw new java.lang.AssertionError(c), identity)

  private val ana = PrincipalId("ana")
  private val bo = PrincipalId("bo")

  private val groups: Vector[Group] = Vector(
    Group(group("trial-team"), Set(ana)),
    Group(group("acme-team"), Set(ana, bo))
  )
  private val grants: Vector[Grant] = Vector(
    Grant(group("trial-team"), Label.at(Level.Internal, trial)),
    Grant(group("acme-team"), Label.at(Level.Confidential, acme))
  )

  private val visibility =
    Visibility
      .of(compartments, RoomLabels.Public, groups, grants)
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)

  val tests = Tests {
    test("a person is cleared for the join of their groups' grants, and no one else for any") {
      visibility.cleared(ana) ==> Label.at(Level.Confidential, trial, acme)
      visibility.cleared(bo) ==> Label.at(Level.Confidential, acme)
      visibility.cleared(PrincipalId("cy")) ==> Label.Public
    }

    test("grit is cleared for every declared compartment at the top level") {
      visibility.cleared(PrincipalId.Grit) ==>
        Label.at(Level.Restricted, trial, acme, Compartment.Unmapped)
    }

    test("the shipped visibility clears no one beyond public and labels no room") {
      Visibility.Shipped.cleared(ana) ==> Label.Public
      Visibility.Shipped.labelled ==> false
      val labelled = RoomLabels
        .of(Vector(place("slack:acme") -> Label.at(Level.Internal)), Labelled.Mapped(Label.Public))
        .flatMap(rooms => Visibility.of(compartments, rooms, Vector.empty, Vector.empty))
        .map(_.labelled)
      labelled ==> Right(true)
    }

    test("a compartment not declared is refused, saying what names it") {
      val ops = compartment("ops")
      val opsRooms = new Labeller[grit.core.place.Place] {
        def label(item: grit.core.place.Place): Labelled = Labelled.Mapped(Label.Public)
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
  }
}
