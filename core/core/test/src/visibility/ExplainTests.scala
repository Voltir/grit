package grit.core.visibility

import grit.core.identity.{Evidence, Principal, TestAccounts}

import utest.*

/** What a person is told of their clearance names nothing the room's label does not dominate,
  * and shows their accounts by kind alone.
  */
object ExplainTests extends TestSuite {

  import Explained.*

  val tests = Tests {
    test(
      "a sealed direct thread is told its fallen floor and the groups it dominates: neither finance nor leadership appears, though she is still in leadership"
    ) {
      val told = sealedThread
      (
        Before.cleared(Dana).dominates(confidentialBoth),
        told.room,
        told.beyond,
        told.groups.map(g => GroupName.value(g.group))
      ) ==> (true, confidentialTrial, confidentialTrial, Vector("trial", "staff"))
      names(told.toString, Above*) ==> Vector()
    }

    test(
      "a direct thread begun below a raised clearance names no group its label does not dominate"
    ) {
      val told = raisedThread
      (told.beyond, told.groups.map(g => GroupName.value(g.group))) ==>
        (confidentialTrial, Vector("trial", "staff"))
      names(told.toString, Above*) ==> Vector()
    }

    test(
      "at a person's full clearance every group they are in is named, and beyond is that clearance"
    ) {
      val told = fullClearance
      (told.room, told.beyond, told.groups.map(g => (GroupName.value(g.group), g.label))) ==> (
        confidentialBoth,
        confidentialBoth,
        Vector(
          ("trial", confidentialTrial),
          ("leadership", confidentialBoth),
          ("staff", Label.at(Level.Internal))
        )
      )
    }

    test(
      "a person cleared for public, asked in a room labelled above that, is told of no group, and beyond is public"
    ) {
      val ed = TestAccounts.principal(TestAccounts.account("slack:T/U-ed"))
      val told = Before.explain(Some(ed), confidentialBoth)
      (told.room, told.beyond, told.groups) ==> (confidentialBoth, Label.Public, Vector())
    }

    test(
      "a person's accounts are shown by kind, evidence and membership, and so are the ways into their groups: no account's or realm's name"
    ) {
      val told = fullClearance
      told.asker ==> Explanation.Asker.Person(
        Vector(
          Explanation.Shown(Explanation.Namespace.of(dana), Evidence.Vouched, member = true),
          Explanation.Shown(Explanation.Namespace.of(danaAtHome), Evidence.Home, member = false)
        )
      )
      told.groups.map(g =>
        (
          GroupName.value(g.group),
          g.through.map(Explanation.Namespace.value),
          g.members.map(Explanation.Namespace.value)
        )
      ) ==> Vector(
        ("trial", Set("slack", "test"), Set()),
        ("leadership", Set("slack"), Set()),
        ("staff", Set(), Set("slack"))
      )
      names(told.toString, Named*) ==> Vector()
    }

    test(
      "no one asking is told no one, beyond public; grit is grit, beyond the room's label; each in no group"
    ) {
      (
        Before.explain(None, confidentialBoth),
        Before.explain(Some(Principal.Grit), confidentialBoth)
      ) ==> (
        Explanation(confidentialBoth, Label.Public, Explanation.Asker.Nobody, Vector()),
        Explanation(confidentialBoth, confidentialBoth, Explanation.Asker.Grit, Vector())
      )
    }
  }
}
