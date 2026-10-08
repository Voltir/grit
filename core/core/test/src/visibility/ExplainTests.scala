package grit.core.visibility

import grit.core.identity.{Account, Evidence, Held, Principal, Realm, TestAccounts}

import utest.*
import TestLabels.{compartment, group}

/** What a person is told of their clearance names nothing the room's label does not dominate,
  * and shows their accounts by kind alone.
  */
object ExplainTests extends TestSuite {

  private val trial = compartment("trial")
  private val finance = compartment("finance")
  private val compartments =
    Compartments
      .of(Vector(trial, finance))
      .fold(c => throw new java.lang.AssertionError(c), identity)

  private val dana = TestAccounts.account("slack:T/U-dana")
  private val danaAtHome = TestAccounts.account("test:R/V-dana")
  private val workspace =
    Realm.of("slack", "T").fold(e => throw new java.lang.AssertionError(e), identity)

  private val confidentialTrial = Label.at(Level.Confidential, trial)
  private val confidentialBoth = Label.at(Level.Confidential, trial, finance)

  /** `trial` (Dana's two accounts by name) grants confidential·trial; `leadership` (Dana's
    * work account) confidential·{trial, finance}; `staff` (the workspace's full members)
    * internal.
    */
  private def visibility(leadership: Set[Account]): Visibility =
    Visibility
      .of(
        compartments,
        RoomLabels.Public,
        Vector(
          Group(group("trial"), Set(dana, danaAtHome)),
          Group(group("leadership"), leadership),
          Group(group("staff"), Set.empty, Set(workspace))
        ),
        Vector(
          Grant(group("trial"), confidentialTrial),
          Grant(group("leadership"), confidentialBoth),
          Grant(group("staff"), Label.at(Level.Internal))
        )
      )
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)

  private val Before = visibility(Set(dana))
  private val After = visibility(Set.empty)

  /** Dana, holding her work account, vouched and a full member, and her home account. */
  private val Dana: Principal =
    Principal.Person(
      TestAccounts.principalId(dana),
      Set(
        Held(dana, Evidence.Vouched, member = true),
        Held(danaAtHome, Evidence.Home, member = false)
      )
    )

  /** Whether `text` holds any of `names`. */
  private def names(text: String, names: String*): Vector[String] =
    names.toVector.filter(text.contains)

  val tests = Tests {
    test(
      "a sealed direct thread is told its fallen floor and the groups it dominates: neither finance nor leadership appears"
    ) {
      // Stored at confidential·{trial, finance}; Dana now out of leadership, so the floor is
      // what she is cleared for now.
      val floor = confidentialBoth.meet(After.cleared(Dana))
      val told = After.explain(Some(Dana), floor)
      (told.beyond, told.groups.map(g => GroupName.value(g.group))) ==>
        (confidentialTrial, Vector("trial", "staff"))
      names(told.toString, "finance", "leadership") ==> Vector()
    }

    test(
      "a direct thread begun below a raised clearance names no group its label does not dominate"
    ) {
      // Begun at confidential·trial; Dana since added to leadership.
      val told = Before.explain(Some(Dana), confidentialTrial)
      (told.beyond, told.groups.map(g => GroupName.value(g.group))) ==>
        (confidentialTrial, Vector("trial", "staff"))
      names(told.toString, "finance", "leadership") ==> Vector()
    }

    test(
      "at a person's full clearance every group they are in is named, and beyond is that clearance"
    ) {
      val told = Before.explain(Some(Dana), Before.cleared(Dana))
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

    test("a person cleared for public, asked in a public room, is told of no group") {
      val nobody = TestAccounts.principal(TestAccounts.account("slack:T/U-ed"))
      val told = Before.explain(Some(nobody), Label.Public)
      (told.beyond, told.groups) ==> (Label.Public, Vector())
    }

    test(
      "a person's accounts are shown by kind, evidence and membership, and so are the ways into their groups: no account's or realm's name"
    ) {
      val told = Before.explain(Some(Dana), Before.cleared(Dana))
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
      names(told.toString, "R/V", "T/U", "T/", "dana") ==> Vector()
    }

    test("no one asking is told no one, and grit is grit, each in no group") {
      (
        Before.explain(None, Label.Public),
        Before.explain(Some(Principal.Grit), confidentialBoth).asker
      ) ==> (
        Explanation(Label.Public, Label.Public, Explanation.Asker.Nobody, Vector()),
        Explanation.Asker.Grit
      )
    }
  }
}
