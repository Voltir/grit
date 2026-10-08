package grit.core.visibility

import grit.core.identity.{Account, Evidence, Held, Principal, Realm, TestAccounts}

import TestLabels.{compartment, group}

/** A person, Dana, and the deployments that clear her, as the suites of what she is told of
  * her clearance build them: ExplainTests over the explanation, and grit.tools' over its
  * rendered text.
  */
object Explained {

  val trial = compartment("trial")
  val finance = compartment("finance")
  val compartments =
    Compartments
      .of(Vector(trial, finance))
      .fold(c => throw new java.lang.AssertionError(c), identity)

  val dana = TestAccounts.account("slack:T/U-dana")
  val danaAtHome = TestAccounts.account("test:R/V-dana")
  val workspace =
    Realm.of("slack", "T").fold(e => throw new java.lang.AssertionError(e), identity)

  val confidentialTrial = Label.at(Level.Confidential, trial)
  val confidentialBoth = Label.at(Level.Confidential, trial, finance)

  /** `trial` (Dana's two accounts by name) grants confidential·trial; `leadership` (Dana's
    * work account) confidential·{trial, finance}; `staff` (the workspace's full members)
    * internal.
    */
  def visibility(leadership: Set[Account]): Visibility =
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

  val Before = visibility(Set(dana))
  val After = visibility(Set.empty)

  /** Dana, holding her work account, vouched and a full member, and her home account. */
  val Dana: Principal =
    Principal.Person(
      TestAccounts.principalId(dana),
      Set(
        Held(dana, Evidence.Vouched, member = true),
        Held(danaAtHome, Evidence.Home, member = false)
      )
    )

  /** Whether `text` holds any of `names`. */
  def names(text: String, names: String*): Vector[String] =
    names.toVector.filter(text.contains)

  /** A sealed direct thread: stored at confidential·{trial, finance}, Dana since out of
    * leadership, so its floor is what she is cleared for now.
    */
  def sealedThread: Explanation =
    After.explain(Some(Dana), confidentialBoth.meet(After.cleared(Dana)))

  /** A direct thread begun at confidential·trial, Dana since added to leadership. */
  def raisedThread: Explanation = Before.explain(Some(Dana), confidentialTrial)

  /** Dana asked at her full clearance. */
  def fullClearance: Explanation = Before.explain(Some(Dana), Before.cleared(Dana))

  /** What no explanation of [[sealedThread]] or [[raisedThread]] may hold: the compartment and
    * group they do not dominate.
    */
  val Above: Vector[String] = Vector("finance", "leadership")

  /** What no explanation may hold: Dana's accounts' and her workspace's names. */
  val Named: Vector[String] = Vector("R/V", "T/U", "T/", "dana")
}
