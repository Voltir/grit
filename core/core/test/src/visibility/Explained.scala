package grit.core.visibility

import grit.core.identity.{Account, Evidence, Held, Principal, Realm, TestAccounts}
import grit.core.store.Tx
import grit.dbos.sql.TestTx

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

  /** Dana, holding her work account, vouched and a full member, and her home account; the home
    * account first, so an explanation shows them in its own order, not the set's.
    */
  val Dana: Principal =
    Principal.Person(
      TestAccounts.principalId(dana),
      Set(
        Held(danaAtHome, Evidence.Home, member = false),
        Held(dana, Evidence.Vouched, member = true)
      )
    )

  /** `principal`'s clearance in a transaction under `v`, nothing recorded beside it. */
  def cleared(v: Visibility, principal: Principal): Label =
    Tx.clearanceOf(principal)(using TestTx.inForce(v))

  /** What `asker` is told in a room labelled `room`, in a transaction under `v` and `recorded`. */
  def explain(
      v: Visibility,
      asker: Option[Principal],
      room: Label,
      recorded: Recorded = Recorded.Empty
  ): Explanation =
    Tx.explain(asker, room)(using TestTx.inForce(v, recorded))

  /** Whether `text` holds any of `names`. */
  def names(text: String, names: String*): Vector[String] =
    names.toVector.filter(text.contains)

  /** Restricted·trial: above anything any group here grants. */
  val restrictedTrial = Label.at(Level.Restricted, trial)

  /** A sealed direct thread: stored at [[restrictedTrial]], begun when Dana was cleared for it,
    * and she since cleared for less, though still in leadership; its floor is confidential·trial,
    * below leadership's grant.
    */
  def sealedThread: Explanation =
    explain(Before, Some(Dana), restrictedTrial.meet(cleared(Before, Dana)))

  /** A direct thread begun at confidential·trial, Dana since added to leadership. */
  def raisedThread: Explanation = explain(Before, Some(Dana), confidentialTrial)

  /** Dana asked at her full clearance. */
  def fullClearance: Explanation = explain(Before, Some(Dana), cleared(Before, Dana))

  /** What no explanation of [[sealedThread]] or [[raisedThread]] may hold: the compartment and
    * group they do not dominate.
    */
  val Above: Vector[String] = Vector("finance", "leadership")

  /** What no explanation may hold: Dana's accounts' and her workspace's names. */
  val Named: Vector[String] = Vector("R/V", "T/U", "T/", "dana")
}
