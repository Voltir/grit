package grit.core.visibility

import grit.core.identity.{Account, Evidence, Realm}

/** What a person asking in a room labelled `room` is told of why they read what they read
  * ([[grit.core.store.Tx.explain]], ADR 0032), naming nothing `room` does not dominate:
  * `beyond`, what a turn there reads outside the room (`room` met with the asker's clearance,
  * [[Clearance.inRoom]]); `asker`, who they are taken to be; and `groups`, each group they are in
  * whose grant `room` dominates, in the deployment's order. Asked in a direct message, whose
  * label is the asker's clearance, it names what they are cleared for and nothing more.
  */
final case class Explanation(
    room: Label,
    beyond: Label,
    asker: Explanation.Asker,
    groups: Vector[Explanation.In]
)

object Explanation {

  /** `e` in plain words: the room's label, what the asker reads beyond it, who they are taken
    * to be (each account by its kind alone), the groups named (each by how they are in it:
    * named, a full member, or added through grit), the two rules (a room's members read its own
    * speech up to its label; anything said elsewhere is read there up to the room's label met
    * with the asker's clearance), and that it never says whether anything is hidden. It names
    * nothing `e` does not hold.
    */
  def text(e: Explanation): String =
    Vector(
      s"This conversation is labelled ${Label.shown(e.room)}.",
      s"From anywhere else, I read for you up to ${Label.shown(e.beyond)}.",
      who(e.asker),
      groups(e.groups),
      Rules,
      Hidden
    ).mkString("\n\n")

  /** How a room's speech and everything else is read, and how a label is written. */
  private val Rules: String =
    "How it works: what is said in a room is read there, by its members, up to the room's " +
      "label. Anything said elsewhere is read here up to this room's label met with your " +
      "clearance. A label is shown as its level, then the compartments it is in."

  /** The line every answer ends with. */
  private val Hidden: String = "I never say whether anything is hidden from you."

  private def who(asker: Asker): String =
    asker match {
      case Asker.Nobody => "No one I know asked this, so I tell no one's clearance."
      case Asker.Grit =>
        "This turn is my own, not a person's, so there is no person's clearance to tell."
      case Asker.Person(accounts) => "I know you by " + accounts.map(shown).mkString("; ") + "."
    }

  private def shown(s: Shown): String = {
    val how = s.evidence match {
      case Evidence.Home => "your own"
      case Evidence.Vouched => "linked to you by an email a trusted source confirmed"
    }
    val member = if (s.member) ", a full member of its source" else ""
    s"a ${Namespace.value(s.namespace)} account, $how$member"
  }

  private def groups(in: Vector[In]): String =
    if (in.isEmpty) "You are in no group that clears you for anything here."
    else
      ("Your groups, each with what it clears you for:" +: in.map { i =>
        val ways =
          i.through.toVector.sorted.map(n => s"through your ${Namespace.value(n)} account") ++
            i.members.toVector.sorted
              .map(n => s"as a full member of a ${Namespace.value(n)} source") ++
            i.added.toVector.sorted
              .map(n => s"added through grit for your ${Namespace.value(n)} account")
        s"- ${GroupName.value(i.group)}: ${Label.shown(i.label)}, ${ways.mkString(" and ")}"
      }).mkString("\n")

  /** Who a turn answers, as the explanation may show them. */
  enum Asker {

    /** No one: the turn has no first entry, or its account was never seen. */
    case Nobody

    /** grit: a job's run, or a turn grit's own entry begins. */
    case Grit

    /** A person: each of their accounts as it may be shown, ordered by kind. */
    case Person(accounts: Vector[Shown])
  }

  /** The kind of an account or a realm: its namespace (`slack`, `test`), never its name or the
    * scope a realm holds.
    */
  opaque type Namespace = String

  object Namespace {

    /** `account`'s namespace: `slack` of `slack:T0123/U0456`; `local` and `grit` their own. */
    def of(account: Account): Namespace =
      Account.Sourced.of(account).fold(Account.written(account))(Account.Sourced.namespace)

    /** The namespace of the accounts `realm` holds. */
    def of(realm: Realm): Namespace = realm.namespace

    def value(n: Namespace): String = n

    given Ordering[Namespace] = Ordering.String
  }

  /** An account as a room may be told of it: its kind, how it is its person's, and whether a
    * trusted realm attests it a full member.
    */
  final case class Shown(namespace: Namespace, evidence: Evidence, member: Boolean)

  /** `group`, granted `label`, which the asker is in through an account of each kind in
    * `through` (named by the deployment), as a full member of a realm of each kind in
    * `members`, or through an account of each kind in `added` (added through grit).
    */
  final case class In(
      group: GroupName,
      label: Label,
      through: Set[Namespace],
      members: Set[Namespace],
      added: Set[Namespace]
  )
}
