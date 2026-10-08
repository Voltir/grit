package grit.core.visibility

import grit.core.identity.{Account, Evidence, Realm}

/** What a person asking in a room labelled `room` is told of why they read what they read
  * ([[Visibility.explain]], ADR 0032), naming nothing `room` does not dominate: `beyond`, what
  * a turn there reads outside the room (`room` met with the asker's clearance,
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
    * `through`, or as a full member of a realm of each kind in `members`.
    */
  final case class In(
      group: GroupName,
      label: Label,
      through: Set[Namespace],
      members: Set[Namespace]
  )
}
