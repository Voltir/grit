package grit.core.visibility

import grit.core.identity.{Account, Realm}

/** A group's name, as a deployment or a membership source names it. */
opaque type GroupName = String

object GroupName {

  /** `name` trimmed, or why not: it is blank. */
  def of(name: String): Either[String, GroupName] = {
    val trimmed = name.trim
    if (trimmed.isEmpty) Left("a group's name is blank") else Right(trimmed)
  }

  def value(name: GroupName): String = name
}

/** People a deployment groups together: the `accounts` sources know them by, and the `realms`
  * whose full members it takes in. A person is in it through any account linked to them that
  * it names, or any that a realm it names vouches a full member now (ADR 0032).
  */
final case class Group(name: GroupName, accounts: Set[Account], realms: Set[Realm] = Set.empty)

/** `group`'s members are cleared for `label`. */
final case class Grant(group: GroupName, label: Label)

/** The declared members of `group` (never people added through grit) steward `compartment`:
  * they may clear and remove people for it, and remove it from a private room's label.
  * `Steward(c, c)`, `c`'s own group named, makes the compartment's own declared members its
  * stewards.
  */
final case class Steward(compartment: Compartment, group: GroupName)

/** Which groups an account is in, as one source knows it: the declared groups are one; an
  * edge's (a workspace's user groups) another.
  */
trait Memberships extends caps.Pure {
  def groups(account: Account): Set[GroupName]
}
