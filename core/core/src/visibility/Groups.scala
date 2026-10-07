package grit.core.visibility

import grit.core.id.PrincipalId

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

/** People a deployment groups together, by principal. */
final case class Group(name: GroupName, members: Set[PrincipalId])

/** `group`'s members are cleared for `label`. */
final case class Grant(group: GroupName, label: Label)

/** Which groups a person is in, as one source knows it: the declared groups are one; an
  * edge's (a workspace's user groups) another.
  */
trait Memberships extends caps.Pure {
  def groups(person: PrincipalId): Set[GroupName]
}
