package grit.projects.name

import grit.core.claim.ClaimRole
import grit.core.id.Slug

/** The name of a method, as a deployment declares it. */
opaque type MethodName = String

object MethodName {

  /** 64: the most characters a method's name holds. */
  val Longest: Int = 64

  /** `s` as a method's name, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, MethodName] =
    Slug.check("a method's name", s, Some(Longest), leadingDigit = true)
  def value(n: MethodName): String = n
}

/** The name of a kind of artifact a method declares. */
opaque type KindName = String

object KindName {

  /** 64: the most characters a kind's name holds. */
  val Longest: Int = 64

  /** `s` as a kind's name, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, KindName] =
    Slug.check("a kind's name", s, Some(Longest), leadingDigit = true)
  def value(n: KindName): String = n
}

/** The name of a state an artifact of some kind may be in. */
opaque type StateName = String

object StateName {

  /** 64: the most characters a state's name holds. */
  val Longest: Int = 64

  /** `s` as a state's name, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, StateName] =
    Slug.check("a state's name", s, Some(Longest), leadingDigit = true)
  def value(n: StateName): String = n
}

/** The name a compiled transition check is known by in a method's data. */
opaque type CheckName = String

object CheckName {

  /** 64: the most characters a check's name holds. */
  val Longest: Int = 64

  /** `s` as a check's name, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, CheckName] =
    Slug.check("a check's name", s, Some(Longest), leadingDigit = true)
  def value(n: CheckName): String = n

  /** A built-in check's name, a literal [[of]] accepts (`TransitionCheckTests` reads each back). */
  private[projects] def builtin(s: String): CheckName = s
}

/** The name of a role a method shows artifacts to, and a claim is made as. */
opaque type RoleName <: ClaimRole = ClaimRole

object RoleName {

  /** `s` as a role's name, or why not ([[Slug.check]], at most [[ClaimRole.Longest]]
    * characters).
    */
  def of(s: String): Either[String, RoleName] =
    Slug
      .check("a role's name", s, Some(ClaimRole.Longest), leadingDigit = true)
      .flatMap(ClaimRole.of)
  def value(n: RoleName): String = ClaimRole.value(n)
}
