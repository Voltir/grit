package grit.projects.name

/** The name of a method, as a deployment declares it. */
opaque type MethodName = String

object MethodName {

  /** `s` as a method's name, or why not ([[Slug.check]]). */
  def of(s: String): Either[String, MethodName] = Slug.check("a method's name", s, Slug.Longest)
  def value(n: MethodName): String = n
}

/** The name of a kind of artifact a method declares. */
opaque type KindName = String

object KindName {

  /** `s` as a kind's name, or why not ([[Slug.check]]). */
  def of(s: String): Either[String, KindName] = Slug.check("a kind's name", s, Slug.Longest)
  def value(n: KindName): String = n
}

/** The name of a state an artifact of some kind may be in. */
opaque type StateName = String

object StateName {

  /** `s` as a state's name, or why not ([[Slug.check]]). */
  def of(s: String): Either[String, StateName] = Slug.check("a state's name", s, Slug.Longest)
  def value(n: StateName): String = n
}

/** The name a compiled transition check is known by in a method's data. */
opaque type CheckName = String

object CheckName {

  /** `s` as a check's name, or why not ([[Slug.check]]). */
  def of(s: String): Either[String, CheckName] = Slug.check("a check's name", s, Slug.Longest)
  def value(n: CheckName): String = n

  /** A built-in check's name, a literal [[of]] accepts (`TransitionCheckTests` reads each back). */
  private[projects] def builtin(s: String): CheckName = s
}

/** The name of a role a method shows artifacts to. */
opaque type RoleName = String

object RoleName {

  /** `s` as a role's name, or why not ([[Slug.check]]). */
  def of(s: String): Either[String, RoleName] = Slug.check("a role's name", s, Slug.Longest)
  def value(n: RoleName): String = n
}

/** What an artifact is claimed for: the one session, or one agent type in it, that a brief is
  * written for. An edge's own name for it, which grit never reads apart, such as
  * `coder-opus/<sha1 of a directory>`; one slot works one artifact at a time.
  */
opaque type BriefSlot = String

object BriefSlot {

  /** 160: the most characters a slot holds. */
  val Longest: Int = 160

  /** `s` as a slot, or why not: over [[Longest]] characters, or not parts joined by `/`, each
    * as [[Slug.check]] takes a name.
    */
  def of(s: String): Either[String, BriefSlot] =
    if (s.length > Longest) Left(s"a slot holds at most $Longest characters: $s")
    else
      s.split("/", -1)
        .toVector
        .foldLeft[Either[String, Unit]](Right(()))((acc, part) =>
          acc.flatMap(_ => Slug.check("a part of a slot", part, Slug.Longest).map(_ => ()))
        )
        .map(_ => s)
  def value(s: BriefSlot): String = s
}

/** The one form every name and key part here takes. */
object Slug {

  /** 64: the most characters a name holds. */
  val Longest: Int = 64

  /** `s`, or why it is not `what`: empty, over `longest` characters, or holding a character
    * other than a lowercase ASCII letter, a digit or `-`, or starting with `-`.
    */
  def check(what: String, s: String, longest: Int): Either[String, String] =
    if (s.isEmpty) Left(s"$what is empty")
    else if (s.length > longest) Left(s"$what is over $longest characters: $s")
    else if (s.startsWith("-")) Left(s"$what starts with '-': $s")
    else
      s.find(c => !(c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '-')) match {
        case Some(c) => Left(s"$what holds '$c', not a lowercase letter, a digit or '-': $s")
        case None => Right(s)
      }
}
