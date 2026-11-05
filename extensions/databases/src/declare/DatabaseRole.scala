package grit.databases.declare

import java.nio.charset.StandardCharsets

/** A Postgres role name as `current_user` answers it: 1 to 63 bytes of letters, digits, `_`,
  * `$`, `@`, `.` or `-` (an IAM user's `sa@project.iam`), not starting `pg_`.
  */
opaque type DatabaseRole = String

object DatabaseRole {

  /** `name`'s role, or why not, naming the rule it breaks. */
  def of(name: String): Either[String, DatabaseRole] =
    if (name.isEmpty || name.getBytes(StandardCharsets.UTF_8).length > 63)
      Left(s"a role's name is 1 to 63 bytes: $name")
    else if (!name.forall(c => c.isLetterOrDigit || "_$@.-".contains(c)))
      Left(s"a role's name is letters, digits, _, $$, @, . or -: $name")
    else if (name.startsWith("pg_"))
      Left(s"a role's name may not start pg_, which Postgres keeps: $name")
    else Right(name)

  def name(role: DatabaseRole): String = role
}
