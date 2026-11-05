package grit.databases.declare

import utest.*

/** Which names [[DatabaseRole.of]] takes as a Postgres role. */
object DatabaseRoleTests extends TestSuite {

  private def refused(name: String): Boolean = DatabaseRole.of(name).isLeft

  val tests = Tests {
    test("a plain role and a cloud IAM user's name are roles, as written") {
      assert(DatabaseRole.of("leads_reader").map(DatabaseRole.name) == Right("leads_reader"))
      assert(DatabaseRole.of("sa-x@proj-1.iam").map(DatabaseRole.name) == Right("sa-x@proj-1.iam"))
    }

    test("a blank name is refused, naming the length rule") {
      assert(DatabaseRole.of("") == Left("a role's name is 1 to 63 bytes: "))
    }

    test("a name past 63 bytes is refused, and 63 is not") {
      assert(refused("r" * 64), !refused("r" * 63))
    }

    test("a space or a quote is refused, naming the characters a role may hold") {
      assert(
        DatabaseRole.of("a b") == Left("a role's name is letters, digits, _, $, @, . or -: a b"),
        refused("a\"b")
      )
    }

    test("a name Postgres keeps for itself is refused") {
      assert(
        DatabaseRole.of("pg_monitor") == Left(
          "a role's name may not start pg_, which Postgres keeps: pg_monitor"
        )
      )
    }
  }
}
