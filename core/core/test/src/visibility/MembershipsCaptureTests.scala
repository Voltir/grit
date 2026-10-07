package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A membership source answers from what it holds: one that holds a capability does not
  * compile, since a deployment's [[Visibility]] clears every person through it as pure data.
  */
object MembershipsCaptureTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.visibility.*
         |import grit.core.identity.Account
         |trait Provider extends caps.SharedCapability { def call(): String }
         |object Probe {
         |  $body
         |}
         |""".stripMargin
    )

  /* Its type left to inference, so only the trait's own self type can refuse `p`. */
  private def memberships(answers: String): String =
    s"""def make(p: Provider^): Unit = {
       |    val made = new Memberships {
       |      def groups(account: Account): Set[GroupName] = $answers
       |    }
       |    val _ = made.groups(Account.Local)
       |  }""".stripMargin

  val tests = Tests {
    test("control: memberships answering from the account alone compile") {
      probe(memberships("GroupName.of(Account.written(account)).toOption.toSet")) ==> Nil
    }

    test("memberships that call a capability they hold are rejected") {
      val errs = probe(memberships("GroupName.of(p.call()).toOption.toSet"))
      assert(
        errs.exists(e =>
          e.contains("Reference `p` is not included in the allowed capture set {}") &&
            e.contains("self type") && e.contains("Memberships")
        )
      )
    }
  }
}
