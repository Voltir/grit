package grit.core.store

import grit.core.durable.Probes

import utest.*

/** A voucher reaches the database only through the transaction it is given: one that holds a
  * capability does not compile, since the kit hands a deployment's voucher to an edge for as
  * long as the edge serves.
  */
object VoucherCaptureTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.store.*
         |import grit.core.identity.{Account, Realm, Vouched}
         |trait Provider extends caps.SharedCapability { def call(): String }
         |object Probe {
         |  $body
         |}
         |""".stripMargin
    )

  /* Its type left to inference, so only the voucher's own self type can refuse `p`. */
  private def voucher(vouches: String): String =
    s"""def make(p: Provider^): Unit = {
       |    val made = new Voucher {
       |      def realms: Set[Realm] = Set.empty
       |      def vouch(vouched: Vouched)(using Tx^): Either[StoreError, Vector[Linking]] =
       |        $vouches
       |      def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord] =
       |        Right(LastWord(account, None))
       |      def lastWords(realm: Realm)(using Tx^): Either[StoreError, Vector[LastWord]] =
       |        Right(Vector.empty)
       |    }
       |    val _ = made.realms
       |  }""".stripMargin

  val tests = Tests {
    test("control: a voucher answering through the transaction it is given compiles") {
      probe(
        voucher("Right(Vector(Linking.Outside(vouched.account)))")
      ) ==> Nil
    }

    test("a voucher that calls a capability it holds is rejected") {
      val errs = probe(voucher("{ p.call(); Right(Vector.empty) }"))
      assert(
        errs.exists(e =>
          e.contains("Reference `p` is not included in the allowed capture set {}") &&
            e.contains("self type") && e.contains("Voucher")
        )
      )
    }
  }
}
