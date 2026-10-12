package grit.core.store

import grit.core.durable.Probes

import utest.*

/** A voucher reaches the database only through the transaction it is given: one that holds a
  * capability does not compile, since the kit hands a deployment's voucher to an edge for as
  * long as the edge serves.
  */
object VoucherCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.store.*
      |import grit.core.identity.{Account, Realm, Vouched}
      |trait Provider extends caps.SharedCapability { def call(): String }
      |""".stripMargin

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

  private val throughTx = voucher("Right(Vector(Linking.Outside(vouched.account)))")
  private val calling = voucher("{ p.call(); Right(Vector.empty) }")
  private lazy val compiled = Probes.batch(prelude, List(throughTx, calling))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: a voucher answering through the transaction it is given compiles") {
      compiled.errors(throughTx) ==> Nil
    }

    test("a voucher that calls a capability it holds is rejected") {
      val errs = compiled.errors(calling)
      assert(
        errs.exists(e =>
          e.contains("Reference `p` is not included in the allowed capture set {}") &&
            e.contains("self type") && e.contains("Voucher")
        )
      )
    }
  }
}
