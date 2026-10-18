package grit.core.review

import grit.core.durable.Probes

import utest.*

/** What capture checking rejects about reviews: a subject and a verdict cross from the store's
  * transaction to an edge's posting code, so neither may hold a capability, and the functions
  * [[Reviews.as]] keeps are pure.
  */
object ReviewCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.review.*
      |import grit.core.store.*
      |trait Provider extends caps.SharedCapability { def call(): String }
      |final case class Subject(n: Int) extends caps.Pure
      |""".stripMargin

  private val retyped =
    """def retyped(r: Reviews[Prompt, Verdict]): Reviews[Subject, Verdict] =
      |  Reviews.as(r)(p => Some(Subject(1)), s => sys.error("unread"), v => v)
      |""".stripMargin

  private val txSubject =
    """def held(r: Reviews[Tx^, Verdict]): Unit = ()
      |""".stripMargin

  private val impureRead =
    """def calling(r: Reviews[Prompt, Verdict], p: Provider^): Reviews[Subject, Verdict] =
      |  Reviews.as(r)(s => { p.call(); Some(Subject(1)) }, s => sys.error("unread"), v => v)
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(retyped, txSubject, impureRead))

  val tests = Tests {
    test("control: reviews retyped through pure functions compile") {
      compiled.errors(retyped) ==> Nil
    }

    test("reviews whose subject is a transaction are rejected") {
      val errs = compiled.errors(txSubject)
      assert(errs.exists(e => e.contains("Pure") && e.contains("Tx")))
    }

    test("reviews retyped through a read that calls a capability are rejected") {
      val errs = compiled.errors(impureRead)
      assert(errs.exists(_.contains("capability `p` cannot flow into capture set {}")))
    }
  }
}
