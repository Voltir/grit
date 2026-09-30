package grit.core.stitch

import grit.core.id.ConversationId

import utest.*

/** [[Strand.of]]: a star, never a chain. */
object StrandTests extends TestSuite {

  private def c(n: String) = ConversationId(n)

  val tests = Tests {
    test("a strand is its root and the root's direct followers, never a follower's follower") {
      // B and C follow A; D follows C, which placement never makes, but a race can.
      val links = Vector(Link(c("B"), c("A")), Link(c("C"), c("A")), Link(c("D"), c("C")))
      Strand.of(c("B"), links) ==> Set(c("A"), c("B"), c("C"))
      Strand.of(c("A"), links) ==> Set(c("A"), c("B"), c("C"))
      Strand.of(c("D"), links) ==> Set(c("C"), c("D"))
      Strand.of(c("E"), links) ==> Set(c("E"))
    }
  }
}
