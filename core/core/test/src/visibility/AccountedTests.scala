package grit.core.visibility

import utest.*
import TestLabels.place

object AccountedTests extends TestSuite {

  private def at(n: Int): Provenance =
    Provenance(place(s"service:github/octo/repo-$n"), Integrity.Member)

  val tests = Tests {
    test(
      "of keeps each distinct provenance once, in the order first read, and past Most is unaccountable"
    ) {
      val other = Provenance(place("service:github/octo/other"), Integrity.Outside)
      val member = Provenance(place("service:github/octo/grit"), Integrity.Member)
      val outside = Provenance(place("service:github/octo/grit"), Integrity.Outside)
      val thirtyTwo = Vector.tabulate(32)(at)
      (
        Accounted.of(Vector(other, member, other, outside)),
        Accounted.of(thirtyTwo :+ at(0)),
        Accounted.of(thirtyTwo :+ at(32))
      ) ==> (
        Accounted.Items(Vector(other, member, outside)),
        Accounted.Items(thirtyTwo),
        Accounted.Unaccountable
      )
    }
  }
}
