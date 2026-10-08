package grit.core.identity

import utest.*

object EmailTests extends TestSuite {

  val tests = Tests {
    test("an address is trimmed and lowercased, and otherwise kept as given") {
      Email.of("  Alice@Example.COM\n").map(Email.value) ==> Right("alice@example.com")
      // Dots and a plus tag are the address's own: two that differ there are two addresses.
      Email.of("a.b+c@x").map(Email.value) ==> Right("a.b+c@x")
      assert(Email.of("a.b+c@x") != Email.of("ab@x"))
    }

    test("an address with whitespace inside, or not one @ with something either side, is refused") {
      List("a@", "@b", "a@b@c", "ab", "a @b", "a@b\tc", "", " ")
        .map(Email.of(_).isRight) ==> List(false, false, false, false, false, false, false, false)
      Email.of("a @b") ==> Left("an email address has no whitespace inside: a @b")
      Email.of("a@b@c") ==>
        Left("an email address is one @ with something on either side: a@b@c")
    }

    test("an address's domain is what follows its @, lowercased") {
      Email.of("A@Example.COM").map(e => Domain.value(Email.domain(e))) ==> Right("example.com")
    }
  }
}
