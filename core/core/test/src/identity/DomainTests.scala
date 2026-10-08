package grit.core.identity

import utest.*

object DomainTests extends TestSuite {

  val tests = Tests {
    test("a domain is trimmed and lowercased") {
      Domain.of(" Example.COM ").map(Domain.value) ==> Right("example.com")
    }

    test("a domain is refused blank, holding @ or whitespace, or with no . between two labels") {
      List("", "  ", "@x.com", "x", "ex ample.com", "x.", ".x", "a..b").map(Domain.of) ==>
        List(
          Left("a domain is not blank: "),
          Left("a domain is not blank:   "),
          Left("a domain holds no @ or whitespace: @x.com"),
          Left("a domain is labels joined by ., at least two: x"),
          Left("a domain holds no @ or whitespace: ex ample.com"),
          Left("a domain is labels joined by ., at least two: x."),
          Left("a domain is labels joined by ., at least two: .x"),
          Left("a domain is labels joined by ., at least two: a..b")
        )
    }
  }
}
