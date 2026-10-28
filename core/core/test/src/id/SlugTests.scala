package grit.core.id

import utest.*

/** [[Slug.check]]: the refusals every name built on it shares, each naming what was refused. */
object SlugTests extends TestSuite {

  val tests = Tests {
    test("a leading digit is refused only where it is not allowed") {
      (
        Slug.check("a plugin's name", "2fa", None, leadingDigit = false),
        Slug.check("a kind's name", "2fa", None, leadingDigit = true)
      ) ==> (Left("a plugin's name starts with a digit: 2fa"), Right("2fa"))
    }

    test("a slug over its cap is refused, and one at it kept") {
      (
        Slug.check("a key", "abcde", Some(4), leadingDigit = true),
        Slug.check("a key", "abcd", Some(4), leadingDigit = true)
      ) ==> (Left("a key is over 4 characters: abcde"), Right("abcd"))
    }

    test("a character other than a lowercase letter, a digit or '-' is refused, by name") {
      Slug.check("a state's name", "in_review", None, leadingDigit = true) ==>
        Left("a state's name holds '_', not a lowercase letter, a digit or '-': in_review")
    }
  }
}
