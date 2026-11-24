package grit.mcp.searchconsole

import grit.core.visibility.Integrity

import utest.*

/** What [[SiteUrl]] and [[Filter.Expression]] take and refuse, and what a grouping's rows are
  * believed at.
  */
object ConsoleTypesTests extends TestSuite {

  val tests = Tests {
    test(
      "a domain property or an http(s) prefix ending in / is a site, and anything else is refused, saying which form it lacks"
    ) {
      Vector(
        "sc-domain:example.com",
        " https://www.example.com/ ",
        "http://www.example.com/shop/",
        "sc-domain:",
        "sc-domain:example.com/",
        "https://www.example.com",
        "https:///",
        "example.com"
      ).map(SiteUrl.of(_).map(SiteUrl.value)) ==> Vector(
        Right("sc-domain:example.com"),
        Right("https://www.example.com/"),
        Right("http://www.example.com/shop/"),
        Left("sc-domain: is not sc-domain: and a bare host"),
        Left("sc-domain:example.com/ is not sc-domain: and a bare host"),
        Left("https://www.example.com does not end in /, as a URL-prefix property does"),
        Left("https:/// names no host"),
        Left("example.com is neither sc-domain: and a host nor an http(s) URL ending in /")
      )
    }

    test("query and page rows are believed at Outside, the others at Member") {
      Grouping.values.toVector.map(g => g -> g.integrity) ==> Vector(
        Grouping.Query -> Integrity.Outside,
        Grouping.Page -> Integrity.Outside,
        Grouping.Country -> Integrity.Member,
        Grouping.Device -> Integrity.Member,
        Grouping.Date -> Integrity.Member,
        Grouping.Hour -> Integrity.Member
      )
    }

    test("an expression empty or over 4096 characters is refused") {
      Vector("", "a" * 4096, "a" * 4097).map(
        Filter.Expression.of(_).map(Filter.Expression.value(_).length)
      ) ==> Vector(
        Left("A filter needs some text to match."),
        Right(4096),
        Left("A filter's text is at most 4096 characters; this is 4097.")
      )
    }
  }
}
