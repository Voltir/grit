package grit.app.config

import grit.core.identity.Domain

import utest.*

/** The domains the reference deployment claims, as `GRIT_CLAIMED_DOMAINS` sets them. */
object ClaimedTests extends TestSuite {

  private def domain(text: String): Domain = Domain.of(text).fold(sys.error, identity)

  val tests = Tests {
    test("a comma-separated list is its domains, each trimmed and lowercased") {
      Claimed.fromEnv(Map(Claimed.DomainsVar -> "example.com, Example.org")) ==>
        Right(Set(domain("example.com"), domain("example.org")))
    }

    test("unset or blank claims none") {
      (Claimed.fromEnv(Map.empty), Claimed.fromEnv(Map(Claimed.DomainsVar -> "  "))) ==>
        (Right(Set.empty), Right(Set.empty))
    }

    test("a malformed domain is refused, naming the variable and the domain") {
      Claimed.fromEnv(Map(Claimed.DomainsVar -> "example.com, @bad")) ==>
        Left("GRIT_CLAIMED_DOMAINS: a domain holds no @ or whitespace: @bad")
    }
  }
}
