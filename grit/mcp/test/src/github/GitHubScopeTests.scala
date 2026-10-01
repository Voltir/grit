package grit.mcp.github

import grit.mcp.scope.Bound

import utest.*

/** [[GitHubScope]]: a repository's and an owner's bounds as GitHub's tools name them. */
object GitHubScopeTests extends TestSuite {

  val tests = Tests {
    test("a repository is a bound on its owner and name; an owner, on owner alone") {
      (GitHubScope.repository("the-actual-best/actualbest"), GitHubScope.owner("octocat")) ==> (
        Bound.of("owner" -> "the-actual-best", "repo" -> "actualbest"),
        Bound.of("owner" -> "octocat")
      )
    }

    test("a repository not two non-blank segments, or an owner blank or with /, is refused") {
      (
        Vector("actualbest", "a/b/c", "/actualbest", "the-actual-best/", " / ")
          .map(GitHubScope.repository),
        Vector("", "the-actual-best/actualbest").map(GitHubScope.owner)
      ) ==> (
        Vector(
          Left("a repository is {owner}/{name}, not 'actualbest'"),
          Left("a repository is {owner}/{name}, not 'a/b/c'"),
          Left("a repository is {owner}/{name}, not '/actualbest'"),
          Left("a repository is {owner}/{name}, not 'the-actual-best/'"),
          Left("a repository is {owner}/{name}, not ' / '")
        ),
        Vector(
          Left("an owner is a login with no /, not ''"),
          Left("an owner is a login with no /, not 'the-actual-best/actualbest'")
        )
      )
    }
  }
}
