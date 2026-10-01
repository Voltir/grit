package grit.mcp.scope

import grit.mcp.client.FakeMcpServer
import grit.mcp.wire.{McpTool, Skipped}

import utest.*

/** [[McpScope]] and [[Bound]] over the entries GitHub's read-only server lists. */
object McpScopeTests extends TestSuite {

  /** The GitHub tools Bort allows, as GitHub's server lists them. */
  private val Allowed: Vector[String] = Vector(
    "get_file_contents",
    "list_commits",
    "get_commit",
    "search_code",
    "issue_read",
    "list_issues",
    "search_issues",
    "pull_request_read",
    "list_pull_requests",
    "search_pull_requests"
  )

  /** `names`' entries from GitHub's list, read as grit offers them. */
  private def listed(names: Vector[String]): Vector[McpTool] =
    McpTool
      .page(ujson.Obj("tools" -> ujson.Arr.from(names.map(FakeMcpServer.github))), "github")
      .fold(e => throw new java.lang.AssertionError(e.message), _.tools)

  private def tool(name: String): McpTool =
    listed(Vector(name)).headOption.getOrElse(throw new java.lang.AssertionError(name))

  private def bound(first: (String, String), rest: (String, String)*): Bound =
    Bound.of(first, rest*).fold(why => throw new java.lang.AssertionError(why), identity)

  private val Repository = bound("owner" -> "the-actual-best", "repo" -> "actualbest")

  val tests = Tests {
    test("within a repository, of Bort's tools exactly the three searches are not offered") {
      val scope = McpScope.within(Repository)
      listed(Allowed).flatMap(scope.excludes) ==> Vector(
        Skipped.OutOfScope("search_code"),
        Skipped.OutOfScope("search_issues"),
        Skipped.OutOfScope("search_pull_requests")
      )
    }

    test(
      "within an owner, a tool requiring the owner alone is offered; one requiring no owner is not"
    ) {
      val scope = McpScope.within(bound("owner" -> "the-actual-best"))
      listed(Vector("list_issue_types", "get_team_members", "get_me")).map(t =>
        (t.name, scope.excludes(t))
      ) ==> Vector(
        ("list_issue_types", None),
        ("get_team_members", Some(Skipped.OutOfScope("get_team_members"))),
        ("get_me", Some(Skipped.OutOfScope("get_me")))
      )
    }

    test("a call within a bound is sent as given; one outside it is refused, naming both") {
      val scope = McpScope.within(Repository)
      val read = tool("get_file_contents")
      def call(owner: ujson.Value, repo: Option[ujson.Value]): Either[String, ujson.Obj] =
        scope.request(
          read,
          ujson.Obj.from(
            Vector("owner" -> owner, "path" -> ujson.Str("README.md")) ++
              repo.map("repo" -> _)
          )
        )
      val limited = "its calls are limited to owner the-actual-best with repo actualbest"
      Vector(
        call("the-actual-best", Some("actualbest")),
        call("actualbest", Some("actualbest")),
        call("The-Actual-Best", Some("actualbest")),
        call("the-actual-best", None),
        call("the-actual-best", Some(ujson.Arr("actualbest")))
      ) ==> Vector(
        Right(
          ujson.Obj("owner" -> "the-actual-best", "path" -> "README.md", "repo" -> "actualbest")
        ),
        Left(s"$limited, and this one has owner actualbest with repo actualbest"),
        Left(s"$limited, and this one has owner The-Actual-Best with repo actualbest"),
        Left(s"$limited, and this one has owner the-actual-best with no repo"),
        Left(s"$limited, and this one has owner the-actual-best with repo [\"actualbest\"]")
      )
    }

    test("a call is sent when any bound its tool requires admits it, and only those are named") {
      val scope = McpScope.within(Repository, bound("owner" -> "octocat"))
      val types = tool("list_issue_types")
      Vector(
        scope.request(tool("list_commits"), ujson.Obj("owner" -> "octocat", "repo" -> "x")),
        scope.request(types, ujson.Obj("owner" -> "octocat")),
        // the repository bound does not hold list_issue_types, which does not require repo
        scope.request(types, ujson.Obj("owner" -> "the-actual-best", "repo" -> "actualbest"))
      ) ==> Vector(
        Right(ujson.Obj("owner" -> "octocat", "repo" -> "x")),
        Right(ujson.Obj("owner" -> "octocat")),
        Left(
          "its calls are limited to owner octocat, and this one has owner the-actual-best"
        )
      )
    }

    test("a call of a tool the scope does not offer is refused, its arguments in bounds or not") {
      McpScope
        .within(Repository)
        .request(
          tool("search_issues"),
          ujson.Obj("query" -> "x", "owner" -> "the-actual-best", "repo" -> "actualbest")
        ) ==> Left("its scope does not offer search_issues")
    }

    test("a bound naming an argument twice, or with a blank name or value, is refused") {
      Vector(
        Bound.of("owner" -> "a", "owner" -> "b"),
        Bound.of(" " -> "a"),
        Bound.of("owner" -> "")
      ) ==> Vector(
        Left("a bound names owner twice"),
        Left("a bound's argument has a blank name"),
        Left("a bound's owner is blank")
      )
    }
  }
}
