package grit.mcp.github

import grit.mcp.client.FakeMcpServer
import grit.mcp.scope.{Bound, McpScope}
import grit.mcp.wire.McpTool

import utest.*

/** [[GitHubScope]]: a repository's and an owner's bounds as GitHub's tools name them, and its
  * scope over the tools and the search answers GitHub's read-only server gives.
  */
object GitHubScopeTests extends TestSuite {

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

  private def listed(names: Vector[String]): Vector[McpTool] =
    McpTool
      .page(ujson.Obj("tools" -> ujson.Arr.from(names.map(FakeMcpServer.github))), "github")
      .fold(e => throw new java.lang.AssertionError(e.message), _.tools)

  private def tool(name: String): McpTool =
    listed(Vector(name)).headOption.getOrElse(throw new java.lang.AssertionError(name))

  private def right[A](either: Either[String, A]): A =
    either.fold(why => throw new java.lang.AssertionError(why), identity)

  /** Within `octocat/Hello-World`, the repository the captured answers' kept results are in. */
  private val HelloWorld: McpScope =
    right(GitHubScope.repository("octocat/Hello-World").flatMap(GitHubScope.within(_)))

  /** `result`'s one text block's JSON. */
  private def payload(result: ujson.Obj): ujson.Obj =
    ujson.Obj.from(ujson.read(result("content")(0)("text").str).obj)

  /** `result` with its search JSON changed by `change`. */
  private def changed(result: ujson.Obj)(change: ujson.Obj => Unit): ujson.Obj = {
    val p = payload(result)
    change(p)
    val copy = ujson.Obj.from(result.obj)
    copy("content") = ujson.Arr(ujson.Obj("type" -> "text", "text" -> ujson.write(p)))
    copy
  }

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

    test("within a repository, every one of Bort's tools is offered") {
      listed(Allowed).flatMap(HelloWorld.excludes) ==> Vector.empty
    }

    test(
      "a captured search answer is shown with exactly its results in the repository, its " +
        "total_count those kept, and a block saying how many were withheld"
    ) {
      val withheld = "Withheld as outside this server's scope (owner octocat with repo Hello-World)"
      val url = "https://api.github.com/repos/octocat/Hello-World"
      def shown(name: String): Either[String, (Int, String, Option[String])] =
        HelloWorld.shown(tool(name), FakeMcpServer.githubSearch(name)).map { r =>
          (
            payload(r)("items").arr.size,
            ujson.write(r("content")(0)),
            r("content").arr.lift(1).map(_("text").str)
          )
        }

      /** `name`'s captured answer's text block with only the results `in` the repository. */
      def only(name: String, in: ujson.Value => Boolean): String = {
        val kept = changed(FakeMcpServer.githubSearch(name)) { p =>
          val items = p("items").arr.filter(in)
          p("items") = items
          p("total_count") = items.size
        }
        ujson.write(kept("content")(0))
      }
      Vector("search_issues", "search_pull_requests", "search_code").map(shown) ==> Vector(
        Right(
          (
            2,
            only("search_issues", _("repository_url").str == url),
            Some(s"$withheld: 1 of the 3 results in this answer.")
          )
        ),
        Right(
          (
            2,
            only("search_pull_requests", _("repository_url").str == url),
            Some(s"$withheld: 1 of the 3 results in this answer.")
          )
        ),
        Right(
          (
            1,
            only("search_code", _("repository").str == "octocat/Hello-World"),
            Some(s"$withheld: 1 of the 2 results in this answer.")
          )
        )
      )
    }

    test("a search answer whose results do not name their repository as GitHub does is not shown") {
      def without(name: String, key: String): ujson.Obj =
        changed(FakeMcpServer.githubSearch(name))(_("items").arr.foreach(_.obj.remove(key)))
      def set(name: String, key: String, value: String): ujson.Obj =
        changed(FakeMcpServer.githubSearch(name))(_("items")(0)(key) = value)
      Vector(
        // what a `fields` naming no repository field would answer
        "search_issues" -> without("search_issues", "repository_url"),
        "search_code" -> without("search_code", "repository"),
        "search_pull_requests" -> set(
          "search_pull_requests",
          "repository_url",
          "https://github.com/octocat/Hello-World"
        ),
        "search_code" -> set("search_code", "repository", "octocat")
      ).map((name, sent) => HelloWorld.shown(tool(name), sent)) ==> Vector(
        Left("a result's place cannot be read: it has no repository_url string"),
        Left("a result's place cannot be read: it has no repository string"),
        Left(
          "a result's place cannot be read: its repository_url " +
            "https://github.com/octocat/Hello-World is not " +
            "https://api.github.com/repos/{owner}/{name}"
        ),
        Left("a result's place cannot be read: its repository octocat is not {owner}/{name}")
      )
    }

    test(
      "an issue search is sent only with the repository's owner and repo, and never with fields"
    ) {
      val issues = tool("search_issues")
      Vector(
        HelloWorld.request(
          issues,
          ujson.Obj(
            "query" -> "bug",
            "owner" -> "octocat",
            "repo" -> "Hello-World",
            "fields" -> ujson.Arr("title")
          )
        ),
        HelloWorld.request(issues, ujson.Obj("query" -> "bug")),
        HelloWorld.request(
          tool("search_code"),
          ujson.Obj("query" -> "bug", "fields" -> ujson.Arr())
        )
      ) ==> Vector(
        Right(ujson.Obj("query" -> "bug", "owner" -> "octocat", "repo" -> "Hello-World")),
        Left(
          "its calls are limited to owner octocat with repo Hello-World, " +
            "and this one has no owner with no repo"
        ),
        Right(ujson.Obj("query" -> "bug"))
      )
    }

    test("a bound on an argument other than owner and repo is refused") {
      Bound.of("team" -> "core").flatMap(GitHubScope.within(_)) ==>
        Left("search_code's results are placed by owner, repo, never by a bound's team")
    }
  }
}
