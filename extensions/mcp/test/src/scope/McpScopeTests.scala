package grit.mcp.scope

import scala.util.chaining.*

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

  /** Results under `results`, counted at `count`, each placed by its `where`, `{owner}/{repo}`;
    * `fields` never sent.
    */
  private val Where: Attribution = Attribution
    .of(
      "results",
      Some("count"),
      Vector("owner", "repo"),
      Set("fields"),
      result =>
        result.objOpt
          .flatMap(_.get("where"))
          .flatMap(_.strOpt)
          .map(_.split("/", -1).toVector)
          .toRight("it has no where")
    )
    .fold(why => throw new java.lang.AssertionError(why), identity)

  /** Within [[Repository]], `search_issues` and `search_code` attributed by [[Where]]. */
  private val Searched: McpScope =
    McpScope
      .attributed(Map("search_issues" -> Where, "search_code" -> Where), Repository)
      .fold(why => throw new java.lang.AssertionError(why), identity)

  /** A `tools/call` result whose one text block is `payload`, written compactly. */
  private def answer(payload: ujson.Value): ujson.Obj =
    ujson.Obj(
      "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> ujson.write(payload))),
      "resultType" -> "complete"
    )

  private def result(where: String, title: String): ujson.Obj =
    ujson.Obj("title" -> title, "where" -> where)

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

    test("a tool whose answers are attributed is offered within any bound") {
      Vector("search_issues", "search_code", "search_pull_requests").map(n =>
        Searched.excludes(tool(n))
      ) ==> Vector(None, None, Some(Skipped.OutOfScope("search_pull_requests")))
    }

    test(
      "an attributed call is sent less its unsent arguments when the bound's arguments its " +
        "schema declares are equal, and refused otherwise"
    ) {
      val issues = tool("search_issues")
      Vector(
        Searched.request(
          issues,
          ujson.Obj(
            "query" -> "x",
            "owner" -> "the-actual-best",
            "repo" -> "actualbest",
            "fields" -> ujson.Arr("title")
          )
        ),
        Searched.request(issues, ujson.Obj("query" -> "x", "owner" -> "the-actual-best")),
        // search_code declares neither owner nor repo: its answers alone hold it
        Searched.request(tool("search_code"), ujson.Obj("query" -> "x", "fields" -> ujson.Arr()))
      ) ==> Vector(
        Right(ujson.Obj("query" -> "x", "owner" -> "the-actual-best", "repo" -> "actualbest")),
        Left(
          "its calls are limited to owner the-actual-best with repo actualbest, " +
            "and this one has owner the-actual-best with no repo"
        ),
        Right(ujson.Obj("query" -> "x"))
      )
    }

    test(
      "an attributed answer is shown with only its results within a bound, its count the kept, " +
        "and a block saying how many were withheld"
    ) {
      val sent = answer(
        ujson.Obj(
          "count" -> 40,
          "more" -> true,
          "results" -> ujson.Arr(
            result("the-actual-best/actualbest", "in"),
            result("elsewhere/actualbest", "out"),
            result("the-actual-best/other", "out too"),
            result("the-actual-best/actualbest", "in too")
          )
        )
      )
      Searched.shown(tool("search_issues"), sent) ==> Right(
        ujson.Obj(
          "content" -> ujson.Arr(
            ujson.Obj(
              "type" -> "text",
              "text" -> ujson.write(
                ujson.Obj(
                  "count" -> 2,
                  "more" -> true,
                  "results" -> ujson.Arr(
                    result("the-actual-best/actualbest", "in"),
                    result("the-actual-best/actualbest", "in too")
                  )
                )
              )
            ),
            ujson.Obj(
              "type" -> "text",
              "text" -> ("Withheld as outside this server's scope (owner the-actual-best with " +
                "repo actualbest): 2 of the 4 results in this answer.")
            )
          ),
          "resultType" -> "complete"
        )
      )
    }

    test("an attributed answer with every result within a bound is shown as sent, recounted") {
      val sent = answer(
        ujson.Obj("count" -> 9, "results" -> ujson.Arr(result("the-actual-best/actualbest", "a")))
      )
      Searched.shown(tool("search_code"), sent) ==> Right(
        answer(
          ujson.Obj("count" -> 1, "results" -> ujson.Arr(result("the-actual-best/actualbest", "a")))
        )
      )
    }

    test("an error answer, or the answer of a tool not attributed, is shown as it is") {
      val error = ujson.Obj(
        "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> "rate limited")),
        "isError" -> true,
        "structuredContent" -> ujson.Obj("why" -> "rate")
      )
      (
        Searched.shown(tool("search_issues"), error),
        Searched.shown(tool("get_file_contents"), FakeMcpServer.githubFileContents)
      ) ==> (Right(error), Right(FakeMcpServer.githubFileContents))
    }

    test("an attributed answer it cannot hold to its bounds is not shown, saying why") {
      val one = ujson.Obj("results" -> ujson.Arr(result("the-actual-best/actualbest", "in")))
      val text = ujson.Obj("type" -> "text", "text" -> ujson.write(one))
      def content(blocks: ujson.Value*): ujson.Obj = ujson.Obj("content" -> ujson.Arr(blocks*))
      Vector(
        answer(one).tap(_("structuredContent") = one),
        content(text, text),
        content(ujson.Obj("type" -> "image", "data" -> "", "mimeType" -> "image/png")),
        content(ujson.Obj("type" -> "text", "text" -> "no results")),
        answer(ujson.Arr(one)),
        answer(ujson.Obj("results" -> one)),
        answer(ujson.Obj("results" -> ujson.Arr(ujson.Obj("title" -> "nowhere")))),
        answer(ujson.Obj("results" -> ujson.Arr(result("the-actual-best", "one segment"))))
      ).map(Searched.shown(tool("search_issues"), _)) ==> Vector(
        Left("its answer has structuredContent, which grit cannot hold to its scope"),
        Left("its answer is not one text block"),
        Left("its answer is not one text block"),
        Left("its answer's text is not a JSON object"),
        Left("its answer's text is not a JSON object"),
        Left("its answer's text has no results array"),
        Left("a result's place cannot be read: it has no where"),
        Left("a result's place has 1 value, not one for each of owner, repo")
      )
    }

    test(
      "an attribution whose keys leave out a bound's argument, or that is malformed, is refused"
    ) {
      def of(items: String, keys: Vector[String]): Either[String, Attribution] =
        Attribution.of(items, None, keys, Set.empty, _ => Left("unread"))
      (
        McpScope.attributed(Map("search_issues" -> Where), bound("team" -> "core")),
        Vector(
          of(" ", Vector("owner")),
          of("items", Vector.empty),
          of("items", Vector("owner", " ")),
          of("items", Vector("owner", "owner"))
        )
      ) ==> (
        Left("search_issues's results are placed by owner, repo, never by a bound's team"),
        Vector(
          Left("an attribution's items is blank"),
          Left("an attribution names no key"),
          Left("an attribution's key is blank"),
          Left("an attribution names owner twice")
        )
      )
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
