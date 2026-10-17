package grit.mcp.github

import grit.core.place.{Place, Service}
import grit.core.visibility.{Accounted, Integrity, Itemised, Provenance}
import grit.mcp.client.FakeMcpServer
import grit.mcp.wire.McpCalled

import utest.*

/** [[GitHubItems]] over GitHub's recorded answers (extensions/mcp/README.md). */
object GitHubItemsTests extends TestSuite {

  private val Github: Service = Service.of("github").getOrElse(throw new java.lang.AssertionError())

  private val Read = GitHubItems.accounts(Github)

  /** octocat/Hello-World's place, where every recording but the searches' second result lies. */
  private val Hello = Place
    .read("service:github/octocat/hello-world")
    .fold(why => throw new java.lang.AssertionError(why), identity)

  private val Elsewhere = Place
    .read("service:github/elsewhere-org/elsewhere")
    .fold(why => throw new java.lang.AssertionError(why), identity)

  /** The call's `owner` and `repo`, and `more`. */
  private def asked(owner: String, repo: String, more: (String, ujson.Value)*): ujson.Obj =
    ujson.Obj.from(Vector("owner" -> ujson.Str(owner), "repo" -> ujson.Str(repo)) ++ more)

  private val AtHello = asked("octocat", "Hello-World")

  /** `called`'s items, failing the test unless it is read whole, as every GitHub answer is. */
  private def whole(called: McpCalled): Accounted = Read.items(called) match {
    case Itemised.Whole(items) => items
    case other =>
      throw new java.lang.AssertionError(s"${called.tool}'s answer is not whole: $other")
  }

  /** `tool` called with `arguments`, answered as the recording `answer`: its items. */
  private def items(tool: String, arguments: ujson.Obj, answer: String): Accounted =
    whole(McpCalled(tool, arguments, FakeMcpServer.githubAnswer(answer)))

  private def method(m: String): ujson.Obj = asked("octocat", "Hello-World", "method" -> m)

  private def at(p: Place, level: Integrity): Accounted = Accounted.of(Vector(Provenance(p, level)))

  private def text(t: String): ujson.Obj =
    ujson.Obj("content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> t)))

  val tests = Tests {
    test("a repository is placed under the service, lowercased; one not owner/name is refused") {
      (
        GitHubItems.repository(Github, "Octocat/Hello-World"),
        GitHubItems.owner(Github, "Octocat"),
        Vector("widgets", "a/b/c", " /x", "o/").map(GitHubItems.repository(Github, _)),
        Vector("", " ", "a/b").map(GitHubItems.owner(Github, _))
      ) ==> (
        Right(Hello),
        Place.read("service:github/octocat"),
        Vector("widgets", "a/b/c", " /x", "o/").map(n =>
          Left(s"a repository is {owner}/{name}, not '$n'")
        ),
        Vector("", " ", "a/b").map(n => Left(s"an owner is a login with no /, not '$n'"))
      )
    }

    test(
      "an author's association: owner, member and collaborator at member, any other at outside"
    ) {
      Association.values.toVector.map(a => (a.written, Association.of(a.written).map(_.level))) ==>
        Vector(
          "OWNER" -> Some(Integrity.Member),
          "MEMBER" -> Some(Integrity.Member),
          "COLLABORATOR" -> Some(Integrity.Member),
          "CONTRIBUTOR" -> Some(Integrity.Outside),
          "FIRST_TIME_CONTRIBUTOR" -> Some(Integrity.Outside),
          "FIRST_TIMER" -> Some(Integrity.Outside),
          "MANNEQUIN" -> Some(Integrity.Outside),
          "NONE" -> Some(Integrity.Outside)
        )
      Association.of("member") ==> None
    }

    test("an issue is its author's, by association") {
      items("issue_read", method("get"), "issue-read") ==> at(Hello, Integrity.Outside)
    }

    test("a bot's comment is untrusted, whatever its association; a collaborator's is member") {
      // Both bot comments are CONTRIBUTOR, which is outside for a person.
      items("issue_read", method("get_comments"), "issue-read-comments") ==> Accounted.of(
        Vector(Provenance(Hello, Integrity.Untrusted), Provenance(Hello, Integrity.Member))
      )
    }

    test("an issue's labels, and a pull request's diff, files and status, are untrusted") {
      Vector(
        items("issue_read", method("get_labels"), "issue-read-labels"),
        items("pull_request_read", method("get_diff"), "pull-request-read-diff"),
        items("pull_request_read", method("get_files"), "pull-request-read-files"),
        items("pull_request_read", method("get_status"), "pull-request-read-status")
      ) ==> Vector.fill(4)(at(Hello, Integrity.Untrusted))
    }

    test("a pull request, which names no association, is untrusted") {
      items("pull_request_read", method("get"), "pull-request-read") ==>
        at(Hello, Integrity.Untrusted)
    }

    test("a pull request's comments and reviews are their authors', a bot's untrusted") {
      (
        items("pull_request_read", method("get_comments"), "pull-request-read-comments"),
        items("pull_request_read", method("get_reviews"), "pull-request-read-reviews")
      ) ==> (
        at(Hello, Integrity.Outside),
        Accounted.of(
          Vector(Provenance(Hello, Integrity.Untrusted), Provenance(Hello, Integrity.Outside))
        )
      )
    }

    test("review comments, which name no association, are untrusted") {
      items(
        "pull_request_read",
        method("get_review_comments"),
        "pull-request-read-review-comments"
      ) ==> at(Hello, Integrity.Untrusted)
    }

    test("the issue and pull-request lists, which name no association, are untrusted") {
      (
        items("list_issues", AtHello, "list-issues"),
        items("list_pull_requests", AtHello, "list-pull-requests")
      ) ==> (at(Hello, Integrity.Untrusted), at(Hello, Integrity.Untrusted))
    }

    test("a search's results are each their author's, at the repository each names") {
      Vector("search_issues", "search_pull_requests").map(tool =>
        whole(McpCalled(tool, ujson.Obj("query" -> "x"), FakeMcpServer.githubSearch(tool)))
      ) ==> Vector.fill(2)(
        Accounted.of(
          Vector(Provenance(Hello, Integrity.Member), Provenance(Elsewhere, Integrity.Member))
        )
      )
    }

    test("a file read on the default branch, with no ref or sha, is member") {
      items("get_file_contents", asked("o", "r", "path" -> "README.md"), "get-file-contents") ==>
        at(Hello, Integrity.Member)
    }

    test("a file read at a branch or tag is member: the repository's own writing") {
      Vector("main", "a-branch", "v1.2.0", "refs/heads/main", "refs/tags/v1", "abc123")
        .map(r =>
          items(
            "get_file_contents",
            asked("o", "r", "path" -> "README.md", "ref" -> ujson.Str(r)),
            "get-file-contents"
          )
        ) ==> Vector.fill(6)(at(Hello, Integrity.Member))
    }

    test("a file read at a sha, or at a pull request's ref, is outside") {
      val sha = "0123456789abcdef0123456789ABCDEF01234567"
      Vector(
        "sha" -> ujson.Str("0a"),
        "ref" -> ujson.Str(sha),
        "ref" -> ujson.Str("abc1234"),
        "ref" -> ujson.Str("refs/pull/12/head"),
        "ref" -> ujson.Str("pull/12/merge")
      ).map(r =>
        items("get_file_contents", asked("o", "r", "path" -> "README.md", r), "get-file-contents")
      ) ==> Vector.fill(5)(at(Hello, Integrity.Outside))
    }

    test("an item is placed at the repository its answer names, not the one the call asked for") {
      items(
        "issue_read",
        asked("elsewhere-org", "elsewhere", "method" -> "get"),
        "issue-read"
      ) ==> at(Hello, Integrity.Outside)
    }

    test("an answer that does not parse is unaccountable") {
      val issue = FakeMcpServer.githubIssue
      val renamed = ujson.read(issue("content")(0)("text").str)
      renamed("issue")("html_url") = "https://example.com/issue/7"
      Vector(
        whole(McpCalled("issue_read", method("get"), text("not JSON"))),
        whole(McpCalled("issue_read", method("get"), text(ujson.write(renamed)))),
        whole(McpCalled("issue_read", method("get_everything"), issue)),
        whole(McpCalled("issue_read", ujson.Obj("method" -> "get_labels"), text("[]")))
      ) ==> Vector.fill(4)(Accounted.Unaccountable)
    }

    test("a tool it does not cover is unread") {
      Vector("search_code", "list_commits", "get_commit", "get_me").map(tool =>
        whole(McpCalled(tool, AtHello, FakeMcpServer.githubSearch("search_code")))
      ) ==> Vector.fill(4)(Accounted.Unread)
    }
  }
}
