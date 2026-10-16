package grit.mcp.github

import grit.core.place.{Place, Service}
import grit.core.visibility.{Accounted, Integrity, Provenance}
import grit.mcp.wire.McpCalled

import utest.*

/** What [[GitHubItems]] needs of GitHub's answers, and what it makes of them: the fields each
  * answer names its items' authors and repositories by. The recordings keep it in the unit tier
  * ([[GitHubItemsRecordedTests]]); `LiveProbe` runs it against GitHub's server, so the
  * recordings are held to what the server answers as of that run.
  */
trait GitHubItemsContract extends TestSuite {

  /** The repository every call reads, `{owner}/{name}`. */
  protected def repository: String

  /** An issue of [[repository]] with at least one comment. */
  protected def issue: Int

  /** A pull request of [[repository]] with at least one review. */
  protected def pull: Int

  /** GitHub's result to `tool` called with `arguments`. */
  protected def answer(tool: String, arguments: ujson.Obj): ujson.Obj

  private val Github: Service = Service.of("github").getOrElse(throw new java.lang.AssertionError())

  private def place: Place =
    GitHubItems
      .repository(Github, repository)
      .fold(why => throw new java.lang.AssertionError(why), identity)

  private def arguments(more: (String, ujson.Value)*): ujson.Obj = {
    val (owner, name) = repository.span(_ != '/')
    ujson.Obj.from(Vector("owner" -> ujson.Str(owner), "repo" -> ujson.Str(name.drop(1))) ++ more)
  }

  /** `tool` called with `args`: the JSON its first text block holds (null when none does), and
    * its items.
    */
  private def read(tool: String, args: ujson.Obj): (ujson.Value, Accounted) = {
    val result = answer(tool, args)
    val text = result("content").arr.find(_("type").str == "text").map(_("text").str)
    (
      text.flatMap(t => scala.util.Try(ujson.read(t)).toOption).getOrElse(ujson.Null),
      GitHubItems.accounts(Github).items(McpCalled(tool, args, result))
    )
  }

  /** Whether `item` names an association GitHub writes and a URL in [[repository]]. */
  private def named(item: ujson.Value): Boolean =
    item.obj.get("author_association").flatMap(_.strOpt).flatMap(Association.of).isDefined &&
      item.obj
        .get("html_url")
        .flatMap(_.strOpt)
        .exists(_.toLowerCase.startsWith(s"https://github.com/${repository.toLowerCase}/"))

  /** Whether `items` are read, each at [[repository]]'s place. */
  private def placed(items: Accounted): Boolean = items match {
    case Accounted.Items(found) => found.nonEmpty && found.forall(_.place == place)
    case _ => false
  }

  val tests = Tests {
    test("an issue names its author's association, and its URL its repository") {
      val (body, items) = read("issue_read", arguments("method" -> "get", "issue_number" -> issue))
      assert(named(body("issue")), placed(items))
    }

    test("each of an issue's comments names its author's association and its URL its repository") {
      val (body, items) =
        read("issue_read", arguments("method" -> "get_comments", "issue_number" -> issue))
      val comments = body("comments").arr.toVector
      assert(comments.nonEmpty, comments.forall(named), placed(items))
    }

    test(
      "each of a pull request's reviews names its author's association and its URL its repository"
    ) {
      val (body, items) =
        read("pull_request_read", arguments("method" -> "get_reviews", "pullNumber" -> pull))
      val reviews = body.arr.toVector
      assert(reviews.nonEmpty, reviews.forall(named), placed(items))
    }

    test("a pull request names no association, so it is untrusted") {
      val (body, items) =
        read("pull_request_read", arguments("method" -> "get", "pullNumber" -> pull))
      (body.obj.contains("author_association"), items) ==>
        (false, Accounted.of(Vector(Provenance(place, Integrity.Untrusted))))
    }

    test("a file on the default branch names its repository in its resource's URI, at member") {
      val (_, items) = read("get_file_contents", arguments("path" -> "README.md"))
      items ==> Accounted.of(Vector(Provenance(place, Integrity.Member)))
    }
  }
}
