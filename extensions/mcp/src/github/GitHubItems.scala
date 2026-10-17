package grit.mcp.github

import scala.util.Try

import grit.core.place.{Namespace, Place, Service}
import grit.core.visibility.{Accounted, Accounts, Integrity, Itemised, Provenance}
import grit.mcp.wire.McpCalled

/** How GitHub's results are believed (ADR 0036): repositories and owners as places under the
  * service's, and the items of its read tools by their authors.
  */
object GitHubItems {

  /** `fullName`, `{owner}/{name}`, as the place under `service`'s that holds its items,
    * lowercased as GitHub's names are case-insensitive; or why not: not two non-blank segments.
    */
  def repository(service: Service, fullName: String): Either[String, Place] =
    GitHubScope
      .named(fullName)
      .map((owner, name) => under(service, Vector(owner, name)))

  /** `login`'s place under `service`'s, holding every repository it owns, lowercased; or why
    * not: blank or holding `/`.
    */
  def owner(service: Service, login: String): Either[String, Place] =
    if (login.isBlank || login.contains('/')) Left(s"an owner is a login with no /, not '$login'")
    else Right(under(service, Vector(login)))

  /** The items of GitHub's issue, pull-request and file tools at `service`, each at the place of
    * the repository its answer names (the call's `owner` and `repo` when it names none).
    *   - An issue, a pull request, and each comment and review, by its author's
    *     `author_association` ([[Association.level]]); untrusted when it names none (a pull
    *     request, the issue and pull-request lists, review comments) or when its author is a bot
    *     (a login ending `[bot]`, or a `type` of `Bot`).
    *   - A pull request's diff, files or status, and an issue's labels: untrusted.
    *   - A file read on the default branch, or at a `ref` naming a branch or tag, at member: only
    *     the repository's writers push those. One read at a `sha`, or at a `ref` of 7 to 40 hex
    *     digits or starting `refs/pull/` or `pull/`, at outside: a fork's commit or a pull
    *     request's head may be reached through the repository.
    *
    * Any other tool's (code search and commits among them) are unread. An answer that does not
    * read as its tool's, a method of these tools not named here, or a place it cannot read, is
    * unaccountable. Every answer is shown whole ([[Itemised.Whole]]).
    */
  def accounts(service: Service): Accounts[McpCalled] = new Accounts[McpCalled] {
    def items(result: McpCalled): Itemised = Itemised.Whole(GitHubItems.items(service, result))
  }

  private def under(service: Service, segments: Vector[String]): Place =
    Place.under(Namespace.Service, service.name +: segments.map(_.toLowerCase))

  /** Where a result's `html_url` and a file's `uri` name their repository. */
  private val Web = "https://github.com/"
  private val Repo = "repo://"

  /** One item: where it lies and what its author is vouched at, or why its place is unread. */
  private type Read = Either[String, Provenance]

  private def items(service: Service, c: McpCalled): Accounted = {
    val method = c.arguments.obj.get("method").flatMap(_.strOpt)
    def parsed(pick: ujson.Value => Option[Vector[ujson.Value]]): Accounted =
      json(c.result).flatMap(pick) match {
        case None => Accounted.Unaccountable
        case Some(found) => all(found.map(authored(service, c, _)))
      }
    def asked(level: Integrity): Accounted = all(
      Vector(called(service, c).map(Provenance(_, level)))
    )
    def at(key: String): ujson.Value => Option[Vector[ujson.Value]] =
      _.objOpt.flatMap(_.get(key)).flatMap(_.arrOpt).map(_.toVector)
    val one: ujson.Value => Option[Vector[ujson.Value]] = v => v.objOpt.map(_ => Vector(v))
    val each: ujson.Value => Option[Vector[ujson.Value]] = _.arrOpt.map(_.toVector)
    (c.tool, method) match {
      case ("issue_read", Some("get")) => parsed(_.objOpt.flatMap(_.get("issue")).flatMap(one))
      case ("issue_read", Some("get_comments")) => parsed(at("comments"))
      case ("issue_read", Some("get_sub_issues")) => parsed(at("sub_issues"))
      case ("issue_read", Some("get_labels")) => asked(Integrity.Untrusted)
      case ("pull_request_read", Some("get")) => parsed(one)
      case ("pull_request_read", Some("get_comments" | "get_reviews")) => parsed(each)
      case ("pull_request_read", Some("get_review_comments")) =>
        parsed(
          at("review_threads")(_).flatMap(threads =>
            threads.foldLeft(Option(Vector.empty[ujson.Value]))((kept, thread) =>
              kept.flatMap(k => at("comments")(thread).map(k ++ _))
            )
          )
        )
      case ("pull_request_read", Some("get_diff" | "get_files" | "get_status")) =>
        asked(Integrity.Untrusted)
      case ("issue_read" | "pull_request_read", _) => Accounted.Unaccountable
      case ("list_issues", _) => parsed(at("issues"))
      case ("list_pull_requests", _) => parsed(each)
      case ("search_issues" | "search_pull_requests", _) => parsed(at("items"))
      case ("get_file_contents", _) =>
        val args = c.arguments.obj
        val foreign = args.contains("sha") || args.get("ref").exists(_.strOpt.forall(outsiders))
        val level = if (foreign) Integrity.Outside else Integrity.Member
        all(Vector(file(service, c).map(Provenance(_, level))))
      case _ => Accounted.Unread
    }
  }

  /** Whether a file's `ref` may name a commit the repository's writers did not push: a sha, or a
    * pull request's head or merge.
    */
  private def outsiders(ref: String): Boolean =
    ref.matches("[0-9a-fA-F]{7,40}") || ref.startsWith("refs/pull/") || ref.startsWith("pull/")

  /** Every item, or unaccountable when one's place is unread. */
  private def all(read: Vector[Read]): Accounted =
    if (read.exists(_.isLeft)) Accounted.Unaccountable
    else Accounted.of(read.collect { case Right(p) => p })

  /** The JSON object or array the result's first text block holds. */
  private def json(result: ujson.Obj): Option[ujson.Value] =
    text(result).flatMap(t => Try(ujson.read(t)).toOption)

  private def text(result: ujson.Obj): Option[String] =
    result.obj
      .get("content")
      .flatMap(_.arrOpt)
      .flatMap(_.collectFirst {
        case b if b.objOpt.flatMap(_.get("type")).contains(ujson.Str("text")) =>
          b.obj.get("text").flatMap(_.strOpt)
      })
      .flatten

  /** An item an author wrote: at its `repository_url`'s repository, else its `html_url`'s, else
    * the call's; by its author's association, untrusted for a bot or none.
    */
  private def authored(service: Service, c: McpCalled, item: ujson.Value): Read = item match {
    case o: ujson.Obj => authored(service, c, o.value)
    case _ => Left("an item is not an object")
  }

  private def authored(
      service: Service,
      c: McpCalled,
      o: collection.Map[String, ujson.Value]
  ): Read = {
    def named(key: String, prefix: String): Option[Either[String, Place]] =
      o.get(key)
        .map(v => v.strOpt.toRight(s"its $key is not a string").flatMap(within(service, prefix, _)))
    val place = named("repository_url", GitHubScope.Api)
      .orElse(named("html_url", Web))
      .getOrElse(called(service, c))
    val user = o.get("user").flatMap(_.objOpt)
    val bot = user.exists(u =>
      u.get("login").flatMap(_.strOpt).exists(_.endsWith("[bot]")) ||
        u.get("type").flatMap(_.strOpt).contains("Bot")
    )
    val level =
      if (bot) Integrity.Untrusted
      else
        o.get("author_association")
          .flatMap(_.strOpt)
          .flatMap(Association.of)
          .fold(Integrity.Untrusted)(_.level)
    place.map(Provenance(_, level))
  }

  /** A file's place: the repository its resource's `uri` names, else the call's. */
  private def file(service: Service, c: McpCalled): Either[String, Place] =
    c.result.obj
      .get("content")
      .flatMap(_.arrOpt)
      .flatMap(_.collectFirst(Function.unlift { (b: ujson.Value) =>
        b.objOpt
          .flatMap(_.get("resource"))
          .flatMap(_.objOpt)
          .flatMap(_.get("uri"))
          .flatMap(_.strOpt)
      }))
      .fold(called(service, c))(within(service, Repo, _))

  /** The repository `url` names after `prefix`, its first two segments. */
  private def within(service: Service, prefix: String, url: String): Either[String, Place] =
    if (!url.startsWith(prefix)) Left(s"'$url' is not under $prefix")
    else repository(service, url.stripPrefix(prefix).split("/", -1).take(2).mkString("/"))

  /** The repository the call's `owner` and `repo` name. */
  private def called(service: Service, c: McpCalled): Either[String, Place] = {
    def arg(key: String) = c.arguments.obj.get(key).flatMap(_.strOpt)
    (arg("owner"), arg("repo")) match {
      case (Some(owner), Some(repo)) => repository(service, s"$owner/$repo")
      case _ => Left("the call names no owner and repo")
    }
  }
}

/** GitHub's `author_association`, `written` as GitHub sends it: how an author stands to a
  * repository.
  */
enum Association(val written: String) {
  case Owner extends Association("OWNER")
  case Member extends Association("MEMBER")
  case Collaborator extends Association("COLLABORATOR")
  case Contributor extends Association("CONTRIBUTOR")
  case FirstTimeContributor extends Association("FIRST_TIME_CONTRIBUTOR")
  case FirstTimer extends Association("FIRST_TIMER")
  case Mannequin extends Association("MANNEQUIN")
  case NoAssociation extends Association("NONE")

  /** What an author so placed is believed at, before the repository's cap: owner, member and
    * collaborator at member; any other at outside.
    */
  def level: Integrity = this match {
    case Owner | Member | Collaborator => Integrity.Member
    case Contributor | FirstTimeContributor | FirstTimer | Mannequin | NoAssociation =>
      Integrity.Outside
  }
}

object Association {

  /** The association GitHub writes as `written`; `None` for one it does not. */
  def of(written: String): Option[Association] = values.find(_.written == written)
}
