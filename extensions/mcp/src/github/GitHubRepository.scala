package grit.mcp.github

import java.io.IOException
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8

import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*

import grit.core.clock.Clock
import grit.core.host.{HostError, RemoteRepository}
import grit.mcp.client.{Authorizer, McpServer}

/** [[RemoteRepository]] over GitHub's REST API at `api`, for one repository, each request
  * carrying `authorizer`'s bearer got at `clock`'s time, timing out after
  * [[GitHubRepository.Timeout]], a redirect never followed. A failure to reach GitHub, or an
  * answer its methods do not name, is `Failed` with GitHub's status and its message (at most
  * [[GitHubRepository.MessageMost]] characters, control characters dropped), never the token;
  * an answer past [[GitHubRepository.BodyMost]] bytes is `Failed` too.
  */
final class GitHubRepository private (
    api: URI,
    owner: String,
    name: String,
    authorizer: Authorizer,
    clock: Clock
) extends RemoteRepository {

  private val http = HttpClient
    .newBuilder()
    .connectTimeout(GitHubRepository.Timeout.toJava)
    .followRedirects(HttpClient.Redirect.NEVER)
    .build()

  private val repo = s"repos/$owner/$name"

  /** `https://github.com/{owner}/{name}.git`. */
  val pushedTo: String = s"https://github.com/$owner/$name.git"

  def open(branch: String, title: String, body: String): Either[HostError, URI] =
    for {
      about <- answered(send("GET", repo, Vector.empty, None), 200)
      base <- text(about, "default_branch")
      made = ujson.Obj("title" -> title, "head" -> branch, "base" -> base, "body" -> body)
      at <- send("POST", s"$repo/pulls", Vector.empty, Some(made)).flatMap {
        case (201, answer) => GitHubRepository.parsed(answer).flatMap(address)
        case (422, answer) if GitHubRepository.exists(answer) =>
          pullRequest(branch).flatMap {
            case Some(RemoteRepository.PullRequest.Open(at)) => Right(at)
            case _ => Left(GitHubRepository.failed(422, answer))
          }
        case (status, answer) => Left(GitHubRepository.failed(status, answer))
      }
    } yield at

  def pullRequest(branch: String): Either[HostError, Option[RemoteRepository.PullRequest]] = {
    val query = Vector(
      "head" -> s"$owner:$branch",
      "state" -> "all",
      "sort" -> "created",
      "direction" -> "desc",
      "per_page" -> "100"
    )
    answered(send("GET", s"$repo/pulls", query, None), 200).flatMap {
      case ujson.Arr(items) =>
        items.toVector
          .filter(ours(branch))
          .foldLeft[Either[HostError, Vector[RemoteRepository.PullRequest]]](Right(Vector.empty)) {
            (kept, item) => kept.flatMap(ps => standing(item).map(ps :+ _))
          }
          .map(ps =>
            ps.collectFirst { case o: RemoteRepository.PullRequest.Open => o }.orElse(ps.headOption)
          )
      case _ => Left(HostError.Failed("GitHub's list of pull requests is not a list"))
    }
  }

  /** Whether `item` is a pull request from this repository's `branch`. */
  private def ours(branch: String)(item: ujson.Value): Boolean =
    (for {
      head <- item.objOpt.flatMap(_.get("head"))
      ref <- head.objOpt.flatMap(_.get("ref")).flatMap(_.strOpt)
      from <- head.objOpt.flatMap(_.get("repo")).flatMap(_.objOpt).flatMap(_.get("full_name"))
      full <- from.strOpt
    } yield ref == branch && full.equalsIgnoreCase(s"$owner/$name")).getOrElse(false)

  /** How `item` stands. */
  private def standing(item: ujson.Value): Either[HostError, RemoteRepository.PullRequest] =
    for {
      at <- address(item)
      state <- text(item, "state")
      merged = item.objOpt.flatMap(_.get("merged_at")).exists(!_.isNull)
      head <- item.objOpt
        .flatMap(_.get("head"))
        .flatMap(_.objOpt)
        .flatMap(_.get("sha"))
        .flatMap(_.strOpt)
        .toRight(HostError.Failed("GitHub named a pull request with no head commit"))
      pull <- state match {
        case "open" => Right(RemoteRepository.PullRequest.Open(at))
        case "closed" if merged => Right(RemoteRepository.PullRequest.Merged(at, head))
        case "closed" => Right(RemoteRepository.PullRequest.Closed(at, head))
        case other => Left(HostError.Failed(s"GitHub named a pull request $other"))
      }
    } yield pull

  private def address(item: ujson.Value): Either[HostError, URI] =
    text(item, "html_url").flatMap(u =>
      scala.util
        .Try(URI.create(u))
        .toEither
        .left
        .map(_ => HostError.Failed("GitHub named a pull request at no address"))
    )

  private def text(v: ujson.Value, field: String): Either[HostError, String] =
    v.objOpt
      .flatMap(_.get(field))
      .flatMap(_.strOpt)
      .toRight(HostError.Failed(s"GitHub's answer has no $field"))

  /** `sent`'s body parsed, when its status is `ok`; otherwise `Failed`. */
  private def answered(
      sent: Either[HostError, (Int, String)],
      ok: Int
  ): Either[HostError, ujson.Value] =
    sent.flatMap {
      case (status, answer) if status == ok => GitHubRepository.parsed(answer)
      case (status, answer) => Left(GitHubRepository.failed(status, answer))
    }

  /** A `method` of `path` below `api` with `query` and `body`: its status and body, the body
    * at most [[GitHubRepository.BodyMost]] bytes.
    */
  private def send(
      method: String,
      path: String,
      query: Vector[(String, String)],
      body: Option[ujson.Value]
  ): Either[HostError, (Int, String)] =
    authorizer
      .bearer(clock)
      .left
      .map(miss => HostError.Failed(s"no token for GitHub: ${miss.why}"))
      .flatMap { bearer =>
        val encoded = query
          .map((k, v) => s"$k=${URLEncoder.encode(v, UTF_8).replace("+", "%20")}")
          .mkString("&")
        val request = HttpRequest
          .newBuilder(api.resolve(if (encoded.isEmpty) path else s"$path?$encoded"))
          .timeout(GitHubRepository.Timeout.toJava)
          .header("Authorization", s"Bearer ${bearer.value}")
          .header("Accept", "application/vnd.github+json")
          .header("X-GitHub-Api-Version", GitHubRepository.ApiVersion)
          .method(
            method,
            body.fold(HttpRequest.BodyPublishers.noBody())(b =>
              HttpRequest.BodyPublishers.ofString(ujson.write(b), UTF_8)
            )
          )
          .build()
        try {
          val response = http.send(request, HttpResponse.BodyHandlers.ofInputStream())
          val in = response.body()
          try {
            val bytes = in.readNBytes(GitHubRepository.BodyMost + 1)
            if (bytes.length > GitHubRepository.BodyMost)
              Left(HostError.Failed(s"GitHub's answer passed ${GitHubRepository.BodyMost} bytes"))
            else Right((response.statusCode, new String(bytes, UTF_8)))
          } finally in.close()
        } catch {
          case _: HttpTimeoutException =>
            Left(HostError.Failed(s"GitHub gave no answer within ${GitHubRepository.Timeout}"))
          // Its class alone: an I/O error's message may name the request.
          case e: IOException => Left(HostError.Failed(s"GitHub: ${e.getClass.getSimpleName}"))
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
            Left(HostError.Failed("GitHub: interrupted"))
        }
      }

  override def toString: String = s"GitHubRepository($owner/$name)"
}

object GitHubRepository {

  /** GitHub's REST API. */
  val Api: String = "https://api.github.com/"

  /** The version of GitHub's REST API asked for. */
  val ApiVersion: String = "2022-11-28"

  /** How long one request may take: 30 seconds. */
  val Timeout: FiniteDuration = 30.seconds

  /** The most bytes of an answer read: 1 MiB. */
  val BodyMost: Int = 1024 * 1024

  /** The most characters of GitHub's message kept in a failure. */
  val MessageMost: Int = 200

  /** The repository `fullName`, `{owner}/{name}`, at `api`, or why not: `api` not `https`
    * (`http` only on a loopback host), or `fullName` not two segments each of ASCII letters,
    * digits, `.`, `_` or `-`, neither `.` nor `..`.
    */
  def of(
      api: URI,
      fullName: String,
      authorizer: Authorizer,
      clock: Clock
  ): Either[String, GitHubRepository^] =
    GitHubScope.named(fullName) match {
      case _ if !McpServer.secure(api) =>
        Left(s"GitHub's API must be https (http only on a loopback host): '$api'")
      case Left(why) => Left(why)
      case Right((owner, name))
          if !Vector(owner, name).forall(s => Segment.matches(s) && s != "." && s != "..") =>
        Left(
          s"a repository is {owner}/{name}, each of ASCII letters, digits, '.', '_' or '-': " +
            s"not '$fullName'"
        )
      case Right((owner, name)) => Right(new GitHubRepository(api, owner, name, authorizer, clock))
    }

  private val Segment = "[A-Za-z0-9._-]+".r

  private[github] def parsed(answer: String): Either[HostError, ujson.Value] =
    scala.util
      .Try(ujson.read(answer))
      .toEither
      .left
      .map(_ => HostError.Failed("GitHub's answer is not JSON"))

  /** Whether `answer` is GitHub's refusal of a pull request one from that branch already is. */
  private def exists(answer: String): Boolean =
    parsed(answer).toOption
      .flatMap(_.objOpt)
      .flatMap(_.get("errors"))
      .flatMap(_.arrOpt)
      .exists(
        _.exists(e =>
          e.objOpt.flatMap(_.get("message")).flatMap(_.strOpt).exists(_.contains("already exists"))
        )
      )

  /** `status` and GitHub's message in `answer`, when it is JSON with one, as a failure. */
  private def failed(status: Int, answer: String): HostError = {
    val message = parsed(answer).toOption
      .flatMap(_.objOpt)
      .flatMap(_.get("message"))
      .flatMap(_.strOpt)
      .map(_.filterNot(_.isControl).take(MessageMost))
      .filter(_.nonEmpty)
    HostError.Failed(s"GitHub answered $status" + message.fold("")(m => s": $m"))
  }
}
