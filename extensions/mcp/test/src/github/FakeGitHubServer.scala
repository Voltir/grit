package grit.mcp.github

import java.net.{InetSocketAddress, URI, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}

import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Credential, KeyFiles}

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** GitHub's REST API for pull requests in this JVM, on a loopback port, for the repository
  * `acme/app`, its answers shaped as the hand-made fixtures under `github/rest/` are
  * (extensions/mcp/README.md): the repository (`repo.json`); a list of pull requests, filtered
  * by `state` and by the branch of `head` alone, its owner ignored, as GitHub is reported to
  * filter a malformed one, so [[foreign]] items come back too; a pull request opened (201,
  * `pull.json`'s shape), or refused while one from that branch is open (422,
  * `error-already-exists.json`). Each request must carry [[FakeGitHubServer.Token]] as its
  * bearer and the API version; one that does not is answered 401. [[answering]] overrides a
  * path's answer. Every request's method, path and query are kept, in [[requests]].
  */
final class FakeGitHubServer private () {

  private val pulls = new CopyOnWriteArrayList[ujson.Obj]()
  private val overrides = new ConcurrentHashMap[String, (Int, String, Map[String, String])]()
  private val kept = new CopyOnWriteArrayList[(String, String, String)]()
  private val bodies = new CopyOnWriteArrayList[ujson.Value]()

  private val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  http.createContext("/", exchange => answer(exchange))
  http.start()

  /** The API's base, as [[GitHubRepository.of]] takes it. */
  def api: URI = URI.create(s"http://127.0.0.1:${http.getAddress.getPort}/")

  /** A [[GitHubRepository]] of `acme/app` over this fake, carrying [[FakeGitHubServer.Token]]. */
  def repository(): GitHubRepository^ = {
    val token = Variable("FAKE_GITHUB_TOKEN")
    val authorizer = Credential
      .open(
        Credential.static(token),
        Map(Variable.value(token) -> FakeGitHubServer.Token),
        KeyFiles.Local
      )
      .fold(r => throw new java.lang.AssertionError(r.message), identity)
    GitHubRepository
      .of(api, "acme/app", authorizer, Clock.system()) match {
      case Right(r) => r
      case Left(why) => throw new java.lang.AssertionError(why)
    }
  }

  /** Every request's method, path and query, in the order received. */
  def requests: Vector[(String, String, String)] = kept.asScala.toVector

  /** Every pull request asked for, as its request's body, in the order received. */
  def posted: Vector[ujson.Value] = bodies.asScala.toVector

  /** How many pull requests were opened. */
  def opened: Int = pulls.asScala.count(p => p("head")("repo")("full_name").str == "acme/app")

  /** `path`, answered `status` with `body` and `headers` from now on. */
  def answering(
      path: String,
      status: Int,
      body: String,
      headers: Map[String, String] = Map.empty
  ): Unit = {
    val _ = overrides.put(path, (status, body, headers))
  }

  /** `branch`'s open pull request closed unmerged, its head at `head`. */
  def close(branch: String, head: String): Unit = ended(branch, head, merged = false)

  /** `branch`'s open pull request merged, its head at `head`. */
  def merge(branch: String, head: String): Unit = ended(branch, head, merged = true)

  /** A pull request open from another repository's `branch`, `{owner}/{name}` `from`. */
  def foreign(branch: String, from: String): Unit = {
    val _ = pulls.add(made(branch, from))
  }

  def stop(): Unit = http.stop(0)

  private def ended(branch: String, head: String, merged: Boolean): Unit =
    pulls.asScala
      .find(p => p("head")("ref").str == branch && p("state").str == "open")
      .foreach { p =>
        p("state") = "closed"
        p("head")("sha") = head
        if (merged) p("merged_at") = "2026-11-23T00:00:00Z"
      }

  private def made(branch: String, from: String): ujson.Obj = {
    val p = ujson.read(FakeGitHubServer.fixture("pull.json")).obj
    val n = pulls.size + 1
    p("number") = n
    p("html_url") = s"https://github.com/acme/app/pull/$n"
    p("head")("ref") = branch
    p("head")("repo")("full_name") = from
    ujson.Obj.from(p)
  }

  private def answer(exchange: HttpExchange): Unit = {
    val path = exchange.getRequestURI.getRawPath.stripPrefix("/")
    val query = Option(exchange.getRequestURI.getRawQuery).getOrElse("")
    val method = exchange.getRequestMethod
    val _ = kept.add((method, path, query))
    val sent = new String(exchange.getRequestBody.readAllBytes(), UTF_8)
    val headers = exchange.getRequestHeaders
    val authorised =
      Option(headers.getFirst("Authorization")).contains(s"Bearer ${FakeGitHubServer.Token}") &&
        Option(headers.getFirst("X-GitHub-Api-Version")).contains(GitHubRepository.ApiVersion)
    val (status, body, extra) =
      if (!authorised) (401, FakeGitHubServer.fixture("error-unauthorised.json"), Map.empty)
      else
        Option(overrides.get(path)).getOrElse {
          (method, path) match {
            case ("GET", "repos/acme/app") =>
              (200, FakeGitHubServer.fixture("repo.json"), Map.empty)
            case ("GET", "repos/acme/app/pulls") => (200, listed(params(query)), Map.empty)
            case ("POST", "repos/acme/app/pulls") =>
              val asked = ujson.read(sent)
              val _ = bodies.add(asked)
              opening(asked)
            case _ => (404, FakeGitHubServer.fixture("error-not-found.json"), Map.empty)
          }
        }
    extra.foreach((k, v) => exchange.getResponseHeaders.add(k, v))
    val bytes = body.getBytes(UTF_8)
    exchange.sendResponseHeaders(status, if (bytes.isEmpty) -1 else bytes.length.toLong)
    if (bytes.nonEmpty) exchange.getResponseBody.write(bytes)
    exchange.close()
  }

  private def opening(sent: ujson.Value): (Int, String, Map[String, String]) = {
    val branch = sent("head").str
    if (pulls.asScala.exists(p => p("head")("ref").str == branch && p("state").str == "open"))
      (422, FakeGitHubServer.fixture("error-already-exists.json"), Map.empty)
    else {
      val p = made(branch, "acme/app")
      val _ = pulls.add(p)
      (201, ujson.write(p), Map.empty)
    }
  }

  private def listed(query: Map[String, String]): String = {
    val branch = query.get("head").map(_.dropWhile(_ != ':').drop(1))
    val state = query.getOrElse("state", "open")
    val kept = pulls.asScala.toVector.reverse.filter(p =>
      branch.forall(_ == p("head")("ref").str) && (state == "all" || p("state").str == state)
    )
    ujson.write(ujson.Arr.from(kept))
  }

  private def params(query: String): Map[String, String] =
    query
      .split("&")
      .toVector
      .filter(_.nonEmpty)
      .map(_.split("=", 2) match {
        case Array(k, v) => URLDecoder.decode(k, UTF_8) -> URLDecoder.decode(v, UTF_8)
        case other => URLDecoder.decode(other.mkString, UTF_8) -> ""
      })
      .toMap
}

object FakeGitHubServer {

  /** The token every request must carry. */
  val Token = "ghs_fakeGitHubToken0123"

  /** The hand-made fixture `github/rest/{name}`. */
  def fixture(name: String): String = {
    val in = Option(getClass.getResourceAsStream(s"/github/rest/$name"))
      .getOrElse(throw new java.lang.AssertionError(s"no fixture github/rest/$name"))
    try new String(in.readAllBytes(), UTF_8)
    finally in.close()
  }

  def start(): FakeGitHubServer = new FakeGitHubServer()
}
