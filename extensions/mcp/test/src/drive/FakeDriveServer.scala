package grit.mcp.drive

import java.net.{InetSocketAddress, URI, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Credential, KeyFiles}

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** Drive's v3 REST API in this JVM, on a loopback port, answering from the recordings
  * ([[Recorded]]) the requests [[DriveRest]] makes: a search with `search-fulltext`, a file
  * with its `metadata-*` recording or, for any other id the searches name, its entry there; a
  * file's grants with its drive's `permissions-*`; a Doc's markdown export with `export-doc.md`,
  * a Sheet's CSV export with `export-sheet.csv`, any other export with
  * `error-export-folder` (403); an id the recordings do not name with `error-no-file` (404); a
  * listing of shared drives with `drives-list`. Each request must be a GET carrying
  * [[FakeDriveServer.Token]], `supportsAllDrives=true` when it asks of files, and `fields`
  * unless it reads content; one that does not is answered 400. Every request's path
  * and query are kept, in [[requests]].
  */
final class FakeDriveServer private () {

  // Test scaffolding: set by the test's thread and read by the server's, each under `lock`,
  // and no capability is reached through any of it.
  private val lock = new Object

  // Each var is read and written only inside `lock.synchronized`.
  @caps.unsafe.untrackedCaptures
  private var kept = Vector.empty[(String, Map[String, String])]

  @caps.unsafe.untrackedCaptures
  private var overrides = Map.empty[String, (Int, String)]

  private val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  http.createContext("/drive/v3/", exchange => answer(exchange))
  http.start()

  /** The API's base, as [[DriveRest]] takes it. */
  def api: URI = URI.create(s"http://127.0.0.1:${http.getAddress.getPort}/drive/v3/")

  /** A [[DriveRest]] over this fake, carrying [[FakeDriveServer.Token]], reading the system's clock. */
  def rest(): DriveRest^ = rest(Clock.system())

  /** A [[DriveRest]] over this fake, carrying [[FakeDriveServer.Token]], reading `clock`. */
  def rest(clock: Clock): DriveRest^{clock} = {
    val token = Variable("FAKE_DRIVE_TOKEN")
    val authorizer = Credential
      .open(
        Credential.static(token),
        Map(Variable.value(token) -> FakeDriveServer.Token),
        KeyFiles.Local
      )
      .fold(r => throw new java.lang.AssertionError(r.message), identity)
    new DriveRest(api, authorizer, clock)
  }

  /** Every request's path below the API and its query, in the order received. */
  def requests: Vector[(String, Map[String, String])] = lock.synchronized(kept)

  /** Requests for `path` below the API are answered `status` with `body` from now. */
  def answering(path: String, status: Int, body: String): Unit =
    lock.synchronized { overrides = overrides.updated(path, (status, body)) }

  /** Stops answering. */
  def stop(): Unit = http.stop(0)

  private def answer(exchange: HttpExchange): Unit = {
    val path = exchange.getRequestURI.getRawPath.stripPrefix("/drive/v3/")
    val query = Option(exchange.getRequestURI.getRawQuery).toVector
      .flatMap(_.split('&').toVector)
      .map { kv =>
        val (k, v) = kv.span(_ != '=')
        URLDecoder.decode(k, UTF_8) -> URLDecoder.decode(v.drop(1), UTF_8)
      }
      .toMap
    val overridden = lock.synchronized {
      kept = kept :+ (path -> query)
      overrides.get(path)
    }
    val content = query.contains("alt") || path.endsWith("/export")
    val refused =
      if (exchange.getRequestMethod != "GET") Some("not a GET")
      else if (
        !Option(exchange.getRequestHeaders.getFirst("Authorization"))
          .contains(s"Bearer ${FakeDriveServer.Token}")
      ) Some("no bearer")
      else if (path.startsWith("files") && !query.get("supportsAllDrives").contains("true"))
        Some("no supportsAllDrives")
      else if (!content && !query.contains("fields")) Some("no fields")
      else None
    val (status, body) = refused match {
      case Some(why) =>
        (400, ujson.write(ujson.Obj("error" -> ujson.Obj("code" -> 400, "message" -> why))))
      case None => overridden.getOrElse(recorded(path, query))
    }
    val bytes = body.getBytes(UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "application/json")
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    exchange.getResponseBody.write(bytes)
    exchange.close()
  }

  private def recorded(path: String, query: Map[String, String]): (Int, String) =
    path.split('/').toVector match {
      case Vector("files") => (200, Recorded.text("search-fulltext.json"))
      case Vector("drives") => (200, Recorded.text("drives-list.json"))
      case Vector("files", id) =>
        FakeDriveServer.Metadata
          .get(id)
          .map(r => (200, Recorded.text(s"$r.json")))
          .orElse(FakeDriveServer.listed.get(id).map(f => (200, ujson.write(f))))
          .getOrElse(FakeDriveServer.missing)
      case Vector("files", id, "permissions") =>
        FakeDriveServer.listed
          .get(id)
          .flatMap(_.obj.get("driveId"))
          .flatMap(_.strOpt)
          .map(d => (200, Recorded.text(s"permissions-$d.json")))
          .getOrElse(FakeDriveServer.missing)
      case Vector("files", id, "export") =>
        val kind = FakeDriveServer.listed.get(id).flatMap(_.obj.get("mimeType")).flatMap(_.strOpt)
        (kind, query.get("mimeType")) match {
          case (None, _) => FakeDriveServer.missing
          case (Some(DriveFile.Document), Some("text/markdown")) =>
            (200, Recorded.text("export-doc.md"))
          case (Some(DriveFile.Spreadsheet), Some("text/csv")) =>
            (200, Recorded.text("export-sheet.csv"))
          case _ => (403, Recorded.text("error-export-folder.json"))
        }
      case _ => FakeDriveServer.missing
    }
}

object FakeDriveServer {

  /** The only bearer it takes. */
  val Token: String = "drive-token"

  /** The `metadata-*` recording of each id that has one. */
  private val Metadata: Map[String, String] = Map(
    "drive-a" -> "metadata-drive-a",
    "drive-b" -> "metadata-drive-b",
    "file-1" -> "metadata-file-in-folder",
    "file-4" -> "metadata-file-top",
    "folder-2" -> "metadata-folder"
  )

  /** Every file and folder the recorded searches name, by id. */
  private lazy val listed: Map[String, ujson.Value] =
    Vector("search-files", "search-folders", "metadata-drive-a", "metadata-drive-b")
      .map(Recorded.json)
      .flatMap(a => a.obj.get("files").flatMap(_.arrOpt).map(_.toVector).getOrElse(Vector(a)))
      .flatMap(f => f.obj.get("id").flatMap(_.strOpt).map(_ -> f))
      .toMap

  private def missing: (Int, String) = (404, Recorded.text("error-no-file.json"))

  def start(): FakeDriveServer = new FakeDriveServer()
}
