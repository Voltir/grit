package grit.mcp.drive

import java.net.{InetSocketAddress, URI, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Credential, KeyFiles}

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** Drive's v3 REST API in this JVM, on a loopback port, answering from the recordings
  * ([[Recorded]]) and the hand-made `made-*` fixtures the requests [[DriveRest]] makes: a
  * search with `search-fulltext`, or, when its query holds `'{id}' in parents`, with those of
  * its files whose parents name that id; a listing of one folder's own files (a query of
  * `'{id}' in parents` and no words) with its `made-children-{id}`, or none when there is no
  * such file; a file with its `metadata-*` recording or, for any
  * other id the searches or the `made-*` files name, its entry there; a text file's content
  * (`alt=media`) with its `made-{id}.txt`, byte-order mark and all; a file's grants with its
  * own `made-permissions-{id}` when it has one, else its drive's `permissions-*`; a Doc's
  * markdown export with `export-doc.md`, a Sheet's CSV export with `export-sheet.csv`, any
  * other export with `error-export-folder` (403); an id the recordings do not name with
  * `error-no-file` (404); a listing of shared drives with `drives-list`. The Slides API's
  * `presentations/{id}`, below [[slides]], is answered only as [[answering]] sets. Each request must be a
  * GET carrying [[FakeDriveServer.Token]], `supportsAllDrives=true` when it asks of files, and
  * `fields` unless it reads content; one that does not is answered 400. Every request's path
  * and query are kept, in [[requests]]. A recording's files carry their `version`, digits a
  * capture keeps as Drive wrote them ([[Neutral]]).
  */
final class FakeDriveServer private () {

  // Test scaffolding: set by the test's thread and read by the server's, each under `lock`,
  // and no capability is reached through any of it.
  private val lock = new Object

  // Each var is read and written only inside `lock.synchronized`.
  @caps.unsafe.untrackedCaptures
  private var kept = Vector.empty[(String, Map[String, String])]

  @caps.unsafe.untrackedCaptures
  private var overrides = Map.empty[String, (Int, Vector[Byte])]

  private val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  http.createContext("/drive/v3/", exchange => answer(exchange))
  http.createContext("/slides/v1/", exchange => answer(exchange))
  http.start()

  /** The API's base, as [[DriveRest]] takes it. */
  def api: URI = URI.create(s"http://127.0.0.1:${http.getAddress.getPort}/drive/v3/")

  /** The Slides API's base, as [[DriveRest]] takes it. */
  def slides: URI = URI.create(s"http://127.0.0.1:${http.getAddress.getPort}/slides/v1/")

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
    new DriveRest(api, slides, authorizer, clock)
  }

  /** Every request's path below the API and its query, in the order received. */
  def requests: Vector[(String, Map[String, String])] = lock.synchronized(kept)

  /** Requests for `path` below the API are answered `status` with `body` from now; a file's
    * content (`alt=media`) apart from its metadata, as `files/{id}?alt=media`.
    */
  def answering(path: String, status: Int, body: String): Unit =
    answeringBytes(path, status, body.getBytes(UTF_8).toVector)

  /** As [[answering]], with `body`'s bytes as they are. */
  def answeringBytes(path: String, status: Int, body: Vector[Byte]): Unit =
    lock.synchronized { overrides = overrides.updated(path, (status, body)) }

  /** Stops answering. */
  def stop(): Unit = http.stop(0)

  private def answer(exchange: HttpExchange): Unit = {
    val path =
      exchange.getRequestURI.getRawPath.stripPrefix("/drive/v3/").stripPrefix("/slides/v1/")
    val query = Option(exchange.getRequestURI.getRawQuery).toVector
      .flatMap(_.split('&').toVector)
      .map { kv =>
        val (k, v) = kv.span(_ != '=')
        URLDecoder.decode(k, UTF_8) -> URLDecoder.decode(v.drop(1), UTF_8)
      }
      .toMap
    val media = query.get("alt").contains("media")
    val overridden = lock.synchronized {
      kept = kept :+ (path -> query)
      overrides.get(if (media) s"$path?alt=media" else path)
    }
    val content = media || path.endsWith("/export")
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
    def text(answer: (Int, String)): (Int, Vector[Byte]) =
      (answer._1, answer._2.getBytes(UTF_8).toVector)
    val (status, bytes) = refused match {
      case Some(why) =>
        text((400, ujson.write(ujson.Obj("error" -> ujson.Obj("code" -> 400, "message" -> why)))))
      case None => overridden.getOrElse(text(recorded(path, query)))
    }
    exchange.getResponseHeaders.set("Content-Type", "application/json")
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    val out = exchange.getResponseBody
    bytes.foreach(b => out.write(b.toInt))
    exchange.close()
  }

  private def recorded(path: String, query: Map[String, String]): (Int, String) = {
    val media = query.get("alt").contains("media")
    path.split('/').toVector match {
      case Vector("files") if query.get("q").exists(!_.contains("fullText contains")) =>
        query.get("q").flatMap(FakeDriveServer.Parent.findFirstMatchIn).map(_.group(1)) match {
          case Some(parent) if FakeDriveServer.Children(parent) =>
            (200, Recorded.text(s"made-children-$parent.json"))
          case _ => (200, ujson.write(ujson.Obj("files" -> ujson.Arr())))
        }
      case Vector("files") =>
        val all = Recorded.json("search-fulltext")
        query.get("q").flatMap(FakeDriveServer.Parent.findFirstMatchIn).map(_.group(1)) match {
          case None => (200, ujson.write(all))
          case Some(parent) =>
            val within = all.obj.get("files").flatMap(_.arrOpt).toVector.flatten.filter { f =>
              f.obj.get("parents").flatMap(_.arrOpt).exists(_.exists(_.strOpt.contains(parent)))
            }
            (200, ujson.write(ujson.Obj.from(all.obj.toSeq :+ ("files" -> ujson.Arr(within*)))))
        }
      case Vector("drives") => (200, Recorded.text("drives-list.json"))
      case Vector("files", id) if media =>
        FakeDriveServer.listed
          .get(id)
          .filter(_.obj.get("mimeType").flatMap(_.strOpt).exists(_.startsWith("text/")))
          .map(_ => (200, Recorded.text(s"made-$id.txt")))
          .getOrElse(FakeDriveServer.missing)
      case Vector("files", id) =>
        FakeDriveServer.Metadata
          .get(id)
          .map(r => (200, Recorded.text(s"$r.json")))
          .orElse(FakeDriveServer.listed.get(id).map(f => (200, ujson.write(f))))
          .getOrElse(FakeDriveServer.missing)
      case Vector("files", id, "permissions") if FakeDriveServer.OwnGrants(id) =>
        (200, Recorded.text(s"made-permissions-$id.json"))
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

  /** The parent a search's query holds it to, as [[DriveRest.search]] writes it. */
  private val Parent = "'([A-Za-z0-9_-]+)' in parents".r

  /** The folders and drives whose own files are listed by hand, `made-children-{id}`. */
  private val Children: Set[String] = Set("drive-a", "folder-3")

  /** The ids with grants of their own, `made-permissions-{id}`. */
  private val OwnGrants: Set[String] = Set("file-text")

  /** Every file and folder the recorded searches and the `made-*` files name, by id, a hand-made
    * entry over a recorded one.
    */
  private lazy val listed: Map[String, ujson.Value] =
    (Vector(
      "search-files",
      "search-folders",
      "metadata-drive-a",
      "metadata-drive-b",
      "made-file-text"
    ) ++ Children.toVector.sorted.map(c => s"made-children-$c"))
      .map(Recorded.json)
      .flatMap(a => a.obj.get("files").flatMap(_.arrOpt).map(_.toVector).getOrElse(Vector(a)))
      .flatMap(f => f.obj.get("id").flatMap(_.strOpt).map(_ -> f))
      .toMap

  private def missing: (Int, String) = (404, Recorded.text("error-no-file.json"))

  def start(): FakeDriveServer = new FakeDriveServer()
}
