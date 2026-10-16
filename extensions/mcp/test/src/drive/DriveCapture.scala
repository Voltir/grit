package grit.mcp.drive

import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Duration

import scala.collection.mutable
import scala.util.Try

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Credential, LiveProbe}

/** Drive's v3 REST answers recorded as fixtures, read-only (no model call, no spend):
  *
  * {{{
  * GRIT_DRIVE_SA_KEY=<key file or its JSON> ./mill grit.mcp.test.runMain \
  *   grit.mcp.drive.DriveCapture <domain> <dir> [writer@…]
  * }}}
  *
  * The key comes from `GRIT_DRIVE_SA_KEY` alone, as a path or as the JSON, and is never
  * printed or written. `<domain>` is the Workspace domain whose addresses are people; each
  * `writer` the exact address of an account that is not a person (another service's) that
  * edits files. It asks, at `drive.readonly`, each with the `fields` rev 3b names:
  * `drives.list`; `files.list` in every drive for folders, for other files, and for
  * `fullText contains 'a'`; `files.get` of a file whose parent is no folder, a folder's file, a
  * folder and each drive's id; `drives.get` of each drive; `permissions.list` of a file in each
  * drive; a Doc's export as `text/plain` and as `text/markdown`, and a Sheet's as `text/csv`;
  * and, for their error bodies, a folder's export (a 403) and a file id that names nothing (a
  * 404). Each answer is written into `dir` with [[Neutral]]'s stand-ins, never as it came:
  * JSON as `<name>.json`, an export as `<name>.txt`, `.md` or `.csv`. What it found is printed
  * as counts and booleans, never a name, an id or an address. Exits 1 when the key is unset or
  * not a key, or Drive refuses its token.
  */
object DriveCapture {

  /** Drive's v3 REST API. */
  val Api = "https://www.googleapis.com/drive/v3/"

  val Key: Variable = Variable("GRIT_DRIVE_SA_KEY")

  val ReadOnly: Set[String] = Set("https://www.googleapis.com/auth/drive.readonly")

  val Folder = "application/vnd.google-apps.folder"

  val Doc = "application/vnd.google-apps.document"

  val Sheet = "application/vnd.google-apps.spreadsheet"

  /** The fields a file is asked for, in a list and alone. */
  val FileFields = "id,name,mimeType,parents,driveId,owners(emailAddress)"

  /** A file or folder as Drive names it: its id, type, parents and drive. */
  final case class Item(
      id: String,
      mimeType: String,
      parents: Vector[String],
      drive: Option[String]
  )

  def main(args: Array[String]): Unit = {
    val (domain, dir, writers) = args.toVector match {
      case Vector(d, o, w*) => (d, Path.of(o), w.toSet)
      case _ =>
        System.err.println("usage: DriveCapture <domain> <dir> [writer@…]")
        sys.exit(2)
    }
    val keyJson = sys.env.get(Variable.value(Key)).map(_.trim).filter(_.nonEmpty) match {
      case None =>
        System.err.println(s"${Variable.value(Key)} is not set")
        sys.exit(1)
      case Some(v) if v.startsWith("{") => v
      case Some(v) =>
        Try(Files.readString(Path.of(v))).getOrElse {
          System.err.println(s"${Variable.value(Key)} is neither a key's JSON nor a readable file")
          sys.exit(1)
        }
    }
    val self = Try(ujson.read(keyJson)("client_email").str).getOrElse {
      System.err.println(s"${Variable.value(Key)} names no client_email")
      sys.exit(1)
    }
    val authorizer =
      Credential.open(
        Credential.serviceAccount(Key, ReadOnly),
        sys.env.updated(Variable.value(Key), keyJson)
      ) match {
        case Right(a) => a
        case Left(why) =>
          System.err.println(why.message)
          sys.exit(1)
      }
    val clock = Clock.system()
    val http = HttpClient.newHttpClient()
    // Scoped mutation: every answer, kept in memory until the stand-ins are known, and the lines
    // printed at the end.
    val answers = mutable.ArrayBuffer.empty[(String, Either[String, ujson.Obj])]
    val said = mutable.ArrayBuffer.empty[String]
    def report(line: String): Unit = said += line

    /** `path` with `query` asked of Drive: its status and body, or why it was not asked. */
    def get(path: String, query: Vector[(String, String)]): Either[String, (Int, String)] =
      authorizer.bearer(clock).left.map(_.message).map { bearer =>
        val q = query.map((k, v) => s"$k=${URLEncoder.encode(v, UTF_8)}").mkString("&")
        val request = LiveProbe
          .bearing(HttpRequest.newBuilder(URI.create(s"$Api$path?$q")), bearer)
          .timeout(Duration.ofSeconds(60))
          .GET()
          .build()
        val response = http.send(request, HttpResponse.BodyHandlers.ofString())
        (response.statusCode, response.body)
      }

    /** `path` asked, its JSON answer kept as `recording` (an error's body too). */
    def json(recording: String, path: String, query: (String, String)*): Option[ujson.Obj] =
      get(path, query.toVector) match {
        case Left(why) =>
          report(s"$recording: not asked: $why")
          None
        case Right((status, body)) =>
          Try(ujson.read(body)).toOption.flatMap(_.objOpt).map(ujson.Obj.from(_)) match {
            case Some(o) =>
              answers += recording -> Right(o)
              val reasons = o.value
                .get("error")
                .flatMap(_.objOpt)
                .flatMap(_.get("errors"))
                .flatMap(_.arrOpt)
                .toVector
                .flatten
                .flatMap(_.objOpt.flatMap(_.get("reason")).flatMap(_.strOpt))
              report(s"$recording: HTTP $status${
                  if (reasons.isEmpty) "" else reasons.mkString(", reasons ", ", ", "")
                }")
              Option.when(status == 200)(o)
            case None =>
              report(s"$recording: HTTP $status, not JSON")
              None
          }
      }

    /** `path` asked, its text answer kept as `recording` when Drive answered it. */
    def text(recording: String, path: String, query: (String, String)*): Unit =
      get(path, query.toVector) match {
        case Left(why) => report(s"$recording: not asked: $why")
        case Right((status, body)) =>
          report(s"$recording: HTTP $status, ${body.linesIterator.size} lines")
          if (status == 200) answers += recording -> Left(body)
      }

    val drivesListed =
      json("drives-list", "drives", "pageSize" -> "100", "fields" -> "drives(id,name)")
    if (drivesListed.isEmpty) {
      said.foreach(println)
      System.err.println("drives.list was not answered; nothing written")
      sys.exit(1)
    }
    val drives = drivesListed.toVector.flatMap(items(_, "drives")).map(_.id)

    def search(recording: String, q: String, size: Int): Vector[Item] =
      json(
        recording,
        "files",
        "q" -> q,
        "corpora" -> "allDrives",
        "includeItemsFromAllDrives" -> "true",
        "supportsAllDrives" -> "true",
        "pageSize" -> size.toString,
        "fields" -> s"nextPageToken,incompleteSearch,files($FileFields)"
      ).toVector.flatMap(items(_, "files"))

    val folders = search("search-folders", s"mimeType = '$Folder' and trashed = false", 100)
    val files = search("search-files", s"mimeType != '$Folder' and trashed = false", 100)
    val _ = search("search-fulltext", "fullText contains 'a' and trashed = false", 20)
    val all = folders ++ files
    val folderIds = folders.map(_.id).toSet
    val knownDrives = (drives ++ all.flatMap(_.drive)).distinct
    report(s"found ${drives.size} drives listed, ${folders.size} folders, ${files.size} files")
    report(s"items naming a driveId: ${all.count(_.drive.isDefined)} of ${all.size}")
    report(s"items with no parents: ${all.count(_.parents.isEmpty)}, of them in a drive: ${all
        .count(i => i.parents.isEmpty && i.drive.isDefined)}")
    report(s"items with more than one parent: ${all.count(_.parents.size > 1)}")
    val tops = files.filter(f => f.drive.isDefined && !f.parents.exists(folderIds))
    report(
      s"top files of a drive: ${tops.size}, their parent the drive's id: ${tops
          .count(t => t.parents.headOption == t.drive)}, empty parents: ${tops.count(_.parents.isEmpty)}"
    )

    def metadata(recording: String, id: String): Option[ujson.Obj] =
      json(
        recording,
        s"files/$id",
        "supportsAllDrives" -> "true",
        "fields" -> s"$FileFields,inheritedPermissionsDisabled"
      )
    tops.headOption.foreach(t => metadata("metadata-file-top", t.id))
    files.find(_.parents.exists(folderIds)).foreach(f => metadata("metadata-file-in-folder", f.id))
    folders.headOption.foreach { f =>
      metadata("metadata-folder", f.id).foreach { r =>
        report(
          s"metadata-folder: names inheritedPermissionsDisabled ${r.value.contains("inheritedPermissionsDisabled")}"
        )
      }
    }
    knownDrives.zipWithIndex.foreach { case (d, i) =>
      val letter = ('a' + i).toChar
      metadata(s"metadata-drive-$letter", d).foreach { r =>
        report(
          s"metadata-drive-$letter: a folder ${r.value.get("mimeType").flatMap(_.strOpt).contains(Folder)}, " +
            s"named ${r.value.get("name").flatMap(_.strOpt).exists(_.trim.nonEmpty)}, " +
            s"names parents ${r.value.contains("parents")}"
        )
      }
      json(s"drive-$letter", s"drives/$d", "fields" -> "id,name")
      all.find(_.drive.contains(d)).foreach { f =>
        json(
          s"permissions-drive-$letter",
          s"files/${f.id}/permissions",
          "supportsAllDrives" -> "true",
          "pageSize" -> "100",
          "fields" -> "nextPageToken,permissions(type,role,emailAddress,domain,permissionDetails(inherited,role))"
        ).foreach { r =>
          val t = ujson.write(r)
          report(
            s"permissions-drive-$letter: names the deployment's own address ${t.contains(self)}, " +
              s"permissionDetails.inherited ${t.contains("\"inherited\"")}, a next page ${r.value.contains("nextPageToken")}"
          )
        }
      }
    }

    files.find(_.mimeType == Doc).foreach { d =>
      text("export-doc.txt", s"files/${d.id}/export", "mimeType" -> "text/plain")
      text("export-doc.md", s"files/${d.id}/export", "mimeType" -> "text/markdown")
    }
    files.find(_.mimeType == Sheet).foreach { s =>
      text("export-sheet.csv", s"files/${s.id}/export", "mimeType" -> "text/csv")
    }
    folders.headOption.foreach { f =>
      json("error-export-folder", s"files/${f.id}/export", "mimeType" -> "text/plain")
    }
    json("error-no-file", "files/0000000000000000000000000", "supportsAllDrives" -> "true")

    val neutral = new Neutral(domain, self, writers, folderIds, knownDrives)
    Files.createDirectories(dir)
    answers.foreach { case (recording, answer) =>
      val (file, written) = answer match {
        case Right(o) =>
          (dir.resolve(s"$recording.json"), ujson.write(neutral.result(o), indent = 2))
        case Left(body) => (dir.resolve(recording), neutral.body(body))
      }
      Files.writeString(file, written)
      println(s"wrote $file")
    }
    said.foreach(println)
  }

  /** The items `answer` lists under `key`, each its id, type, parents and drive. */
  private def items(answer: ujson.Obj, key: String): Vector[Item] =
    answer.value.get(key).flatMap(_.arrOpt).toVector.flatten.flatMap(_.objOpt).flatMap { o =>
      o.get("id").flatMap(_.strOpt).map { id =>
        Item(
          id,
          o.get("mimeType").flatMap(_.strOpt).getOrElse(""),
          o.get("parents").flatMap(_.arrOpt).toVector.flatten.flatMap(_.strOpt),
          o.get("driveId").flatMap(_.strOpt)
        )
      }
    }
}
