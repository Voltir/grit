package grit.mcp.drive

import java.net.http.HttpClient
import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.util.Try

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Credential, LiveProbe, McpServer}
import grit.mcp.scope.McpScope
import grit.mcp.wire.{McpError, McpTool, Rpc, Sse}

/** Drive's MCP server's answers recorded as fixtures, read-only (no model call, no spend):
  *
  * {{{
  * GRIT_DRIVE_SA_KEY=<key file or its JSON> ./mill grit.mcp.test.runMain \
  *   grit.mcp.drive.DriveCapture <domain> <dir> [writer@…]
  * }}}
  *
  * The key comes from `GRIT_DRIVE_SA_KEY` alone, as a path or as the JSON, and is never
  * printed or written. `<domain>` is the Workspace domain whose addresses are people; each
  * `writer` the exact address of an account that is not a person (another service's) that
  * edits files. It asks with `drive.readonly`: `tools/list`; `list_recent_files`;
  * `search_files` with each of [[Queries]], then with `parentId = '<id>'` for each parent the
  * items met so far name (recording the first answer holding a folder, a file, and an item of
  * a parent); `get_file_metadata` of a file whose parent is no folder, a folder's file, a
  * folder, and each parent no item met as a folder; `get_file_permissions` of a file under each
  * such parent that is no folder (a shared drive, as far as the answers tell); and
  * `read_file_content` of one Google Doc, comments left out. Each answer is written into `dir`
  * with [[Neutral]]'s stand-ins, never as it came; `tools/list` is written as it came, as it
  * names nothing of the deployment's. What it found is printed as kinds, counts and booleans,
  * and an error's text masked, never a title, an id or an address. Exits 1 when the key is
  * unset or not a key, or `tools/list` is not read.
  */
object DriveCapture {

  /** Drive's MCP endpoint. */
  val Endpoint = "https://drivemcp.googleapis.com/mcp/v1"

  val Key: Variable = Variable("GRIT_DRIVE_SA_KEY")

  val ReadOnly: Set[String] = Set("https://www.googleapis.com/auth/drive.readonly")

  val Folder = "application/vnd.google-apps.folder"

  val Doc = "application/vnd.google-apps.document"

  /** `search_files` queries in the tool's documented grammar, tried in order; then
    * `parentId = '<id>'` for each parent an item met by any other route names.
    */
  val Queries: Vector[String] = Vector(
    s"mimeType = '$Folder'",
    s"mimeType != '$Folder'",
    "sharedWithMe = true",
    "title contains 'a'",
    "modifiedTime > '2000-01-01T00:00:00Z'"
  )

  /** The most of an error's text printed. */
  val Said = 300

  /** A file or folder as an answer names it: its id, type, parent and drive, when given. */
  final case class Item(id: String, mimeType: String, parent: Option[String], drive: Option[String])

  def main(args: Array[String]): Unit = {
    val (domain, dir, writers) = args.toVector match {
      case Vector(d, o, w*) => (d, Path.of(o), w.toSet)
      case _ =>
        System.err.println("usage: DriveCapture <domain> <dir> [writer@…]")
        sys.exit(2)
    }
    val json = sys.env.get(Variable.value(Key)).map(_.trim).filter(_.nonEmpty) match {
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
    val self = Try(ujson.read(json)("client_email").str).getOrElse {
      System.err.println(s"${Variable.value(Key)} names no client_email")
      sys.exit(1)
    }
    val env = sys.env.updated(Variable.value(Key), json)
    val opened = for {
      server <- McpServer.of(
        "drive",
        Endpoint,
        Credential.serviceAccount(Key, ReadOnly),
        McpScope.Open
      )
      authorizer <- Credential.open(server.credential, env).left.map(_.message)
    } yield (server, authorizer)
    val (server, authorizer) = opened match {
      case Right(o) => o
      case Left(why) =>
        System.err.println(why)
        sys.exit(1)
    }
    val clock = Clock.system()
    val http = HttpClient.newHttpClient()
    // Scoped mutation: request ids, and every answer, kept in memory until stand-ins are known.
    var next = 0L
    val answers = mutable.ArrayBuffer.empty[(String, ujson.Obj)]
    val said = mutable.ArrayBuffer.empty[String]

    def ask(call: Rpc.Call): Either[McpError, ujson.Obj] =
      authorizer.bearer(clock).flatMap { bearer =>
        next += 1
        val id = next
        val response = LiveProbe.sent(http, server, bearer, id, call)
        val streamed =
          response.headers.firstValue("Content-Type").orElse("").startsWith("text/event-stream")
        val message =
          if (streamed) Sse.response(response.body.linesIterator, id)
          else Try(ujson.read(response.body)).toOption.toRight(McpError.Unreadable("not JSON"))
        message.flatMap(Rpc.result(id, _)).left.map {
          case McpError.Unreadable(_) if response.statusCode != 200 =>
            McpError.Status(response.statusCode)
          case e => e
        }
      }

    def report(line: String): Unit = said += line

    val tools = ask(Rpc.Call.ListTools(None)).flatMap(r => McpTool.page(r, server.name).map(r -> _))
    val page = tools match {
      case Right((result, page)) =>
        Files.createDirectories(dir)
        Files.writeString(dir.resolve("tools-list.json"), ujson.write(result, indent = 2))
        println(s"tools offered: ${page.tools.map(_.name).mkString(", ")}")
        println(s"tools skipped: ${page.skipped.size}")
        page
      case Left(e) =>
        System.err.println(s"tools/list: ${kind(e)}")
        sys.exit(1)
    }
    val tool = page.tools.map(t => t.name -> t).toMap

    def call(name: String, recording: String, arguments: ujson.Obj): Option[ujson.Obj] =
      tool.get(name) match {
        case None =>
          report(s"$recording: $name is not offered; not called")
          None
        case Some(t) =>
          ask(Rpc.Call.CallTool(t, arguments)) match {
            case Right(r) =>
              answers += recording -> r
              report(s"$recording: answered, isError ${isError(r)}${errorText(r)}")
              Some(r)
            case Left(e) =>
              report(s"$recording: ${kind(e)}")
              None
          }
      }

    val recent = call(
      "list_recent_files",
      "list-recent-files",
      ujson.Obj("pageSize" -> 100, "orderBy" -> "lastModified")
    ).map(items).getOrElse(Vector.empty)
    report(
      s"list-recent-files: ${recent.size} items, ${recent.count(_.mimeType == Folder)} folders"
    )

    // The first search answer holding a folder, a file, and an item of a named parent, recorded.
    val recorded = mutable.Set.empty[String]
    def search(q: String, label: String, parent: Boolean): Vector[Item] =
      tool.get("search_files") match {
        case None =>
          report("search_files is not offered; not called")
          Vector.empty
        case Some(t) =>
          ask(Rpc.Call.CallTool(t, ujson.Obj("query" -> q))) match {
            case Right(r) =>
              val found = items(r)
              report(s"search $label: isError ${isError(r)}, ${found.size} items${errorText(r)}")
              val names =
                if (parent) Vector("search-in-parent" -> found.nonEmpty)
                else
                  Vector(
                    "search-folders" -> found.exists(_.mimeType == Folder),
                    "search-files" -> found.exists(_.mimeType != Folder)
                  )
              names.collect { case (n, true) if !recorded(n) => n }.foreach { n =>
                recorded += n
                answers += n -> r
              }
              found
            case Left(e) =>
              report(s"search $label: ${kind(e)}")
              Vector.empty
          }
      }

    val searched = Queries.zipWithIndex.flatMap { case (q, i) =>
      search(q, s"query ${i + 1}", parent = false)
    }
    val parents = (recent ++ searched).flatMap(_.parent).distinct.take(10)
    val inParents = parents.zipWithIndex.flatMap { case (p, i) =>
      search(s"parentId = '$p'", s"parentId of item ${i + 1}", parent = true)
    }
    val pool = (recent ++ searched ++ inParents).distinctBy(_.id)
    val folders = pool.filter(_.mimeType == Folder)
    val files = pool.filterNot(_.mimeType == Folder)
    val folderIds = folders.map(_.id).toSet
    val top = files.find(_.parent.exists(p => !folderIds(p)))
    val inFolder = files.find(_.parent.exists(folderIds))
    val folder = folders.find(_.parent.exists(folderIds)).orElse(folders.headOption)
    val drives = (folders ++ files).flatMap(_.parent).filterNot(folderIds).distinct
    report(
      s"found ${folders.size} folders, ${files.size} files, ${drives.size} parents that are no folder"
    )
    report(s"items name a driveId: ${(folders ++ files).exists(_.drive.isDefined)}")

    def metadata(it: Item, recording: String): Unit =
      call("get_file_metadata", recording, ujson.Obj("fileId" -> it.id)).foreach { r =>
        val read = items(r).find(_.id == it.id)
        report(
          s"$recording: names its parent ${read.exists(_.parent.isDefined)}, " +
            s"the same parent as search ${read.flatMap(_.parent) == it.parent}, " +
            s"names a driveId ${read.exists(_.drive.isDefined)}"
        )
      }
    top.foreach(metadata(_, "metadata-file-top"))
    inFolder.foreach(metadata(_, "metadata-file-in-folder"))
    folder.foreach(metadata(_, "metadata-folder"))
    // A parent no item met as a folder: a shared drive, or a folder no answer listed.
    val asFolders = drives.zipWithIndex.flatMap { case (d, i) =>
      call("get_file_metadata", s"metadata-parent-${i + 1}", ujson.Obj("fileId" -> d)).toVector
        .flatMap { r =>
          val read = items(r).find(_.id == d)
          report(
            s"metadata-parent-${i + 1}: an item ${read.isDefined}, a folder ${read
                .exists(_.mimeType == Folder)}, titled ${read.isDefined && titled(r)}, names a parent ${read
                .exists(_.parent.isDefined)}"
          )
          read.filter(_.mimeType == Folder).map(_.id)
        }
    }.toSet
    val roots = drives.filterNot(asFolders)
    top.foreach { t =>
      report(s"the top file's driveId is its parent: ${t.drive.isDefined && t.drive == t.parent}")
    }

    def under(d: String): Option[Item] =
      files.find(f => f.parent.contains(d) || f.drive.contains(d)).orElse {
        val held = folders.filter(_.parent.contains(d)).map(_.id).toSet
        files.find(_.parent.exists(held))
      }
    roots.zipWithIndex.foreach { case (d, i) =>
      val letter = ('a' + i).toChar
      under(d).foreach { f =>
        call("get_file_permissions", s"permissions-drive-$letter", ujson.Obj("fileId" -> f.id))
          .foreach { r =>
            val text = ujson.write(r)
            report(
              s"permissions-drive-$letter: names the deployment's own address ${text.contains(self)}, " +
                s"permissionDetails ${text.contains("permissionDetails")}, " +
                s"inherited ${text.contains("\"inherited\"") || text.contains("\\\"inherited\\\"")}"
            )
          }
      }
    }

    files.find(_.mimeType == Doc).foreach { d =>
      call(
        "read_file_content",
        "read-file-content",
        ujson.Obj("fileId" -> d.id, "includeComments" -> false)
      )
        .foreach(r =>
          report(s"read-file-content: names fileContent ${ujson.write(r).contains("fileContent")}")
        )
    }

    val neutral = new Neutral(domain, self, writers, folderIds ++ asFolders, roots)
    answers.foreach { case (recording, r) =>
      val file = dir.resolve(s"$recording.json")
      Files.writeString(file, ujson.write(neutral.result(r), indent = 2))
      println(s"wrote $file")
    }
    said.foreach(println)
  }

  /** Whether `result` is an error. */
  private def isError(result: ujson.Obj): Boolean =
    result.value.get("isError").flatMap(_.boolOpt).getOrElse(false)

  /** `e` as its kind and status, and what the server said [[masked]]. */
  private def kind(e: McpError): String = e match {
    case McpError.Rpc(code, said) => s"error $code: ${masked(said)}"
    case McpError.Rejected(status, said) => s"HTTP $status: ${masked(said)}"
    case McpError.Status(code) => s"HTTP $code"
    case McpError.Unauthorized(_) => "HTTP 401"
    case McpError.Forbidden(_) => "HTTP 403"
    case other => other.productPrefix
  }

  /** An error result's text blocks, [[masked]], after `: `; empty when `result` is no error. */
  private def errorText(result: ujson.Obj): String =
    if (!isError(result)) ""
    else
      result.value
        .get("content")
        .flatMap(_.arrOpt)
        .toVector
        .flatten
        .flatMap(_.objOpt.flatMap(_.get("text")).flatMap(_.strOpt))
        .map(masked)
        .mkString(": ", " | ", "")

  /** `said`, at most [[Said]] characters, with any run of 20 or more id characters as `<id>`
    * and any word holding `@` as `<address>`, so no id or address is printed.
    */
  private def masked(said: String): String =
    said.linesIterator
      .mkString(" ")
      .split(" ")
      .map(w => if (w.contains('@')) "<address>" else w.replaceAll("[A-Za-z0-9_-]{20,}", "<id>"))
      .mkString(" ")
      .take(Said)

  /** The JSON `result` holds: its `structuredContent`, else each text block that is JSON. */
  private def bodies(result: ujson.Obj): Vector[ujson.Value] =
    result.value.get("structuredContent").toVector ++
      result.value.get("content").flatMap(_.arrOpt).toVector.flatten.flatMap { block =>
        block.objOpt
          .flatMap(_.get("text"))
          .flatMap(_.strOpt)
          .flatMap(t => Try(ujson.read(t)).toOption)
      }

  /** Every object in `result` naming an `id` and a `mimeType`. */
  private def items(result: ujson.Obj): Vector[Item] = {
    def walk(v: ujson.Value): Vector[Item] = v match {
      case o: ujson.Obj =>
        val here = for {
          id <- o.value.get("id").flatMap(_.strOpt)
          mime <- o.value.get("mimeType").flatMap(_.strOpt)
        } yield Item(
          id,
          mime,
          o.value
            .get("parentId")
            .flatMap(_.strOpt)
            .orElse(
              o.value.get("parents").flatMap(_.arrOpt).flatMap(_.headOption).flatMap(_.strOpt)
            ),
          o.value.get("driveId").flatMap(_.strOpt)
        )
        here.toVector ++ o.value.values.toVector.flatMap(walk)
      case ujson.Arr(vs) => vs.toVector.flatMap(walk)
      case _ => Vector.empty
    }
    bodies(result).headOption.toVector.flatMap(walk).distinctBy(_.id)
  }

  /** Whether some object in `result` names a non-blank `title` or `name`. */
  private def titled(result: ujson.Obj): Boolean = {
    def walk(v: ujson.Value): Boolean = v match {
      case o: ujson.Obj =>
        Vector("title", "name").exists(k =>
          o.value.get(k).flatMap(_.strOpt).exists(_.trim.nonEmpty)
        ) ||
        o.value.values.exists(walk)
      case ujson.Arr(vs) => vs.exists(walk)
      case _ => false
    }
    bodies(result).exists(walk)
  }
}
