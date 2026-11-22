package grit.projects.render

import scala.util.Try

import grit.core.document.DocRef
import grit.core.id.DocumentVersionId
import grit.projects.artifact.Pin
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project, Rank}
import grit.projects.name.{
  ArtifactKey,
  ArtifactNumber,
  ArtifactRef,
  Handle,
  KindName,
  MethodName,
  ProjectKey,
  RoleName,
  StateName
}

/** A project as files, one per artifact, and back. */
object ProjectMarkdown {

  /** `project.md`: the file holding a project's key and pin. */
  val Index: String = "project.md"

  /** `project` as files: one per artifact at the numbers of its chain ([[Project.chain]]),
    * `12.md` for a root and `12/15.md` for its child, holding [[text]]; and one [[Index]]
    * holding the project's `key`, and its pin's `method`, `version` and `digest`. A retitle or a
    * new handle moves no file; a new parent does. [[read]] of them is a project with the same
    * heads and links; read again after a second render, the same project: markdown is written
    * for a reader, so a body may normalise once.
    */
  def render(project: Project): Vector[(String, String)] =
    (Index -> front(
      Vector(
        "key" -> ProjectKey.value(project.key),
        "method" -> MethodName.value(project.pin.method),
        "version" -> project.pin.version.toString,
        "digest" -> project.pin.digest.hex
      )
    )) +: project.artifacts.map(a => path(project, a.head.key) -> text(a))

  /** The path [[render]] writes `key`'s file at in `project`. */
  private def path(project: Project, key: ArtifactKey): String =
    project.chain(key).map(k => ArtifactNumber.value(k.number)).mkString("", "/", ".md")

  /** `artifact`'s file text as [[render]] writes it: YAML front matter (`id`, `handle`, `kind`,
    * `state`, `title`, `parent`, `rank`, `cites`, `by`; a title and a `by` as quoted strings, an
    * absent one as `null`, references by id, cites as a flow list), then its body as markdown.
    */
  def text(artifact: Artifact): String =
    filed(artifact.head, None, Vector.empty, Body.markdown(artifact.body))

  /** `artifact` kept as `version`, as `project_show` gives it: [[text]] with a last front
    * matter line `version: <n>`, which [[readShown]] reads back and [[read]] never does: an export
    * holds no version. When `over` names any role, a line after it,
    * `brief: <role> <size> of 9,500`, each such role in order, `; ` between them, says that role's
    * brief of it would be `size` characters, past what a brief holds ([[Brief.size]]).
    */
  def shown(
      artifact: Artifact,
      version: DocumentVersionId,
      over: Vector[(role: RoleName, size: Int)] = Vector.empty
  ): String =
    filed(artifact.head, Some(version), over, Body.markdown(artifact.body))

  /** `text` as [[shown]] writes it, read back: the artifact, and the version it was kept as, its
    * `brief` line, when it has one, passed over; or why not: front matter that is not [[shown]]'s
    * (an exported file's, with no `version`, among them), a version that is no whole number of 1
    * or more, or what [[read]] refuses of one artifact's file.
    */
  def readShown(text: String): Either[String, (Artifact, DocumentVersionId)] =
    for {
      (fields, body) <- frontMatter(text, HeadFields :+ "version", Vector(BriefField))
      version <- fields("version").toLongOption
        .flatMap(DocumentVersionId.of)
        .toRight(s"version ${fields("version")} is no version: a whole number, 1 or more")
      artifact <- headed(fields, body)
    } yield (artifact, version)

  /** `head`'s front matter, `version` and the briefs `over` their budget last ([[shown]]), then
    * `text`.
    */
  private def filed(
      head: ArtifactHead,
      version: Option[DocumentVersionId],
      over: Vector[(role: RoleName, size: Int)],
      text: String
  ): String =
    fronted(head, version, over) + "\n" + text + "\n"

  /** `artifact` kept as `version` without its whole body: [[shown]]'s front matter, then its body's
    * first paragraph whole ([[Body.lead]]), then `Sections: ` and its `##` headings' titles, `; `
    * between them (`none` when it has none), with the size of its body ([[AnswerText.size]]):
    * `Sections: Goal; Reads (2.4 KB in all)`.
    */
  def head(
      artifact: Artifact,
      version: DocumentVersionId,
      over: Vector[(role: RoleName, size: Int)] = Vector.empty
  ): String = {
    val sections = Body.sections(artifact.body)
    val lead = Body.lead(artifact.body, Int.MaxValue)
    fronted(artifact.head, Some(version), over) + "\n" + Vector(lead)
      .filter(_.nonEmpty)
      .map(_ + "\n\n")
      .mkString +
      s"Sections: ${if (sections.isEmpty) "none" else sections.mkString("; ")} " +
      s"(${AnswerText.size(artifact.body)} in all)\n"
  }

  /** `artifact` kept as `version` with one section of its body: [[shown]]'s front matter, then
    * the section titled `title` ([[Body.section]]); or why that section is refused.
    */
  def section(
      artifact: Artifact,
      version: DocumentVersionId,
      title: String,
      over: Vector[(role: RoleName, size: Int)] = Vector.empty
  ): Either[String, String] =
    Body.section(artifact.body, title).map(filed(artifact.head, Some(version), over, _))

  /** `h` as front matter, `version` and then the briefs `over` their budget last when given. */
  private def fronted(
      h: ArtifactHead,
      version: Option[DocumentVersionId],
      over: Vector[(role: RoleName, size: Int)] = Vector.empty
  ): String =
    front(
      Vector(
        "id" -> h.key.value,
        "handle" -> h.handle.fold("null")(Handle.value),
        "kind" -> KindName.value(h.kind),
        "state" -> StateName.value(h.state),
        "title" -> quoted(h.title),
        "parent" -> h.parent.fold("null")(_.value),
        "rank" -> Rank.value(h.rank),
        "cites" -> h.cites.map(_.value).mkString("[", ", ", "]"),
        "by" -> h.by.fold("null")(quoted)
      ) ++ version.map(v => "version" -> DocumentVersionId.value(v).toString) ++
        Option
          .when(over.nonEmpty)(
            BriefField -> over
              .map(o => f"${RoleName.value(o.role)} ${o.size}%,d of ${DocRef.BriefChars}%,d")
              .mkString("; ")
          )
          .toVector
    )

  /** The front matter field [[shown]] names the briefs over their budget in. */
  private val BriefField: String = "brief"

  /** Files as [[render]] writes them, as a project, or every file that is not one, with why: a
    * missing or unreadable [[Index]], a file whose front matter is not [[render]]'s, a body
    * [[Body.read]] refuses, what [[Project.of]] refuses, or a file not at its chain's path, each
    * named by the file it is about.
    */
  def read(files: Vector[(String, String)]): Either[Vector[(String, String)], Project] = {
    val (indexes, others) = files.partition(_._1 == Index)
    val pinned = indexes match {
      case Vector((_, text)) => index(text).left.map(Index -> _)
      case Vector() => Left(Index -> s"no $Index")
      case _ => Left(Index -> s"$Index is there ${indexes.size} times")
    }
    val artifacts = others.map((path, text) => artifact(text).map(path -> _).left.map(path -> _))
    val bad = pinned.left.toOption.toVector ++ artifacts.flatMap(_.left.toOption)
    pinned match {
      case Right((key, pin)) if bad.isEmpty =>
        val read = artifacts.flatMap(_.toOption)
        def at(k: ArtifactKey): Vector[String] = read.collect {
          case (path, a) if a.head.key == k => path
        }
        Project.refusals(key, read.map(_._2)) match {
          case Vector() =>
            Project.of(key, pin, read.map(_._2)).left.map(why => Vector(Index -> why)).flatMap {
              project =>
                read.collect {
                  case (found, a) if found != path(project, a.head.key) =>
                    found -> s"${a.head.key.value} belongs at ${path(project, a.head.key)}, its parents' numbers then its own"
                } match {
                  case Vector() => Right(project)
                  case misplaced => Left(misplaced)
                }
            }
          case refused => Left(refused.flatMap((k, why) => at(k).map(_ -> why)))
        }
      case _ => Left(bad)
    }
  }

  private val IndexFields: Vector[String] = Vector("key", "method", "version", "digest")
  private val HeadFields: Vector[String] =
    Vector("id", "handle", "kind", "state", "title", "parent", "rank", "cites", "by")

  private[projects] def front(fields: Vector[(String, String)]): String =
    fields.map((k, v) => s"$k: $v").mkString("---\n", "\n", "\n---\n")

  private[projects] def quoted(s: String): String = ujson.write(ujson.Str(s))

  private def index(text: String): Either[String, (ProjectKey, Pin)] =
    for {
      (fields, _) <- frontMatter(text, IndexFields)
      key <- ProjectKey.of(fields("key"))
      method <- MethodName.of(fields("method"))
      version <- fields("version").toIntOption.toRight(
        s"version is not a number: ${fields("version")}"
      )
      pin <- Pin.read(method, version, fields("digest"))
    } yield (key, pin)

  private def artifact(text: String): Either[String, Artifact] =
    frontMatter(text, HeadFields).flatMap((fields, body) => headed(fields, body))

  /** The artifact `fields`, a front matter's [[HeadFields]], and `body` hold, or why not. */
  private def headed(fields: Fields, body: String): Either[String, Artifact] =
    for {
      key <- ArtifactKey.read(fields("id"))
      handle <- nullable(fields("handle"))(Handle.of)
      kind <- KindName.of(fields("kind"))
      state <- StateName.of(fields("state"))
      title <- unquoted("title", fields("title"))
      parent <- nullable(fields("parent"))(ArtifactKey.read)
      rank <- Rank.of(fields("rank"))
      cites <- list(fields("cites"))
      by <- nullable(fields("by"))(unquoted("by", _))
      read <- Body.read(body, ArtifactRef.id)
    } yield Artifact(ArtifactHead(key, handle, kind, state, title, parent, rank, cites, by), read)

  /** `text`'s front matter, holding exactly `names`, each once, as each name's value (empty
    * for any other name), and what follows it.
    */
  private def frontMatter(
      text: String,
      names: Vector[String],
      optional: Vector[String] = Vector.empty
  ): Either[String, (Fields, String)] = {
    val end = text.indexOf("\n---\n", 3)
    if (!text.startsWith("---\n") || end < 0) Left("no front matter between --- lines")
    else {
      val lines = text.substring(4, end).split("\n", -1).toVector
      val fields = lines.map { l =>
        l.indexOf(": ") match {
          case -1 => Left(s"not a `name: value` line: $l")
          case i => Right(l.take(i) -> l.drop(i + 2))
        }
      }
      fields.collectFirst { case Left(why) => why } match {
        case Some(why) => Left(why)
        case None =>
          val pairs = fields.collect { case Right(p) => p }
          val named = pairs.map(_._1).filterNot(optional.contains)
          if (named.sorted != names.sorted || pairs.size - named.size > optional.size)
            Left(s"front matter names ${named.mkString(", ")}, not ${names.mkString(", ")}")
          else {
            val values = pairs.toMap
            Right(Fields(values) -> text.substring(end + 5))
          }
      }
    }
  }

  /** A front matter's values by name, empty for a name it does not hold. */
  private final case class Fields(values: Map[String, String]) extends caps.Pure {
    def apply(name: String): String = values.getOrElse(name, "")
  }

  private def unquoted(name: String, v: String): Either[String, String] =
    Try(ujson.read(v)).toOption match {
      case Some(ujson.Str(s)) => Right(s)
      case _ => Left(s"$name is not a quoted string: $v")
    }

  private def nullable[A](v: String)(f: String => Either[String, A]): Either[String, Option[A]] =
    if (v == "null") Right(None) else f(v).map(Some(_))

  private def list(v: String): Either[String, Vector[ArtifactKey]] =
    if (!v.startsWith("[") || !v.endsWith("]")) Left(s"cites is not a [list]: $v")
    else {
      val inner = v.drop(1).dropRight(1).trim
      val read =
        if (inner.isEmpty) Vector.empty
        else inner.split(",").toVector.map(s => ArtifactKey.read(s.trim))
      read.collectFirst { case Left(why) => why }.toLeft(read.collect { case Right(k) => k })
    }
}
