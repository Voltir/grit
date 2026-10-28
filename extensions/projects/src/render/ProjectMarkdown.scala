package grit.projects.render

import scala.util.Try

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.name.{ArtifactKey, KindName, MethodName, Pin, ProjectKey, StateName}

/** A project as files, one per artifact, and back. */
object ProjectMarkdown {

  /** `project.md`: the file holding a project's key and pin. */
  val Index: String = "project.md"

  /** `project` as files: `<key>.md` each, YAML front matter (`key`, `kind`, `state`, `title`,
    * `parent`, `cites`, `by`; a title and a `by` as quoted strings, an absent one as `null`,
    * cites as a flow list) then its body as markdown; and one [[Index]] holding its `key`, and
    * its pin's `method`, `version` and `digest`. [[read]] of them is a project with the same
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
    )) +: project.artifacts.map(file)

  /** `artifact`'s file as [[render]] writes it: its path, and its text. */
  def file(artifact: Artifact): (String, String) = {
    val h = artifact.head
    s"${h.key.value}.md" -> (front(
      Vector(
        "key" -> h.key.value,
        "kind" -> KindName.value(h.kind),
        "state" -> StateName.value(h.state),
        "title" -> quoted(h.title),
        "parent" -> h.parent.fold("null")(_.value),
        "cites" -> h.cites.map(_.value).mkString("[", ", ", "]"),
        "by" -> h.by.fold("null")(quoted)
      )
    ) + "\n" + Body.markdown(artifact.body) + "\n")
  }

  /** Files as [[render]] writes them, as a project, or every file that is not one, with why: a
    * missing or unreadable [[Index]], a file whose front matter is not [[render]]'s, a file not
    * at its key's path, a body [[Body.read]] refuses, or what [[Project.of]] refuses, named by
    * the file of the artifact it is about.
    */
  def read(files: Vector[(String, String)]): Either[Vector[(String, String)], Project] = {
    val (indexes, others) = files.partition(_._1 == Index)
    val pinned = indexes match {
      case Vector((_, text)) => index(text).left.map(Index -> _)
      case Vector() => Left(Index -> s"no $Index")
      case _ => Left(Index -> s"$Index is there ${indexes.size} times")
    }
    val artifacts = others.map((path, text) => artifact(path, text).left.map(path -> _))
    val bad = pinned.left.toOption.toVector ++ artifacts.flatMap(_.left.toOption)
    pinned match {
      case Right((key, pin)) if bad.isEmpty =>
        val read = artifacts.flatMap(_.toOption)
        Project.refusals(key, read) match {
          case Vector() => Project.of(key, pin, read).left.map(why => Vector(Index -> why))
          case refused => Left(refused.map((k, why) => s"${k.value}.md" -> why))
        }
      case _ => Left(bad)
    }
  }

  private val IndexFields: Vector[String] = Vector("key", "method", "version", "digest")
  private val HeadFields: Vector[String] =
    Vector("key", "kind", "state", "title", "parent", "cites", "by")

  private def front(fields: Vector[(String, String)]): String =
    fields.map((k, v) => s"$k: $v").mkString("---\n", "\n", "\n---\n")

  private def quoted(s: String): String = ujson.write(ujson.Str(s))

  private def index(text: String): Either[String, (ProjectKey, Pin)] =
    for {
      (fields, _) <- split(text, IndexFields)
      key <- ProjectKey.of(fields("key"))
      method <- MethodName.of(fields("method"))
      version <- fields("version").toIntOption.toRight(
        s"version is not a number: ${fields("version")}"
      )
      pin <- Pin.read(method, version, fields("digest"))
    } yield (key, pin)

  private def artifact(path: String, text: String): Either[String, Artifact] =
    for {
      (fields, body) <- split(text, HeadFields)
      key <- ArtifactKey.read(fields("key"))
      _ <- Either.cond(
        path == s"${key.value}.md",
        (),
        s"the file of ${key.value} is not ${key.value}.md"
      )
      kind <- KindName.of(fields("kind"))
      state <- StateName.of(fields("state"))
      title <- unquoted("title", fields("title"))
      parent <- nullable(fields("parent"))(ArtifactKey.read)
      cites <- list(fields("cites"))
      by <- nullable(fields("by"))(unquoted("by", _))
      read <- Body.read(body)
    } yield Artifact(ArtifactHead(key, kind, state, title, parent, cites, by), read)

  /** `text`'s front matter, holding exactly `names`, each once, as each name's value (empty
    * for any other name), and what follows it.
    */
  private def split(text: String, names: Vector[String]): Either[String, (Fields, String)] = {
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
          val named = pairs.map(_._1)
          if (named.sorted != names.sorted)
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
