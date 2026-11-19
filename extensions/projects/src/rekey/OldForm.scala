package grit.projects.rekey

import scala.util.Try

import grit.projects.artifact.{Body, Pin}
import grit.projects.name.{MethodName, ProjectKey}
import grit.projects.render.ProjectMarkdown
import grit.projects.render.ProjectMarkdown.{front, quoted}
import grit.prose.form.{Mark, Span}
import grit.prose.json.ProseJson
import grit.prose.markdown.Markdown

/** A project of the old form (ADR 0063), before ids, read as it was exported then. */
object OldForm {

  /** Project `project`, holding to `pin`, as [[ProjectMarkdown.render]] wrote it before ids,
    * byte for byte, from what its documents hold: `stored`, each artifact's store key and data;
    * or why not, naming the key: data that is not an old-form artifact's (its head with no id,
    * its body prose's stored form).
    */
  def exported(
      project: ProjectKey,
      pin: Pin,
      stored: Vector[(String, ujson.Value)]
  ): Either[String, Vector[(String, String)]] = {
    def text(head: collection.Map[String, ujson.Value], field: String): Either[String, String] =
      head.get(field).flatMap(_.strOpt).toRight(s"its head holds no $field")
    def nullable(head: collection.Map[String, ujson.Value], field: String)(
        shown: String => String
    ): Either[String, String] = head.get(field) match {
      case Some(ujson.Str(v)) => Right(shown(v))
      case Some(ujson.Null) | None => Right("null")
      case Some(other) => Left(s"its $field is ${other.render()}")
    }
    val index = ProjectMarkdown.Index -> front(
      Vector(
        "key" -> ProjectKey.value(project),
        "method" -> MethodName.value(pin.method),
        "version" -> pin.version.toString,
        "digest" -> pin.digest.hex
      )
    )
    stored
      .foldLeft[Either[String, Vector[(String, String)]]](Right(Vector(index))) {
        case (so, (key, data)) =>
          so.flatMap { done =>
            (for {
              head <- data.objOpt
                .flatMap(_.get("head"))
                .flatMap(_.objOpt)
                .toRight("it holds no head")
              _ <-
                if (head.contains("id")) Left("it has an id: it is not of the old form")
                else Right(())
              kind <- text(head, "kind")
              state <- text(head, "state")
              title <- text(head, "title")
              parent <- nullable(head, "parent")(identity)
              cites <- head
                .get("cites")
                .flatMap(_.arrOpt)
                .toRight("its head holds no cites")
                .flatMap { cs =>
                  Try(cs.map(_.str).mkString("[", ", ", "]")).toOption
                    .toRight("its cites are not keys")
                }
              by <- nullable(head, "by")(quoted)
              body <- data.objOpt.flatMap(_.get("body")).toRight("it holds no body")
              markdown <- markdown(body)
            } yield s"$key.md" -> (front(
              Vector(
                "key" -> key,
                "kind" -> kind,
                "state" -> state,
                "title" -> quoted(title),
                "parent" -> parent,
                "cites" -> cites,
                "by" -> by
              )
            ) + "\n" + markdown + "\n")).left
              .map(why => s"$key cannot be exported: $why")
              .map(done :+ _)
          }
      }
  }

  /** The markdown a stored body of the old form was exported as: as [[Body.markdown]] writes
    * it, each reference a link to `artifact:` and its key as stored; or why not, as
    * [[ProseJson.read]] refuses.
    */
  def markdown(json: ujson.Value): Either[String, String] =
    ProseJson
      .read(
        json,
        {
          case ujson.Str(s) => Right(s)
          case other => Left(s"a reference is not a key: $other")
        }
      )
      .map(doc =>
        Markdown.writeExact(doc.mapSpans { s =>
          Span(s.text, s.marks ++ s.notes.map(k => Mark.Link(Body.Scheme + k)), Vector.empty)
        })
      )
}
