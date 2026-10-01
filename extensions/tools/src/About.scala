package grit.tools

import scala.io.{Codec, Source}
import scala.util.Using

import grit.core.tool.{Args, Field, Gate, Outcome, Retry, Tool, ToolName, ToolSpec}

/** `about`: what grit is and how it works, for when the person asks, from docs shipped with
  * this module.
  */
object About {

  /** What `about` can be asked about, in the order its schema lists them. [[Subject.Grit]]
    * is the overview, answered when none is given.
    */
  enum Subject(val key: String) {
    case Grit extends Subject("grit")
    case Memory extends Subject("memory")
    case Markers extends Subject("markers")
    case Periods extends Subject("periods")
    case Places extends Subject("places")
  }

  /** `about` over the docs this module ships (`about/{key}.md` among its resources), read
    * now. Each call is answered with the doc of the subject asked for, or the overview; the
    * tool reads nothing more and never fails. `Left` naming each doc that is missing or
    * empty.
    */
  def load(): Either[String, Tool[Option[Subject]]] = {
    val read = Subject.values.toVector.map(s => s -> doc(s.key))
    read.collect { case (s, None) => s.key } match {
      case missing if missing.nonEmpty =>
        Left(s"about: no doc for ${missing.map(k => s"about/$k.md").mkString(", ")}")
      case _ =>
        val docs: Map[Subject, String] = read.collect { case (s, Some(text)) => s -> text }.toMap
        Right(tool(docs))
    }
  }

  /** The tool answering from `docs`, one for each subject. */
  private def tool(docs: Map[Subject, String]): Tool[Option[Subject]] =
    new Tool(
      ToolSpec(
        ToolName("about"),
        "What grit, the harness you work in, is and how it works: its memory (no " +
          "transcript), the [record], [afar] and [gap] labels, periods and how they close, " +
          "and places and edges. Call it when the person asks about grit. `topic` picks one; " +
          "without it, the overview.",
        Args
          .of(
            (topic =
              Field
                .oneOf(
                  "Which part: " + Subject.values.map(_.key).mkString(", ") + ".",
                  Subject.Grit.key,
                  Subject.values.toVector.drop(1).map(_.key)*
                )
                .optional
            )
          )
          .map(_.topic.flatMap(k => Subject.values.find(_.key == k))),
        retry = Retry.Rerun
      ),
      Gate.Free,
      topic => topic.fold("")(_.key),
      topic => Outcome.Done(docs.getOrElse(topic.getOrElse(Subject.Grit), ""))
    )

  /** The doc at `about/{key}.md`, trimmed; `None` when it is missing or blank. */
  private def doc(key: String): Option[String] =
    Option(getClass.getResourceAsStream(s"/about/$key.md"))
      .flatMap(in => Using(Source.fromInputStream(in)(using Codec.UTF8))(_.mkString).toOption)
      .map(_.trim)
      .filter(_.nonEmpty)
}
