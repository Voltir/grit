package grit.projects.render

import scala.util.Random

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.name.{ArtifactKey, KindName, MethodName, Pin, ProjectKey, StateName}
import grit.prose.form.{Block, Doc, Level, Mark, Span, Text}
import grit.prose.json.ProseJson

/** Projects from a seed, their titles and bodies holding exactly the hazards
  * `Markdown.writeExact`'s doc names: `\`, a backtick, brackets, `<`, `!`, `*` and `_` in text,
  * marked spans starting or ending in a space, and a span marked strong then one marked emphasis
  * ending inside a word; and line breaks, some after a space, which a body reads back only when
  * written as breaks; code holding a line break before two backticks, which reads back with a
  * space for it; beside them, references to other artifacts of the project.
  */
object ProjectGen {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val hazards: Vector[String] = Vector("\\", "`", "[", "]", "<", "!", "*", "_")
  private val words: Vector[String] = Vector("plan", "the", "cache", "zoë", "x2", "ends")

  /** The project of `seed`: one to six artifacts, a forest by parent, each citing others. */
  def project(seed: Int): Project = {
    val r = new Random(seed)
    val key = right(ProjectKey.of(s"p${r.nextInt(100)}"))
    val n = 1 + r.nextInt(6)
    val keys = Vector.tabulate(n)(i => right(ArtifactKey.of(key, s"a$i")))
    val artifacts = keys.zipWithIndex.map { (k, i) =>
      val others = keys.filterNot(_ == k)
      val head = ArtifactHead(
        k,
        right(KindName.of(pick(r, Vector("change", "note")))),
        right(StateName.of(pick(r, Vector("open", "building", "done")))),
        line(r),
        Option.when(i > 0 && r.nextBoolean())(keys(r.nextInt(i))),
        others.filter(_ => r.nextInt(3) == 0),
        Option.when(r.nextBoolean())(line(r))
      )
      Artifact(head, body(r, others))
    }
    val pin = right(
      Pin.read(
        right(MethodName.of("m")),
        1 + r.nextInt(3),
        Vector.fill(32)(f"${r.nextInt(256)}%02x").mkString
      )
    )
    right(Project.of(key, pin, artifacts))
  }

  private def pick(r: Random^, from: Vector[String]): String = from(r.nextInt(from.size))

  /** A title or a `by`: words and hazards, with what front matter must quote. */
  private def line(r: Random^): String =
    Vector
      .fill(1 + r.nextInt(5))(pick(r, words ++ hazards ++ Vector("\"", ": ", "#", "---")))
      .mkString(" ")

  private def body(r: Random^, others: Vector[ArtifactKey]): Body = {
    val blocks =
      Block.Paragraph(Text.of(Vector(Span.plain("Start.")))) +: Vector.fill(1 + r.nextInt(4)) {
        if (r.nextInt(4) == 0) Block.Heading(Level.of(2), text(r, others))
        else Block.Paragraph(text(r, others))
      }
    right(Body.fromStored(ProseJson.write(Doc(blocks), k => ujson.Str(k.value))))
  }

  /** Spans of words and hazards: plain, marked (some touching spaces), strong then emphasis
    * inside a word, or a reference to one of `others`.
    */
  private def text(r: Random^, others: Vector[ArtifactKey]): Text[ArtifactKey] =
    Text.of(
      Vector
        .fill(1 + r.nextInt(6)) {
          val w = pick(r, words) + pick(r, hazards ++ Vector("", "")) + pick(r, words)
          r.nextInt(7) match {
            case 0 => Vector(Span(w, Set(Mark.Strong)))
            case 1 => Vector(Span(s" $w ", Set(Mark.Emphasis)))
            case 2 =>
              Vector(Span(if (r.nextInt(3) == 0) s"$w\n``$w" else w, Set(Mark.Code)))
            case 3 =>
              Vector(
                Span("ab", Set(Mark.Strong)),
                Span("cd", Set(Mark.Emphasis)),
                Span("ef ", Set.empty)
              )
            case 4 if others.nonEmpty =>
              Vector(Span(w, Set.empty, Vector(others(r.nextInt(others.size)))))
            case 5 => Vector(Span.plain(w + (if (r.nextBoolean()) " \n" else "\n")))
            case _ => Vector(Span.plain(w + " "))
          }
        }
        .flatten
    )
}
