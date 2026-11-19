package grit.projects.render

import scala.util.Random

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.method.{
  ArtifactKind,
  Declared,
  Example,
  Method,
  Mover,
  Movers,
  RoleView,
  Transition,
  TransitionAction,
  TransitionChecks
}
import grit.projects.name.{
  ArtifactKey,
  ArtifactNumber,
  Handle,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  RoleName,
  StateName
}
import grit.prose.form.{Block, Doc, Level, Mark, Span, Text}
import grit.prose.json.ProseJson

/** Projects from a seed, their titles and bodies holding exactly the hazards
  * `Markdown.writeExact`'s doc names: `\`, a backtick, brackets, `<`, `!`, `*` and `_` in text,
  * marked spans starting or ending in a space, and a span marked strong then one marked emphasis
  * ending inside a word; and line breaks, some after a space, which a body reads back only when
  * written as breaks; code holding a line break before two backticks, which reads back with a
  * space for it; beside them, references to other artifacts of the project. And methods from a
  * seed, with examples over such projects.
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
    val keys = Vector.tabulate(n)(i => ArtifactKey.of(key, right(ArtifactNumber.of(i + 1))))
    val artifacts = keys.zipWithIndex.map { (k, i) =>
      val others = keys.filterNot(_ == k)
      val head = ArtifactHead(
        k,
        Option.when(r.nextBoolean())(right(Handle.of(s"a$i"))),
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

  /** The method of `seed`, each of whose checks is one of `TransitionChecks.all`, and up to two
    * examples over [[project]]s, their moves not run: one to three kinds, each of one to four
    * states reached in a chain, some ending in their last; transitions besides the chain, some
    * naming checks, some merging deltas and some made only by people; up to two role views, some
    * carrying local keys; moves by a person or an agent, some expected refused as only people's.
    */
  def method(seed: Int): Declared = {
    val r = new Random(seed)
    val checks = TransitionChecks.all.map(_.name)
    val kinds = Vector("change", "note", "spec").take(1 + r.nextInt(3)).map { k =>
      val first = right(StateName.of(s"$k-s0"))
      val rest = Vector.tabulate(r.nextInt(4))(i => right(StateName.of(s"$k-s${i + 1}")))
      ArtifactKind(
        right(KindName.of(k)),
        first,
        rest,
        rest.lastOption.filter(_ => r.nextBoolean()).toSet,
        Vector.fill(r.nextInt(3))(pick(r, words))
      )
    }
    def some[A](as: Vector[A]): Vector[A] = as.filter(_ => r.nextInt(3) == 0)
    val transitions = kinds.flatMap { k =>
      val all = k.all
      val chain = all.zip(all.drop(1))
      val extra = all.filterNot(k.ends).flatMap(from => some(all).map(from -> _))
      (chain ++ extra).map { (from, to) =>
        Transition(
          k.name,
          from,
          to,
          some(checks),
          if (r.nextInt(4) == 0) TransitionAction.MergeDeltas(kinds(r.nextInt(kinds.size)).name)
          else TransitionAction.Nothing,
          if (r.nextInt(3) == 0) Movers.People else Movers.Anyone
        )
      }
    }
    val views = Vector.tabulate(r.nextInt(3)) { i =>
      right(
        RoleView.of(
          right(RoleName.of(s"r$i")),
          kinds.filter(_ => r.nextBoolean()).map(_.name).toSet + kinds(i % kinds.size).name,
          r.nextInt(RoleView.Deepest + 1),
          Set.fill(r.nextInt(3))(right(Handle.of(s"a${r.nextInt(6)}")))
        )
      )
    }
    val examples = Vector.tabulate(r.nextInt(3)) { i =>
      val start = project(r.nextInt())
      Example(
        s"example $i: ${line(r)}",
        start,
        Vector.fill(r.nextInt(4)) {
          Example.Move(
            start.artifacts(r.nextInt(start.artifacts.size)).head.key,
            right(StateName.of(pick(r, Vector("open", "building", "done")))),
            r.nextInt(3) match {
              case 0 => Example.Expect.Allowed
              case 1 => Example.Expect.RefusedPersonOnly
              case _ => Example.Expect.RefusedBy(checks(r.nextInt(checks.size)))
            },
            if (r.nextBoolean()) Mover.Person else Mover.Agent
          )
        }
      )
    }
    val method = Method.of(
      right(MethodName.of("m")),
      1 + r.nextInt(5),
      kinds,
      transitions,
      views,
      TransitionChecks.all
    )
    Declared(right(method.left.map(_.mkString("; "))), examples)
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
