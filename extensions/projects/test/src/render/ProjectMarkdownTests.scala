package grit.projects.render

import grit.core.id.DocumentVersionId
import grit.projects.artifact.Pin
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project, Rank}
import grit.projects.method.MethodFixtures
import grit.projects.name.{ArtifactKey, ArtifactNumber, KindName, MethodName, ProjectKey, StateName}

import utest.*

object ProjectMarkdownTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private val pin: Pin = right(Pin.read(right(MethodName.of("m")), 1, "0f" * 32))

  private def key(local: String): ArtifactKey = MethodFixtures.key(s"p/$local")

  private def artifact(
      local: String,
      title: String,
      body: String,
      parent: Option[String] = None
  ): Artifact =
    Artifact(
      ArtifactHead(
        key(local),
        MethodFixtures.handleOf(key(local)),
        right(KindName.of("note")),
        right(StateName.of("open")),
        title,
        parent.map(key),
        Rank.Middle,
        Vector.empty,
        None
      ),
      right(Body.read(body, MethodFixtures.ids))
    )

  private def heads(project: Project): Vector[ArtifactHead] = project.artifacts.map(_.head)
  private def links(project: Project): Vector[Vector[ArtifactKey]] =
    project.artifacts.map(a => Body.cites(a.body))

  /** Why `project` breaks the round-trip law, with its files, or `None`. */
  private def broken(project: Project): Option[String] = {
    val files = ProjectMarkdown.render(project)
    val once = ProjectMarkdown.read(files)
    val twice = once.flatMap(q => ProjectMarkdown.read(ProjectMarkdown.render(q)))
    val byKey = project.artifacts.map(a => a.head.key -> a.head).toMap
    def numbers(h: ArtifactHead): Vector[Int] =
      h.parent.flatMap(byKey.get).fold(Vector.empty)(numbers) :+ ArtifactNumber.value(h.key.number)
    // Of numbers and parents alone, so a retitle or a new handle moves no file.
    val filed = project.artifacts.map(a => numbers(a.head).mkString("", "/", ".md"))
    val why =
      if (files.map(_._1) != ProjectMarkdown.Index +: filed)
        Some(s"not each at its parents' numbers: ${files.map(_._1)} != $filed")
      else if (
        ProjectMarkdown.read(files.reverse).map(q => heads(q).sortBy(_.key)) != Right(
          heads(project).sortBy(_.key)
        )
      )
        Some(s"read depends on the files' order: ${ProjectMarkdown.read(files.reverse)}")
      else if (once.map(heads) != Right(heads(project))) Some(s"heads differ after one pass: $once")
      else if (once.map(links) != Right(links(project)))
        Some(s"links differ after one pass: ${once.map(links)} != ${links(project)}")
      else if (twice != once)
        Some(
          s"not a fixed point after one pass: ${once.map(_.artifacts.map(a => Body.markdown(a.body)))} then ${twice.map(_.artifacts.map(a => Body.markdown(a.body)))}"
        )
      else None
    why.map(w => s"$w\nfiles:\n${files.map((path, text) => s"== $path\n$text").mkString}")
  }

  val tests = Tests {
    test("read after render is a fixed point after one pass, and keeps every head and link") {
      val failing =
        (0 until 200).iterator.map(seed => seed -> broken(ProjectGen.project(seed))).collectFirst {
          case (seed, Some(why)) => s"seed $seed: $why"
        }
      assert(failing == None)
    }
    test("a file whose front matter names an unknown parent is reported, with the file") {
      val project = right(Project.of(p, pin, Vector(artifact("a", "A", "Some text."))))
      val files = ProjectMarkdown.render(project).map { (path, text) =>
        path -> text.replace("parent: null", "parent: p/7")
      }
      assert(
        ProjectMarkdown.read(files) == Left(
          Vector("1.md" -> "p/1's parent p/7 is no artifact of it")
        )
      )
    }
    test("a reference inside a body survives a rename of the cited artifact's title") {
      val cited = artifact("b", "The design", "What it is.")
      val citing = artifact("a", "A", "Built as [the design](artifact:p/b) says.")
      val renamed = cited.copy(head = cited.head.copy(title = "The final design"))
      val read = ProjectMarkdown.read(
        ProjectMarkdown.render(right(Project.of(p, pin, Vector(citing, renamed))))
      )
      val body =
        read.map(_.artifacts.filter(_.head.key == key("a")).map(a => Body.markdown(a.body)))
      assert(
        body == Right(Vector("Built as [the design](artifact:p/2) says.")),
        read.map(_.artifacts.flatMap(a => Body.cites(a.body))) == Right(Vector(key("b")))
      )
    }
    test(
      "a body with brackets and asterisks in plain text shows them bare, and reads back the same"
    ) {
      val text = "Tagged [edges, projects]: 2 * 3 is six, snake_case [x] done."
      val body = right(Body.read(text, MethodFixtures.ids))
      assert(
        Body.markdown(body) == text,
        Body.read(Body.markdown(body), MethodFixtures.ids) == Right(body)
      )
    }

    test("an artifact as project_show gives it reads back whole, with its version") {
      // Its body as read does once: markdown normalises in one pass.
      def normal(a: Artifact): Artifact =
        Artifact(a.head, right(Body.read(Body.markdown(a.body), MethodFixtures.ids)))
      val failing = (0 until 200).iterator
        .flatMap(seed => ProjectGen.project(seed).artifacts.map(a => seed -> normal(a)))
        .zipWithIndex
        .map { case ((seed, a), i) =>
          val version = DocumentVersionId.of(i.toLong + 1).getOrElse(sys.error("no version"))
          (seed, a, ProjectMarkdown.readShown(ProjectMarkdown.shown(a, version)), version)
        }
        .collectFirst {
          case (seed, a, read, version) if read != Right((a, version)) =>
            s"seed $seed, ${a.head.key.value}: $read"
        }
      assert(failing == None)
    }

    test("an exported file, which holds no version, is not read as shown") {
      val text = ProjectMarkdown.text(artifact("a", "A", "Body."))
      ProjectMarkdown.readShown(text) ==> Left(
        "front matter names id, handle, kind, state, title, parent, rank, cites, by, " +
          "not id, handle, kind, state, title, parent, rank, cites, by, version"
      )
    }

    test("a shown file whose version is no whole number of 1 or more is refused, naming it") {
      val one = DocumentVersionId.of(1).getOrElse(sys.error("no version"))
      val shown = ProjectMarkdown.shown(artifact("a", "A", "Body."), one)
      def at(version: String) =
        ProjectMarkdown.readShown(shown.replace("version: 1\n", s"version: $version\n"))
      assert(shown.contains("version: 1\n"))
      at("0") ==> Left("version 0 is no version: a whole number, 1 or more")
      at("x") ==> Left("version x is no version: a whole number, 1 or more")
    }
    test("a hard-wrapped paragraph is kept as one paragraph with no breaks") {
      val body = right(
        Body.read("A plan wrapped\nat its source's width,\nas files are.", MethodFixtures.ids)
      )
      assert(Body.markdown(body) == "A plan wrapped at its source's width, as files are.")
    }
    test("a parent cycle is refused, naming each artifact in it") {
      val cycle = Vector(
        artifact("a", "A", "x", parent = Some("b")),
        artifact("b", "B", "y", parent = Some("a"))
      )
      assert(Project.of(p, pin, cycle) == Left("p/1 is its own ancestor; p/2 is its own ancestor"))
    }
    test("each artifact's file is at its parents' numbers, the root's first") {
      val nested = right(
        Project.of(
          p,
          pin,
          Vector(
            artifact("a", "A", "x", parent = Some("c")),
            artifact("b", "B", "y", parent = Some("a")),
            artifact("c", "C", "z")
          )
        )
      )
      ProjectMarkdown.render(nested).map(_._1) ==> Vector(
        "project.md",
        "3/1.md",
        "3/1/2.md",
        "3.md"
      )
    }
    test("a file not at its parents' numbers is refused, naming it") {
      val nested = right(
        Project.of(
          p,
          pin,
          Vector(artifact("a", "A", "x", parent = Some("b")), artifact("b", "B", "y"))
        )
      )
      val moved = ProjectMarkdown.render(nested).map {
        case (_, text) if text.contains("title: \"A\"") => "1.md" -> text
        case file => file
      }
      ProjectMarkdown.read(moved) ==> Left(
        Vector("1.md" -> "p/1 belongs at 2/1.md, its parents' numbers then its own")
      )
    }
  }
}
