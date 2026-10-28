package grit.projects.render

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.name.{ArtifactKey, KindName, MethodName, Pin, ProjectKey, StateName}

import utest.*

object ProjectMarkdownTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private val pin: Pin = right(Pin.of(right(MethodName.of("m")), 1, "0f" * 32))

  private def key(local: String): ArtifactKey = right(ArtifactKey.of(p, local))

  private def artifact(
      local: String,
      title: String,
      body: String,
      parent: Option[String] = None
  ): Artifact =
    Artifact(
      ArtifactHead(
        key(local),
        right(KindName.of("note")),
        right(StateName.of("open")),
        title,
        parent.map(key),
        Vector.empty,
        None
      ),
      right(Body.read(body))
    )

  private def heads(project: Project): Vector[ArtifactHead] = project.artifacts.map(_.head)
  private def links(project: Project): Vector[Vector[ArtifactKey]] =
    project.artifacts.map(a => Body.cites(a.body))

  /** Why `project` breaks the round-trip law, with its files, or `None`. */
  private def broken(project: Project): Option[String] = {
    val files = ProjectMarkdown.render(project)
    val once = ProjectMarkdown.read(files)
    val twice = once.flatMap(q => ProjectMarkdown.read(ProjectMarkdown.render(q)))
    val why =
      if (once.map(heads) != Right(heads(project))) Some(s"heads differ after one pass: $once")
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
      val started = System.nanoTime()
      val failing =
        (0 until 200).iterator.map(seed => seed -> broken(ProjectGen.project(seed))).collectFirst {
          case (seed, Some(why)) => s"seed $seed: $why"
        }
      val millis = (System.nanoTime() - started) / 1000000
      // A unit test's budget, not a measure: one second failed under a module's other suites.
      assert(failing == None, millis < 5000)
    }
    test("a file whose front matter names an unknown parent is reported, with the file") {
      val project = right(Project.of(p, pin, Vector(artifact("a", "A", "Some text."))))
      val files = ProjectMarkdown.render(project).map { (path, text) =>
        path -> text.replace("parent: null", "parent: p/ghost")
      }
      assert(
        ProjectMarkdown.read(files) == Left(
          Vector("p/a.md" -> "p/a's parent p/ghost is no artifact of it")
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
        body == Right(Vector("Built as [the design](artifact:p/b) says.")),
        read.map(_.artifacts.flatMap(a => Body.cites(a.body))) == Right(Vector(key("b")))
      )
    }
    test("a parent cycle is refused, naming each artifact in it") {
      val cycle = Vector(
        artifact("a", "A", "x", parent = Some("b")),
        artifact("b", "B", "y", parent = Some("a"))
      )
      assert(Project.of(p, pin, cycle) == Left("p/a is its own ancestor; p/b is its own ancestor"))
    }
  }
}
