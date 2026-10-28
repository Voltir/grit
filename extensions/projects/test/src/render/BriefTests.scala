package grit.projects.render

import scala.util.Random

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.method.RoleView
import grit.projects.name.{ArtifactKey, KindName, MethodName, Pin, ProjectKey, RoleName, StateName}

import utest.*

object BriefTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private val pin: Pin = right(Pin.of(right(MethodName.of("m")), 1, "0f" * 32))

  /** The header of judged `p/j`, titled `j`, under method `m` version 1 and that pin. */
  private val header = "p/j | note | open | j | m v1 | pin 0f0f0f0f0f0f"

  private def key(local: String): ArtifactKey = right(ArtifactKey.of(p, local))

  private def artifact(
      local: String,
      kind: String,
      title: String,
      body: String,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty
  ): Artifact =
    Artifact(
      ArtifactHead(
        key(local),
        right(KindName.of(kind)),
        right(StateName.of("open")),
        title,
        parent.map(key),
        cites.map(key),
        None
      ),
      right(Body.read(body))
    )

  private def project(artifacts: Vector[Artifact]): Project = right(Project.of(p, pin, artifacts))

  private def view(kinds: Set[String], depth: Int): RoleView =
    right(RoleView.of(right(RoleName.of("builder")), kinds.map(k => right(KindName.of(k))), depth))

  private val everything: RoleView = view(Set("change", "note"), 3)

  private val fifty = "x" * 50

  val tests = Tests {
    test("a brief never exceeds its budget") {
      val results = for {
        seed <- (1 to 30).toVector
        pr = ProjectGen.project(seed)
        a <- pr.artifacts
        chars <- Vector(120, 400, 1500)
      } yield (chars, Brief.of(pr, a.head.key, everything, chars))
      assert(
        results.exists(_._2.isRight),
        results.forall { (chars, brief) => brief.fold(_ => true, b => b.text.length <= chars) }
      )
    }

    test("an artifact that fits once a larger one is pointed out is given whole") {
      val judged = artifact("j", "note", "j", "J", cites = Vector("a", "b"))
      val pr = project(
        Vector(judged, artifact("a", "note", "a", fifty * 10), artifact("b", "note", "b", fifty))
      )
      // `b`, the farther, is pointed out first, then `a`; with `a` a pointer, `b` fits whole.
      val expected =
        header + "\n\nJ\n\n## Cited\n\n- p/a (note, a, 500 chars)\n\n### p/b — b\n\n" + fifty
      assert(
        Brief.of(pr, key("j"), everything, expected.length).map(b => (b.text, b.pointed)) ==
          Right((expected, Vector(key("a"))))
      )
    }

    test("what does not fit is a pointer line, the farthest citation first") {
      val judged = artifact("j", "note", "j", "J", cites = Vector("a", "b", "c"))
      val pr = project(
        Vector(
          judged,
          artifact("a", "note", "a", fifty),
          artifact("b", "note", "b", fifty),
          artifact("c", "note", "c", fifty)
        )
      )
      // Only `a` fits whole: `b` and `c` are the farthest, so they become pointers.
      val expected =
        header + "\n\nJ\n\n## Cited\n\n### p/a — a\n\n" + fifty +
          "\n\n- p/b (note, b, 50 chars)\n\n- p/c (note, c, 50 chars)"
      assert(
        Brief.of(pr, key("j"), everything, expected.length).map(b => (b.text, b.pointed)) ==
          Right((expected, Vector(key("b"), key("c"))))
      )
    }

    test("an artifact over the budget is Left with its size") {
      val size = header.length + "\n\n".length + "Hello.".length
      val pr = project(Vector(artifact("j", "note", "j", "Hello.")))
      assert(
        Brief.of(pr, key("j"), everything, size - 1) == Left(BriefRefusal.Over(size)),
        Brief.of(pr, key("j"), everything, size).isRight
      )
    }

    test("a role view's kinds and depth bound what is cited") {
      // j cites a (note), a cites b (change), b cites c (note), c cites d (note).
      val pr = project(
        Vector(
          artifact("j", "note", "j", "J", cites = Vector("a")),
          artifact("a", "note", "a", "B", cites = Vector("b")),
          artifact("b", "change", "b", "B", cites = Vector("c")),
          artifact("c", "note", "c", "B", cites = Vector("d")),
          artifact("d", "note", "d", "B")
        )
      )
      val twoLinks = Brief.of(pr, key("j"), view(Set("note"), 2), 10000)
      val threeLinks = Brief.of(pr, key("j"), view(Set("note"), 3), 10000)
      assert(
        twoLinks.map(_.text.contains("### p/a — a")) == Right(true),
        twoLinks.map(_.text.contains("p/b")) == Right(false),
        twoLinks.map(_.text.contains("p/c")) == Right(false),
        threeLinks.map(_.text.contains("### p/c — c")) == Right(true),
        threeLinks.map(_.text.contains("p/d")) == Right(false)
      )
    }

    test("a brief is the same for a project's artifacts in any order") {
      val seeds = (1 to 20).toVector
      val shuffled = seeds.map { seed =>
        val pr = ProjectGen.project(seed)
        (pr, right(Project.of(pr.key, pr.pin, new Random(seed + 1000).shuffle(pr.artifacts))))
      }
      assert(shuffled.exists((pr, s) => pr.artifacts != s.artifacts))
      assert(
        shuffled.forall { (pr, s) =>
          pr.artifacts.forall { a =>
            Brief.of(pr, a.head.key, everything, 400) == Brief.of(s, a.head.key, everything, 400)
          }
        }
      )
    }

    test("a key that is no artifact of the project is Unknown") {
      val pr = project(Vector(artifact("j", "note", "j", "J")))
      assert(Brief.of(pr, key("zz"), everything, 1000) == Left(BriefRefusal.Unknown(key("zz"))))
    }
  }
}
