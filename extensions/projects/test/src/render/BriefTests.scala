package grit.projects.render

import scala.util.Random

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project, Rank}
import grit.projects.method.{MethodFixtures, RoleView}
import grit.projects.name.{
  ArtifactKey,
  Handle,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  RoleName,
  StateName
}

import utest.*

object BriefTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private val pin: Pin = right(Pin.read(right(MethodName.of("m")), 1, "0f" * 32))

  /** The header of judged `p/j`, titled `j`, under method `m` version 1 and that pin. */
  private val header = "p/9 j | note | open | j | m v1 | pin 0f0f0f0f0f0f"

  private def key(local: String): ArtifactKey = MethodFixtures.key(s"p/$local")

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
        MethodFixtures.handleOf(key(local)),
        right(KindName.of(kind)),
        right(StateName.of("open")),
        title,
        parent.map(key),
        Rank.Middle,
        cites.map(key),
        None
      ),
      right(Body.read(body, MethodFixtures.ids))
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
        header + "\n\nJ\n\n## Cited\n\n- p/1 a (note, a, 500 chars)\n\n### p/2 b — b\n\n" + fifty
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
        header + "\n\nJ\n\n## Cited\n\n### p/1 a — a\n\n" + fifty +
          "\n\n- p/2 b (note, b, 50 chars)\n\n- p/3 c (note, c, 50 chars)"
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
        twoLinks.map(_.text.contains("### p/1 a — a")) == Right(true),
        twoLinks.map(_.text.contains("p/b")) == Right(false),
        twoLinks.map(_.text.contains("p/c")) == Right(false),
        threeLinks.map(_.text.contains("### p/3 c — c")) == Right(true),
        threeLinks.map(_.text.contains("p/d")) == Right(false)
      )
    }

    test("a parent is followed up the artifact's own chain, never from what it cites") {
      // j's parent is m, whose parent is r; j cites c, whose parent is the index i.
      val pr = project(
        Vector(
          artifact("j", "note", "j", "J", parent = Some("m"), cites = Vector("c")),
          artifact("m", "note", "m", "M", parent = Some("r")),
          artifact("r", "note", "r", "R"),
          artifact("c", "note", "c", "C", parent = Some("i")),
          artifact("i", "note", "i", "I")
        )
      )
      Brief
        .of(pr, key("j"), view(Set("note"), 2), 10000)
        .map(_.text.linesIterator.filter(_.startsWith("### ")).toVector) ==> Right(
        Vector("### p/3 c — c", "### p/11 m — m", "### p/41 r — r")
      )
    }

    test(
      "carried artifacts are given whole, cited or not, never pointed out, before what is cited"
    ) {
      val judged = artifact("j", "note", "j", "J", cites = Vector("a", "rules"))
      val pr = project(
        Vector(
          judged,
          artifact("a", "note", "a", fifty),
          artifact("rules", "note", "rules", fifty),
          artifact("more", "note", "more", "M")
        )
      )
      val carrying = right(
        RoleView.of(
          right(RoleName.of("builder")),
          Set(right(KindName.of("note"))),
          1,
          Set(right(Handle.of("rules")), right(Handle.of("more")), right(Handle.of("none")))
        )
      )
      val carried =
        header + "\n\nJ\n\n## Carried\n\n### p/42 more — more\n\nM\n\n### p/33 rules — rules\n\n" + fifty
      val expected = carried + "\n\n## Cited\n\n- p/1 a (note, a, 50 chars)"
      assert(
        Brief.of(pr, key("j"), carrying, expected.length).map(b => (b.text, b.pointed)) ==
          Right((expected, Vector(key("a")))),
        Brief.of(pr, key("j"), carrying, carried.length - 1) ==
          Left(BriefRefusal.Over(carried.length))
      )
    }

    test("a brief shows its artifact, then what it carries, then what it cites, as given") {
      val judged = artifact("j", "note", "j", "J", cites = Vector("a", "b", "c", "rules"))
      val pr = project(
        Vector(
          judged,
          artifact("a", "note", "a", fifty),
          artifact("b", "note", "b", fifty),
          artifact("c", "note", "c", fifty),
          artifact("rules", "note", "rules", "R")
        )
      )
      val carrying = right(
        RoleView.of(
          right(RoleName.of("builder")),
          Set(right(KindName.of("note"))),
          1,
          Set(right(Handle.of("rules")))
        )
      )
      // `a` and `b` fit as pointers; `c` does not fit even as one, so it is not shown.
      val fits =
        header + "\n\nJ\n\n## Carried\n\n### p/33 rules — rules\n\nR\n\n## Cited\n\n" +
          "- p/1 a (note, a, 50 chars)\n\n- p/2 b (note, b, 50 chars)"
      assert(
        Brief.of(pr, key("j"), carrying, fits.length).map(b => (b.text, b.shows)) ==
          Right((fits, Vector(key("j"), key("rules"), key("a"), key("b"))))
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
