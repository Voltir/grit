package grit.projects.artifact

import java.time.Instant

import grit.core.document.{Document, InMemoryDocuments}
import grit.core.id.DocKey
import grit.core.visibility.Label
import grit.projects.name.{Handle, MethodName, ProjectKey}

import utest.*

/** [[Rekey.plan]]'s laws, over documents of the old form as the store held them before ids. */
object RekeyTests extends TestSuite {
  import grit.projects.method.MethodFixtures.right

  import OldFormFixtures.{data, kept}

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private val pin: Pin = right(Pin.read(right(MethodName.of("m")), 1, "0f" * 32))
  private val t0 = Instant.parse("2026-10-01T12:00:00Z")

  /** An old-form document under `key` at `label`, first kept `minutes` after `t0`. */
  private def old(
      key: String,
      title: String,
      minutes: Int,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty,
      label: Label = Label.Public
  ): Rekey.Old = {
    val first = t0.plusSeconds(60L * minutes)
    Rekey.Old(
      kept(
        new InMemoryDocuments,
        right(DocKey.of(key)),
        label,
        title,
        data(title, parent, cites),
        first
      ),
      first
    )
  }

  /** The documents a plan leaves, each the one it replaces holding the artifact it wrote. */
  private def left(planned: Rekey.Planned): Vector[(Document, Artifact)] =
    planned.artifacts.map { (before, after) =>
      before.copy(data = right(ArtifactJson.write(after))._2) -> after
    }

  private def planned(legacy: Rekey.Old*): Rekey.Planned =
    Rekey
      .plan(p, pin, legacy.toVector)
      .fold(why => throw new java.lang.AssertionError(why.mkString("; ")), identity)

  /** Three milestones, out of order by title, a note citing two of them and one under the first;
    * the two written at minute 1 listed out of key order, so only the tie by key orders them.
    */
  private val roadmap: Vector[Rekey.Old] = Vector(
    old("p/step", "A step", 1, parent = Some("p/m9"), cites = Vector("p/notes")),
    old("p/m10", "M10: later", 1),
    old("p/m9", "M9: earlier", 2),
    old("p/notes", "Notes", 0, cites = Vector("p/m9", "p/m10"))
  )

  val tests: Tests = Tests {
    test("each artifact is numbered by its first version, ties by key, and named by its old key") {
      planned(roadmap*).artifacts.map(_.after.head.called) ==>
        Vector("p/1 notes", "p/2 m10", "p/3 step", "p/4 m9")
    }
    test("every link, in a head or a body, resolves to the id of what it named") {
      val after = planned(roadmap*).artifacts.map(_.after)
      after.map(a =>
        (
          a.head.called,
          a.head.parent.map(_.value),
          a.head.cites.map(_.value),
          Body.markdown(a.body)
        )
      ) ==> Vector(
        ("p/1 notes", None, Vector("p/4", "p/2"), "See [m9](artifact:p/4)[m10](artifact:p/2)."),
        ("p/2 m10", None, Vector(), "See ."),
        ("p/3 step", Some("p/4"), Vector("p/1"), "See [notes](artifact:p/1)."),
        ("p/4 m9", None, Vector(), "See .")
      )
    }
    test("Project.of accepts what it plans, the milestones ranked by their M numbers") {
      Project
        .of(p, pin, planned(roadmap*).artifacts.map(_.after))
        .map(
          _.artifacts
            .filter(_.head.parent.isEmpty)
            .sorted(using ArtifactHead.ranked.on[Artifact](_.head))
            .map(_.head.called)
        ) ==>
        Right(Vector("p/4 m9", "p/2 m10", "p/1 notes"))
    }
    test("nothing a plan writes is of the old form, so a second plan finds nothing to re-key") {
      left(planned(roadmap*)).collect {
        case (d, _) if ArtifactJson.isOldForm(d) => DocKey.value(d.key)
      } ==> Vector.empty
    }
    test("a key led by a digit takes the handle k-<key>, listed as carried") {
      val got = planned(old("p/0042-x", "Digits", 0))
      (
        got.artifacts.map(_.after.head.called),
        got.prefixed.map(r => (DocKey.value(r.was), Handle.value(r.handle)))
      ) ==>
        (Vector("p/1 k-0042-x"), Vector(("p/0042-x", "k-0042-x")))
    }
    test("a cite of an artifact the transaction does not read refuses the plan, naming it") {
      Rekey.plan(
        p,
        pin,
        Vector(old("p/a", "A", 0, cites = Vector("p/gone")))
      ) ==>
        Left(
          Vector(
            "p/a cannot be re-keyed: it cites p/gone, which this transaction reads no artifact under"
          )
        )
    }
    test("a carried handle another artifact already holds refuses the plan") {
      Rekey.plan(
        p,
        pin,
        Vector(old("p/k-1x", "A", 0), old("p/1x", "B", 1))
      ) ==>
        Left(
          Vector("project p cannot be re-keyed: p/2 cannot take the handle k-1x: p/1 k-1x holds it")
        )
    }
    test("the table maps each number to its store key, a document per label") {
      val secret = grit.core.visibility.TestLabels.Trial
      val got = planned(old("p/a", "A", 0), old("p/b", "B", 1, label = secret))
      got.table.map(t => (DocKey.value(t.key), t.label == secret, t.data)) ==> Vector(
        ("p#rekeyed-0", false, ujson.Obj("keys" -> ujson.Obj("1" -> "a"))),
        ("p#rekeyed-0", true, ujson.Obj("keys" -> ujson.Obj("2" -> "b")))
      )
    }
  }
}
