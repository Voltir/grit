package grit.projects.artifact

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, Document, DocumentTerms, InMemoryDocuments}
import grit.core.id.{DocKey, PluginName}
import grit.core.place.Place
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx
import grit.projects.name.{ArtifactNumber, Handle, MethodName, Pin, ProjectKey}

import utest.*

/** [[Rekey.plan]]'s laws, over documents of the old form as the store held them before ids. */
object RekeyTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private val pin: Pin = right(Pin.read(right(MethodName.of("m")), 1, "0f" * 32))
  private val t0 = Instant.parse("2026-10-01T12:00:00Z")

  /** The old form's data of an artifact titled `title`, under `parent`, citing `cites` in its
    * head and linking each in its body.
    */
  private def data(title: String, parent: Option[String], cites: Vector[String]): ujson.Value = {
    val spans = ujson.Str("See ") +: cites.zipWithIndex.map { (c, i) =>
      ujson.Obj("t" -> c.stripPrefix("p/"), "n" -> ujson.Arr(i))
    } :+ ujson.Obj("t" -> ".")
    ujson.Obj(
      "head" -> ujson.Obj(
        "kind" -> "note",
        "state" -> "open",
        "title" -> title,
        "parent" -> parent.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
        "cites" -> ujson.Arr.from(cites.map(ujson.Str(_))),
        "by" -> "nick"
      ),
      "body" -> ujson.Obj(
        "v" -> 1,
        "notes" -> ujson.Arr.from(cites.map(ujson.Str(_))),
        "blocks" -> ujson.Arr(
          ujson.Obj(
            "type" -> "para",
            "text" -> ujson.Arr.from(spans.map {
              case ujson.Str(s) => ujson.Obj("t" -> s)
              case other => other
            })
          )
        )
      )
    )
  }

  /** An old-form document under `key` at `label`, first kept `minutes` after `t0`. */
  private def old(
      key: String,
      title: String,
      minutes: Int,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty,
      label: Label = Label.Public
  ): Rekey.Old = {
    val documents = new InMemoryDocuments
    val plugin = right(PluginName.of("projects"))
    val terms = right(
      DocLabel.of("projects").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 100))
    )
    val under = right(DocKey.of(key))
    given grit.core.store.Tx = TestTx.fake(grit.core.visibility.Clearance.of(label))
    val d = (for {
      _ <- documents
        .keeper(plugin, terms)
        .write(
          under,
          label,
          Integrity.Member,
          Place.Everywhere,
          right(DocText.of(title)),
          data(title, parent, cites),
          None,
          t0.plusSeconds(60L * minutes),
          Set.empty
        )
      d <- documents.shelf(plugin).current(under, label)
    } yield d).left.map(_.toString).flatMap(_.toRight("nothing kept"))
    Rekey.Old(right(d), t0.plusSeconds(60L * minutes))
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
    test("a second plan over what the first left is empty") {
      val once = planned(roadmap*)
      val leftOld = left(once).map(_._1).filter(ArtifactJson.isOldForm).map(Rekey.Old(_, t0))
      (
        once.next,
        leftOld,
        Rekey.plan(p, pin, leftOld).map(again => (again.artifacts, again.table))
      ) ==> (right(ArtifactNumber.of(5)), Vector.empty, Right((Vector.empty, Vector.empty)))
    }
    test("a key led by a digit takes the handle k-<key>, listed as carried") {
      val got = planned(old("p/0042-x", "Digits", 0))
      (
        got.artifacts.map(_.after.head.called),
        got.renamed.map(r => (DocKey.value(r.was), Handle.value(r.handle)))
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
