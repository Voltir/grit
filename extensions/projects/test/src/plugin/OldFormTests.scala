package grit.projects.plugin

import grit.core.document.{DocRef, DocText}
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.visibility.{Integrity, Label}
import grit.projects.method.MethodFixtures

import utest.*

/** A project of the old form (ADR 0063), its artifacts keyed by name and its index holding no
  * `next`, as the store held it before ids: every command but `project_rekey` and
  * `project_export` is refused, and its export is the one it had.
  */
object OldFormTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.right

  /** Data of `p/a`, citing `p/b`, and of `p/b`, as the old form stored them. */
  private val stored: Vector[(String, String)] = Vector(
    "p/a" -> ("""{"head":{"kind":"note","state":"open","title":"A title","parent":"p/b",""" +
      """"cites":["p/b"],"by":"nick"},"body":{"v":1,"notes":["p/b"],"blocks":""" +
      """[{"type":"para","text":[{"t":"See "},{"t":"b","n":[0]},{"t":"."}]}]}}"""),
    "p/b" -> ("""{"head":{"kind":"note","state":"open","title":"B \"quoted\"","parent":null,""" +
      """"cites":[],"by":null},"body":{"v":1,"notes":[],"blocks":""" +
      """[{"type":"para","text":[{"t":"Plain."}]}]}}""")
  )

  /** Project `p` as the old form left it: its index with no `next`, and [[stored]]. */
  private def oldProject(): (Projects, Store) = {
    val projects = plugin()
    val store = Store()
    started(projects, store)
    val written = inTx {
      for {
        index <- store.keeper.current(right(DocKey.of("p")), Label.Public)
        d <- index.toRight(grit.core.store.StoreError.Invalid("no index"))
        _ <- store.keeper.write(
          d.key,
          d.label,
          d.integrity,
          d.place,
          d.text,
          ujson.Obj.from(d.data.obj.toSeq.filterNot(_._1 == "next")),
          None,
          at,
          Set(right(DocRef.of("pinned", "p")))
        )
      } yield ()
    }
    written.fold(e => throw new java.lang.AssertionError(e.toString), identity)
    stored.foreach { (key, data) =>
      inTx(
        store.keeper.write(
          right(DocKey.of(key)),
          Label.Public,
          Integrity.Authority,
          Place.Everywhere,
          right(DocText.of("Old.")),
          ujson.read(data),
          None,
          at,
          Set(right(DocRef.of("project", "p")))
        )
      ).fold(e => throw new java.lang.AssertionError(e.toString), _ => ())
    }
    (projects, store)
  }

  private val refused: CommandOutcome = CommandOutcome.Refused(
    "project p is in the old form, its artifacts keyed by name: re-key it with project_rekey first"
  )

  val tests: Tests = Tests {
    test("every command over a project of the old form is refused, saying to re-key it") {
      val (projects, store) = oldProject()
      Vector[Vector[(String, ujson.Value)]](
        Vector(
          "name" -> "project_keep",
          "project" -> "p",
          "kind" -> "note",
          "title" -> "T",
          "body" -> "x"
        ),
        Vector("name" -> "project_keep", "key" -> "p/a", "title" -> "T", "body" -> "x"),
        Vector("name" -> "project_move", "key" -> "p/a", "state" -> "closed"),
        Vector("name" -> "project_show", "key" -> "p/a"),
        Vector("name" -> "project_list", "project" -> "p"),
        Vector("name" -> "project_claim", "key" -> "p/a", "role" -> "builder", "holder" -> "h")
      ).map(args => ask(projects, store, args.head._2.str, args.tail*)) ==> Vector.fill(6)(refused)
    }
    test("project_rekey is refused, not built yet, and leaves the project as it was") {
      val (projects, store) = oldProject()
      (
        ask(projects, store, "project_rekey", "project" -> "p"),
        ask(projects, store, "project_show", "key" -> "p/a")
      ) ==> (
        CommandOutcome.Refused("project_rekey is not built yet: project p is left as it is"),
        refused
      )
    }
    test("a project of the old form exports as it did before ids, byte for byte") {
      val (projects, store) = oldProject()
      val files = ujson
        .read(done(ask(projects, store, "project_export", "project" -> "p")))("files")
        .arr
        .map(f => f("path").str -> f("text").str)
        .toVector
      files.map(_._1) ==> Vector("p/a.md", "p/b.md", "project.md")
      files.take(2) ==> Vector(
        "p/a.md" -> ("---\nkey: p/a\nkind: note\nstate: open\ntitle: \"A title\"\nparent: p/b\n" +
          "cites: [p/b]\nby: \"nick\"\n---\n\nSee [b](artifact:p/b).\n"),
        "p/b.md" -> ("---\nkey: p/b\nkind: note\nstate: open\ntitle: \"B \\\"quoted\\\"\"\n" +
          "parent: null\ncites: []\nby: null\n---\n\nPlain.\n")
      )
    }
  }
}
