package grit.projects.plugin

import grit.core.document.{DocRef, DocText}
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.visibility.{Integrity, Label}
import grit.projects.artifact.{Artifact, Body}
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
  private def oldProject(more: (String, String)*): (Projects, Store) = {
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
    (stored ++ more).foreach { (key, data) =>
      inTx(
        store.keeper.write(
          right(DocKey.of(key)),
          Label.Public,
          // p/b as a turn kept it, believed lower than a person's own word.
          if (key == "p/b") Integrity.Member else Integrity.Authority,
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

  /** The document kept under `key` at public. */
  private def current(store: Store, key: String): grit.core.document.Document =
    inTx(store.keeper.current(right(DocKey.of(key)), Label.Public))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      .getOrElse(throw new java.lang.AssertionError(s"nothing kept under $key"))

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
    test("a re-key keeps how far each artifact was believed, a turn's lower") {
      val (projects, store) = oldProject()
      done(ask(projects, store, "project_rekey", "project" -> "p"))
      (believedOf(store, "p/a"), believedOf(store, "p/b")) ==> (Some("authority"), Some("member"))
    }
    test("a re-keyed project answers by id, handle and old key, and mints above its highest") {
      val (projects, store) = oldProject()
      val answer = done(ask(projects, store, "project_rekey", "project" -> "p"))
      val head = (key: String) =>
        done(ask(projects, store, "project_show", "key" -> key, "head" -> true)).linesIterator
          .take(3)
          .toVector
      (
        answer,
        head("p/1"),
        head("p/a") == head("p/1"),
        Body.markdown(right(Artifact.of(current(store, "p/a"))).body),
        done(
          ask(
            projects,
            store,
            "project_keep",
            "project" -> "p",
            "kind" -> "note",
            "title" -> "C",
            "body" -> "c"
          )
        ).linesIterator.next().split(" Now").head
      ) ==> (
        "p: 2 artifacts, 1 body links, 1 head cites, 0 handles carried\n" +
          "It gave ids p/1 to p/2; the next new artifact is p/3.",
        Vector("---", "id: p/1", "handle: a"),
        true,
        "See [b](artifact:p/2).",
        "Kept p/3 (note, open)."
      )
    }
    test(
      "project_stored gives the key an artifact is kept under: a re-keyed one's old key, a new one's id"
    ) {
      val (projects, store) = oldProject()
      done(ask(projects, store, "project_rekey", "project" -> "p"))
      done(
        ask(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "c",
          "kind" -> "note",
          "title" -> "C",
          "body" -> "c"
        )
      )
      Vector("p/1", "p/a", "p/3", "p/c", "p/9").map(k =>
        ask(projects, store, "project_stored", "key" -> k)
      ) ==> Vector(
        CommandOutcome.Done("p/a"),
        CommandOutcome.Done("p/a"),
        CommandOutcome.Done("p/3"),
        CommandOutcome.Done("p/3"),
        CommandOutcome.Refused("project p has no artifact p/9 (its highest is p/3)")
      )
    }
    test("a re-keyed artifact's history and show-at read its versions from before the re-key") {
      val (projects, store) = oldProject()
      val later = at.plusSeconds(60)
      val asked = (name: String, args: Seq[(String, ujson.Value)]) =>
        done(askAt(later)(projects, store, name, args*))
      asked("project_rekey", Seq("project" -> "p"))
      (
        asked("project_history", Seq("key" -> "p/1")),
        asked("project_show", Seq("key" -> "p/a", "at" -> at.toString)),
        // The re-key changed nothing its export shows: no line only one version holds.
        asked(
          "project_history",
          Seq("key" -> "p/a", "from_version" -> 4, "to_version" -> 6)
        ).linesIterator
          .filter(l => !l.startsWith("  "))
          .mkString("\n")
      ) ==> (
        "6 2026-10-27T12:01:00Z by nick (open): A title\n" +
          "4 2026-10-27T12:00:00Z by nick (open): A title",
        "---\nid: p/1\nhandle: a\nkind: note\nstate: open\ntitle: \"A title\"\nparent: p/2\n" +
          "rank: n\ncites: [p/2]\nby: \"nick\"\nversion: 4\n---\n\nSee [b](artifact:p/2).\n",
        "--- p/1 version 4\n+++ p/1 version 6"
      )
    }
    test(
      "an old-form artifact above a re-keyed project's index refuses it, and no re-key numbers it"
    ) {
      val (projects, store) = oldProject()
      done(ask(projects, store, "project_rekey", "project" -> "p"))
      val trial = grit.core.visibility.TestLabels.Trial
      def high(projects: Projects, store: Store, name: String, args: (String, ujson.Value)*) =
        askBy(direct(), cleared = grit.core.visibility.Clearance.of(trial))(
          projects,
          store,
          name,
          args*
        )
      locally {
        given grit.core.store.Tx =
          grit.dbos.sql.TestTx.fake(grit.core.visibility.Clearance.of(trial))
        store.keeper
          .write(
            right(DocKey.of("p/late")),
            trial,
            Integrity.Authority,
            Place.Everywhere,
            right(DocText.of("Old.")),
            ujson.read(stored(1)._2),
            None,
            at,
            Set(right(DocRef.of("project", "p")))
          )
          .fold(e => throw new java.lang.AssertionError(e.toString), _ => ())
      }
      val leftOver = CommandOutcome.Refused(
        "project p was re-keyed, but 1 artifact of the old form is kept above its index, where " +
          "the re-key could not read it: p/late (kept [level: public, in: {trial}]). No command " +
          "can number it, since that would write the index from above it. Use the project from " +
          "a room at its index's label ([level: public]), which does not read it."
      )
      (
        high(projects, store, "project_show", "key" -> "p/1"),
        high(projects, store, "project_rekey", "project" -> "p"),
        ask(projects, store, "project_show", "key" -> "p/1").isInstanceOf[CommandOutcome.Done]
      ) ==> (
        leftOver,
        CommandOutcome.Refused(
          "project p is kept labelled [level: public], below what this room writes ([level: " +
            "public, in: {trial}]): re-key from a room at its label"
        ),
        true
      )
    }
    test("a dry re-key counts what it would do and keeps nothing") {
      val (projects, store) = oldProject()
      val before = store.count
      (
        done(ask(projects, store, "project_rekey", "project" -> "p", "dry" -> true)),
        store.count,
        ask(projects, store, "project_show", "key" -> "p/a")
      ) ==> (
        "p: 2 artifacts, 1 body links, 1 head cites, 0 handles carried\n" +
          "It would give ids p/1 to p/2; the next new artifact is p/3. Nothing was kept.",
        before,
        refused
      )
    }
    test("a survey lists each project left to re-key, and none once it is re-keyed") {
      val (projects, store) = oldProject()
      val survey = () => done(ask(projects, store, "project_rekey", "dry" -> true))
      val first = survey()
      done(ask(projects, store, "project_rekey", "project" -> "p"))
      (first, survey()) ==> (
        "p: 2 artifacts, 1 body links, 1 head cites, 0 handles carried",
        "No project is left to re-key."
      )
    }
    test("a re-key refused keeps nothing, naming the artifact citing one it cannot read") {
      val (projects, store) = oldProject(
        "p/c" -> ("""{"head":{"kind":"note","state":"open","title":"C","parent":null,""" +
          """"cites":["p/gone"],"by":null},"body":{"v":1,"notes":[],"blocks":""" +
          """[{"type":"para","text":[{"t":"C."}]}]}}""")
      )
      val before = store.count
      (ask(projects, store, "project_rekey", "project" -> "p"), store.count) ==> (
        CommandOutcome.Refused(
          "project p cannot be re-keyed, so nothing was kept: p/c cannot be re-keyed: it cites " +
            "p/gone, which this transaction reads no artifact under"
        ),
        before
      )
    }
    test("a name led by a digit is carried as k-<name>, which its old key no longer reaches") {
      val (projects, store) = oldProject(
        "p/0042-x" -> ("""{"head":{"kind":"note","state":"open","title":"X","parent":null,""" +
          """"cites":[],"by":null},"body":{"v":1,"notes":[],"blocks":""" +
          """[{"type":"para","text":[{"t":"X."}]}]}}""")
      )
      (
        done(ask(projects, store, "project_rekey", "project" -> "p")),
        ask(projects, store, "project_show", "key" -> "p/k-0042-x", "head" -> true)
          .isInstanceOf[CommandOutcome.Done]
      ) ==> (
        "p: 3 artifacts, 1 body links, 1 head cites, 1 handles carried\n" +
          "It gave ids p/1 to p/3; the next new artifact is p/4.\n" +
          "p/1 k-0042-x was p/0042-x, a name a handle cannot be: that key no longer names it.",
        true
      )
    }
    test("a re-key is refused to a caller believed lower than a person's own word") {
      val (projects, store) = oldProject()
      askBelieved(Integrity.Member)(projects, store, "project_rekey", "project" -> "p") ==>
        CommandOutcome.Refused(s"A project is re-keyed only by ${ProjectCommands.PersonOnlyBy}.")
    }
    test("a find meeting an artifact of the old form says to re-key its project") {
      val (projects, store) = oldProject()
      askAt(at.plusSeconds(1))(projects, store, "project_find", "query" -> "Old") ==> refused
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
