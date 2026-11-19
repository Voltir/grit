package grit.projects.plugin

import scala.concurrent.duration.*

import grit.core.document.DocumentTerms
import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.identity.Principal
import grit.core.plugin.{CommandBy, CommandOutcome}
import grit.core.tool.{ArgsError, ToolName}
import grit.core.visibility.Integrity
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project, Rank}
import grit.projects.method.MethodFixtures
import grit.projects.name.{ArtifactKey, ArtifactNumber, Handle, Pin, ProjectKey}
import grit.projects.render.{Brief, ProjectMarkdown}

import utest.*

object ProjectCommandsTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{note, right, state}

  /** The fixtures' plugin, its turns proposing changes alone. */
  private def proposing(): Projects =
    plugin(TurnCommands.Proposing(MethodFixtures.change.name))

  /** The fixtures' method, a change made done only by people. */
  private val peopleFinish = method(MethodFixtures.peopleFinish)

  /** Keeps note `p/<local>`, titled `Title <local>`, under `parent`. */
  private def under(projects: Projects, store: Store, local: String, parent: String): Unit =
    done(
      ask(
        projects,
        store,
        "project_keep",
        "project" -> "p",
        "handle" -> local,
        "kind" -> "note",
        "title" -> s"Title $local",
        "body" -> "Below.",
        "parent" -> parent
      )
    )

  val tests: Tests = Tests {

    test("a move whose check fails is refused naming it, and keeps nothing") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "No reason given.")
      done(ask(projects, store, "project_move", "key" -> "p/a", "state" -> "building"))
      def versions =
        done(ask(projects, store, "project_history", "key" -> "p/a")).linesIterator.size
      val before = versions
      val answer = ask(projects, store, "project_move", "key" -> "p/a", "state" -> "done")
      assert(
        answer == CommandOutcome.Refused(
          "p/1 a cannot move to done: sections-present: its body lacks ## Why"
        ),
        before == 2,
        versions == before,
        shown(projects, store, "p/a").contains("state: building")
      )
    }

    test("a move whose checks hold keeps the new state and its mover") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nBecause.")
      done(askAs(other)(projects, store, "project_move", "key" -> "p/a", "state" -> "building"))
      done(askAs(other)(projects, store, "project_move", "key" -> "p/a", "state" -> "done"))
      val text = shown(projects, store, "p/a")
      assert(text.contains("state: done"), text.contains(s"by: \"$otherBy\""))
    }

    test("a keep through a turn names the person and the turn as its writer") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val turn = TurnRef(ConversationId("c7"), TurnSeq(5))
      done(
        askBy(CommandBy.Turn(person, turn))(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "a",
          "kind" -> "change",
          "title" -> "Title a",
          "body" -> "## Why\n\nBecause."
        )
      )
      val text = shown(projects, store, "p/a")
      val writer = s"by: \"$by, through grit (turn 5 of conversation c7)\""
      assert(text.contains(writer))
    }

    test(
      "a move only people make is refused through a turn and below Authority, and made at it"
    ) {
      val projects = plugin()
      val store = Store(declared = Vector(peopleFinish))
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nBecause.")
      val turn = CommandBy.Turn(person, TurnRef(ConversationId("c7"), TurnSeq(5)))
      def move(who: CommandBy, to: String) =
        askBy(who)(projects, store, "project_move", "key" -> "p/a", "state" -> to)
      val building = move(turn, "building")
      val refused = (move(turn, "done"), move(CommandBy.Direct(person, Integrity.Member), "done"))
      val stayed = shown(projects, store, "p/a").contains("state: building")
      val personal = move(direct(), "done")
      (
        building.getClass,
        refused,
        stayed,
        personal.getClass,
        shown(projects, store, "p/a")
          .contains("state: done")
      ) ==> (
        classOf[CommandOutcome.Done],
        (CommandOutcome.Refused(personOnlyMove), CommandOutcome.Refused(personOnlyMove)),
        true,
        classOf[CommandOutcome.Done],
        true
      )
    }

    test(
      "a turn's batch with a move only people make is refused, naming the step, and keeps nothing"
    ) {
      val projects = plugin()
      val store = Store(declared = Vector(peopleFinish))
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nBecause.")
      def step(to: String) = ujson.Obj("command" -> "move", "key" -> "p/a", "state" -> to)
      val refused = askBy(CommandBy.Turn(person, TurnRef(ConversationId("c7"), TurnSeq(5))))(
        projects,
        store,
        "project_batch",
        "project" -> "p",
        "steps" -> ujson.Arr(step("building"), step("done"))
      )
      (refused, shown(projects, store, "p/a").contains("state: proposed")) ==> (
        CommandOutcome.Refused(
          s"Nothing was kept: step 2 of 2 (move p/a to done) was refused: $personOnlyMove"
        ),
        true
      )
    }

    test("a proposing plugin offers turns project_propose alone") {
      proposing().commands.filter(_.offeredToTurns).map(c => ToolName.value(c.spec.name)) ==>
        Vector("project_propose")
    }

    test("a keep that changes nothing answers the version current") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      def keep() = ask(
        projects,
        store,
        "project_keep",
        (naming(projects, store, "a") ++
          Vector[(String, ujson.Value)]("kind" -> "note", "title" -> "A", "body" -> "Text."))*
      )
      val first = keep()
      val version = versionOf(projects, store, "p/a")
      (first, keep()) ==> (
        CommandOutcome.Done(s"Kept p/1 a (note, open). Now at version $version."),
        CommandOutcome.Done(s"Kept p/1 a (note, open). Now at version $version.")
      )
    }

    test(
      "project_propose keeps a new artifact of its kind in its first state, naming the turn as its writer"
    ) {
      val projects = proposing()
      val store = Store()
      started(projects, store)
      val turn = TurnRef(ConversationId("c7"), TurnSeq(5))
      val answer = askBy(CommandBy.Turn(person, turn))(
        projects,
        store,
        "project_propose",
        "project" -> "p",
        "handle" -> "idea",
        "title" -> "An idea",
        "body" -> "## Why\n\nBecause."
      )
      val text = shown(projects, store, "p/idea")
      (
        answer,
        text.contains("kind: change"),
        text.contains("state: proposed"),
        text.contains(s"by: \"$by, through grit (turn 5 of conversation c7)\"")
      ) ==> (CommandOutcome.Done("Kept p/1 idea (change, proposed)."), true, true, true)
    }

    test("a create believes the project's index at no more than its version before") {
      val projects = plugin()
      val store = Store()
      done(askBelieved(Integrity.Member)(projects, store, "project_new", "project" -> "p"))
      kept(projects, store, "a", "note", "A.")
      (believedOf(store, "p"), believedOf(store, "p/1")) ==> (Some("member"), Some("authority"))
    }
    test("a keep giving an artifact a handle another holds is refused, naming the holder") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "A.")
      kept(projects, store, "b", "note", "B.")
      def rename(key: String, to: String) =
        ask(
          projects,
          store,
          "project_keep",
          "key" -> key,
          "handle" -> to,
          "title" -> "T",
          "body" -> "x"
        )
      (rename("p/1", "b"), rename("p/2", "a")) ==> (
        CommandOutcome.Refused("p/1 a cannot take the handle b: p/2 b holds it"),
        CommandOutcome.Refused("p/2 b cannot take the handle a: p/1 a holds it")
      )
    }
    test(
      "a keep taking a handle a role carries is kept, and one renaming or removing it is refused, naming the role"
    ) {
      val projects = plugin()
      val store = Store(declared = Vector(managing))
      started(projects, store)
      kept(projects, store, "a", "note", "A.")
      def rename(to: String) =
        ask(
          projects,
          store,
          "project_keep",
          "key" -> "p/1",
          "handle" -> to,
          "title" -> "T",
          "body" -> "x"
        )
      val taken = rename("process")
      val carried =
        "p/1 process keeps its handle: the brief of role manager carries the artifact named process"
      (taken.getClass, rename("rules"), rename("")) ==> (
        classOf[CommandOutcome.Done],
        CommandOutcome.Refused(carried),
        CommandOutcome.Refused(carried)
      )
    }
    test("project_propose refuses a handle another artifact holds, keeping nothing") {
      val projects = proposing()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Kept by a person.")
      val before = shown(projects, store, "p/a")
      val answer = ask(
        projects,
        store,
        "project_propose",
        "project" -> "p",
        "handle" -> "a",
        "title" -> "Over it",
        "body" -> "## Why\n\nBecause."
      )
      (answer, shown(projects, store, "p/a")) ==> (
        CommandOutcome.Refused("p/2 cannot take the handle a: p/1 a holds it"),
        before
      )
    }

    test(
      "a turn is offered the reads, new, keep, edit and move; not batch, withdraw, sweep, migrate or claims"
    ) {
      plugin().commands.filter(_.offeredToTurns).map(c => ToolName.value(c.spec.name)).sorted ==>
        Vector(
          "project_brief",
          "project_edit",
          "project_export",
          "project_find",
          "project_history",
          "project_keep",
          "project_list",
          "project_move",
          "project_new",
          "project_show",
          "project_stale"
        )
    }

    test("a move no transition allows is refused, naming the states it may go to") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "Text.")
      val answer = ask(projects, store, "project_move", "key" -> "p/a", "state" -> "done")
      assert(
        answer == CommandOutcome.Refused(
          "p/1 a (change, proposed) cannot move to done; it may move to building"
        )
      )
    }

    test("project_keep cannot change a state") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "Text.")
      done(ask(projects, store, "project_move", "key" -> "p/a", "state" -> "building"))
      kept(projects, store, "a", "change", "Other text.")
      val text = shown(projects, store, "p/a")
      assert(text.contains("state: building"), text.contains("Other text."))
      val keep = projects.commands.find(c => ToolName.value(c.spec.name) == "project_keep")
      val withState = keep.map(
        _.spec.args.read(
          ujson.Obj("key" -> "p/a", "title" -> "t", "body" -> "b", "state" -> "done")
        )
      )
      assert(withState.exists(_.left.exists {
        case ArgsError.Unexpected("state", _) => true
        case _ => false
      }))
    }

    test("keeping a body alone leaves the head's cites and parent as they were") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "b", "note", "Cited.")
      done(
        ask(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "a",
          "kind" -> "note",
          "title" -> "A",
          "body" -> "Text.",
          "parent" -> "p/b",
          "cites" -> ujson.Arr("p/b")
        )
      )
      done(ask(projects, store, "project_keep", "key" -> "p/a", "title" -> "A", "body" -> "New."))
      val text = shown(projects, store, "p/a")
      assert(text.contains("parent: p/1\nrank: n\ncites: [p/1]\n"), text.contains("New."))
    }
    test("an empty cites clears them, and an empty parent clears it") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "b", "note", "Cited.")
      def keep(args: (String, ujson.Value)*): String =
        done(
          ask(
            projects,
            store,
            "project_keep",
            (naming(projects, store, "a") ++
              Vector[(String, ujson.Value)]("title" -> "A", "body" -> "Text.") ++ args)*
          )
        )
      keep("kind" -> "note", "parent" -> "p/b", "cites" -> ujson.Arr("p/b"))
      keep("cites" -> ujson.Arr(), "parent" -> "")
      // Out from under b, it ranks after b, the last of the artifacts under none.
      assert(shown(projects, store, "p/a").contains("parent: null\nrank: naab\ncites: []\n"))
    }
    test("a keep naming an artifact the project lacks is refused, keeping nothing") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val before = store.count
      val answer = ask(
        projects,
        store,
        "project_keep",
        "project" -> "p",
        "handle" -> "a",
        "kind" -> "note",
        "title" -> "A",
        "body" -> "See [b](artifact:p/b)."
      )
      assert(answer match {
        case CommandOutcome.Refused(why) => why.contains("p/b")
        case CommandOutcome.Done(_) => false
      })
      assert(store.count == before)
    }

    test("a keep whose parent the project lacks is refused naming its nearest keys") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "plan-c5-runs-turns", "note", "The checkpoint.")
      kept(projects, store, "zebra", "note", "Far from it.")
      val keep = (parent: String) =>
        ask(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "n",
          "kind" -> "note",
          "title" -> "N",
          "body" -> "Text.",
          "parent" -> parent
        )
      (keep("p/plan-c5"), keep("p/q")) ==> (
        CommandOutcome.Refused(
          "project p has no artifact named plan-c5 (nearest: plan-c5-runs-turns)"
        ),
        CommandOutcome.Refused("project p has no artifact named q")
      )
    }

    test("a write past 20,000 documents is refused") {
      val terms = Projects.Terms
      assert(
        terms.bound == 20_000,
        terms.keepLast == 20,
        terms.retention == 180.days,
        terms.full == DocumentTerms.Full.Refuse
      )
      val projects = plugin()
      val small = right(
        DocumentTerms.of(terms.label, terms.weight, terms.retention, 3, terms.keepLast, terms.full)
      )
      val store = Store(small)
      started(projects, store)
      kept(projects, store, "a", "note", "Text.")
      val answer = ask(
        projects,
        store,
        "project_keep",
        "project" -> "p",
        "handle" -> "b",
        "kind" -> "note",
        "title" -> "B",
        "body" -> "Text."
      )
      assert(
        answer == CommandOutcome.Refused(
          "this deployment keeps at most 3 project documents and holds that many already, so nothing was kept"
        ),
        store.count == 3
      )
    }

    test("a batch the deployment's bound refuses partway keeps none of its steps") {
      val terms = Projects.Terms
      val small = right(
        DocumentTerms.of(terms.label, terms.weight, terms.retention, 3, terms.keepLast, terms.full)
      )
      val store = Store(small)
      val projects = plugin()
      started(projects, store)
      def keep(local: String) =
        ujson.Obj(
          "command" -> "keep",
          "handle" -> local,
          "kind" -> "note",
          "title" -> local,
          "body" -> "Text."
        )
      val answer = ask(
        projects,
        store,
        "project_batch",
        "project" -> "p",
        "steps" -> ujson.Arr(keep("a"), keep("b"))
      )
      (answer, store.count) ==> (
        CommandOutcome.Refused(
          "this deployment keeps at most 3 project documents and holds that many already, so " +
            "nothing was kept"
        ),
        2
      )
    }

    test("export pages concatenate to ProjectMarkdown.render's files") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val locals = Vector.tabulate(25)(i => f"a$i%02d")
      locals.foreach(l => kept(projects, store, l, "note", s"Text of $l."))
      val p = right(ProjectKey.of("p"))
      val m = method()
      // Each create ranks after the last: `n`, then stepping on.
      val ranks = Iterator.iterate(Rank.Middle)(r => right(Rank.between(Some(r), None)))
      val expected = right(
        Project.of(
          p,
          right(Pin.of(m.name, m.version, m.digest)),
          locals.zip(ranks).zipWithIndex.map { case ((l, rank), i) =>
            Artifact(
              ArtifactHead(
                ArtifactKey.of(p, right(ArtifactNumber.of(i + 1))),
                Some(right(Handle.of(l))),
                note.name,
                state("open"),
                s"Title $l",
                None,
                rank,
                Vector.empty,
                Some(by)
              ),
              right(Body.read(s"Text of $l.", MethodFixtures.ids))
            )
          }
        )
      )
      def pages(
          after: Option[String],
          got: Vector[(String, String)],
          calls: Int
      ): (Vector[(String, String)], Int) = {
        val args = Vector("project" -> ujson.Str("p")) ++ after.map(a => "after" -> ujson.Str(a))
        val page = ujson.read(done(ask(projects, store, "project_export", args*)))
        val files = page("files").arr.toVector.map(f => f("path").str -> f("text").str)
        page("next").strOpt match {
          case Some(next) => pages(Some(next), got ++ files, calls + 1)
          case None => (got ++ files, calls + 1)
        }
      }
      val (files, calls) = pages(None, Vector.empty, 0)
      assert(files == ProjectMarkdown.render(expected).sortBy(_._1), calls == 2)
    }

    test("project_show answers an artifact as its export file, and refuses a key it lacks") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Text.")
      val text = shown(projects, store, "p/a")
      assert(
        text.startsWith("---\nkey: p/1\nhandle: a\nkind: note\nstate: open\n"),
        text.endsWith("Text.\n")
      )
      assert(
        ask(projects, store, "project_show", "key" -> "p/z") == CommandOutcome.Refused(
          "project p has no artifact named z"
        )
      )
    }

    /** Keeps `p/a`, a change titled `title` holding `body`, `m` minutes after [[at]], as `who`. */
    def keptAt(
        projects: Projects,
        store: Store,
        m: Int,
        title: String,
        body: String,
        who: Principal.Person = person
    ): Unit =
      done(
        askAs(who, at.plusSeconds(60L * m))(
          projects,
          store,
          "project_keep",
          (naming(projects, store, "a") ++ Vector[(String, ujson.Value)](
            "kind" -> "change",
            "title" -> title,
            "body" -> body
          ))*
        )
      )

    test(
      "history of a withdrawn artifact a re-key gave an id reads where the re-key left it, by its id alone"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "A.")
      kept(projects, store, "b", "note", "B.")
      storedUnder(store, "p/1", "p/old")
      storedUnder(store, "p/2", "p/other")
      def plant(names: (Int, String)*): Unit = inTx(
        store.keeper.write(
          right(grit.core.id.DocKey.of("p#rekeyed-0")),
          grit.core.visibility.Label.Public,
          Integrity.Authority,
          grit.core.place.Place.Everywhere,
          right(grit.core.document.DocText.of("Where a re-key left its artifacts.")),
          right(
            grit.projects.artifact.Rekeyed.data(
              right(ProjectKey.of("p")),
              names.toVector.map((n, k) =>
                right(ArtifactNumber.of(n)) -> right(grit.core.id.DocKey.of(k))
              )
            )
          ),
          None,
          at,
          Set.empty
        )
      ).fold(e => throw new java.lang.AssertionError(e.toString), _ => ())
      done(ask(projects, store, "project_withdraw", "key" -> "p/a"))
      // p/1 named where p/2 is kept: its newest version's id is not the one asked for.
      plant(1 -> "p/other")
      val misnamed = ask(projects, store, "project_history", "key" -> "p/1")
      plant(1 -> "p/old")
      val atOld = inTx(
        store.keeper.versions(
          right(grit.core.id.DocKey.of("p/old")),
          grit.core.visibility.Label.Public,
          10
        )
      ).fold(
        e => throw new java.lang.AssertionError(e.toString),
        _.map {
          case grit.core.document.DocumentVersion.Holding(d) => d.version
          case grit.core.document.DocumentVersion.Withdrawal(v, _) => v
        }.map(grit.core.id.DocumentVersionId.value)
      )
      (
        misnamed,
        ask(projects, store, "project_history", "key" -> "p/1") match {
          case CommandOutcome.Done(text) =>
            text.linesIterator.map(_.takeWhile(_ != ' ').toLong).toVector
          case other => other
        },
        atOld.size
      ) ==> (CommandOutcome.Refused("project p has no artifact p/1 (its highest is p/2)"), atOld, 2)
    }
    test("history lists versions newest first with who wrote each") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(projects, store, 1, "First", "## Why\n\nOne.")
      keptAt(projects, store, 2, "Second", "## Why\n\nTwo.", other)
      done(
        askAt(at.plusSeconds(180))(
          projects,
          store,
          "project_move",
          "key" -> "p/a",
          "state" -> "building"
        )
      )
      val lines =
        done(ask(projects, store, "project_history", "key" -> "p/a")).linesIterator.toVector
      val numbers = lines.map(_.takeWhile(_ != ' ').toLongOption)
      (
        numbers.flatten == numbers.flatten.sorted.reverse,
        numbers.size,
        lines.map(_.dropWhile(_ != ' '))
      ) ==> (
        (
          true,
          3,
          Vector(
            s" 2026-10-27T12:03:00Z by $by (building): Second",
            s" 2026-10-27T12:02:00Z by $otherBy (proposed): Second",
            s" 2026-10-27T12:01:00Z by $by (proposed): First"
          )
        )
      )
    }

    test("history of two versions is a line diff of their export files, older first") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(projects, store, 1, "Same", "## Why\n\nOne.\n\nKept.")
      keptAt(projects, store, 2, "Same", "## Why\n\nTwo.\n\nKept.")
      val versions = done(ask(projects, store, "project_history", "key" -> "p/a")).linesIterator
        .map(_.takeWhile(_ != ' '))
        .toVector
      val (newer, older) = (versions(0), versions(1))
      val diff = done(
        ask(
          projects,
          store,
          "project_history",
          "key" -> "p/a",
          "from_version" -> older.toLong,
          "to_version" -> newer.toLong
        )
      ).linesIterator.toVector
      (diff.take(2), diff.filter(l => l.startsWith("-") || l.startsWith("+")).drop(2)) ==> (
        (Vector(s"--- p/1 version $older", s"+++ p/1 version $newer"), Vector("- One.", "+ Two."))
      )
    }

    test("project_show with keys shows each in order, an unkept one said in its place") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Alpha.")
      kept(projects, store, "b", "note", "Beta.")
      val several = done(
        ask(projects, store, "project_show", "keys" -> ujson.Arr("p/b", "p/z", "p/a"))
      )
      several ==> Vector(
        shown(projects, store, "p/b"),
        "project p has no artifact named z\n",
        shown(projects, store, "p/a")
      ).mkString
    }

    test("project_show with head shows the head, its opening paragraph and its sections, no body") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(
        projects,
        store,
        "a",
        "note",
        "Opening words, all of them.\n\n## Goal\n\nThe goal's text.\n\n## Reads\n\nWhat to read."
      )
      val headed = done(ask(projects, store, "project_show", "key" -> "p/a", "head" -> true))
      headed ==>
        "---\nkey: p/1\nhandle: a\nkind: note\nstate: open\ntitle: \"Title a\"\nparent: null\nrank: n\ncites: []\n" +
        s"by: ${shown(projects, store, "p/a").linesIterator.find(_.startsWith("by: ")).fold("")(_.drop(4))}\n" +
        s"version: ${versionOf(projects, store, "p/a")}\n---\n\n" +
        "Opening words, all of them.\n\nSections: Goal; Reads (0.1 KB in all)\n"
    }

    test("project_show's head and project_list both give a body's size rounded up to a tenth") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "x" * 1030)
      val headed = done(ask(projects, store, "project_show", "key" -> "p/a", "head" -> true))
      val listed = done(ask(projects, store, "project_list", "project" -> "p"))
      (headed.linesIterator.toVector.lastOption, listed.contains(" · 1.1 KB · ")) ==>
        (Some("Sections: none (1.1 KB in all)"), true)
    }

    test("project_show refuses key and keys together, and neither, naming both") {
      val projects = plugin()
      val show = projects.commands.find(c => ToolName.value(c.spec.name) == "project_show")
      def message(args: ujson.Obj) =
        show.map(_.spec.args.read(args).left.map(_.message).swap.getOrElse(""))
      (
        message(ujson.Obj("key" -> "p/a", "keys" -> ujson.Arr("p/b"))),
        message(ujson.Obj())
      ) ==> (
        Some("Give key or keys, not both."),
        Some("`key` is missing: it takes an artifact's key, or give keys.")
      )
    }

    test("history refuses a lone to_version by naming from_version") {
      val projects = plugin()
      val history = projects.commands.find(c => ToolName.value(c.spec.name) == "project_history")
      val read = history.map(_.spec.args.read(ujson.Obj("key" -> "p/a", "to_version" -> 2)))
      read.map(_.left.toOption) ==> Some(
        Some(
          ArgsError.Refused(
            "from_version",
            "`from_version` is missing: give both versions to compare them, or neither for the whole history"
          )
        )
      )
    }

    test("show at an instant shows that version's body") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(projects, store, 1, "First", "## Why\n\nOne.")
      keptAt(projects, store, 3, "Second", "## Why\n\nTwo.")
      def shownAt(m: Int) =
        ask(
          projects,
          store,
          "project_show",
          "key" -> "p/a",
          "at" -> at.plusSeconds(60L * m).toString
        )
      (
        shownAt(2) match {
          case CommandOutcome.Done(text) => text.linesIterator.toVector.lastOption
          case CommandOutcome.Refused(why) => Some(why)
        },
        shownAt(3) match {
          case CommandOutcome.Done(text) => text.linesIterator.toVector.lastOption
          case CommandOutcome.Refused(why) => Some(why)
        },
        shownAt(0)
      ) ==> (
        (
          Some("One."),
          Some("Two."),
          CommandOutcome.Refused("p/a had no version at 2026-10-27T12:00:00Z")
        )
      )
    }

    test(
      "a project_list line leaves out what the arguments fixed, and gives its size, the day it last changed and how many sit under it"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val day = 86_400L
      // c's markdown is 207 bytes: 0.3 KB, rounded up to a tenth.
      kept(projects, store, "a", "note", "Text.")
      done(
        askAt(at.plusSeconds(day))(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "c",
          "kind" -> "change",
          "title" -> "Title c",
          "parent" -> "p/a",
          "body" -> ("## Why\n\n" + ("word " * 40).trim)
        )
      )
      done(
        askAt(at.plusSeconds(2 * day))(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "b",
          "kind" -> "note",
          "title" -> "Title b",
          "parent" -> "p/a",
          "body" -> "Ended."
        )
      )
      done(
        askAt(at.plusSeconds(2 * day))(
          projects,
          store,
          "project_move",
          "key" -> "p/b",
          "state" -> "closed"
        )
      )
      def lines(args: (String, ujson.Value)*): Vector[String] =
        done(ask(projects, store, "project_list", (("project", ujson.Str("p")) +: args)*))
          .split("\n")
          .toVector
          .drop(1)
      (
        lines(),
        lines("kind" -> "note", "state" -> "open"),
        lines("state" -> "closed"),
        lines("parent" -> "p/a", "ended" -> true)
      ) ==> (
        Vector(
          "p/2 c (change, proposed): Title c · under p/1 · 0.3 KB · changed 2026-10-28",
          "p/1 a (note, open): Title a · 0.1 KB · changed 2026-10-27 · 1 under it"
        ),
        Vector("p/1 a: Title a · 0.1 KB · changed 2026-10-27 · 1 under it"),
        Vector("p/3 b (note): Title b · under p/1 · 0.1 KB · changed 2026-10-29"),
        Vector(
          "p/3 b (note, closed): Title b · 0.1 KB · changed 2026-10-29",
          "p/2 c (change, proposed): Title c · 0.3 KB · changed 2026-10-28"
        )
      )
    }

    test("project_list with opening ends each line with the opening words of its text") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "b", "note", "# Bee\n\nThe second one.\n\nMore after.")
      kept(projects, store, "c", "change", "## Why\n\n" + ("word " * 40).trim)
      // The lead is cut at a word to 120 characters, its heading skipped.
      done(ask(projects, store, "project_list", "project" -> "p", "opening" -> true))
        .split("\n")
        .toVector
        .drop(1) ==> Vector(
        "p/1 b (note, open): Title b · 0.1 KB · changed 2026-10-27 · The second one.",
        "p/2 c (change, proposed): Title c · 0.3 KB · changed 2026-10-27 · " +
          ("word " * 23).trim + " word…"
      )
    }

    test(
      "project_list leaves out ended artifacts unless ended is true or state names an end state"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Live.")
      kept(projects, store, "b", "note", "Ended.")
      done(ask(projects, store, "project_move", "key" -> "p/b", "state" -> "closed"))
      // The keys of the lines after the count line.
      def list(args: (String, ujson.Value)*): Vector[String] =
        done(ask(projects, store, "project_list", (("project", ujson.Str("p")) +: args)*))
          .split("\n")
          .toVector
          .drop(1)
          .map(_.takeWhile(c => c != ' ' && c != ':'))
      list() ==> Vector("p/1")
      (list("ended" -> true), list("state" -> "closed")) ==> (Vector("p/1", "p/2"), Vector("p/2"))
    }

    test(
      "project_list answers at most 40, most recently changed first, under a line counting all that match, and says how many were cut"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      (1 to 42).foreach { i =>
        done(
          askAt(at.plusSeconds(i))(
            projects,
            store,
            "project_keep",
            "project" -> "p",
            "handle" -> f"n$i%02d",
            "kind" -> "note",
            "title" -> s"Title $i",
            "body" -> "Body."
          )
        )
      }
      done(
        askAt(at.plusSeconds(43))(
          projects,
          store,
          "project_move",
          "key" -> "p/n01",
          "state" -> "closed"
        )
      )
      val lines = done(ask(projects, store, "project_list", "project" -> "p")).split("\n").toVector
      (
        lines.headOption,
        lines.size,
        lines.slice(1, 41).map(_.takeWhile(_ != ' ')),
        lines.lastOption
      ) ==> (
        Some("41 match: note 41 (open 41). 1 ended one not listed; ended: true lists it."),
        42,
        (42 to 3 by -1).map(i => s"p/$i").toVector,
        Some("1 more not shown; skip: 40 shows the next, or narrow with kind, state or parent.")
      )
    }

    test(
      "project_list with skip lists the 40 after those passed over, under the same count, its cut line naming the skip after them"
    ) {
      val projects = plugin()
      val store = Store()
      notes(projects, store, 85)
      val lines =
        done(ask(projects, store, "project_list", "project" -> "p", "skip" -> 40))
          .split("\n")
          .toVector
      (
        lines.headOption,
        lines.size,
        lines.slice(1, 41).map(_.takeWhile(_ != ' ')),
        lines.lastOption
      ) ==> (
        Some("85 match: note 85 (open 85)."),
        42,
        (45 to 6 by -1).map(i => s"p/$i").toVector,
        Some("5 more not shown; skip: 80 shows the next, or narrow with kind, state or parent.")
      )
    }

    test("project_list's last page has no cut line") {
      val projects = plugin()
      val store = Store()
      notes(projects, store, 85)
      done(ask(projects, store, "project_list", "project" -> "p", "skip" -> 80))
        .split("\n")
        .toVector
        .map(_.takeWhile(_ != ' ')) ==> ("85" +: (5 to 1 by -1).map(i => s"p/$i").toVector)
    }

    test(
      "project_list with a skip at or past the end answers its count, then that no line is past it"
    ) {
      val projects = plugin()
      val store = Store()
      notes(projects, store, 3)
      (
        done(ask(projects, store, "project_list", "project" -> "p", "skip" -> 3)),
        refusedArgs(projects, "project_list", "project" -> "p", "skip" -> -1)
      ) ==> (
        "3 match: note 3 (open 3).\nNo line past 3.",
        Some("`skip` takes a whole number from 0 to 20000, not -1.")
      )
    }

    test(
      "project_list counts by state alone when kind is given, and says how many ended ones it left out when nothing else matches"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nLive.")
      kept(projects, store, "b", "note", "Ended.")
      done(ask(projects, store, "project_move", "key" -> "p/b", "state" -> "closed"))
      val none = done(ask(projects, store, "project_list", "project" -> "p", "kind" -> "note"))
      kept(projects, store, "c", "note", "Live.")
      val counted = done(
        ask(projects, store, "project_list", "project" -> "p", "kind" -> "note", "ended" -> true)
      )
      (none, counted.linesIterator.nextOption()) ==> (
        "No artifact matches. 1 ended one not listed; ended: true lists it.",
        Some("2 match: closed 1, open 1.")
      )
    }

    test("project_list with parent lists only the artifacts directly under it, not under them") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Top.")
      under(projects, store, "b", "p/a")
      under(projects, store, "c", "p/b")
      under(projects, store, "d", "p/a")
      done(ask(projects, store, "project_list", "project" -> "p", "parent" -> "p/a")) ==>
        "2 match: note 2 (open 2).\n" +
        "p/2 b (note, open): Title b · 0.1 KB · changed 2026-10-27 · 1 under it\n" +
        "p/4 d (note, open): Title d · 0.1 KB · changed 2026-10-27"
    }

    test(
      "project_list with parent and depth lists that many levels under it, each below the first naming what it sits under"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Top.")
      under(projects, store, "b", "p/a")
      under(projects, store, "c", "p/b")
      under(projects, store, "d", "p/a")
      under(projects, store, "e", "p/c")
      under(projects, store, "f", "p/a")
      done(ask(projects, store, "project_move", "key" -> "p/f", "state" -> "closed"))
      done(
        ask(projects, store, "project_list", "project" -> "p", "parent" -> "p/a", "depth" -> 2)
      ) ==>
        "3 match: note 3 (open 3). 1 ended one not listed; ended: true lists it.\n" +
        "p/2 b (note, open): Title b · 0.1 KB · changed 2026-10-27 · 1 under it\n" +
        "p/3 c (note, open): Title c · under p/2 · 0.1 KB · changed 2026-10-27 · 1 under it\n" +
        "p/4 d (note, open): Title d · 0.1 KB · changed 2026-10-27"
    }

    test(
      "project_list cut below the first level says to use a smaller depth or a parent further down"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Top.")
      under(projects, store, "b", "p/a")
      (1 to 41).foreach(i => under(projects, store, f"n$i%02d", "p/b"))
      done(
        ask(projects, store, "project_list", "project" -> "p", "parent" -> "p/a", "depth" -> 2)
      ).split("\n").toVector.lastOption ==>
        Some(
          "2 more not shown; skip: 40 shows the next, or use a smaller depth or a parent " +
            "further down."
        )
    }

    test(
      "project_list's kind and ended choose the lines of a depth walk without stopping it below the artifacts they leave out"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Top.")
      done(
        ask(
          projects,
          store,
          "project_keep",
          "project" -> "p",
          "handle" -> "x",
          "kind" -> "change",
          "title" -> "Title x",
          "parent" -> "p/a",
          "body" -> "## Why\n\nOther kind."
        )
      )
      under(projects, store, "y", "p/x")
      under(projects, store, "z", "p/a")
      under(projects, store, "w", "p/z")
      done(ask(projects, store, "project_move", "key" -> "p/z", "state" -> "closed"))
      done(
        ask(
          projects,
          store,
          "project_list",
          "project" -> "p",
          "parent" -> "p/a",
          "depth" -> 2,
          "kind" -> "note"
        )
      ) ==>
        "2 match: open 2. 1 ended one not listed; ended: true lists it.\n" +
        "p/3 y (open): Title y · under p/2 · 0.1 KB · changed 2026-10-27\n" +
        "p/5 w (open): Title w · under p/4 · 0.1 KB · changed 2026-10-27"
    }

    test("project_list refuses depth without a parent") {
      val read = plugin().commands
        .find(c => ToolName.value(c.spec.name) == "project_list")
        .map(_.spec.args.read(ujson.Obj("project" -> "p", "depth" -> 2)).left.map(_.message))
      read ==> Some(
        Left(
          ArgsError.Refused("depth", "depth counts levels under a parent: give parent too.").message
        )
      )
    }

    test(
      "project_list refuses a parent in another project, and one the project lacks naming its nearest"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "plan-c5", "note", "Top.")
      val read = projects.commands
        .find(c => ToolName.value(c.spec.name) == "project_list")
        .map(_.spec.args.read(ujson.Obj("project" -> "p", "parent" -> "q/plan-c5")))
      (
        read.map(_.left.map(_.message)),
        ask(projects, store, "project_list", "project" -> "p", "parent" -> "p/plan-c")
      ) ==> (
        Some(
          Left(ArgsError.Refused("parent", "The parent q/plan-c5 is not in project p.").message)
        ),
        CommandOutcome.Refused("project p has no artifact named plan-c (nearest: plan-c5)")
      )
    }

    test("project_find finds artifacts by the words of their bodies") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "The cache is cold.")
      kept(projects, store, "b", "note", "Nothing here.")
      // A find reads the documents current at its moment: kept before it.
      val found =
        done(askAt(at.plusSeconds(1))(projects, store, "project_find", "query" -> "cache"))
      assert(found == "p/1 a (note, open): Title a")
    }
    test("project_find finds an artifact a re-key left under its name, by its id") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "The cache is cold.")
      storedUnder(store, "p/1", "p/cache-notes")
      done(
        askAt(at.plusSeconds(1))(projects, store, "project_find", "query" -> "cache")
      ) ==> "p/1 a (note, open): Title a"
    }

    test("project_find leaves out ended artifacts unless asked for them") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "The cache is cold.")
      kept(projects, store, "b", "note", "The cache was dropped.")
      done(ask(projects, store, "project_move", "key" -> "p/b", "state" -> "closed"))
      def found(more: (String, ujson.Value)*) =
        done(
          askAt(at.plusSeconds(1))(
            projects,
            store,
            "project_find",
            (Seq("query" -> ujson.Str("cache")) ++ more)*
          )
        ).linesIterator.toVector.sorted
      assert(found() == Vector("p/1 a (note, open): Title a"))
      assert(
        found("ended" -> true) == Vector(
          "p/1 a (note, open): Title a",
          "p/2 b (note, closed): Title b"
        )
      )
    }

    test("project_new refuses a project that exists, and a method not declared") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      assert(
        ask(projects, store, "project_new", "project" -> "p") == CommandOutcome.Refused(
          "project p exists already"
        )
      )
      val unknown = ask(projects, store, "project_new", "project" -> "q", "method" -> "other")
      assert(
        unknown == CommandOutcome.Refused(
          "No method other is declared here: declare it with project_declare."
        )
      )
    }

    test(
      "project_brief gives a role's brief, refusing a role not declared, no budget and one too small"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nBecause.")
      def briefed(args: (String, ujson.Value)*): CommandOutcome =
        ask(projects, store, "project_brief", ("key" -> ujson.Str("p/a")) +: args*)
      val text = done(briefed("role" -> "builder", "chars" -> 7000))
      val pin = method().digest.hex.take(Brief.PinDigits)
      val stranger = briefed("role" -> "reviewer", "chars" -> 7000)
      val small = briefed("role" -> "builder", "chars" -> 10)
      val unbudgeted = projects.commands
        .find(c => ToolName.value(c.spec.name) == "project_brief")
        .map(_.spec.args.read(ujson.Obj("key" -> "p/a", "role" -> "builder")).left.map(_.message))
        .map(_.map(_ => ()))
      assert(
        unbudgeted == Some(Left("`chars` is missing: it takes a whole number from 1 to 100000.")),
        text.startsWith(s"p/1 a | change | proposed | Title a | grit v1 | pin $pin\n"),
        text.contains("Because."),
        stranger == CommandOutcome.Refused(
          "method grit version 1 shows no role reviewer; its roles: builder"
        ),
        small match {
          case CommandOutcome.Refused(why) =>
            why.startsWith("p/a with what its role always carries is ") && why.endsWith(", over 10")
          case CommandOutcome.Done(_) => false
        }
      )
    }
  }
}
