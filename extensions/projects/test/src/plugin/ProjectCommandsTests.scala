package grit.projects.plugin

import scala.concurrent.duration.*

import grit.core.document.DocumentTerms
import grit.core.plugin.Answered
import grit.core.tool.{ArgsError, ToolName}
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.method.{Example, MethodFixtures}
import grit.projects.name.{ArtifactKey, MethodName, Pin, ProjectKey}
import grit.projects.render.{Brief, ProjectMarkdown}

import utest.*

object ProjectCommandsTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{note, right, state, transitions}

  val tests: Tests = Tests {

    test("a move whose check fails is refused naming it, and keeps nothing") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "No reason given.")
      done(ask(projects, store, "project_move", "key" -> "p/a", "to" -> "building"))
      val before = store.count
      val answer = ask(projects, store, "project_move", "key" -> "p/a", "to" -> "done")
      assert(answer match {
        case Answered.Refused(why) => why.contains("sections-present") && why.contains("Why")
        case Answered.Done(_) => false
      })
      assert(store.count == before, shown(projects, store, "p/a").contains("state: building"))
    }

    test("a move whose checks hold keeps the new state and its mover") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nBecause.")
      done(ask(projects, store, "project_move", "key" -> "p/a", "to" -> "building"))
      done(ask(projects, store, "project_move", "key" -> "p/a", "to" -> "done"))
      val text = shown(projects, store, "p/a")
      assert(text.contains("state: done"), text.contains(s"by: \"$by\""))
    }

    test("a move no transition allows is refused, naming the states it may go to") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "Text.")
      val answer = ask(projects, store, "project_move", "key" -> "p/a", "to" -> "done")
      assert(
        answer == Answered.Refused(
          "p/a (change, proposed) cannot move to done; it may move to building"
        )
      )
    }

    test("a move under a changed method digest is refused") {
      val store = Store()
      started(plugin(), store)
      kept(plugin(), store, "a", "change", "Text.")
      val changed = plugin(
        method(transitions :+ MethodFixtures.move(note, "open", "closed", "sections-present"))
      )
      val answer = ask(changed, store, "project_move", "key" -> "p/a", "to" -> "building")
      assert(answer match {
        case Answered.Refused(why) => why.contains("changed") && why.contains("new version")
        case Answered.Done(_) => false
      })
      assert(shown(changed, store, "p/a").contains("state: proposed"))
    }

    test("project_keep cannot change a state") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "Text.")
      done(ask(projects, store, "project_move", "key" -> "p/a", "to" -> "building"))
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

    test("a keep naming an artifact the project lacks is refused, keeping nothing") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val before = store.count
      val answer = ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/a",
        "kind" -> "note",
        "title" -> "A",
        "body" -> "See [b](artifact:p/b)."
      )
      assert(answer match {
        case Answered.Refused(why) => why.contains("p/b")
        case Answered.Done(_) => false
      })
      assert(store.count == before)
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
        DocumentTerms.of(terms.label, terms.weight, terms.retention, 2, terms.keepLast, terms.full)
      )
      val store = Store(small)
      started(projects, store)
      kept(projects, store, "a", "note", "Text.")
      val answer = ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/b",
        "kind" -> "note",
        "title" -> "B",
        "body" -> "Text."
      )
      assert(answer == Answered.Refused(Projects.full(2)), store.count == 2)
    }

    test("export pages concatenate to ProjectMarkdown.render's files") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      val locals = Vector.tabulate(25)(i => f"a$i%02d")
      locals.foreach(l => kept(projects, store, l, "note", s"Text of $l."))
      val p = right(ProjectKey.of("p"))
      val m = method()
      val expected = right(
        Project.of(
          p,
          right(Pin.of(m.name, m.version, m.digest)),
          locals.map { l =>
            Artifact(
              ArtifactHead(
                right(ArtifactKey.of(p, l)),
                note.name,
                state("open"),
                s"Title $l",
                None,
                Vector.empty,
                Some(by)
              ),
              right(Body.read(s"Text of $l."))
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
      assert(text.startsWith("---\nkey: p/a\nkind: note\nstate: open\n"), text.endsWith("Text.\n"))
      assert(
        ask(projects, store, "project_show", "key" -> "p/z") == Answered.Refused(
          "project p has no artifact p/z"
        )
      )
    }

    /** Keeps `p/a`, a change titled `title` holding `body`, `m` minutes after [[at]]. */
    def keptAt(projects: Projects, store: Store, m: Int, title: String, body: String): Unit =
      done(
        askAt(at.plusSeconds(60L * m))(
          projects,
          store,
          "project_keep",
          "key" -> "p/a",
          "kind" -> "change",
          "title" -> title,
          "body" -> body
        )
      )

    test("history lists versions newest first with who wrote each") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(projects, store, 1, "First", "## Why\n\nOne.")
      keptAt(projects, store, 2, "Second", "## Why\n\nTwo.")
      done(
        askAt(at.plusSeconds(180))(
          projects,
          store,
          "project_move",
          "key" -> "p/a",
          "to" -> "building"
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
            s" 2026-10-27T12:02:00Z by $by (proposed): Second",
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
          "from" -> older.toLong,
          "to" -> newer.toLong
        )
      ).linesIterator.toVector
      (diff.take(2), diff.filter(l => l.startsWith("-") || l.startsWith("+")).drop(2)) ==> (
        (Vector(s"--- p/a version $older", s"+++ p/a version $newer"), Vector("- One.", "+ Two."))
      )
    }

    test("history refuses a lone to by naming from") {
      val projects = plugin()
      val history = projects.commands.find(c => ToolName.value(c.spec.name) == "project_history")
      val read = history.map(_.spec.args.read(ujson.Obj("key" -> "p/a", "to" -> 2)))
      assert(read.exists(_.left.exists {
        case ArgsError.Refused("from", _) => true
        case _ => false
      }))
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
          case Answered.Done(text) => text.linesIterator.toVector.lastOption
          case Answered.Refused(why) => Some(why)
        },
        shownAt(3) match {
          case Answered.Done(text) => text.linesIterator.toVector.lastOption
          case Answered.Refused(why) => Some(why)
        },
        shownAt(0)
      ) ==> (
        (
          Some("One."),
          Some("Two."),
          Answered.Refused("p/a had no version at 2026-10-27T12:00:00Z")
        )
      )
    }

    test("project_list lists the heads of a kind and a state, by key") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "b", "note", "Text.")
      kept(projects, store, "a", "note", "Text.")
      kept(projects, store, "c", "change", "Text.")
      done(ask(projects, store, "project_move", "key" -> "p/b", "to" -> "closed"))
      val open = done(ask(projects, store, "project_list", "project" -> "p", "state" -> "open"))
      assert(open == "p/a (note, open): Title a")
      val all = done(ask(projects, store, "project_list", "project" -> "p"))
      assert(
        all == "p/a (note, open): Title a\np/b (note, closed): Title b\np/c (change, proposed): Title c"
      )
    }

    test("project_find finds artifacts by the words of their bodies") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "The cache is cold.")
      kept(projects, store, "b", "note", "Nothing here.")
      val found = done(ask(projects, store, "project_find", "query" -> "cache"))
      assert(found == "p/a (note, open): Title a")
    }

    test("project_new refuses a project that exists, and a method not declared") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      assert(
        ask(projects, store, "project_new", "project" -> "p") == Answered.Refused(
          "project p exists already"
        )
      )
      val unknown = ask(projects, store, "project_new", "project" -> "q", "method" -> "other")
      assert(unknown == Answered.Refused("no method is named other; the methods here: grit"))
    }

    test("every command's does line tells a project artifact from a claude.ai Artifact") {
      val lines = plugin().commands.map(_.spec.does)
      assert(lines.size == 11, lines.forall(_.endsWith(Projects.NotClaudeAi)))
    }

    test(
      "project_brief gives a role's brief, refusing a role not declared and a budget too small"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "change", "## Why\n\nBecause.")
      def briefed(args: (String, ujson.Value)*): Answered =
        ask(projects, store, "project_brief", ("key" -> ujson.Str("p/a")) +: args*)
      val text = done(briefed("role" -> "builder"))
      val pin = method().digest.hex.take(Brief.PinDigits)
      val stranger = briefed("role" -> "reviewer")
      val small = briefed("role" -> "builder", "chars" -> 10)
      assert(
        text.startsWith(s"p/a | change | proposed | Title a | grit v1 | pin $pin\n"),
        text.contains("Because."),
        stranger == Answered.Refused(
          "method grit version 1 shows no role reviewer; its roles: builder"
        ),
        small match {
          case Answered.Refused(why) => why.startsWith("p/a alone is ") && why.endsWith(", over 10")
          case Answered.Done(_) => false
        }
      )
    }

    test("Projects.of refuses a default it lacks, and two methods of one name and version") {
      val m = method()
      val other = right(MethodName.of("other"))
      assert(
        Projects.of(Vector(m -> Vector.empty), other) == Left(ProjectsRefusal.NoDefault(other))
      )
      assert(
        Projects.of(
          Vector(m, method(transitions.take(1) ++ transitions.drop(1))).map(_ -> Vector.empty),
          methodName
        ) ==
          Left(ProjectsRefusal.Twice(methodName, 1))
      )
    }
    test("a method whose example fails is refused, naming the example and its move") {
      val m = method()
      val bare = MethodFixtures.artifact(
        MethodFixtures.head("p/a", MethodFixtures.change, "proposed"),
        "No reason given."
      )
      val done = Example.Move(MethodFixtures.key("p/a"), state("done"), Example.Expect.Allowed)
      val example = Example(
        "a bare change is done",
        MethodFixtures.project(bare),
        Vector(
          Example.Move(MethodFixtures.key("p/a"), state("building"), Example.Expect.Allowed),
          done
        )
      )
      assert(Projects.of(Vector(m -> Vector(example)), methodName) match {
        case Left(ProjectsRefusal.Unbenched(name, 1, Vector(failed))) =>
          name == methodName && failed.example == "a bare change is done" && failed.at == 2 &&
          failed.move == done
        case _ => false
      })
    }
  }
}
