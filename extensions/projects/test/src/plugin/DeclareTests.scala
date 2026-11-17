package grit.projects.plugin

import grit.core.document.DocText
import grit.core.id.{ConversationId, Sha256, TurnRef, TurnSeq}
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.projects.artifact.ArtifactJson
import grit.projects.method.{Example, MethodFixtures, MethodJson, TransitionChecks}
import grit.projects.name.{Pin, ProjectKey}

import utest.*

/** `project_declare` and `project_methods`: a method version kept as a document of its own, read
  * by every command from the store, never changed once declared, and never shown as an artifact.
  */
object DeclareTests extends TestSuite {
  import CommandFixtures.*
  import MethodFixtures.{note, right, state, transitions}

  /** [[method]] with one transition more: version 1 under another digest. */
  private val changed = method(transitions :+ MethodFixtures.move(note, "open", "closed"))

  /** [[changed]]'s kinds and moves as version 2. */
  private val second = right(
    grit.projects.method.Method
      .of(
        methodName,
        2,
        Vector(MethodFixtures.change, note),
        transitions :+ MethodFixtures.move(note, "open", "closed"),
        MethodFixtures.views,
        TransitionChecks.all
      )
      .left
      .map(_.toString)
  )

  /** A change kept bare, then moved to building and to done, expected allowed throughout. */
  private val bareDone = {
    val bare = MethodFixtures.artifact(
      MethodFixtures.head("p/a", MethodFixtures.change, "proposed"),
      "No reason given."
    )
    Example(
      "a bare change is done",
      MethodFixtures.project(bare),
      Vector(
        Example.Move(MethodFixtures.key("p/a"), state("building"), Example.Expect.Allowed),
        Example.Move(MethodFixtures.key("p/a"), state("done"), Example.Expect.Allowed)
      )
    )
  }

  /** What a declaration answers when its caller is not believed at Authority, giving it directly. */
  private val byAuthority =
    "A method is declared only by a caller believed at Authority, giving the command directly; " +
      "never through a turn, or by a session believed lower."

  /** `p`'s index rewritten to pin [[methodName]] at `version` under `digest`. */
  private def repinned(store: Store, version: Int, digest: Sha256): Unit = {
    val (key, text, data, refs) = right(
      ArtifactJson.index(right(ProjectKey.of("p")), right(Pin.of(methodName, version, digest)), by)
    )
    inTx(
      store.keeper
        .write(key, Label.Public, Integrity.Authority, Place.Everywhere, text, data, None, at, refs)
    ).fold(e => throw new java.lang.AssertionError(e.toString), _ => ())
  }

  val tests = Tests {
    test("a method whose worked example fails is refused, naming the example and its move") {
      val store = Store(declared = Vector.empty)
      (
        ask(plugin(), store, "project_declare", declaring(method(), Vector(bareDone))),
        store.count
      ) ==> (
        CommandOutcome.Refused(
          "The method grit version 1 is refused: its worked examples come out otherwise than " +
            "they say: example \"a bare change is done\", move 2 (p/a to done) expected allowed, " +
            "but p/a cannot move to done: sections-present: its body lacks ## Why."
        ),
        0
      )
    }
    test("a method naming a check not compiled here is refused, naming the check") {
      val bare = right(Projects.of(Vector.empty, methodName).left.map(_.toString))
      val store = Store(declared = Vector.empty)
      (ask(bare, store, "project_declare", declaring(method())), store.count) ==> (
        CommandOutcome.Refused(
          "The method does not read: check sections-present is not compiled."
        ),
        0
      )
    }
    test("a declaration through a turn, or believed below Authority, is refused, keeping nothing") {
      val store = Store(declared = Vector.empty)
      val turn = CommandBy.Turn(person, TurnRef(ConversationId("c7"), TurnSeq(5)))
      val member = CommandBy.Direct(person, Integrity.Member)
      (
        askBy(turn)(plugin(), store, "project_declare", declaring(method())),
        askBy(member)(plugin(), store, "project_declare", declaring(method())),
        store.count
      ) ==> (CommandOutcome.Refused(byAuthority), CommandOutcome.Refused(byAuthority), 0)
    }
    test("a version declared again under its digest changes nothing; under another is refused") {
      val store = Store()
      val before = store.count
      (
        ask(plugin(), store, "project_declare", declaring(method())),
        ask(plugin(), store, "project_declare", declaring(changed)),
        store.count
      ) ==> (
        CommandOutcome.Done(
          "The method grit version 1 is declared already, under the same digest; nothing changed."
        ),
        CommandOutcome.Refused(
          s"The method grit version 1 is declared already, under digest ${method().digest.hex}, " +
            "and a version never changes: declare this one as version 2."
        ),
        before
      )
    }
    test("project_methods lists each version with its digest, and gives one as JSON it declares") {
      val store = Store()
      done(
        ask(plugin(), store, "project_declare", declaring(second))
      )
      val one = done(ask(plugin(), store, "project_methods", "name" -> "grit", "version" -> 2))
      (
        done(ask(plugin(), store, "project_methods")),
        done(ask(plugin(), store, "project_methods", "name" -> "other")),
        MethodJson.read(ujson.read(one), TransitionChecks.all).map(_.method.digest)
      ) ==> (
        s"grit@1 ${method().digest.hex}\ngrit@2 ${second.digest.hex}",
        "No method other is declared here.",
        Right(second.digest)
      )
    }
    test("a declared method is in no list, find, stale or sweep answer, and in no brief") {
      val store = Store()
      started(plugin(), store)
      kept(plugin(), store, "a", "note", "Text.")
      done(
        ask(
          plugin(),
          store,
          "project_claim",
          "key" -> "p/a",
          "role" -> "builder",
          "holder" -> "h"
        )
      )
      val brief = briefOf(store, "h").fold("")(d => grit.core.document.DocText.value(d.text))
      (
        done(ask(plugin(), store, "project_list", "project" -> "p")).contains("grit@"),
        done(askAt(at.plusSeconds(1))(plugin(), store, "project_find", "query" -> "version")),
        done(ask(plugin(), store, "project_stale", "project" -> "p", "weeks" -> 1))
          .contains("grit@"),
        done(
          ask(
            plugin(),
            store,
            "project_sweep",
            "project" -> "p",
            "kinds" -> ujson.Arr("note"),
            "before" -> "2026-10-27",
            "dry" -> true
          )
        ).contains("grit@"),
        brief.nonEmpty && !brief.contains("grit@") && !brief.contains("Method grit")
      ) ==> (false, "Nothing matches.", false, false, true)
    }
    test(
      "a move is refused when its pinned version is not declared, or its digest is not the pin's"
    ) {
      val store = Store()
      started(plugin(), store)
      kept(plugin(), store, "a", "change", "## Why\n\nBecause.")
      def move = ask(plugin(), store, "project_move", "key" -> "p/a", "state" -> "building")
      repinned(store, 2, method().digest)
      val undeclared = move
      repinned(store, 1, changed.digest)
      (undeclared, move, shown(plugin(), store, "p/a").contains("state: proposed")) ==> (
        CommandOutcome.Refused(
          "the method grit version 2 is not declared here; its versions here: 1"
        ),
        CommandOutcome.Refused(
          "project p holds to method grit version 1, whose rules have changed since the project " +
            "started; declare a new version of the method rather than changing this one"
        ),
        true
      )
    }
    test(
      "a declaration from a room writing above the methods' label is refused, keeping nothing"
    ) {
      val store = Store(declared = Vector.empty)
      def above(name: String, args: (String, ujson.Value)*) =
        askBy(direct(), at, Clearance.of(TestLabels.Trial))(plugin(), store, name, args*)
      (
        above("project_declare", declaring(method())),
        above("project_new", "project" -> "p")
      ) ==> (
        CommandOutcome.Refused(
          s"The method grit version 1 is refused: methods are kept at " +
            s"${Label.shown(Label.Public)}, which every caller of the project commands reads, " +
            s"and this room keeps what it writes at ${Label.shown(TestLabels.Trial)}; declare " +
            s"it from a room labelled ${Label.shown(Label.Public)}."
        ),
        CommandOutcome.Refused("No method grit is declared here: declare it with project_declare.")
      )
    }
    test(
      "a copy of a version at another label is never read: a room above it reads the declared one"
    ) {
      val store = Store()
      val (key, refs) =
        (right(MethodCommands.key(methodName, 1)), right(MethodCommands.refs(methodName)))
      inTx(
        store.keeper.write(
          key,
          TestLabels.Trial,
          Integrity.Authority,
          Place.Everywhere,
          right(DocText.of("Method grit version 1.")),
          MethodJson.written(changed, Vector.empty),
          None,
          at,
          refs
        )
      ).fold(e => throw new java.lang.AssertionError(e.toString), _ => ())
      val above = askBy(direct(), at, Clearance.of(TestLabels.Trial))(
        plugin(),
        store,
        "project_methods",
        "name" -> "grit",
        "version" -> 1
      )
      above ==> ask(plugin(), store, "project_methods", "name" -> "grit", "version" -> 1)
    }
    test("project_new refuses when no version of its method is declared") {
      ask(plugin(), Store(declared = Vector.empty), "project_new", "project" -> "p") ==>
        CommandOutcome.Refused(
          "No method grit is declared here: declare it with project_declare."
        )
    }
    test("Projects.of refuses two compiled checks of one name, naming it") {
      Projects.of(TransitionChecks.all ++ TransitionChecks.all.take(1), methodName).map(_ => ()) ==>
        Left(
          grit.projects.method.MethodRefusal
            .CheckTwice(TransitionChecks.all.headOption.fold(MethodFixtures.check("x"))(_.name))
        )
    }
  }
}
