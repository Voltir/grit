package grit.projects.plugin

import grit.core.document.DocText
import grit.core.id.{ConversationId, Sha256, TurnRef, TurnSeq}
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.projects.artifact.{Artifact, ArtifactHead, ArtifactJson}
import grit.projects.method.{
  ArtifactKind,
  Example,
  MethodFixtures,
  MethodJson,
  TransitionCheck,
  TransitionChecks
}
import grit.projects.name.{ArtifactNumber, CheckName, Pin, ProjectKey}

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
  private val second = method(transitions :+ MethodFixtures.move(note, "open", "closed"), 2)

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

  /** What a command answers over a project whose method no longer reads here. */
  private val unreadable =
    "grit@1 is declared but does not read: check sections-present is compiled at version 2, not 1"

  /** `p`'s index rewritten to pin [[methodName]] at `version` under `digest`. */
  private def repinned(store: Store, version: Int, digest: Sha256): Unit = {
    val (key, text, data, refs) = right(
      ArtifactJson.index(
        right(ProjectKey.of("p")),
        right(Pin.of(methodName, version, digest)),
        by,
        ArtifactNumber.First
      )
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
            "they say: example \"a bare change is done\", move 2 (p/1 to done) expected allowed; " +
            "but p/1 a cannot move to done: sections-present: its body lacks ## Why."
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
      ) ==> (CommandOutcome.Refused(declareRefused), CommandOutcome.Refused(declareRefused), 0)
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
    test("a declared method is in no find answer, though its text matches") {
      // list, stale, sweep and a brief read a project by its ref, which a method never holds.
      val store = Store()
      started(plugin(), store)
      kept(plugin(), store, "a", "note", "Kept at its first version.")
      done(
        askAt(at.plusSeconds(1))(plugin(), store, "project_find", "query" -> "version")
      ) ==> "p/1 a (note, open): Title a"
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
        above("project_methods", "name" -> "grit", "version" -> 1)
      ) ==> (
        CommandOutcome.Refused(
          s"The method grit version 1 is refused: methods are kept at " +
            s"${Label.shown(Label.Public)}, which every caller of the project commands reads, " +
            s"and this room keeps what it writes at ${Label.shown(TestLabels.Trial)}; declare " +
            s"it from a room labelled ${Label.shown(Label.Public)}."
        ),
        CommandOutcome.Refused("the method grit version 1 is not declared here")
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
      val read = MethodJson.read(ujson.read(done(above)), TransitionChecks.all)
      (above, read.map(_.method.digest)) ==> (
        ask(plugin(), store, "project_methods", "name" -> "grit", "version" -> 1),
        Right(method().digest)
      )
    }
    test(
      "project_list refuses a project whose declared method no longer reads, and lists one whose method is undeclared"
    ) {
      val store = Store()
      started(plugin(), store)
      kept(plugin(), store, "a", "change", "## Why\n\nBecause.")
      val sections = MethodFixtures.check("sections-present")
      val bumped = Projects
        .of(
          TransitionChecks.all.map { c =>
            if (c.name != sections) c
            else
              new TransitionCheck {
                val name: CheckName = c.name
                val version: Int = c.version + 1
                def holds(
                    judged: Artifact,
                    heads: Vector[ArtifactHead],
                    kinds: Vector[ArtifactKind]
                ): Either[String, Unit] = c.holds(judged, heads, kinds)
              }
          },
          methodName
        )
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      val unread = ask(bumped, store, "project_list", "project" -> "p")
      repinned(store, 2, method().digest)
      (unread, ask(plugin(), store, "project_list", "project" -> "p")) ==> (
        CommandOutcome.Refused(unreadable),
        CommandOutcome.Done(
          "1 match: change 1 (proposed 1).\n" +
            "p/1 a (change, proposed): Title a · 0.1 KB · changed 2026-10-27"
        )
      )
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
