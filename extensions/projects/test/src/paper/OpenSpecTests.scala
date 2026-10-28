package grit.projects.paper

import grit.projects.artifact.{Artifact, Body}
import grit.projects.method.Example.{Expect, Move}
import grit.projects.method.MethodFixtures.*
import grit.projects.method.{
  ArtifactKind,
  Example,
  Method,
  MethodBench,
  MoveRefusal,
  RoleView,
  Transition,
  TransitionAction,
  TransitionChecks
}
import grit.projects.name.MethodName
import grit.projects.plugin.{CommandFixtures, Projects}

import utest.*

/** OpenSpec (proposal, then delta specs, design and tasks, then archive) as method data: the
  * paper check that the method language can say it.
  */
object OpenSpecTests extends TestSuite {

  private val spec =
    ArtifactKind(kind("spec"), state("current"), Vector.empty, Set.empty, Vector("Purpose"))
  private val change = ArtifactKind(
    kind("change"),
    state("proposed"),
    Vector(state("specified"), state("designed"), state("tasked"), state("archived")),
    Set(state("archived")),
    Vector("Why", "What changes")
  )
  private val design =
    ArtifactKind(
      kind("design"),
      state("drafted"),
      Vector(state("accepted")),
      Set(state("accepted")),
      Vector.empty
    )
  private val task =
    ArtifactKind(
      kind("task"),
      state("open"),
      Vector(state("done")),
      Set(state("done")),
      Vector.empty
    )

  private def step(k: ArtifactKind, from: String, to: String, checks: String*): Transition =
    Transition(k.name, state(from), state(to), checks.toVector.map(check), TransitionAction.Nothing)

  /** OpenSpec as method data, for the plugin's suites to run its commands under. */
  private[projects] val openSpec: Method = right(
    Method
      .of(
        right(MethodName.of("openspec")),
        1,
        Vector(spec, change, design, task),
        Vector(
          step(change, "proposed", "specified", "sections-present"),
          step(change, "specified", "designed"),
          step(change, "designed", "tasked"),
          Transition(
            change.name,
            state("tasked"),
            state("archived"),
            Vector(check("children-ended")),
            TransitionAction.MergeDeltas(spec.name)
          ),
          step(design, "drafted", "accepted"),
          step(task, "open", "done")
        ),
        Vector(
          right(RoleView.of(role("author"), Set(spec.name, change.name, design.name, task.name), 2))
        ),
        TransitionChecks.all
      )
      .left
      .map(_.mkString("; "))
  )

  private val specBody = "## Purpose\n\nSign people in.\n\n## Login\n\nA password alone."
  private val deltas =
    "## Why\n\nPasswords leak.\n\n## What changes\n\nA second factor.\n\n" +
      "## Delta: Login\n\nA password and a code.\n\n## Delta: Recovery\n\nA code by mail."

  private val theSpec = artifact(head("p/auth", spec, "current"), specBody)
  private def theChange(
      in: String,
      cites: Vector[String] = Vector("p/auth"),
      body: String = deltas
  ) =
    artifact(head("p/2fa", change, in, cites = cites), body)
  private val theDesign =
    artifact(head("p/2fa-design", design, "drafted", parent = Some("p/2fa")), "How.")
  private val theTask = artifact(head("p/2fa-task", task, "open", parent = Some("p/2fa")), "Do it.")

  private def to(k: String, s: String, expect: Expect = Expect.Allowed) =
    Move(key(k), state(s), expect)

  private val examples: Vector[Example] = Vector(
    Example(
      "a change is archived once its design and tasks are done",
      project(theSpec, theChange("proposed"), theDesign, theTask),
      Vector(
        to("p/2fa", "specified"),
        to("p/2fa", "designed"),
        to("p/2fa", "tasked"),
        to("p/2fa", "archived", Expect.RefusedBy(check("children-ended"))),
        to("p/2fa-design", "accepted"),
        to("p/2fa-task", "done"),
        to("p/2fa", "archived")
      )
    ),
    Example(
      "a proposal without its why is not specified",
      project(theSpec, theChange("proposed", body = "## What changes\n\nA second factor.")),
      Vector(to("p/2fa", "specified", Expect.RefusedBy(check("sections-present"))))
    )
  )

  private def merged(in: Vector[Artifact]): Either[MoveRefusal, Option[Artifact]] =
    openSpec.move(in, key("p/2fa"), state("archived")).map(_.changed)

  val tests = Tests {
    test("OpenSpec passes Method.of and its examples pass the bench") {
      assert(MethodBench.failures(openSpec, examples) == Vector.empty)
    }
    test("archiving merges each delta into the spec cited, replacing its section or adding one") {
      val expected = right(
        Body.read(
          "## Purpose\n\nSign people in.\n\n## Login\n\nA password and a code.\n\n" +
            "## Recovery\n\nA code by mail."
        )
      )
      assert(
        merged(Vector(theSpec, theChange("tasked"))) == Right(
          Some(Artifact(theSpec.head, expected))
        )
      )
    }
    test("archiving is refused citing no spec, or past the cap") {
      val tasked = theChange("tasked", cites = Vector.empty)
      val citesTask = theChange("tasked", cites = Vector("p/2fa-task"))
      val full = artifact(theSpec.head, "## Purpose\n\n" + "a" * (Body.StoredMost - 200))
      val big = theChange("tasked", body = "## Delta: More\n\n" + "b" * 400)
      val done = artifact(theTask.head.copy(state = state("done")), "Do it.")
      assert(
        merged(Vector(theSpec, tasked)) == Left(
          MoveRefusal.NoInto(tasked.head, state("archived"), spec.name)
        ),
        merged(Vector(theSpec, citesTask, done)) == Left(
          MoveRefusal.NoInto(citesTask.head, state("archived"), spec.name)
        ),
        merged(Vector(full, big)) match {
          case Left(MoveRefusal.Unmerged(head, _, into, why)) =>
            head == big.head && into == full.head && why.contains(s"over ${Body.StoredMost}")
          case _ => false
        }
      )
    }
    test("a merge into a change citing two specs is refused, naming both") {
      val billing = artifact(head("p/billing", spec, "current"), "## Purpose\n\nBill people.")
      val both = theChange("tasked", cites = Vector("p/auth", "p/billing"))
      assert(
        merged(Vector(theSpec, billing, both)) == Left(
          MoveRefusal.IntoSeveral(
            both.head,
            state("archived"),
            spec.name,
            Vector(key("p/auth"), key("p/billing"))
          )
        )
      )
    }
    test("the spec is found whatever the order of cites") {
      val taskFirst = theChange("tasked", cites = Vector("p/2fa-task", "p/auth"))
      val done = artifact(theTask.head.copy(state = state("done")), "Do it.")
      val (first, last) = (
        merged(Vector(theSpec, theChange("tasked"), done)),
        merged(Vector(theSpec, taskFirst, done))
      )
      assert(first.exists(_.nonEmpty), last == first)
    }
    test("project_move to archived keeps the spec merged, recording its mover") {
      import CommandFixtures.{ask, askAs, done as answered, other, otherBy, shown, Store}
      val projects =
        right(Projects.of(Vector(openSpec -> examples), openSpec.name).left.map(_.toString))
      val store = Store()
      answered(ask(projects, store, "project_new", "project" -> "p"))
      def keep(local: String, kind: String, body: String, cites: String*) = answered(
        ask(
          projects,
          store,
          "project_keep",
          "key" -> s"p/$local",
          "kind" -> kind,
          "title" -> local,
          "body" -> body,
          "cites" -> ujson.Arr.from(cites.map(ujson.Str(_)))
        )
      )
      keep("auth", "spec", specBody)
      keep("2fa", "change", deltas, "p/auth")
      Vector("specified", "designed", "tasked", "archived").foreach { s =>
        answered(askAs(other)(projects, store, "project_move", "key" -> "p/2fa", "to" -> s))
      }
      val spec = shown(projects, store, "p/auth")
      assert(
        spec.contains("A password and a code."),
        spec.contains("## Recovery"),
        !spec.contains("A password alone."),
        spec.contains(s"by: \"$otherBy\"")
      )
    }
  }
}
