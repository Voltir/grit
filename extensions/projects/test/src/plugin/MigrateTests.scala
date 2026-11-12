package grit.projects.plugin

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersionId}
import grit.core.plugin.{CommandBy, CommandOutcome}
import grit.core.visibility.{Clearance, Label, TestLabels}
import grit.projects.method.{ArtifactKind, Method, MethodFixtures, TransitionChecks}
import grit.projects.name.KindName
import grit.projects.render.ProjectMarkdown

import utest.*

/** `project_migrate` over a project of the method's version 1, to its version 2. */
object MigrateTests extends TestSuite {
  import CommandFixtures.*
  import MethodFixtures.{change, note, transitions, views}

  /** A kind version 2 declares that version 1 does not. */
  private val memo = ArtifactKind(
    MethodFixtures.kind("memo"),
    MethodFixtures.state("open"),
    Vector(),
    Set(),
    Vector()
  )

  private val version2: Method = Method
    .of(methodName, 2, Vector(change, note, memo), transitions, views, TransitionChecks.all)
    .fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** The plugin declaring both versions: a project it starts holds to version 2. */
  private val both: Projects = Projects
    .of(Vector(method() -> Vector.empty, version2 -> Vector.empty), methodName)
    .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** When [[other]] migrates, a day after the artifacts were kept. */
  private val later: Instant = at.plusSeconds(86_400)

  /** Project `p` under version 1: notes `p/a` and `p/b`, and the change `p/c`, kept by [[person]]. */
  private def holding(store: Store): Unit = {
    val v1 = plugin()
    started(v1, store)
    kept(v1, store, "a", "note", "A.")
    kept(v1, store, "b", "note", "B.")
    kept(v1, store, "c", "change", "## Why\n\nBecause.")
  }

  /** `p` moved to version 2 by [[other]] at [[later]], from a room `cleared`, with `more`. */
  private def migratedFrom(
      store: Store,
      cleared: Clearance,
      more: (String, ujson.Value)*
  ): CommandOutcome =
    askBy(CommandBy.Direct(other), later, cleared)(
      both,
      store,
      "project_migrate",
      (Vector[(String, ujson.Value)]("project" -> "p", "version" -> 2) ++ more)*
    )

  private def migrated(store: Store, more: (String, ujson.Value)*): CommandOutcome =
    migratedFrom(store, Clearance.of(Label.Public), more*)

  private val noteToMemo: (String, ujson.Value) =
    "kinds" -> ujson.Arr(ujson.Obj("from" -> "note", "to" -> "memo"))

  /** Each artifact's document, by key: its version, when it was written, and its data. */
  private def artifacts(
      store: Store
  ): Map[String, (DocumentVersionId, Instant, ujson.Value)] =
    inTx(store.keeper.newest(1_000))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      .filterNot(d => DocKey.value(d.key) == "p")
      .map(d => DocKey.value(d.key) -> (d.version, d.written, d.data))
      .toMap

  /** `p` as its export reads back: its pin's version, and each artifact's key and kind. */
  private def exported(store: Store): Either[String, (Int, Vector[(String, String)])] = {
    val page = ujson.read(done(ask(both, store, "project_export", "project" -> "p")))
    ProjectMarkdown
      .read(page("files").arr.toVector.map(f => f("path").str -> f("text").str))
      .left
      .map(_.toString)
      .map(p =>
        (p.pin.version, p.artifacts.map(a => a.head.key.value -> KindName.value(a.head.kind)))
      )
  }

  val tests = Tests {
    test(
      "a migration renaming nothing keeps no artifact again, each left as it was, and moves the project's pin"
    ) {
      val store = Store()
      holding(store)
      val before = artifacts(store)
      (migrated(store), artifacts(store) == before, exported(store).map(_._1)) ==> (
        (
          CommandOutcome.Done(
            "Moved project p to method grit version 2; no artifact renamed, none kept again."
          ),
          true,
          Right(2)
        )
      )
    }

    test(
      "a migration renaming one kind keeps again exactly its artifacts, written by the migrator, and leaves the others as they were"
    ) {
      val store = Store()
      holding(store)
      val before = artifacts(store)
      val answer = migrated(store, noteToMemo)
      val after = artifacts(store)
      (
        answer,
        after.get("p/c") == before.get("p/c"),
        Vector("p/a", "p/b").map(k =>
          after
            .get(k)
            .map((v, written, data) =>
              (before.get(k).exists(_._1 != v), written, data("head")("by").strOpt)
            )
        )
      ) ==> (
        (
          CommandOutcome.Done(
            "Moved project p to method grit version 2; 2 artifacts renamed and kept again: p/a, p/b."
          ),
          true,
          Vector.fill(2)(Some((true, later, Some(otherBy))))
        )
      )
    }

    test("what a migration leaves exports and reads back under the new version, renamed") {
      val store = Store()
      holding(store)
      done(migrated(store, noteToMemo))
      exported(store) ==> Right(
        (2, Vector("p/a" -> "memo", "p/b" -> "memo", "p/c" -> "change"))
      )
    }

    test("past 40 renamed keys the answer is cut, saying how many more") {
      val store = Store()
      notes(plugin(), store, 42)
      migrated(store, noteToMemo) ==> CommandOutcome.Done(
        "Moved project p to method grit version 2; 42 artifacts renamed and kept again: " +
          (1 to 40).map(i => f"p/n$i%02d").mkString(", ") + "; 2 more not shown."
      )
    }

    test(
      "from a room above its label, a migration is refused naming the project and the artifacts it would rewrite, not those it leaves"
    ) {
      val store = Store()
      holding(store)
      val before = artifacts(store)
      (
        migratedFrom(store, Clearance.of(TestLabels.Trial), noteToMemo),
        artifacts(store) == before
      ) ==> (
        (
          CommandOutcome.Refused(
            Vector("project p", "p/b", "p/a")
              .map(subject =>
                s"$subject is kept labelled [level: public], below what this room writes " +
                  "([level: public, in: {trial}]): migrate from a room at its label"
              )
              .mkString("; ")
          ),
          true
        )
      )
    }
  }
}
