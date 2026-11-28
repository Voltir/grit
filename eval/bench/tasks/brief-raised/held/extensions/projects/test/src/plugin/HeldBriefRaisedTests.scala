package grit.projects.plugin

import grit.core.document.{DocText, Document, DocumentKeeper}
import grit.core.id.DocKey
import grit.core.plugin.CommandOutcome
import grit.core.store.StoreError
import grit.core.visibility.{Integrity, Label, Visibility}
import grit.projects.method.MethodFixtures

import utest.*

/** A claimed artifact's brief raised again once what it shows is raised, and left alone by a
  * lower keep of what it does not show; and the declare refusal's next step.
  */
object HeldBriefRaisedTests extends TestSuite {
  import CommandFixtures.*
  import MethodFixtures.right

  private val briefKey: DocKey = right(DocKey.of("p/m#brief"))

  private def failed(e: StoreError): Nothing = throw new java.lang.AssertionError(e.toString)

  /** The brief of `p/m` current at public. */
  private def current(store: Store): Option[Document] =
    inTx(store.keeper.current(briefKey, Label.Public)).fold(failed, identity)

  /** The brief's text and how far it is believed, as [[Integrity.written]] writes it. */
  private def brief(store: Store): Option[(String, String)] =
    current(store).map(d => (DocText.value(d.text), Integrity.written(d.integrity)))

  /** The versions of `p/m`'s brief kept at public, withdrawals included. */
  private def versions(store: Store): Int =
    inTx(store.keeper.versions(briefKey, Label.Public, 100)).fold(failed, _.size)

  /** Project `p` under [[managing]]: `p/c`; `p/b`, which nothing names; `p/process`, which a
    * manager's brief carries; and `p/m`, citing `p/c`, claimed for a manager.
    */
  private def claimedProject(): (Projects, Store) = {
    val projects = plugin()
    val store = Store(declared = Vector(managing))
    started(projects, store)
    kept(projects, store, "c", "note", "Cited.")
    kept(projects, store, "b", "note", "Named by nothing.")
    kept(projects, store, "process", "note", "How work is run.")
    done(
      ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/m",
        "kind" -> "note",
        "title" -> "Title m",
        "body" -> "The manager's note.",
        "cites" -> ujson.Arr("p/c")
      )
    )
    done(
      ask(
        projects,
        store,
        "project_claim",
        "key" -> "p/m",
        "role" -> "manager",
        "holder" -> "coder-opus/abc"
      )
    )
    (projects, store)
  }

  /** `p/c` kept again, its text unchanged. */
  private val citedAgain: Vector[(String, ujson.Value)] =
    Vector[(String, ujson.Value)]("key" -> "p/c", "title" -> "Title c", "body" -> "Cited.")

  val tests = Tests {
    test(
      "a brief lowered by a lower keep of what it shows is raised, its text unchanged, once a caller believed at the meet's identity keeps that artifact unchanged"
    ) {
      val (projects, store) = claimedProject()
      val first = brief(store)
      done(askBelieved(Integrity.Member)(projects, store, "project_keep", citedAgain*))
      val lowered = brief(store)
      done(
        askBelieved(Integrity.top(Visibility.Shipped.compartments))(
          projects,
          store,
          "project_keep",
          citedAgain*
        )
      )
      (first.map(_._2), lowered.map(_._2), brief(store)) ==>
        (Some("authority"), Some("member"), first)
    }
    test("a lower keep of an artifact a lowered brief does not show writes no version of it") {
      val (projects, store) = claimedProject()
      done(askBelieved(Integrity.Member)(projects, store, "project_keep", citedAgain*))
      val before = versions(store)
      done(
        askThrough(DocumentKeeper.believedAtMost(store.keeper, Integrity.Outside), store.claims)(
          projects,
          store,
          "project_keep",
          "key" -> "p/b",
          "title" -> "Title b",
          "body" -> "New."
        )
      )
      (before, versions(store), brief(store).map(_._2)) ==> (2, 2, Some("member"))
    }
    test("a declaration refused below Authority tells its caller to ask a person to declare it") {
      val store = Store(declared = Vector.empty)
      val answer =
        askBelieved(Integrity.Member)(plugin(), store, "project_declare", declaring(method()))
      val refusal = answer match {
        case CommandOutcome.Refused(why) => Some(why)
        case CommandOutcome.Done(_) => None
      }
      val next = "Ask a person to declare it from the method's file."
      (refusal.map(_.contains(next)), store.count) ==> (Some(true), 0)
    }
  }
}
