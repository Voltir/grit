package grit.projects.plugin

import grit.core.document.{DocRef, Document}
import grit.core.id.DocKey
import grit.core.plugin.Answered
import grit.core.visibility.Label
import grit.projects.method.MethodFixtures

import utest.*

/** `project_claim` and `project_release`: a claim is its own document, `claim/<artifact>`.
  *
  * Two gaps are stated, not closed. Of two claims racing for one artifact, the later loses on
  * the store's one-current-document index and fails with a store error, not a refusal naming
  * the winner; asked again, it is refused naming the first claim. And one live claim per
  * artifact holds only within a label: a claim kept at a label the asker cannot read is another
  * document, unseen. Neither is reached over one store at one label, as here.
  */
object ClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.right

  private def current(store: Store, key: String): Option[Document] =
    inTx(store.keeper.current(right(DocKey.of(key)), Label.Public))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private def claimed(projects: Projects, store: Store): Answered =
    ask(projects, store, "project_claim", "key" -> "p/a")

  /** A project `p` holding one note, `p/a`. */
  private def holding(projects: Projects, store: Store): Unit = {
    started(projects, store)
    kept(projects, store, "a", "note", "Text.")
  }

  val tests: Tests = Tests {

    test("a second claim for one key is refused naming the first") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val first = claimed(projects, store)
      val second = claimed(projects, store)
      assert(
        first == Answered.Done(s"Claimed p/a (note, open) for $by."),
        second == Answered.Refused(
          s"p/a is claimed already, by $by since $at; project_release frees it"
        )
      )
    }

    test("release frees the key") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val released = ask(projects, store, "project_release", "key" -> "p/a")
      val again = claimed(projects, store)
      val unclaimed = {
        done(ask(projects, store, "project_release", "key" -> "p/a"))
        ask(projects, store, "project_release", "key" -> "p/a")
      }
      assert(
        again == Answered.Done(s"Claimed p/a (note, open) for $by."),
        released == Answered.Done("Released p/a."),
        unclaimed == Answered.Refused("p/a is not claimed")
      )
    }

    test("a claim leaves the artifact's version unchanged") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val before = current(store, "p/a").map(_.version)
      done(claimed(projects, store))
      val after = current(store, "p/a").map(_.version)
      val claim = current(store, "claim/p/a")
      val referring =
        inTx(store.keeper.referring(right(DocRef.of("claims", "p/a")), 10))
          .fold(e => throw new java.lang.AssertionError(e.toString), _.map(_.key))
      assert(
        before.isDefined,
        after == before,
        claim.map(_.data) == Some(ujson.Obj("claimed" -> ujson.Obj("by" -> by, "for" -> "p/a"))),
        referring == Vector(right(DocKey.of("claim/p/a")))
      )
    }
  }
}
