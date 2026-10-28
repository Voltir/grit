package grit.projects.plugin

import grit.core.document.{DocRef, Document}
import grit.core.id.DocKey
import grit.core.plugin.CommandOutcome
import grit.core.visibility.Label
import grit.projects.method.MethodFixtures
import grit.projects.name.BriefSlot

import utest.*

/** `project_claim` and `project_release`: a claim is its own document, `claim/<artifact>`, made
  * for one slot (what a brief is written for) as one role; an artifact has one live claim, and a
  * slot works one artifact.
  *
  * Two gaps are stated, not closed. Claims racing each other are not refused: of two for one
  * artifact, the later loses on the store's one-current-document index and fails with a store
  * error, not a refusal naming the winner (asked again, it is refused naming the first); two
  * for one slot on different artifacts may both be kept. And both holds hold only within a
  * label: a claim kept at a label the asker cannot read is not seen. Neither is reached over one
  * store at one label, as here.
  */
object ClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.right

  /** The slot every claim here is for, unless another is named. */
  private val slot: String = "coder-opus/abc"

  private def current(store: Store, key: String): Option[Document] =
    inTx(store.keeper.current(right(DocKey.of(key)), Label.Public))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private def referring(store: Store, kind: String, ref: String): Vector[DocKey] =
    inTx(store.keeper.referring(right(DocRef.of(kind, ref)), 10))
      .fold(e => throw new java.lang.AssertionError(e.toString), _.map(_.key))

  private def claimed(
      projects: Projects,
      store: Store,
      key: String = "p/a",
      role: String = "builder",
      forSlot: String = slot
  ): CommandOutcome =
    ask(projects, store, "project_claim", "key" -> key, "role" -> role, "for" -> forSlot)

  /** A project `p` holding two notes, `p/a` and `p/b`. */
  private def holding(projects: Projects, store: Store): Unit = {
    started(projects, store)
    kept(projects, store, "a", "note", "Text.")
    kept(projects, store, "b", "note", "Text.")
  }

  val tests: Tests = Tests {

    test("a second claim for one key is refused naming the first") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val second = claimed(projects, store, forSlot = "other")
      assert(
        second == CommandOutcome.Refused(
          s"p/a is claimed already, as builder for $slot by $by since $at; " +
            "project_release frees it"
        )
      )
    }

    test("a second artifact claimed for one slot is refused naming the first") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val second = claimed(projects, store, key = "p/b")
      val elsewhere = claimed(projects, store, key = "p/b", forSlot = "other")
      assert(
        second == CommandOutcome.Refused(
          s"$slot works p/a already, claimed since $at; release it with project_release first"
        ),
        elsewhere == CommandOutcome.Done("Claimed p/b (note, open) as builder for other.")
      )
    }

    test("a claim refuses a role the method does not declare") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val before = store.count
      val answer = claimed(projects, store, role = "reviewer")
      assert(
        answer == CommandOutcome.Refused(
          "method grit version 1 shows no role reviewer; its roles: builder"
        ),
        store.count == before
      )
    }

    test("a claim records who, for which slot, as which role") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val answer = claimed(projects, store)
      val claim = current(store, "claim/p/a")
      assert(
        answer == CommandOutcome.Done(s"Claimed p/a (note, open) as builder for $slot."),
        claim.map(_.data) == Some(
          ujson.Obj("claimed" -> ujson.Obj("by" -> by, "for" -> slot, "role" -> "builder"))
        ),
        referring(store, "claims", "p/a") == Vector(right(DocKey.of("claim/p/a"))),
        referring(store, "claimed-for", slot) == Vector(right(DocKey.of("claim/p/a")))
      )
    }

    test("release frees the key") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val released = ask(projects, store, "project_release", "key" -> "p/a")
      val again = claimed(projects, store, key = "p/b")
      val unclaimed = {
        done(ask(projects, store, "project_release", "key" -> "p/b"))
        ask(projects, store, "project_release", "key" -> "p/b")
      }
      assert(
        again == CommandOutcome.Done(s"Claimed p/b (note, open) as builder for $slot."),
        released.isInstanceOf[CommandOutcome.Done],
        unclaimed == CommandOutcome.Refused("p/b is not claimed")
      )
    }

    test(
      "release, given only the key, answers a JSON object naming the key and the slot it freed"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val released = done(ask(projects, store, "project_release", "key" -> "p/a"))
      (released, scala.util.Try(ujson.read(released)).toOption) ==> (
        (released, Some(ujson.Obj("released" -> "p/a", "for" -> slot)))
      )
    }

    test("a claim leaves the artifact's version unchanged") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val before = current(store, "p/a").map(_.version)
      done(claimed(projects, store))
      val after = current(store, "p/a").map(_.version)
      assert(before.isDefined, after == before, current(store, "claim/p/a").isDefined)
    }

    test("a slot is parts joined by `/`, at most 160 characters") {
      val long = Vector.fill(3)("a" * 60).mkString("/")
      assert(
        BriefSlot.of(slot).map(BriefSlot.value) == Right(slot),
        BriefSlot.of("").isLeft,
        BriefSlot.of("a//b").isLeft,
        BriefSlot.of("Coder/abc").isLeft,
        long.length == 182,
        BriefSlot.of(long) == Left(s"a slot holds at most 160 characters: $long")
      )
    }
  }
}
