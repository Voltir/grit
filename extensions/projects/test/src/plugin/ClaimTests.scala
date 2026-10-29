package grit.projects.plugin

import grit.core.claim.{Claim, ClaimRole}
import grit.core.document.Document
import grit.core.id.{ClaimHolder, DocKey}
import grit.core.plugin.CommandOutcome
import grit.core.visibility.Label
import grit.projects.method.MethodFixtures
import grit.projects.paper.{ElephantGoldfishTests, OpenSpecTests}

import utest.*

/** `project_claim` and `project_release`: a claim is one of the plugin's claims
  * ([[grit.core.claim.Claims]]), never a document, made for one slot (what a brief is written
  * for) as one role at its artifact's label; an artifact has one live claim, and a slot works
  * one artifact, at each label (`ClaimsContract` holds the store to it, races included).
  */
object ClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.right

  /** The slot every claim here is for, unless another is named. */
  private val slot: String = "coder-opus/abc"

  private def current(store: Store, key: String): Option[Document] =
    inTx(store.keeper.current(right(DocKey.of(key)), Label.Public))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

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
      val claim = inTx(store.claims.release(right(DocKey.of("p/a")), Label.Public))
      assert(
        answer == CommandOutcome.Done(s"Claimed p/a (note, open) as builder for $slot."),
        claim == Right(
          Some(
            Claim(
              right(DocKey.of("p/a")),
              right(ClaimHolder.of(slot)),
              right(ClaimRole.of("builder")),
              person,
              at,
              Label.Public,
              at
            )
          )
        )
      )
    }

    test("release frees the key") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      done(ask(projects, store, "project_release", "key" -> "p/a"))
      val again = claimed(projects, store, forSlot = "coder-opus/def")
      assert(
        again == CommandOutcome.Done("Claimed p/a (note, open) as builder for coder-opus/def.")
      )
      done(ask(projects, store, "project_release", "key" -> "p/a"))
      val unclaimed = ask(projects, store, "project_release", "key" -> "p/a")
      assert(unclaimed == CommandOutcome.Refused("p/a is not claimed"))
    }

    test(
      "release, given only the key, answers a JSON object naming the key and the slot it freed"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val released = done(ask(projects, store, "project_release", "key" -> "p/a"))
      ujson.read(released) ==> ujson.Obj("released" -> "p/a", "for" -> slot)
    }

    test("claim, release and claim again keep no document version and mark none") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val before = (store.count, store.documents.tombstones.pending)
      done(claimed(projects, store))
      done(ask(projects, store, "project_release", "key" -> "p/a"))
      done(claimed(projects, store))
      assert((store.count, store.documents.tombstones.pending) == before)
    }

    test(
      "under grit's, OpenSpec's and elephant-goldfish's methods a claim is refused twice, freed and made again"
    ) {
      val under = Vector(
        (method(), "note", "builder"),
        (OpenSpecTests.openSpec, "task", "author"),
        (ElephantGoldfishTests.model, "source", "elephant")
      )
      val answers = under.map { (m, kind, role) =>
        val projects = Projects
          .of(Vector(m -> Vector.empty), m.name)
          .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        val store = Store()
        started(projects, store)
        kept(projects, store, "a", kind, "Text.")
        val first = claimed(projects, store, role = role)
        val second = claimed(projects, store, role = role, forSlot = "other")
        val freed = ask(projects, store, "project_release", "key" -> "p/a")
        val again = claimed(projects, store, role = role, forSlot = "other")
        (
          first.isInstanceOf[CommandOutcome.Done],
          second == CommandOutcome.Refused(
            s"p/a is claimed already, as $role for $slot by $by since $at; project_release frees it"
          ),
          freed.isInstanceOf[CommandOutcome.Done] &&
            ujson.read(done(freed)) == ujson.Obj("released" -> "p/a", "for" -> slot),
          again.isInstanceOf[CommandOutcome.Done]
        )
      }
      assert(answers == Vector.fill(3)((true, true, true, true)))
    }

    test("a claim leaves the artifact's version unchanged") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val before = current(store, "p/a").map(_.version)
      done(claimed(projects, store))
      val after = current(store, "p/a").map(_.version)
      assert(before.isDefined, after == before)
    }
  }
}
