package grit.projects.plugin

import grit.core.claim.{Claim, ClaimRole, Claims}
import grit.core.document.Document
import grit.core.id.{ClaimHolder, DocKey}
import grit.core.plugin.CommandOutcome
import grit.core.visibility.Label
import grit.projects.method.MethodFixtures
import grit.projects.paper.{ElephantGoldfishTests, OpenSpecTests}

import utest.*

/** `project_claim`, `project_release` and `project_claims`: a claim is one of the plugin's
  * claims ([[grit.core.claim.Claims]]), never a document, made for one slot (what a brief is
  * written for) as one role at its artifact's label; an artifact has one live claim, and a slot
  * works one artifact, at each label, until it lapses (`ClaimsContract` holds the store to it,
  * leases included).
  */
object ClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.right

  /** The slot every claim here is for, unless another is named. */
  /** When a claim made at `at` that no session works lapses. */
  private val lapse = at.plusMillis(Claims.Lease.toMillis)

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
            s"it lapses at $lapse unless they keep working; project_release frees it"
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
          s"$slot works p/a already, claimed since $at; it lapses at $lapse unless they keep " +
            "working; release it with project_release first"
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
      "under grit's, OpenSpec's and elephant-goldfish's methods a second claim is refused, and the freed key is claimed again"
    ) {
      val under = Vector(
        (method(), "note", "open", "builder"),
        (OpenSpecTests.openSpec, "task", "open", "author"),
        (ElephantGoldfishTests.model, "source", "current", "elephant")
      )
      under.map { (m, kind, first, role) =>
        val projects = Projects
          .of(Vector(m -> Vector.empty), m.name)
          .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        val store = Store()
        started(projects, store)
        kept(projects, store, "a", kind, "Text.")
        (
          claimed(projects, store, role = role),
          claimed(projects, store, role = role, forSlot = "other"),
          ujson.read(done(ask(projects, store, "project_release", "key" -> "p/a"))),
          claimed(projects, store, role = role, forSlot = "other")
        ) ==> (
          (
            CommandOutcome.Done(s"Claimed p/a ($kind, $first) as $role for $slot."),
            CommandOutcome.Refused(
              s"p/a is claimed already, as $role for $slot by $by since $at; " +
                s"it lapses at $lapse unless they keep working; project_release frees it"
            ),
            ujson.Obj("released" -> "p/a", "for" -> slot),
            CommandOutcome.Done(s"Claimed p/a ($kind, $first) as $role for other.")
          )
        )
      }
    }

    test("a claim is made for any holder a session can name, as its edge spells it") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val answer = claimed(projects, store, forSlot = "Coder Opus/3F2A")
      val claim = inTx(store.claims.release(right(DocKey.of("p/a")), Label.Public))
      assert(
        answer == CommandOutcome.Done("Claimed p/a (note, open) as builder for Coder Opus/3F2A."),
        claim.map(_.map(c => ClaimHolder.value(c.holder))) == Right(Some("Coder Opus/3F2A"))
      )
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

    test("a claim no one works lapses, and another slot takes it") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val taken = askAt(lapse)(
        projects,
        store,
        "project_claim",
        "key" -> "p/a",
        "role" -> "builder",
        "for" -> "other"
      )
      assert(taken == CommandOutcome.Done("Claimed p/a (note, open) as builder for other."))
    }

    test("a claim made again by its slot renews it, and it is listed once") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val again = claimed(projects, store)
      val listed = ujson.read(done(ask(projects, store, "project_claims")))
      assert(
        again == CommandOutcome.Done(s"Claimed p/a (note, open) as builder for $slot."),
        listed("claims").arr.size == 1
      )
    }

    test("project_claims answers each live claim's key, role, slot, who and when, and lapse") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store, key = "p/b"))
      done(claimed(projects, store, forSlot = "other"))
      val listed = ujson.read(done(ask(projects, store, "project_claims")))
      val lapsed = ujson.read(done(askAt(lapse)(projects, store, "project_claims")))
      def one(key: String, slot: String) = ujson.Obj(
        "key" -> key,
        "role" -> "builder",
        "for" -> slot,
        "by" -> by,
        "since" -> at.toString,
        "lapses" -> lapse.toString
      )
      assert(
        listed == ujson.Obj("claims" -> ujson.Arr(one("p/a", "other"), one("p/b", slot))),
        lapsed == ujson.Obj("claims" -> ujson.Arr())
      )
    }
  }
}
