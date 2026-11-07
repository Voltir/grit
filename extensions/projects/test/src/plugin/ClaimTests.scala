package grit.projects.plugin

import grit.core.claim.{Claim, ClaimRole, Claims}
import grit.core.document.{DocText, Document}
import grit.core.id.{ClaimHolder, DocKey}
import grit.core.plugin.CommandOutcome
import grit.core.tool.ToolName
import grit.core.visibility.Label
import grit.projects.method.MethodFixtures
import grit.projects.paper.{ElephantGoldfishTests, OpenSpecTests}

import utest.*

/** `project_claim`, `project_release` and `project_claims`: a claim is one of the plugin's
  * claims ([[grit.core.claim.Claims]]), never a document, made for one holder (what a brief is
  * written for) as one role at its artifact's label; an artifact has one live claim, and a holder
  * works one artifact, at each label, until it lapses (`ClaimsContract` holds the store to it,
  * leases included).
  */
object ClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.right

  /** The holder every claim here is for, unless another is named. */
  /** When a claim made at `at` that no session works lapses. */
  private val lapse = at.plusMillis(Claims.Lease.toMillis)

  private val holder: String = "coder-opus/abc"

  private def current(store: Store, key: String): Option[Document] =
    inTx(store.keeper.current(right(DocKey.of(key)), Label.Public))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private def claimed(
      projects: Projects,
      store: Store,
      key: String = "p/a",
      role: String = "builder",
      forHolder: String = holder
  ): CommandOutcome =
    ask(projects, store, "project_claim", "key" -> key, "role" -> role, "holder" -> forHolder)

  /** A claim's answer saying `said`, its brief kept. */
  private def claimedKept(said: String): CommandOutcome =
    CommandOutcome.Done(ujson.write(ujson.Obj("claimed" -> said, "brief" -> "kept")))

  /** The brief kept for `holder`, as a window would find it just after the claims here. */
  private def briefOf(store: Store, holder: String): Option[Document] = {
    inTx(store.documents.declare(Vector(Projects.Name -> Projects.Terms)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
    inTx(store.documents.briefOf(right(ClaimHolder.of(holder)), at.plusSeconds(1)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }

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
      val second = claimed(projects, store, forHolder = "other")
      assert(
        second == CommandOutcome.Refused(
          s"p/a is claimed already, as builder for $holder by $by since $at; " +
            s"it lapses at $lapse unless they keep working; project_release frees it"
        )
      )
    }

    test("a second artifact claimed for one holder is refused naming the first") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val second = claimed(projects, store, key = "p/b")
      val elsewhere = claimed(projects, store, key = "p/b", forHolder = "other")
      assert(
        second == CommandOutcome.Refused(
          s"$holder works p/a already, claimed since $at; it lapses at $lapse unless they keep " +
            "working; release it with project_release first"
        ),
        elsewhere == claimedKept("Claimed p/b (note, open) as builder for other.")
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

    test("a claim records who, for which holder, as which role") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val answer = claimed(projects, store)
      val claim = inTx(store.claims.release(right(DocKey.of("p/a")), Label.Public))
      assert(
        answer == claimedKept(s"Claimed p/a (note, open) as builder for $holder."),
        claim == Right(
          Some(
            Claim(
              right(DocKey.of("p/a")),
              right(ClaimHolder.of(holder)),
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
      val again = claimed(projects, store, forHolder = "coder-opus/def")
      assert(
        again == claimedKept("Claimed p/a (note, open) as builder for coder-opus/def.")
      )
      done(ask(projects, store, "project_release", "key" -> "p/a"))
      val unclaimed = ask(projects, store, "project_release", "key" -> "p/a")
      assert(unclaimed == CommandOutcome.Refused("p/a is not claimed"))
    }

    test(
      "release, given only the key, answers a JSON object naming the key and the holder it freed"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val released = done(ask(projects, store, "project_release", "key" -> "p/a"))
      ujson.read(released) ==> ujson.Obj("released" -> "p/a", "holder" -> holder)
    }

    test("a claim keeps its artifact's brief for its holder, as project_brief renders it") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val rendered = done(
        ask(projects, store, "project_brief", "key" -> "p/a", "role" -> "builder", "chars" -> 9500)
      )
      briefOf(store, holder).map(_.text).map(DocText.value) ==> Some(rendered)
    }
    test("a release withdraws the brief, claimed still or not") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      done(ask(projects, store, "project_release", "key" -> "p/a"))
      val released = briefOf(store, holder)
      done(claimed(projects, store))
      // A claim gone without a release, as the session sweep deletes a lapsed one.
      inTx(store.claims.release(right(DocKey.of("p/a")), Label.Public))
      val freed = done(ask(projects, store, "project_release", "key" -> "p/a"))
      (
        released,
        ujson.read(freed),
        briefOf(store, holder),
        ask(projects, store, "project_release", "key" -> "p/a")
      ) ==> (
        None,
        ujson.Obj("released" -> "p/a", "holder" -> holder),
        None,
        CommandOutcome.Refused("p/a is not claimed")
      )
    }
    test(
      "a keep that would put a claimed artifact's brief past 9,500 characters is refused naming the characters to cut, keeping nothing"
    ) {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val answer = ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/a",
        "title" -> "Title a",
        "body" -> ("Text. " * 2000).trim
      )
      // 11,999 characters of body, and the brief's header line and blank lines (BriefTests).
      (answer, shown(projects, store, "p/a").endsWith("\nText.\n")) ==> (
        CommandOutcome.Refused(
          s"the brief of p/a, claimed as builder for $holder, would be 12,057 characters, " +
            "2,557 over the 9,500 a brief holds (the artifact with what its role always carries): " +
            "cut at least 2,557 characters from p/a"
        ),
        true
      )
    }

    test(
      "a keep of an artifact a claimed brief carries is refused when that brief would pass the budget"
    ) {
      val view = MethodFixtures.right(
        grit.projects.method.RoleView.of(
          MethodFixtures.role("manager"),
          Set(MethodFixtures.note.name),
          1,
          Set(MethodFixtures.right(grit.projects.name.LocalKey.of("process")))
        )
      )
      val carrying = grit.projects.method.Method
        .of(
          methodName,
          1,
          Vector(MethodFixtures.change, MethodFixtures.note),
          MethodFixtures.transitions,
          Vector(view),
          grit.projects.method.TransitionChecks.all
        )
        .fold(why => throw new java.lang.AssertionError(why.toString), identity)
      val projects = plugin(carrying)
      val store = Store()
      started(projects, store)
      kept(projects, store, "m", "note", "The manager's note.")
      kept(projects, store, "process", "note", "How work is run.")
      done(claimed(projects, store, "p/m", "manager"))
      val answer = ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/process",
        "title" -> "Title process",
        "body" -> ("Rule. " * 2000).trim
      )
      assert(answer match {
        case CommandOutcome.Refused(why) =>
          why.startsWith(s"the brief of p/m, claimed as manager for $holder, would be ") &&
          why.endsWith(" characters from p/process")
        case CommandOutcome.Done(_) => false
      })
    }

    test("a move that would put a claimed artifact's brief past 9,500 characters is refused") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      // 9,441 characters of body come to a 9,499-character brief while p/a is open; closed is
      // two characters longer than open, in the brief's header line.
      kept(projects, store, "a", "note", "x" * 9441)
      done(claimed(projects, store))
      ask(projects, store, "project_move", "key" -> "p/a", "state" -> "closed") ==>
        CommandOutcome.Refused(
          s"the brief of p/a, claimed as builder for $holder, would be 9,501 characters, " +
            "1 over the 9,500 a brief holds (the artifact with what its role always carries): " +
            "cut at least 1 character from p/a"
        )
    }

    test("a keep that shortens a brief already over the budget is kept") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      kept(projects, store, "a", "note", ("Text. " * 2000).trim)
      done(claimed(projects, store))
      val answer = ask(
        projects,
        store,
        "project_keep",
        "key" -> "p/a",
        "title" -> "Title a",
        "body" -> ("Text. " * 1900).trim
      )
      answer ==> CommandOutcome.Done("Kept p/a (note, open).")
    }

    test("a brief over its cap is kept as none, and the claim stands") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      kept(projects, store, "a", "note", "Text. " * 2000)
      val answer = claimed(projects, store)
      (answer, briefOf(store, holder)) ==> (
        CommandOutcome.Done(
          ujson.write(
            ujson.Obj(
              "claimed" -> s"Claimed p/a (note, open) as builder for $holder.",
              "brief" -> "none",
              "note" -> ("No brief was kept: it is over 9,500 characters; shorten the artifact " +
                "or what its role carries.")
            )
          )
        ),
        None
      )
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
          claimed(projects, store, role = role, forHolder = "other"),
          ujson.read(done(ask(projects, store, "project_release", "key" -> "p/a"))),
          claimed(projects, store, role = role, forHolder = "other")
        ) ==> (
          (
            claimedKept(s"Claimed p/a ($kind, $first) as $role for $holder."),
            CommandOutcome.Refused(
              s"p/a is claimed already, as $role for $holder by $by since $at; " +
                s"it lapses at $lapse unless they keep working; project_release frees it"
            ),
            ujson.Obj("released" -> "p/a", "holder" -> holder),
            claimedKept(s"Claimed p/a ($kind, $first) as $role for other.")
          )
        )
      }
    }

    test("project_claim takes a holder, and the claim tools name it so, never a slot") {
      val names = Vector("project_claim", "project_release", "project_claims")
      val schemas = plugin().commands
        .filter(c => names.contains(ToolName.value(c.spec.name)))
        .map(_.spec.schema(strict = false))
      val claimed = schemas
        .find(_.name == "project_claim")
        .map(_.parameters("properties").obj.keySet.toSet)
      assert(
        schemas.size == 3,
        claimed == Some(Set("key", "role", "holder")),
        schemas.forall(s => !s.description.contains("slot") && !s.description.contains("`for`"))
      )
    }

    test("a claim is made for any holder a session can name, as its edge spells it") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      val answer = claimed(projects, store, forHolder = "Coder Opus/3F2A")
      val claim = inTx(store.claims.release(right(DocKey.of("p/a")), Label.Public))
      assert(
        answer == claimedKept("Claimed p/a (note, open) as builder for Coder Opus/3F2A."),
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

    test("a claim no one works lapses, and another holder takes it") {
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
        "holder" -> "other"
      )
      assert(taken == claimedKept("Claimed p/a (note, open) as builder for other."))
    }

    test("a claim made again by its holder renews it, and it is listed once") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store))
      val again = claimed(projects, store)
      val listed = ujson.read(done(ask(projects, store, "project_claims")))
      assert(
        again == claimedKept(s"Claimed p/a (note, open) as builder for $holder."),
        listed("claims").arr.size == 1
      )
    }

    test("project_claims answers each live claim's key, role, holder, who and when, and lapse") {
      val projects = plugin()
      val store = Store()
      holding(projects, store)
      done(claimed(projects, store, key = "p/b"))
      done(claimed(projects, store, forHolder = "other"))
      val listed = ujson.read(done(ask(projects, store, "project_claims")))
      val lapsed = ujson.read(done(askAt(lapse)(projects, store, "project_claims")))
      def one(key: String, holder: String) = ujson.Obj(
        "key" -> key,
        "role" -> "builder",
        "holder" -> holder,
        "by" -> by,
        "since" -> at.toString,
        "lapses" -> lapse.toString
      )
      assert(
        listed == ujson.Obj("claims" -> ujson.Arr(one("p/a", "other"), one("p/b", holder))),
        lapsed == ujson.Obj("claims" -> ujson.Arr())
      )
    }
  }
}
