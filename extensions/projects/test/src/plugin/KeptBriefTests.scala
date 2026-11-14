package grit.projects.plugin

import java.time.Instant

import grit.core.claim.{Claim, ClaimRole, Claimed, Claims}
import grit.core.document.{DocRef, DocText, Document, DocumentKeeper}
import grit.core.id.{ClaimHolder, DocKey}
import grit.core.identity.Principal
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.projects.method.{Method, MethodFixtures, RoleView}

import utest.*

/** A claimed artifact's brief after the commands that change its project: kept again, at its
  * claim's label, whenever the text `project_brief` renders for it differs from the one kept, and
  * by nothing else.
  */
object KeptBriefTests extends TestSuite {
  import CommandFixtures.*
  import MethodFixtures.right

  /** The holder every claim here is for, unless another is named. */
  private val holder: String = "coder-opus/abc"

  /** When a claim made at `at` that no session works lapses. */
  private val lapse = at.plusMillis(Claims.Lease.toMillis)

  private val briefKey: DocKey = right(DocKey.of("p/m#brief"))

  private def failed(e: StoreError): Nothing = throw new java.lang.AssertionError(e.toString)

  /** `project_keep` of `p/<local>` with `body`, titled `Title <local>`. */
  private def keep(
      projects: Projects,
      store: Store,
      local: String,
      body: String,
      more: (String, ujson.Value)*
  ): CommandOutcome =
    ask(
      projects,
      store,
      "project_keep",
      (Vector[(String, ujson.Value)](
        "key" -> s"p/$local",
        "kind" -> "note",
        "title" -> s"Title $local",
        "body" -> body
      ) ++ more)*
    )

  private def claim(
      projects: Projects,
      store: Store,
      key: String,
      forHolder: String = holder,
      when: Instant = at,
      role: String = "manager"
  ) =
    askAt(when)(
      projects,
      store,
      "project_claim",
      "key" -> key,
      "role" -> role,
      "holder" -> forHolder
    )

  /** What `project_brief` renders for `key` as `role`. */
  private def rendered(
      projects: Projects,
      store: Store,
      key: String = "p/m",
      role: String = "manager"
  ): String =
    done(ask(projects, store, "project_brief", "key" -> key, "role" -> role, "chars" -> 9500))

  /** [[managing]], with a reviewer shown notes to depth 1 and carrying nothing. */
  private val reviewing: Method = Method
    .of(
      managing.name,
      managing.version,
      managing.kinds,
      managing.transitions,
      managing.views :+ right(
        RoleView.of(MethodFixtures.role("reviewer"), Set(MethodFixtures.note.name), 1)
      ),
      managing.checks
    )
    .fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** The claims live in `store` at [[at]]. */
  private def liveNow(store: Store): Vector[Claim] =
    inTx(store.claims.live(at)).fold(failed, identity)

  /** `project_keep` of `p/m`, rewritten, a minute after [[at]], by a command whose read of the
    * live claims was `read`: as one reads them when another command commits between its read and
    * its hold on them.
    */
  private def keptAfter(projects: Projects, store: Store, read: Vector[Claim]): CommandOutcome = {
    val before: Claims = new Claims {
      def claim(
          subject: DocKey,
          holder: ClaimHolder,
          role: ClaimRole,
          by: Principal.Person,
          label: Label,
          at: Instant
      )(using Tx^): Either[StoreError, Claimed] =
        store.claims.claim(subject, holder, role, by, label, at)
      def release(subject: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Claim]] =
        store.claims.release(subject, label)
      def live(at: Instant)(using Tx^): Either[StoreError, Vector[Claim]] = Right(read)
      def held(claim: Claim)(using Tx^): Either[StoreError, Option[Claim]] =
        store.claims.held(claim)
    }
    askThrough(store.keeper, before, when = at.plusSeconds(60))(
      projects,
      store,
      "project_keep",
      "key" -> "p/m",
      "title" -> "Title m",
      "body" -> "The note, rewritten.",
      "cites" -> ujson.Arr("p/c")
    )
  }

  private def briefText(
      store: Store,
      forHolder: String = holder,
      when: Instant = at.plusSeconds(1)
  ): Option[String] =
    briefOf(store, forHolder, when).map(d => DocText.value(d.text))

  /** The brief of `p/m` current at public, read at maintenance's clearance. */
  private def current(store: Store): Option[Document] =
    inTx(store.keeper.current(briefKey, Label.Public)).fold(failed, identity)

  /** The versions of `p/m`'s brief kept at public, withdrawals included. */
  private def versions(store: Store): Int =
    inTx(store.keeper.versions(briefKey, Label.Public, 100)).fold(failed, _.size)

  /** Project `p` under [[managing]]: `p/m`, citing `p/c`; `p/c`; `p/b`, which nothing names;
    * and `p/process`, which a manager's brief carries, each kept at [[at]]; `p/m`, of `body`,
    * claimed for [[holder]] at `claimedAt`.
    */
  private def claimedProject(
      body: String = "The manager's note.",
      claimedAt: Instant = at,
      method: Method = managing
  ): (Projects, Store) = {
    val projects = plugin(method)
    val store = Store()
    started(projects, store)
    kept(projects, store, "c", "note", "Cited.")
    kept(projects, store, "b", "note", "Named by nothing.")
    kept(projects, store, "process", "note", "How work is run.")
    done(keep(projects, store, "m", body, "cites" -> ujson.Arr("p/c")))
    claim(projects, store, "p/m", when = claimedAt)
    (projects, store)
  }

  /** Keeps `text` as `p/m`'s brief for [[holder]], as a claim keeps one. */
  private def planted(store: Store, text: String): Unit =
    inTx(
      store.keeper.write(
        briefKey,
        Label.Public,
        Integrity.Authority,
        Place.Everywhere,
        right(DocText.of(text)),
        ujson.Obj("holder" -> holder),
        None,
        at,
        Set(DocRef.brief(right(ClaimHolder.of(holder))))
      )
    ).fold(failed, _ => ())

  val tests = Tests {
    test("a keep of a claimed artifact's body keeps its brief again, as project_brief renders it") {
      val (projects, store) = claimedProject()
      done(keep(projects, store, "m", "The note, rewritten.", "cites" -> ujson.Arr("p/c")))
      val now = rendered(projects, store)
      (briefText(store), now.contains("The note, rewritten.")) ==> (Some(now), true)
    }
    test("a keep that changes only an artifact the claimed brief cites keeps it again") {
      val (projects, store) = claimedProject()
      done(keep(projects, store, "c", "Cited, rewritten."))
      val now = rendered(projects, store)
      (briefText(store), now.contains("Cited, rewritten.")) ==> (Some(now), true)
    }
    test("a keep that changes only an artifact the claimed brief's role carries keeps it again") {
      val (projects, store) = claimedProject()
      done(keep(projects, store, "process", "How work is run, rewritten."))
      val now = rendered(projects, store)
      (briefText(store), now.contains("How work is run, rewritten.")) ==> (Some(now), true)
    }
    test("a move of a claimed artifact keeps its brief again, its header naming the new state") {
      val (projects, store) = claimedProject()
      done(ask(projects, store, "project_move", "key" -> "p/m", "state" -> "closed"))
      val now = rendered(projects, store)
      (briefText(store), now.linesIterator.nextOption().exists(_.contains("| closed |"))) ==>
        (Some(now), true)
    }
    test("a batch that keeps a claimed artifact twice keeps its brief once, with the last text") {
      val (projects, store) = claimedProject()
      def step(body: String) = ujson.Obj(
        "command" -> "keep",
        "key" -> "p/m",
        "title" -> "Title m",
        "body" -> body,
        "cites" -> ujson.Arr("p/c")
      )
      done(
        ask(
          projects,
          store,
          "project_batch",
          "project" -> "p",
          "steps" -> ujson.Arr(step("First."), step("Second."))
        )
      )
      val now = rendered(projects, store)
      (briefText(store), now.contains("Second."), versions(store)) ==> (Some(now), true, 2)
    }
    test("a keep that brings a brief over the budget within it keeps the brief") {
      val (projects, store) = claimedProject(("Text. " * 2000).trim)
      val before = briefText(store)
      done(keep(projects, store, "m", "Short now.", "cites" -> ujson.Arr("p/c")))
      (before, briefText(store)) ==> (None, Some(rendered(projects, store)))
    }
    test("a keep that leaves a brief over the budget withdraws the one kept for it") {
      val (projects, store) = claimedProject(("Text. " * 2000).trim)
      planted(store, "A brief kept before.")
      val before = current(store).map(d => DocText.value(d.text))
      done(keep(projects, store, "b", "Named by nothing, rewritten."))
      (before, current(store)) ==> (Some("A brief kept before."), None)
    }
    test("a keep that changes nothing a brief shows keeps again one whose text differs") {
      val (projects, store) = claimedProject()
      planted(store, "A brief rendered some other way.")
      done(keep(projects, store, "b", "Named by nothing, rewritten."))
      briefText(store) ==> Some(rendered(projects, store))
    }
    test("a keep that changes nothing a brief shows writes no version of it, believed or not") {
      val (projects, store) = claimedProject()
      val before = versions(store)
      val answer = askThrough(
        DocumentKeeper.believedAtMost(store.keeper, Integrity.Member),
        store.claims
      )(projects, store, "project_keep", "key" -> "p/b", "title" -> "Title b", "body" -> "New.")
      (answer, before, versions(store)) ==> (CommandOutcome.Done("Kept p/b (note, open)."), 1, 1)
    }
    test("one keep keeps again the brief of each live claim on the project it changes") {
      val (projects, store) = claimedProject()
      done(claim(projects, store, "p/c", "coder-opus/def"))
      done(keep(projects, store, "process", "How work is run, rewritten."))
      (briefText(store), briefText(store, "coder-opus/def")) ==>
        (Some(rendered(projects, store)), Some(rendered(projects, store, "p/c")))
    }
    test("a dry-run batch keeps no brief again") {
      val (projects, store) = claimedProject()
      val before = briefText(store)
      done(
        ask(
          projects,
          store,
          "project_batch",
          "project" -> "p",
          "steps" -> ujson.Arr(
            ujson.Obj("command" -> "keep", "key" -> "p/m", "title" -> "T", "body" -> "Dry.")
          ),
          "dry" -> true
        )
      )
      (before.isDefined, briefText(store), versions(store)) ==> (true, before, 1)
    }
    test("a keep of an artifact no live claim holds keeps no brief") {
      val projects = plugin(managing)
      val unclaimed = Store()
      started(projects, unclaimed)
      done(keep(projects, unclaimed, "m", "Never claimed."))
      done(keep(projects, unclaimed, "m", "Never claimed, rewritten."))
      val (_, lapsed) = claimedProject()
      val before = current(lapsed)
      done(
        askAt(lapse)(
          projects,
          lapsed,
          "project_keep",
          "key" -> "p/m",
          "title" -> "Title m",
          "body" -> "Kept after the claim lapsed."
        )
      )
      (versions(unclaimed), before.isDefined, current(lapsed)) ==> (0, true, before)
    }
    test(
      "a keep from a room above a claim's label leaves its brief as it was, at its label alone"
    ) {
      val (projects, store) = claimedProject()
      // At the claim's own instant, where a claim made anew is not told from the one renewed.
      val answer = askBy(CommandBy.Direct(person), at, Clearance.of(TestLabels.Trial))(
        projects,
        store,
        "project_keep",
        "key" -> "p/x",
        "kind" -> "note",
        "title" -> "Title x",
        "body" -> "Kept above public, citing p/m.",
        "cites" -> ujson.Arr("p/m")
      )
      (
        answer,
        store.keeper
          .labels(briefKey)(using grit.dbos.sql.TestTx.fake(Clearance.of(TestLabels.Trial)))
          .fold(failed, identity),
        versions(store)
      ) ==> (CommandOutcome.Done("Kept p/x (note, open)."), Vector(Label.Public), 1)
    }
    test("a claim released after a keep read it is not made again, and keeps no brief") {
      val (projects, store) = claimedProject()
      val read = liveNow(store)
      done(ask(projects, store, "project_release", "key" -> "p/m"))
      val answer = keptAfter(projects, store, read)
      (answer, liveNow(store), current(store)) ==>
        (CommandOutcome.Done("Kept p/m (note, open)."), Vector(), None)
    }

    test("a claim released and made again after a keep read it is left as it was made") {
      val (projects, store) = claimedProject()
      val read = liveNow(store)
      done(ask(projects, store, "project_release", "key" -> "p/m"))
      done(claim(projects, store, "p/m", when = at.plusSeconds(30)))
      val again = liveNow(store)
      val answer = keptAfter(projects, store, read)
      (answer, liveNow(store).map(_.made), again.map(_.made)) ==>
        (
          CommandOutcome.Done("Kept p/m (note, open)."),
          Vector(at.plusSeconds(30)),
          Vector(
            at.plusSeconds(30)
          )
        )
    }

    test("a claim made again as another role after a keep read it keeps that role, briefed so") {
      val (projects, store) = claimedProject(method = reviewing)
      val read = liveNow(store)
      done(claim(projects, store, "p/m", role = "reviewer"))
      done(keptAfter(projects, store, read))
      val now = rendered(projects, store, role = "reviewer")
      val brief = briefText(store, when = at.plusSeconds(61))
      (liveNow(store).map(c => ClaimRole.value(c.role)), brief, now.contains("rewritten")) ==>
        (Vector("reviewer"), Some(now), true)
    }
    test("a withdraw of an artifact a claimed brief's role carries keeps the brief again") {
      val (projects, store) = claimedProject()
      done(ask(projects, store, "project_withdraw", "key" -> "p/process"))
      val now = rendered(projects, store)
      (briefText(store), now.contains("How work is run.")) ==> (Some(now), false)
    }
    test("a sweep that withdraws an artifact a claimed brief's role carries keeps it again") {
      // Claimed late on the day everything was kept, and swept within the claim's lease.
      val (projects, store) = claimedProject(claimedAt = Instant.parse("2026-10-27T23:00:00Z"))
      done(ask(projects, store, "project_move", "key" -> "p/process", "state" -> "closed"))
      val sweep = Vector[(String, ujson.Value)](
        "project" -> "p",
        "kinds" -> ujson.Arr("note"),
        "before" -> "2026-10-28"
      )
      val sweptAt = Instant.parse("2026-10-28T00:30:00Z")
      val read = sweptAt.plusSeconds(1)
      done(askAt(sweptAt)(projects, store, "project_sweep", (sweep :+ ("dry" -> ujson.True))*))
      val dry = briefText(store, when = read)
      done(askAt(sweptAt)(projects, store, "project_sweep", sweep*))
      val now = rendered(projects, store)
      (
        dry.exists(_.contains("How work is run.")),
        briefText(store, when = read),
        now.contains("How work")
      ) ==>
        (true, Some(now), false)
    }
  }
}
