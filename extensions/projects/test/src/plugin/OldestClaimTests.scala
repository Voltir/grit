package grit.projects.plugin

import java.time.Instant

import grit.core.plugin.CommandOutcome
import grit.projects.method.{Cover, MethodFixtures, RoleView}

import utest.*

/** `project_claim` given `oldest`: of a kind's artifacts in a state that no live claim holds or
  * covers, the one that entered that state first, ties by number, claimed as a key would be.
  */
object OldestClaimTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{change, note, right, role}

  private def second(n: Int): Instant = at.plusSeconds(n.toLong)

  /** A keep of `p/<local>`, of `kind`, at `when`, `more` in place of the fields it names. */
  private def keptAt(
      when: Instant,
      projects: Projects,
      store: Store,
      local: String,
      kind: String,
      more: (String, ujson.Value)*
  ): Unit =
    done(
      askAt(when)(
        projects,
        store,
        "project_keep",
        (naming(projects, store, local) ++
          keepFields(local, kind, "## Why\n\nBecause.")
            .filterNot((k, _) => more.exists(_._1 == k)) ++
          more)*
      )
    )

  private def movedAt(when: Instant, projects: Projects, store: Store, local: String, to: String) =
    done(askAt(when)(projects, store, "project_move", "key" -> s"p/$local", "state" -> to))

  /** A claim of the oldest `kind` in `state` of `p`, as `as` for `holder`, at `when`. */
  private def oldest(
      projects: Projects,
      store: Store,
      kind: String,
      state: String,
      holder: String = "h1",
      as: String = "builder",
      when: Instant = second(100)
  ): CommandOutcome =
    askAt(when)(
      projects,
      store,
      "project_claim",
      "oldest" -> ujson.Obj("project" -> "p", "kind" -> kind, "state" -> state),
      "role" -> as,
      "holder" -> holder
    )

  private def claimedKept(said: String): CommandOutcome =
    CommandOutcome.Done(ujson.write(ujson.Obj("claimed" -> said, "brief" -> "kept")))

  /** A claim of `p/<local>` as `as` for `holder`, at `when`. */
  private def claimedAt(
      when: Instant,
      projects: Projects,
      store: Store,
      local: String,
      holder: String,
      as: String = "builder"
  ): String =
    done(
      askAt(when)(
        projects,
        store,
        "project_claim",
        "key" -> s"p/$local",
        "role" -> as,
        "holder" -> holder
      )
    )

  val tests = Tests {
    test("oldest claims the one that entered the state first, not the first made or last kept") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(second(1), projects, store, "c", "change")
      keptAt(second(2), projects, store, "d", "change")
      movedAt(second(3), projects, store, "d", "building")
      movedAt(second(4), projects, store, "c", "building")
      keptAt(second(5), projects, store, "d", "change", "body" -> "## Why\n\nKept again.")
      oldest(projects, store, "change", "building") ==>
        claimedKept("Claimed p/2 d (change, building) as builder for h1.")
    }

    test("of two that entered the state at one instant, oldest claims the lower number") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(second(1), projects, store, "b", "note")
      keptAt(second(1), projects, store, "a", "note")
      oldest(projects, store, "note", "open") ==>
        claimedKept("Claimed p/1 b (note, open) as builder for h1.")
    }

    test("oldest skips an artifact another holder's claim holds") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(second(1), projects, store, "a", "note")
      keptAt(second(2), projects, store, "b", "note")
      claimedAt(second(3), projects, store, "a", "h2")
      oldest(projects, store, "note", "open") ==>
        claimedKept("Claimed p/2 b (note, open) as builder for h1.")
    }

    test("oldest skips an artifact under another holder's covering claim") {
      val projects = plugin()
      val store = Store(declared =
        Vector(
          method(roles =
            Vector(
              right(RoleView.of(role("manager"), Set(note.name), 1, covers = Cover.Under)),
              right(RoleView.of(role("builder"), Set(note.name, change.name), 1))
            )
          )
        )
      )
      started(projects, store)
      keptAt(second(1), projects, store, "a", "change")
      keptAt(second(2), projects, store, "b", "note", "parent" -> "p/a")
      keptAt(second(3), projects, store, "c", "note")
      claimedAt(second(4), projects, store, "a", "h2", as = "manager")
      oldest(projects, store, "note", "open") ==>
        claimedKept("Claimed p/3 c (note, open) as builder for h1.")
    }

    test(
      "a state is entered when the version moving it there was written, or else the earliest kept in it"
    ) {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(second(1), projects, store, "x", "change")
      movedAt(second(2), projects, store, "x", "building")
      keptAt(second(3), projects, store, "x", "change", "body" -> "## Why\n\nKept again.")
      keptAt(second(4), projects, store, "x", "change", "body" -> "## Why\n\nAnd again.")
      val key = MethodFixtures.right(grit.core.id.DocKey.of("p/1"))
      val kept = inTx(store.keeper.versions(key, grit.core.visibility.Label.Public, 10))
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      val building = MethodFixtures.state("building")
      def enteredOf(versions: Vector[grit.core.document.DocumentVersion]) =
        ProjectCovers.entered(versions, ProjectCommands.readable, building)
      // The last as if it had left the state and come back: entered at its return.
      val returned = Vector(kept(0), kept(3), kept(2))
      (kept.size, enteredOf(kept), enteredOf(kept.take(2)), enteredOf(kept.take(1))) ==>
        (4, Some(second(2)), Some(second(3)), Some(second(4)))
      enteredOf(returned) ==> Some(second(4))
      enteredOf(kept.drop(3)) ==> None
    }

    test("oldest with none left unclaimed says there is nothing to take") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      keptAt(second(1), projects, store, "a", "note")
      claimedAt(second(2), projects, store, "a", "h2")
      keptAt(second(3), projects, store, "b", "change")
      oldest(projects, store, "note", "open") ==>
        CommandOutcome.Refused("nothing to take: no note in open of p is unclaimed")
    }

    test("oldest refuses a kind the method lacks, and a state its kind lacks, naming theirs") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      (oldest(projects, store, "task", "open"), oldest(projects, store, "note", "building")) ==> (
        CommandOutcome.Refused(
          s"task is no kind of method ${grit.projects.name.MethodName.value(methodName)}; " +
            "its kinds: change, note"
        ),
        CommandOutcome.Refused("note has no state building; its states: open, closed")
      )
    }
  }
}
