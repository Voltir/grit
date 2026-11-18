package grit.projects.plugin

import grit.core.plugin.{CommandBy, CommandOutcome}
import grit.core.visibility.{Integrity, TestLabels}
import grit.projects.name.MethodName

import utest.*

/** How far what a projects command keeps is believed, run as an edge runs it: at no more than
  * what it carries forward. A move, an edit and a rewrite of a citer keep another writer's text,
  * so are believed at the meet of their caller's and the version before; a keep's whole body is
  * its caller's alone.
  */
object CarriedIntegrityTests extends TestSuite {
  import CommandFixtures.*

  private val (session, personally) = (Integrity.Member, Integrity.Authority)

  /** A steward of [[TestLabels.trial]]: `Authority` in it alone, as `Tx.integrityOf` gives. */
  private val steward = Integrity.at(Integrity.Level.Authority, TestLabels.trial)

  /** Command `name` asked by [[steward]] in a deployment declaring [[TestLabels.trial]]. */
  private def asSteward(projects: Projects, store: Store)(
      name: String,
      args: (String, ujson.Value)*
  ): CommandOutcome =
    askThrough(
      store.keeper,
      store.claims,
      CommandBy.Direct(person, steward),
      visibility = TestLabels.Trialled
    )(projects, store, name, args*)

  /** Project `p` with change `p/<local>` kept by a caller believed at `by`. */
  private def changeKeptBy(by: Integrity, local: String = "a"): (Projects, Store) = {
    val projects = plugin()
    val store = Store()
    started(projects, store)
    done(
      askBelieved(by)(
        projects,
        store,
        "project_keep",
        "key" -> s"p/$local",
        "kind" -> "change",
        "title" -> "A",
        "body" -> "## Why\n\nBecause."
      )
    )
    (projects, store)
  }

  /** How far `p/a` is believed once a caller believed at `mover` moves it, kept by one at `keeper`. */
  private def moved(keeper: Integrity, mover: Integrity): Option[String] = {
    val (projects, store) = changeKeptBy(keeper)
    done(askBelieved(mover)(projects, store, "project_move", "key" -> "p/a", "state" -> "building"))
    believedOf(store, "p/a")
  }

  /** How far `p/a` is believed once a caller believed at `editor` edits it, kept by one at `keeper`. */
  private def edited(keeper: Integrity, editor: Integrity): Option[String] = {
    val (projects, store) = changeKeptBy(keeper)
    done(
      askBelieved(editor)(
        projects,
        store,
        "project_edit",
        "key" -> "p/a",
        "version" -> versionOf(projects, store, "p/a"),
        "old" -> "Because.",
        "new" -> "Because it is so."
      )
    )
    believedOf(store, "p/a")
  }

  val tests = Tests {
    // In the move and the edit, the version before decides only the first pair: the second's
    // caller is the lower, and the capped keeper meets every write with its caller's.
    test("a move is believed at the meet of its caller's and the version it moves") {
      (moved(session, personally), moved(personally, session)) ==> (Some("member"), Some("member"))
    }
    test("an edit is believed at the meet of its caller's and the version it changes") {
      (edited(session, personally), edited(personally, session)) ==>
        (Some("member"), Some("member"))
    }
    test("a keep's whole body is believed at its caller's, whoever kept the version before") {
      val (projects, store) = changeKeptBy(session)
      done(
        askBelieved(personally)(
          projects,
          store,
          "project_keep",
          "key" -> "p/a",
          "title" -> "A",
          "body" -> "## Why\n\nA person's reason."
        )
      )
      believedOf(store, "p/a") ==> Some("authority")
    }

    test("a steward's whole keep is believed in the compartments they steward") {
      val projects = plugin()
      val store = Store()
      started(projects, store)
      done(
        asSteward(projects, store)(
          "project_keep",
          "key" -> "p/a",
          "kind" -> "note",
          "title" -> "A",
          "body" -> "A steward's words."
        )
      )
      believedOf(store, "p/a") ==> Some(Integrity.written(steward))
    }

    test("a project a steward starts is believed in the compartments they steward") {
      val projects = plugin()
      val store = Store()
      done(asSteward(projects, store)("project_new", "project" -> "p"))
      believedOf(store, "p") ==> Some(Integrity.written(steward))
    }

    test("a method a steward declares is believed in the compartments they steward") {
      val store = Store(declared = Vector.empty)
      val m = method()
      done(asSteward(plugin(), store)("project_declare", declaring(m)))
      believedOf(store, s"${MethodName.value(m.name)}@${m.version}") ==>
        Some(Integrity.written(steward))
    }
    test("a citer a cascade rewrites keeps how far it was believed") {
      val (projects, store) = changeKeptBy(personally, "x")
      done(
        askBelieved(session)(
          projects,
          store,
          "project_keep",
          "key" -> "p/e",
          "kind" -> "note",
          "title" -> "E",
          "body" -> "Cites x.",
          "cites" -> ujson.Arr("p/x")
        )
      )
      done(
        askBelieved(session)(projects, store, "project_move", "key" -> "p/e", "state" -> "closed")
      )
      done(
        askBelieved(personally)(
          projects,
          store,
          "project_withdraw",
          "key" -> "p/x",
          "cascade" -> true
        )
      )
      (believedOf(store, "p/x"), believedOf(store, "p/e")) ==> (None, Some("member"))
    }
  }
}
