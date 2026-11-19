package grit.projects.plugin

import grit.core.claim.{ClaimRole, Claims}
import grit.core.id.{ClaimHolder, DocKey}
import grit.core.plugin.CommandOutcome
import grit.core.visibility.Label
import grit.projects.method.{MethodFixtures, RoleView}

import utest.*

/** A claim as a role whose view covers holds what sits under its artifact against every other
  * holder's covering claim; plain claims neither refuse one nor are refused by one.
  */
object CoveringTests extends TestSuite {

  import CommandFixtures.*
  import MethodFixtures.{note, right, role}

  private val lapse = at.plusMillis(Claims.Lease.toMillis)

  /** A store whose method has a covering manager and a plain reader, both shown notes. */
  private def covered(): Store = Store(declared =
    Vector(
      method(roles =
        Vector(
          right(RoleView.of(role("manager"), Set(note.name), 1, covers = true)),
          right(RoleView.of(role("reader"), Set(note.name), 1))
        )
      )
    )
  )

  private def claimed(
      projects: Projects,
      store: Store,
      key: String,
      as: String,
      holder: String
  ): CommandOutcome =
    ask(projects, store, "project_claim", "key" -> key, "role" -> as, "holder" -> holder)

  private def under(projects: Projects, store: Store, local: String, parent: Option[String]) =
    ask(
      projects,
      store,
      "project_keep",
      (naming(projects, store, local) ++ Vector[(String, ujson.Value)](
        "kind" -> "note",
        "title" -> local,
        "body" -> "Text.",
        "parent" -> parent.fold(ujson.Null)(p => ujson.Str(s"p/$p"))
      ))*
    )

  /** Project `p`: note `a`, `b` under it, `c` under `b`. */
  private def chain(projects: Projects, store: Store): Unit = {
    started(projects, store)
    done(under(projects, store, "a", None))
    done(under(projects, store, "b", Some("a")))
    done(under(projects, store, "c", Some("b")))
  }

  private def refusedOver(side: String, mine: String, theirs: String, holder: String) =
    CommandOutcome.Refused(
      s"$mine is $side $theirs, which is claimed as manager for $holder by $by since $at; " +
        "that claim covers everything under it, and so would this one; " +
        s"it lapses at $lapse unless they keep working; project_release frees it"
    )

  val tests = Tests {
    test(
      "a covering claim overlapping another holder's is refused, and a plain one inside it is not"
    ) {
      val projects = plugin()
      val store = covered()
      chain(projects, store)
      done(claimed(projects, store, "p/a", "manager", "h1"))
      val below = claimed(projects, store, "p/c", "manager", "h2")
      val plain = claimed(projects, store, "p/b", "reader", "h3")
      val other = covered()
      chain(projects, other)
      done(claimed(projects, other, "p/c", "manager", "h1"))
      val above = claimed(projects, other, "p/a", "manager", "h2")
      (below, plain.isInstanceOf[CommandOutcome.Done], above) ==> (
        refusedOver("under", "p/3 c", "p/1 a", "h1"),
        true,
        refusedOver("above", "p/1 a", "p/3 c", "h1")
      )
    }
    test("a covering claim over a plain one is made") {
      val projects = plugin()
      val store = covered()
      chain(projects, store)
      done(claimed(projects, store, "p/c", "reader", "h1"))
      claimed(projects, store, "p/a", "manager", "h2").isInstanceOf[CommandOutcome.Done] ==> true
    }
    test("a keep moving an artifact so two holders' covering claims overlap is refused") {
      val projects = plugin()
      val store = covered()
      started(projects, store)
      done(under(projects, store, "a", None))
      done(under(projects, store, "b", None))
      done(claimed(projects, store, "p/a", "manager", "h1"))
      done(claimed(projects, store, "p/b", "manager", "h2"))
      under(projects, store, "b", Some("a")) ==> CommandOutcome.Refused(
        "p/2 b would sit under p/1 a, and their covering claims, as manager for h2 and " +
          "as manager for h1, would overlap; release one first with project_release"
      )
    }
    test("a keep in a project already holding an overlap elsewhere is kept") {
      val projects = plugin()
      val store = covered()
      chain(projects, store)
      done(under(projects, store, "d", None))
      done(claimed(projects, store, "p/a", "manager", "h1"))
      // As a higher room, or a method that came to cover, leaves it: a covering claim under another.
      inTx(
        store.claims.claim(
          right(DocKey.of("p/3")),
          right(ClaimHolder.of("h2")),
          right(ClaimRole.of("manager")),
          person,
          Label.Public,
          at
        )
      ).isRight ==> true
      under(projects, store, "d", Some("a")).isInstanceOf[CommandOutcome.Done] ==> true
    }
  }
}
