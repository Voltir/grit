package grit.core.edge

import grit.core.admin.AdministrationContract.{Declared, ada, confidentialTrial, mia, t, unmapped}
import grit.core.admin.{Administration, Answer, Change, Command}
import grit.core.id.SourceId
import grit.core.inbox.Inbox
import grit.core.message.Message
import grit.core.place.Place
import grit.core.store.Origin
import grit.core.visibility.TestLabels.place
import grit.core.visibility.{Label, RoomAccess}

import utest.*

/** What an [[Administration]], a [[Joins]] and an [[Inbox]] over one store keep together, run
  * against the in-memory fakes sharing one record in core and the engine's stores in grit.dbos:
  * what one records of a room, the others read.
  */
abstract class RecordedContract extends TestSuite {

  /** Runs `body` with an administration, joins and inbox over one store under [[Declared]],
    * which has recorded nothing of any room and has [[mia]] and [[ada]] vouched full members by
    * their realm, and with the label the conversation of an origin was created at (`None` for one
    * never begun).
    */
  protected def withStores[A](
      body: (Administration, Joins, Inbox, Origin => Option[Label]) => A
  ): A

  private def ok[A](r: Either[?, A]): A =
    r.fold(e => throw new java.lang.AssertionError(s"the store failed: $e"), identity)

  /** A room of [[T1]]'s workspace, which the deployment declares nothing of. */
  private def room(channel: String): Origin = Origin.Slack("T1", channel, "1.0")

  private val Team: Place = place("slack:T1")

  val tests = Tests {
    test("a room's access a join reports is the access the administration labels it by") {
      withStores { (admin, joins, _, _) =>
        val here = room("C-invited").room
        ok(joins.joined(here, RoomAccess.Invited, None, t(1)))
        ok(admin.run(mia, here, Command.ShowLabel, t(2))) ==>
          Answer.label(unmapped, Answer.Source.Default, Some(RoomAccess.Invited))
      }
    }

    test(
      "a label set through the administration for a room never heard is the label the inbox creates its first conversation at"
    ) {
      withStores { (admin, _, inbox, label) =>
        val here = room("C-unheard")
        val set = ok(admin.run(ada, here.room, Command.SetLabel(confidentialTrial), t(1)))
        ok(inbox.ingest(here, SourceId("m1"), Message.User("hello"), mia))
        (set, label(here)) ==> (
          Answer.relabelled(
            new Change.Relabel(here.room, Label.Public, Change.To.Set(confidentialTrial))
          ),
          Some(confidentialTrial)
        )
      }
    }

    test(
      "a room left and quieted through the administration is kept by forget; spoken again, it is forgotten"
    ) {
      withStores { (admin, joins, _, _) =>
        val here = room("C-quiet").room
        ok(joins.joined(here, RoomAccess.Open, None, t(1)))
        ok(joins.left(here, t(2)))
        ok(admin.run(mia, here, Command.Quiet(true), t(3)))
        val quiet = ok(joins.forget(Team, t(4)))
        ok(admin.run(mia, here, Command.Quiet(false), t(5)))
        (quiet, ok(joins.forget(Team, t(4)))) ==> (0, 1)
      }
    }
  }
}
