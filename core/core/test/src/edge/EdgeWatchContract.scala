package grit.core.edge

import java.time.Instant

import scala.concurrent.duration.Duration

import grit.core.document.DocRef
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, Harness, PluginName, SessionId}
import grit.core.place.Place
import grit.core.session.Event

import utest.*

/** What an edge's routes rely on of [[EdgeWatch]]: what met a watch, that an unmet one lapses,
  * and the account it refuses. Run by the scripted fake in core and by the SQL watch in
  * grit.dbos.
  */
abstract class EdgeWatchContract extends TestSuite {

  /** `body` lent a watch over a store where [[EdgeContracts.Reader]] is linked to a person and
    * [[EdgeContracts.Stranger]] is not, holding what [[hold]] and [[commit]] put there.
    */
  protected def watching[A](body: EdgeWatch^ => A): A

  /** Writes a version of [[EdgeWatchContract.Plugin]]'s `key` holding `ref`, at `written`, in
    * [[EdgeContracts.Room]], which the reader reads: its version.
    */
  protected def hold(key: DocKey, ref: DocRef, written: Instant): DocumentVersionId

  /** Logs a commit at `at` in `worktree` on `session`, the reader's, working as `holder`: the
    * shape it was logged with.
    */
  protected def commit(
      session: SessionId,
      holder: ClaimHolder,
      worktree: Place,
      at: Instant
  ): Event.Shape

  import EdgeContracts.*
  import EdgeWatchContract.*

  private def until(what: WatchFor) = watching(_.until(Reader, Room, what, Duration.Zero))

  val tests = Tests {
    test("a watch met already is met at once, with the refs it holds") {
      val key = got(DocKey.of("contract/met"))
      val version = hold(key, Review, At)
      until(got(WatchFor.Holding.of(Plugin, key, Set(Review, Merged)))) ==>
        Right(Watched.Held(version, Set(Review), At))
    }

    test("a watch nothing meets lapses once its time is up") {
      until(got(WatchFor.Holding.of(Plugin, got(DocKey.of("contract/never")), Set(Review)))) ==>
        Right(Watched.Lapsed)
    }

    test("a commit meets a watch on its worktree or its holder from its time on, never after") {
      val session = got(SessionId.of(got(Harness.of("contract")), "committing"))
      val holder = got(ClaimHolder.of("contract/committer"))
      val worktree = Place.under(grit.core.place.Namespace.Worker, Vector("desk", "/work/c"))
      val shape = commit(session, holder, worktree, At)
      Vector(
        until(WatchFor.Committed(WatchFor.Committer.At(worktree), At)),
        until(WatchFor.Committed(WatchFor.Committer.As(holder), At)),
        until(WatchFor.Committed(WatchFor.Committer.As(holder), At.plusSeconds(1)))
      ) ==> Vector(
        Right(Watched.Committed(session, At, shape)),
        Right(Watched.Committed(session, At, shape)),
        Right(Watched.Lapsed)
      )
    }

    test("an account no person is linked to is refused") {
      watching(
        _.until(Stranger, Room, WatchFor.Committed(WatchFor.Committer.At(Room), At), Duration.Zero)
      ) ==> Left(WatchError.NotVouched(Stranger))
    }
  }
}

object EdgeWatchContract {

  val Plugin: PluginName = got(PluginName.of("contracted"))
  val Review: DocRef = got(DocRef.of("state", "review"))
  val Merged: DocRef = got(DocRef.of("state", "merged"))

  def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)
}
