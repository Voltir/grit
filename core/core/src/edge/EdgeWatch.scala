package grit.core.edge

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.DocRef
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, PluginName, SessionId}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.session.Event

/** What an edge waits on in the store for the people it serves: woken as a write that may meet it
  * commits, never by polling (ADR 0051).
  */
trait EdgeWatch extends caps.SharedCapability {

  /** Waits until `what` is met as the person `by` is linked to reads it then, in `room`
    * ([[grit.core.visibility.Subject.Person]]), or `within` has passed ([[EdgeWatch.bounded]]):
    * what met it, or [[Watched.Lapsed]]. Met at once when it is met already; a write that person
    * does not read never meets it. Blocks its thread for the whole watch.
    * [[WatchError.Busy]] when [[EdgeWatch.Most]] watches are under way;
    * [[WatchError.NotVouched]] for an account no person is linked to;
    * [[WatchError.Unavailable]] when the database could not be read.
    */
  def until(
      by: Account,
      room: Place,
      what: WatchFor,
      within: FiniteDuration
  ): Either[WatchError, Watched]
}

object EdgeWatch {

  /** 16: the watches under way at once; the next is refused [[WatchError.Busy]]. */
  val Most: Int = 16

  /** 10 minutes: the longest a watch lasts. */
  val Longest: FiniteDuration = 10.minutes

  /** How long a watch given `within` lasts: `within` clamped to [0, [[Longest]]]; at 0 it reads
    * once and, unmet, lapses.
    */
  def bounded(within: FiniteDuration): FiniteDuration =
    if (within <= Duration.Zero) Duration.Zero else within.min(Longest)

  /** Watches nothing: every call is `Left(WatchError.Unavailable)`, saying the edge was built
    * without a watch.
    */
  def Refusing: EdgeWatch = new EdgeWatch {
    def until(
        by: Account,
        room: Place,
        what: WatchFor,
        within: FiniteDuration
    ): Either[WatchError, Watched] =
      Left(WatchError.Unavailable("this edge was given no watch"))
  }
}

/** What a watch waits for, as a request carries it. */
sealed trait WatchFor extends caps.Pure

object WatchFor {

  /** `plugin`'s current document under `key`, at any label the watcher reads, holding one of
    * `refs`.
    */
  final case class Holding private (plugin: PluginName, key: DocKey, refs: Set[DocRef])
      extends WatchFor

  object Holding {

    /** The watch, or why not: no refs. */
    def of(plugin: PluginName, key: DocKey, refs: Set[DocRef]): Either[String, Holding] =
      Either.cond(refs.nonEmpty, new Holding(plugin, key, refs), "a watch names a ref to hold")
  }

  /** A `commit` event at or after `since` that `by` names, logged on a session the watcher reads.
    */
  final case class Committed(by: Worker, since: Instant) extends WatchFor

  /** Who made a commit: a session working as `holder` ([[grit.core.session.Session.holder]]), or
    * any session committing in the worktree `at` ([[Event.Shape.committed]]).
    */
  enum Worker extends caps.Pure {
    case As(holder: ClaimHolder)
    case At(worktree: Place)
  }
}

/** What met a watch, as the watcher reads it, or that nothing did. */
enum Watched extends caps.Pure {

  /** The most recently written version meeting [[WatchFor.Holding]], with the refs of those
    * watched for that it holds.
    */
  case Held(version: DocumentVersionId, refs: Set[DocRef], written: Instant)

  /** The earliest commit meeting [[WatchFor.Committed]], ties broken by session, then by when it
    * was logged.
    */
  case Committed(session: SessionId, at: Instant, shape: Event.Shape)

  /** Nothing met the watch before it lapsed. */
  case Lapsed
}

/** Why a watch was not kept. */
enum WatchError {

  /** [[EdgeWatch.Most]] watches were under way. */
  case Busy

  /** No person is linked to the account `by`. */
  case NotVouched(by: Account)

  /** The database could not be read, `cause` saying why. */
  case Unavailable(cause: String)
}
