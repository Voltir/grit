package grit.core.edge

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.document.DocRef
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, PluginName, SessionId}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.session.Event

/** An [[EdgeWatch]] over the versions and commits `script` holds, answering at once, whatever
  * `within`: what meets the watch already, else [[Watched.Lapsed]]. An account not in `vouched`
  * is [[WatchError.NotVouched]]; while the script is refusing, every watch is refused with its
  * error. Every watch is recorded in the script. Held to [[EdgeWatchContract]]; it reads no
  * labels, so every version and commit is the watcher's to read.
  */
final class ScriptedWatch(vouched: Set[Account], script: ScriptedWatch.Script) extends EdgeWatch {

  def until(
      by: Account,
      room: Place,
      what: WatchFor,
      within: FiniteDuration
  ): Either[WatchError, Watched] =
    script.asked(ScriptedWatch.Asked(by, room, what, within)) match {
      case Some(error) => Left(error)
      case None if !vouched.contains(by) => Left(WatchError.NotVouched(by))
      case None => Right(script.met(what).getOrElse(Watched.Lapsed))
    }
}

object ScriptedWatch {

  /** One watch, as it was asked. */
  final case class Asked(by: Account, room: Place, what: WatchFor, within: FiniteDuration)

  /** What a [[ScriptedWatch]] answers from, which a test writes and reads back; plain state, so a
    * suite may hold it in a field where it may not hold the watch.
    */
  final class Script {

    // Each replaced whole by this script's own methods, under its lock; nothing else holds them.
    @caps.unsafe.untrackedCaptures
    private var versions = Vector.empty[Version]
    @caps.unsafe.untrackedCaptures
    private var commits = Vector.empty[Commit]
    @caps.unsafe.untrackedCaptures
    private var watches = Vector.empty[Asked]
    @caps.unsafe.untrackedCaptures
    private var refused = Option.empty[WatchError]

    /** Every watch so far, first first. */
    def asked: Vector[Asked] = synchronized(watches)

    /** Refuses every watch from now on with `error`, or none when `None`. */
    def refusing(error: Option[WatchError]): Unit = synchronized { refused = error }

    /** A version of `plugin`'s `key` holding `refs`, written at `written`: its version. */
    def hold(
        plugin: PluginName,
        key: DocKey,
        refs: Set[DocRef],
        written: Instant
    ): DocumentVersionId = synchronized {
      DocumentVersionId.of(versions.size + 1L) match {
        case Some(version) =>
          versions = versions :+ Version(plugin, key, version, refs, written)
          version
        case None => throw new java.lang.AssertionError("a version is numbered from 1")
      }
    }

    /** A commit at `at` in `worktree`, on `session` working as `holder`, logged with `shape`. */
    def commit(
        session: SessionId,
        holder: Option[ClaimHolder],
        worktree: Place,
        at: Instant,
        shape: Event.Shape
    ): Unit = synchronized {
      commits = commits :+ Commit(session, holder, worktree, at, shape)
    }

    /** Records `watch`: the error it is refused with, when refusing. */
    private[ScriptedWatch] def asked(watch: Asked): Option[WatchError] = synchronized {
      watches = watches :+ watch
      refused
    }

    private[ScriptedWatch] def met(what: WatchFor): Option[Watched] = synchronized {
      what match {
        case h: WatchFor.Holding =>
          versions
            .filter(v => v.plugin == h.plugin && v.key == h.key)
            .lastOption
            .filter(_.refs.exists(h.refs.contains))
            .map(v => Watched.Held(v.version, v.refs.intersect(h.refs), v.written))
        case WatchFor.Committed(by, since) =>
          commits
            .filter(c => !c.at.isBefore(since))
            .filter(c =>
              by match {
                case WatchFor.Committer.As(holder) => c.holder.contains(holder)
                case WatchFor.Committer.At(worktree) => c.worktree == worktree
              }
            )
            .sortBy(c => (c.at.getEpochSecond, c.at.getNano, c.session.local))
            .headOption
            .map(c => Watched.Committed(c.session, c.at, c.shape))
      }
    }
  }

  private final case class Version(
      plugin: PluginName,
      key: DocKey,
      version: DocumentVersionId,
      refs: Set[DocRef],
      written: Instant
  )

  private final case class Commit(
      session: SessionId,
      holder: Option[ClaimHolder],
      worktree: Place,
      at: Instant,
      shape: Event.Shape
  )
}
