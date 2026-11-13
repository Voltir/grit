package grit.commands.routes

import java.time.Instant

import scala.concurrent.duration.*

import grit.commands.declare.HoldingWatch
import grit.commands.worker.{WorkerName, Worktrees}
import grit.core.edge.{EdgeWatch, WatchError, WatchFor, Watched}
import grit.core.id.ClaimHolder
import grit.core.identity.Account
import grit.core.place.Directory
import grit.core.session.Event
import grit.edge.http.Status

/** What `/wait` reads from a body, and how it answers what came of the watch. */
private[commands] object Waited {

  /** What `body` asks a watch for, and how long it waits, `within` read as whole seconds, 0 or
    * more ([[Routes.Within]] when absent), any count clamped by [[EdgeWatch.bounded]]; or why not:
    *   - `{"commit": {"worktree": "<absolute path>"}}`: a commit in that directory on `worker`
    *     ([[Worktrees.at]]), or `{"commit": {"holder": "<holder>"}}`: one by a session working
    *     as the holder; either at or after `since` (an ISO-8601 instant), by default `now`;
    *   - anything else, as `holding` reads it, when there is one.
    * A key the watch it names does not read is refused, naming it.
    */
  def watchOf(
      body: ujson.Obj,
      worker: WorkerName,
      holding: Option[HoldingWatch],
      now: Instant
  ): Either[String, (WatchFor, FiniteDuration)] = {
    val o = body.value
    val within: Either[String, FiniteDuration] = o.get("within") match {
      case None => Right(Routes.Within)
      case Some(w) =>
        w.numOpt
          .filter(s => s >= 0 && s.isWhole)
          .map(s => EdgeWatch.bounded(s.min(EdgeWatch.Longest.toSeconds.toDouble).toLong.seconds))
          .toRight("within is whole seconds, 0 or more")
    }
    val what: Either[String, WatchFor] = o.get("commit") match {
      case Some(c) =>
        for {
          by <- c.objOpt.map(_.value.toMap) match {
            case Some(m) if m.keySet == Set("worktree") =>
              m.get("worktree")
                .flatMap(_.strOpt)
                .flatMap(Directory.of(_).toOption)
                .map(dir => WatchFor.Committer.At(Worktrees.at(worker, dir)))
                .toRight("worktree is an absolute path")
            case Some(m) if m.keySet == Set("holder") =>
              m.get("holder")
                .flatMap(_.strOpt)
                .toRight("holder is a holder's name")
                .flatMap(ClaimHolder.of)
                .map(WatchFor.Committer.As(_))
            case _ => Left("commit names a worktree or a holder")
          }
          since <- o.get("since") match {
            case None => Right(now)
            case Some(s) =>
              s.strOpt
                .flatMap(t => scala.util.Try(Instant.parse(t)).toOption)
                .toRight("since is an ISO-8601 instant")
          }
        } yield WatchFor.Committed(by, since)
      case None =>
        holding
          .flatMap(_.read(body))
          .getOrElse(Left("a watch names a commit, or what this edge's holding watch reads"))
    }
    val reads: Set[String] =
      if (o.contains("commit")) Set("commit", "since", "within")
      else holding.fold(Set.empty[String])(_.keys) + "within"
    val known: Either[String, Unit] = o.keys
      .find(k => !reads.contains(k))
      .map(k => s"this watch reads no $k: only ${reads.toVector.sorted.mkString(", ")}")
      .toLeft(())
    for { w <- what; _ <- known; d <- within } yield (w, d)
  }

  /** What a watch for `what`, waiting `within`, answers when it came to `came`: 200 saying what
    * met it; 408 when nothing did before it lapsed; 503 when [[EdgeWatch.Most]] watches were
    * under way or the database could not be read; 403 for an account no person is linked to.
    * An HTTP client may retry a 408 on its own, which is harmless: a wait only reads.
    */
  def watched(
      what: WatchFor,
      within: FiniteDuration,
      holding: Option[HoldingWatch],
      came: Either[WatchError, Watched]
  ): (Status, String) = (what, came) match {
    case (h: WatchFor.Holding, Right(held: Watched.Held)) =>
      holding.fold((Status.Ok, s"held at version ${held.version}"))(w =>
        (Status.Ok, w.met(h, held))
      )
    case (_, Right(Watched.Committed(session, at, shape))) =>
      val shown = scala.util.Try(ujson.read(Event.Shape.value(shape)).obj).toOption
      def field(k: String) = shown.flatMap(_.get(k)).flatMap(_.strOpt)
      val commit =
        (field(Event.Shape.Sha).map(_.take(12)) ++ field(Event.Shape.Subject)).mkString(" ")
      val in = field(Event.Shape.Worktree).fold("")(w => s" in $w")
      (Status.Ok, s"committed $commit$in, on session ${session.local} at $at")
    case (_, Right(Watched.Lapsed)) =>
      (Status.Timeout, s"nothing met the watch within ${within.toSeconds}s")
    case (_, Right(other)) =>
      (Status.Failed, s"the watch was met by what it did not ask for: $other")
    case (_, Left(WatchError.Busy)) =>
      (Status.Unavailable, s"${EdgeWatch.Most} watches are under way already: wait for one to end")
    case (_, Left(WatchError.NotVouched(by))) =>
      (Status.Forbidden, s"no person is linked to ${Account.written(by)}")
    case (_, Left(WatchError.Unavailable(cause))) =>
      (Status.Unavailable, s"the database is unavailable: $cause")
  }
}
