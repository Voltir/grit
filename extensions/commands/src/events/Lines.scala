package grit.commands.events

import java.nio.charset.StandardCharsets
import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Try

import grit.commands.worker.{Holders, WorkerName, Worktrees}
import grit.core.edge.EdgeSessions
import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.place.{Directory, Place}
import grit.core.session.{Event, Session}

/** A harness's event lines, each one JSON object in grit's words (ADR 0050), as `/events` reads
  * them: the harness's adapter maps its own hooks and tool names onto them, and the edge reads
  * nothing harness-specific.
  */
private[commands] object Lines {

  /** 16,384: the most UTF-8 bytes of a line read; a closing's 2,000 characters fit, escaped. */
  val Longest: Int = 16384

  /** 1 minute: how far past this edge's clock a line's `at` may be. A session's last event keeps
    * its holder's claims from lapsing, so a worker's clock running ahead would otherwise extend
    * its own claims.
    */
  val Skew: FiniteDuration = 1.minute

  /** Every key a line may hold. */
  val Keys: Vector[String] =
    Vector(
      "session",
      "agent",
      "type",
      "dir",
      "worktree",
      "n",
      "at",
      "event",
      "bytes",
      "shape",
      "text"
    )

  /** One line read: `event` on the session `id`, a child of `parent` when an agent's, worked at
    * `at`, as `holder`.
    */
  final case class Line(
      id: SessionId,
      parent: Option[SessionId],
      holder: ClaimHolder,
      at: Place,
      event: Event
  )

  /** A post's lines read: the batch of those read, none when no line was, and each line refused,
    * by its number from 1, and why.
    */
  final case class Batched(batch: Option[EdgeSessions.Batch], refused: Vector[(Int, String)])

  /** `line` as `harness` sent it from `worker`, read at `now`, or why not. A line holds:
    *   - `session`, the harness's own id for the session ([[SessionId.of]]); for an agent's line,
    *     `agent`, the agent's own id, and `type`, its type, together: the line is then on the
    *     session `session/agent` ([[Holders.agentSession]]), a child of `session`;
    *   - `dir`, the session's directory (the starting session's for an agent), whose holder it
    *     works as ([[Holders.of]]); `worktree`, where it worked, by default `dir`; each absolute,
    *     each placed on `worker` ([[Worktrees.at]]);
    *   - `n`, its ordinal, a decimal string above 0, unique within the session, so a line sent
    *     again is known; `at`, an ISO-8601 instant no later than [[Skew]] past `now`;
    *   - `event`, its kind by its grit name ([[Event.Kind.name]]): `started`, `read` (with
    *     `bytes`, a count of 0 or more), `edited`, `ran`, `commit`, `closing` (with `text`, its
    *     summary, [[Session.Summary.of]]), `ended`;
    *   - `shape`, a JSON object of what it was, never its content, by default `{}`, at most
    *     [[Event.Shape.Longest]] bytes: a commit's names its sha under [[Event.Shape.Sha]] and keeps
    *     every other key, its worktree written under [[Event.Shape.Worktree]] and its subject cut
    *     to fit ([[Event.Shape.committed]]); any other's [[Event.Shape.Worktree]], an absolute
    *     path, is written as its place on `worker`.
    * Refused: a line over [[Longest]] bytes, no JSON object, a key not in [[Keys]], or any value
    * above that is missing or out of its bound; the reason names the key.
    */
  def read(line: String, harness: Harness, worker: WorkerName, now: Instant): Either[String, Line] =
    for {
      _ <- Either.cond(
        line.getBytes(StandardCharsets.UTF_8).length <= Longest,
        (),
        s"a line over $Longest bytes"
      )
      o <- Try(ujson.read(line)).toOption.flatMap(_.objOpt).toRight("no JSON object")
      _ <- o.keys
        .find(k => !Keys.contains(k))
        .map(k => s"no key $k is read in a line; these are: ${Keys.mkString(", ")}")
        .toLeft(())
      session <- text(o, "session").flatMap(SessionId.of(harness, _).left.map(w => s"session: $w"))
      a <- optional(o, "agent")
      t <- optional(o, "type")
      agent <- Holders.agent(session, a, t)
      dir <- directory(o, "dir")
      worktree <- if (o.contains("worktree")) directory(o, "worktree") else Right(dir)
      n <- o.get("n") match {
        case Some(ujson.Str(s)) if s.nonEmpty && s.forall(_.isDigit) =>
          s.toLongOption.toRight(s"n: over ${Long.MaxValue}: $s")
        case Some(other) => Left(s"n: a decimal string, not ${ujson.write(other)}")
        case None => Left("n: missing")
      }
      at <- text(o, "at").flatMap(a =>
        Try(Instant.parse(a)).toOption.toRight(s"at: no ISO-8601 instant: $a")
      )
      _ <- Either.cond(
        !at.isAfter(now.plusMillis(Skew.toMillis)),
        (),
        s"at: later than ${Skew.toSeconds}s past this edge's clock: $at"
      )
      named <- text(o, "event")
      kind <- kindOf(named, o)
      _ <- Either.cond(
        !o.contains("text") || kind.isInstanceOf[Event.Kind.Closing],
        (),
        "text: a closing's alone"
      )
      sent <- o.get("shape") match {
        case None => Right(ujson.Obj())
        case Some(s: ujson.Obj) => Right(s)
        case Some(_) => Left("shape: a JSON object")
      }
      place = Worktrees.at(worker, worktree)
      shape <- shaped(kind, sent, place, worker).left.map(w => s"shape: $w")
      event <- Event.of(n, at, kind, shape).left.map(w => s"n: $w")
    } yield Line(
      agent.fold(session)(_._1),
      agent.map(_ => session),
      Holders.of(worker, dir, agent.map(_._2)),
      place,
      event
    )

  /** `body`'s lines, one a line and blank ones skipped, as `harness` sent them from `worker`, read
    * at `now` ([[read]]): one batch of every line read, on their session, at the first's place and
    * as its holder; or why not, the whole post refused: `harness` is grit's own, `body` holds no
    * line, or two lines read are on two sessions.
    */
  def batch(
      body: String,
      harness: Harness,
      worker: WorkerName,
      now: Instant
  ): Either[String, Batched] = {
    val lines = body.split('\n').toVector.zipWithIndex.collect {
      case (l, i) if l.trim.nonEmpty => (i + 1, l)
    }
    val reads = lines.map((i, l) => (i, read(l, harness, worker, now)))
    val ok = reads.collect { case (i, Right(l)) => (i, l) }
    if (harness == Harness.Grit)
      Left("the harness grit is grit's own: only its turns record its sessions")
    else if (lines.isEmpty) Left("the body holds no line")
    else
      ok.headOption match {
        case None => Right(Batched(None, reads.collect { case (i, Left(w)) => (i, w) }))
        case Some((_, first)) =>
          ok.collectFirst {
            case (i, l) if l.id != first.id =>
              s"line $i is session ${l.id.written}, not ${first.id.written}: post each session's lines apart"
          }.toLeft(
            Batched(
              Some(
                EdgeSessions
                  .Batch(first.id, first.parent, first.at, Some(first.holder), ok.map(_._2.event))
              ),
              reads.collect { case (i, Left(w)) => (i, w) }
            )
          )
      }
  }

  private def text(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
    optional(o, key).flatMap(_.toRight(s"$key: missing"))

  private def optional(
      o: collection.Map[String, ujson.Value],
      key: String
  ): Either[String, Option[String]] =
    o.get(key) match {
      case Some(ujson.Str(s)) => Right(Some(s))
      case Some(_) => Left(s"$key: a string")
      case None => Right(None)
    }

  private def directory(o: collection.Map[String, ujson.Value], key: String) =
    text(o, key).flatMap(Directory.of(_).left.map(w => s"$key: $w"))

  /** The kind `named` names, with what it needs of `o`. */
  private def kindOf(
      named: String,
      o: collection.Map[String, ujson.Value]
  ): Either[String, Event.Kind] =
    named match {
      case k if k == Event.Kind.Started.name => Right(Event.Kind.Started)
      case k if k == Event.Kind.Read(0).name =>
        o.get("bytes") match {
          case Some(ujson.Num(b)) if b >= 0 && b == b.floor && b <= Long.MaxValue.toDouble =>
            Right(Event.Kind.Read(b.toLong))
          case Some(other) =>
            Left(s"bytes: a read's count of bytes, 0 or more, not ${ujson.write(other)}")
          case None => Left("bytes: a read's count of bytes, missing")
        }
      case k if k == Event.Kind.Edited.name => Right(Event.Kind.Edited)
      case k if k == Event.Kind.Ran.name => Right(Event.Kind.Ran)
      case k if k == Event.Kind.Commit.name => Right(Event.Kind.Commit)
      case k if k == Event.Kind.Ended.name => Right(Event.Kind.Ended)
      case "closing" =>
        o.get("text") match {
          case Some(ujson.Str(t)) =>
            Session.Summary.of(t).map(Event.Kind.Closing(_)).left.map(w => s"text: $w")
          case Some(_) => Left("text: a closing's summary, a string")
          case None => Left("text: a closing's summary, missing")
        }
      case other => Left(s"event: started, read, edited, ran, commit, closing or ended, not $other")
    }

  /** `sent` as `kind`'s shape at `place` on `worker`. */
  private def shaped(
      kind: Event.Kind,
      sent: ujson.Obj,
      place: Place,
      worker: WorkerName
  ): Either[String, Event.Shape] =
    kind match {
      case Event.Kind.Commit =>
        sent.value.get(Event.Shape.Sha) match {
          case Some(ujson.Str(sha)) =>
            Event.Shape.committed(
              place,
              sha,
              sent.value.get(Event.Shape.Subject).flatMap(_.strOpt),
              sent
            )
          case Some(_) => Left("a commit's sha, a string")
          case None => Left("a commit's sha, missing")
        }
      case _ =>
        sent.value.get(Event.Shape.Worktree) match {
          case None => Event.Shape.of(sent)
          case Some(w) =>
            w.strOpt
              .toRight("its worktree, an absolute path")
              .flatMap(Directory.of)
              .flatMap(d =>
                Event.Shape.of(
                  ujson.Obj.from(
                    sent.value.toSeq.map((k, v) =>
                      if (k == Event.Shape.Worktree) k -> ujson.Str(Worktrees.at(worker, d).written)
                      else k -> v
                    )
                  )
                )
              )
        }
    }
}
