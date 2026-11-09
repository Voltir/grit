package grit.dbos.internal

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.sql.ResultSet
import java.time.Instant

import scala.concurrent.duration.*

import grit.core.host.{BlobError, Blobs, Pending}
import grit.core.id.{Harness, SessionId, Sha256}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.{SqlLabels, SqlStatements, SqlTrialRows}

/** The bench's rows over `blobs`: each trial of a harness at a frozen task, its transcript and
  * its standing text, the newest [[TrialRows.MostTrials]] trials alone. `blobs` must be the
  * bench's own, one store per database: [[prune]] removes each of its blobs no row names, as a
  * file area's sweep does by its files, so two such owners of one store would each remove the
  * other's.
  */
final class TrialRows(blobs: Blobs^) {

  /** `trial` recorded, with `standing`, the standing text whose digest is `trial.standing`, kept
    * as that digest's blob; then every trial but the newest [[TrialRows.MostTrials]] recorded
    * removed with its parts' rows, never a blob ([[prune]]).
    * [[TrialRows.Recorded.Repeat]], writing nothing, when `trial`'s session has a trial already.
    * `Left(Invalid)`, recording nothing, when `standing`'s digest is not `trial.standing`;
    * `Left(DatabaseError)` when it cannot be kept. Waits on any other record or prune until the
    * transaction ends.
    */
  def record(trial: TrialRows.Trial, standing: Option[String])(using
      Tx^
  ): Either[StoreError, TrialRows.Recorded] =
    recorded(trial, standing)(_ => Right(()))

  /** As `record(trial, standing)`, with `transcript` and `diff` kept as its
    * [[TrialRows.Part]]s, each clipped when `clipped` holds its part (it ran past what was
    * received), kept at `label` joined with this transaction's floor (ADR 0030: what a
    * transaction writes is kept at least where it writes); on a repeat neither is kept.
    * `Left(Invalid)`, recording nothing, also when either holds more than [[TrialRows.MostBytes]]
    * bytes.
    */
  def record(
      trial: TrialRows.Trial,
      standing: Option[String],
      transcript: Pending^,
      diff: Pending^,
      clipped: Set[TrialRows.Part],
      label: Label
  )(using
      Tx^
  ): Either[StoreError, TrialRows.Recorded] =
    Vector(transcript.bytes, diff.bytes).find(_ > TrialRows.MostBytes) match {
      case Some(bytes) =>
        Left(
          StoreError.Invalid(s"a trajectory holds at most ${TrialRows.MostBytes} bytes, not $bytes")
        )
      case None =>
        recorded(trial, standing) { id => (tx: Tx^) ?=>
          for {
            _ <- transcript.keep().left.map(e => StoreError.DatabaseError(s"its transcript: $e"))
            _ <- diff.keep().left.map(e => StoreError.DatabaseError(s"its diff: $e"))
            _ <- TrialRows.trajectoryRow(
              id,
              TrialRows.Part.Transcript,
              transcript.sha256,
              transcript.bytes,
              clipped(TrialRows.Part.Transcript),
              label
            )(using tx)
            _ <- TrialRows.trajectoryRow(
              id,
              TrialRows.Part.Diff,
              diff.sha256,
              diff.bytes,
              clipped(TrialRows.Part.Diff),
              label
            )(using tx)
          } yield ()
        }
    }

  /** `trial` recorded as `record(trial, standing)` says, with `rest` of its row's id done before
    * the cap is held; on a repeat, `rest` is not done.
    */
  private def recorded(trial: TrialRows.Trial, standing: Option[String])(
      rest: Long => (Tx^) ?=> Either[StoreError, Unit]
  )(using Tx^): Either[StoreError, TrialRows.Recorded] =
    if (standing.map(Sha256.text) != trial.standing)
      Left(StoreError.Invalid("a trial's standing text is not the one its digest names"))
    else
      TrialRows.begun(trial).flatMap {
        case None => Right(TrialRows.Recorded.Repeat)
        case Some(id) =>
          for {
            _ <- standing.fold(Right(()))(text =>
              blobs
                .receive(
                  ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8)),
                  TrialRows.MostBytes
                )(
                  _.keep()
                )
                .flatMap(identity)
                .left
                .map(e => StoreError.DatabaseError(s"its standing text: $e"))
            )
            _ <- rest(id)
            removed <- TrialRows.evict()
          } yield TrialRows.Recorded.Fresh(removed)
      }

  /** Every blob no trial's part (its transcript or its diff) or standing text names removed, and every pending blob
    * last written before `before` cleared. Waits on any record, and holds records off, until the
    * transaction ends.
    * `Right(Left)` when `blobs` fails: what was removed before it failed stays removed.
    */
  def prune(before: Instant)(using Tx^): Either[StoreError, Either[BlobError, TrialRows.Pruned]] =
    for {
      _ <- TrialRows.lock()
      named <- SqlStatements.many(
        """SELECT sha256 FROM grit.bench_trajectories
          |UNION SELECT standing FROM grit.bench_trials WHERE standing IS NOT NULL""".stripMargin
      )(_ => ())(rs =>
        Sha256.read(rs.getString("sha256")).toRight(s"no digest: ${rs.getString("sha256")}")
      )
      shas <- SqlStatements.valid(named)
    } yield
      for {
        removed <- blobs.removeAllBut(shas.toSet)
        cleared <- blobs.clearPending(before)
      } yield TrialRows.Pruned(removed, cleared)

  /** `session`'s trial's `part`, when it has one this transaction reads. */
  def kept(session: SessionId, part: TrialRows.Part)(using
      Tx^
  ): Either[StoreError, Option[TrialRows.Kept]] =
    SqlTrialRows
      .kept(Harness.value(session.harness), session.local, TrialRows.kind(part))(TrialRows.keptOf)
      .flatMap(found => SqlStatements.valid(found.toVector))
      .map(_.headOption)
}

object TrialRows {

  /** Every trial kept, oldest recorded first: rows alone, which need no blob store. */
  def trials()(using Tx^): Either[StoreError, Vector[Trial]] =
    SqlStatements
      .many("SELECT * FROM grit.bench_trials ORDER BY id")(_ => ())(trialOf)
      .flatMap(SqlStatements.valid)

  /** 60: the most trials kept. */
  val MostTrials: Int = 60

  /** 32 MiB: the most bytes of a transcript kept. */
  val MostBytes: Long = 32L * 1024 * 1024

  /** A frozen task's name: 1 to 80 lower-case letters, digits and `-`, the first no `-`. */
  opaque type TaskName = String

  object TaskName {
    private val Rule = "[a-z0-9][a-z0-9-]{0,79}".r

    /** `name` as a task's name, or why not. */
    def of(name: String): Either[String, TaskName] =
      Either.cond(
        Rule.matches(name),
        name,
        s"a task's name is 1 to 80 lower-case letters, digits and -, the first no -: $name"
      )

    def value(t: TaskName): String = t
  }

  /** One trial: the session that ran it, which names its harness; the task and model; what its
    * model calls consumed and cost, as [[Usage]] counts them, and how many it made; its wall
    * time; before its first edit, the bytes its tools returned and the model calls it made (both
    * `None`: it made no edit); whether the task's checks passed; how many lines its diff
    * differs from the task's known diff by (`None`: it left none); and the digest of the standing
    * text its coder's brief began with (`None`: its harness has none, as Claude Code's).
    */
  final case class Trial private (
      session: SessionId,
      task: TaskName,
      model: ModelId,
      usage: Usage,
      calls: Int,
      wall: FiniteDuration,
      readBeforeEdit: Option[Long],
      callsBeforeEdit: Option[Int],
      passed: Boolean,
      distance: Option[Long],
      standing: Option[Sha256]
  ) extends caps.Pure

  object Trial {

    /** The trial of these, or why not: a count, a byte count or a wall time below 0; calls
      * before the first edit past `calls`; only one of `readBeforeEdit` and `callsBeforeEdit`
      * given; or cache reads and writes past `usage.input`.
      */
    def of(
        session: SessionId,
        task: TaskName,
        model: ModelId,
        usage: Usage,
        calls: Int,
        wall: FiniteDuration,
        readBeforeEdit: Option[Long],
        callsBeforeEdit: Option[Int],
        passed: Boolean,
        distance: Option[Long],
        standing: Option[Sha256]
    ): Either[String, Trial] = {
      val tokens = Vector(usage.input, usage.output, usage.cacheRead, usage.cacheWrite)
        .map(Tokens.value)
      if (tokens.exists(_ < 0) || usage.costUsd.exists(_ < 0))
        Left("a trial's tokens and cost are at least 0")
      else if (
        Tokens.value(usage.cacheRead) + Tokens.value(usage.cacheWrite) > Tokens.value(usage.input)
      )
        Left("a trial's cache reads and writes are part of its input")
      else if (
        calls < 0 || wall.toMillis < 0 || readBeforeEdit.exists(_ < 0) || distance.exists(_ < 0)
      )
        Left("a trial's calls, wall time, bytes read and distance are at least 0")
      else if (readBeforeEdit.isDefined != callsBeforeEdit.isDefined)
        Left("a trial's bytes read and calls before its first edit are given together")
      else if (callsBeforeEdit.exists(c => c < 0 || c > calls))
        Left("a trial's calls before its first edit are 0 to its calls")
      else
        Right(
          Trial(
            session,
            task,
            model,
            usage,
            calls,
            wall,
            readBeforeEdit,
            callsBeforeEdit,
            passed,
            distance,
            standing
          )
        )
    }
  }

  /** What a trial keeps beside its row, each a blob of the bench's store: its harness's
    * transcript as it wrote it, and its diff from its task's base, as the rig measures it.
    */
  enum Part {
    case Transcript, Diff
  }

  /** `part` as `bench_trajectories.kind` stores it: a recorded name, never renamed. */
  private[internal] def kind(part: Part): String = part match {
    case Part.Transcript => "transcript"
    case Part.Diff => "diff"
  }

  /** A trial's part as kept: the blob `sha256` of `bytes` bytes, `clipped` when the part ran past
    * [[MostBytes]]; read by whom `label` clears.
    */
  final case class Kept private[dbos] (
      sha256: Sha256,
      bytes: Long,
      clipped: Boolean,
      label: Label
  ) extends caps.Pure {

    /** [[Integrity.Member]], every part's: the repository a trial works in is its operator's
      * own, as the coding tools' results are.
      */
    def integrity: Integrity = Integrity.Member
  }

  enum Recorded {

    /** Recorded, and `removed` older trials removed past [[MostTrials]]. */
    case Fresh(removed: Int)
    case Repeat
  }

  /** What a prune removed: blobs no transcript named, and pending blobs abandoned. */
  final case class Pruned(removed: Int, cleared: Int)

  /** The advisory lock every record and prune takes until its transaction ends. */
  private val Lock = 0x67726974_62656e63L

  private def lock()(using Tx^): Either[StoreError, Unit] =
    SqlStatements.one("SELECT pg_advisory_xact_lock(?)")(_.setLong(1, Lock))(_ => ()).map(_ => ())

  /** Under the bench's lock, `trial`'s row inserted, its id; `None` when its session has one. */
  private def begun(trial: Trial)(using Tx^): Either[StoreError, Option[Long]] =
    lock().flatMap(_ => trialRow(trial))

  /** Every trial but the newest [[MostTrials]] recorded deleted, with its parts' rows; how many. */
  private def evict()(using Tx^): Either[StoreError, Int] =
    SqlStatements.update(
      """DELETE FROM grit.bench_trials
        | WHERE id NOT IN (SELECT id FROM grit.bench_trials ORDER BY id DESC LIMIT ?)""".stripMargin
    )(_.setInt(1, MostTrials))

  /** `trial`'s row inserted, its id; `None` when its session has one. */
  private def trialRow(trial: Trial)(using Tx^): Either[StoreError, Option[Long]] =
    SqlStatements.one(
      """INSERT INTO grit.bench_trials
        |       (harness, local, task, model, input_tokens, cache_read_tokens, cache_write_tokens,
        |        output_tokens, thinking_tokens, cost_usd, calls, wall_ms, read_before_edit,
        |        calls_before_edit, passed, distance, standing)
        |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        |ON CONFLICT (harness, local) DO NOTHING
        |RETURNING id""".stripMargin
    ) { ps =>
      val u = trial.usage
      ps.setString(1, Harness.value(trial.session.harness))
      ps.setString(2, trial.session.local)
      ps.setString(3, TaskName.value(trial.task))
      ps.setString(4, ModelId.value(trial.model))
      ps.setLong(5, Tokens.value(u.input))
      ps.setLong(6, Tokens.value(u.cacheRead))
      ps.setLong(7, Tokens.value(u.cacheWrite))
      ps.setLong(8, Tokens.value(u.output))
      ps.setObject(9, u.thinking.map(t => Long.box(Tokens.value(t))).orNull)
      ps.setBigDecimal(10, u.costUsd.map(_.bigDecimal).orNull)
      ps.setInt(11, trial.calls)
      ps.setLong(12, trial.wall.toMillis)
      ps.setObject(13, trial.readBeforeEdit.map(Long.box).orNull)
      ps.setObject(14, trial.callsBeforeEdit.map(Int.box).orNull)
      ps.setBoolean(15, trial.passed)
      ps.setObject(16, trial.distance.map(Long.box).orNull)
      ps.setString(17, trial.standing.map(_.hex).orNull)
    }(_.getLong("id"))

  /** The `part` row of trial `id`, at `label` joined with the transaction's floor. */
  private def trajectoryRow(
      id: Long,
      part: Part,
      sha: Sha256,
      bytes: Long,
      clipped: Boolean,
      label: Label
  )(using tx: Tx^): Either[StoreError, Unit] = {
    val kept = label.join(Tx.clearance(tx).floor)
    SqlStatements
      .update(
        s"""INSERT INTO grit.bench_trajectories
           |       (trial_id, kind, sha256, bytes, clipped, classification_id)
           |VALUES (?, ?, ?, ?, ?, ${SqlLabels.Interned})""".stripMargin
      ) { ps =>
        ps.setLong(1, id)
        ps.setString(2, kind(part))
        ps.setString(3, sha.hex)
        ps.setLong(4, bytes)
        ps.setBoolean(5, clipped)
        SqlLabels.bind(ps, 6, kept)
      }
      .map(_ => ())
  }

  private def trialOf(rs: ResultSet): Either[String, Trial] = {
    def maybeLong(c: String) = Option(rs.getObject(c, classOf[java.lang.Long])).map(_.longValue)
    val standing = Option(rs.getString("standing"))
    for {
      digest <- standing.fold(Right(None))(hex =>
        Sha256.read(hex).map(Some(_)).toRight(s"no digest: $hex")
      )
      h <- Harness.of(rs.getString("harness"))
      s <- SessionId.of(h, rs.getString("local"))
      t <- TaskName.of(rs.getString("task"))
      m <- ModelId.of(rs.getString("model")).toRight(s"no model id: ${rs.getString("model")}")
      r <- Trial.of(
        s,
        t,
        m,
        Usage(
          Tokens(rs.getLong("input_tokens")),
          Tokens(rs.getLong("output_tokens")),
          Tokens(rs.getLong("cache_read_tokens")),
          Option(rs.getBigDecimal("cost_usd")).map(BigDecimal(_)),
          Tokens(rs.getLong("cache_write_tokens")),
          maybeLong("thinking_tokens").map(Tokens(_))
        ),
        rs.getInt("calls"),
        rs.getLong("wall_ms").millis,
        maybeLong("read_before_edit"),
        Option(rs.getObject("calls_before_edit", classOf[java.lang.Integer])).map(_.intValue),
        rs.getBoolean("passed"),
        maybeLong("distance"),
        digest
      )
    } yield r
  }

  private def keptOf(rs: ResultSet): Either[String, Kept] =
    Sha256
      .read(rs.getString("sha256"))
      .toRight(s"no digest: ${rs.getString("sha256")}")
      .map(Kept(_, rs.getLong("bytes"), rs.getBoolean("clipped"), SqlLabels.read(rs)))
}
