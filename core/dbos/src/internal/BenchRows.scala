package grit.dbos.internal

import java.sql.ResultSet

import scala.concurrent.duration.*

import grit.core.host.{Blobs, Pending}
import grit.core.id.{Harness, SessionId, Sha256}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.{SqlBenchRows, SqlLabels, SqlStatements}

/** The bench's rows over `blobs`: each trial of a harness at a frozen task and its trajectory,
  * the newest [[BenchRows.Kept]] trials alone. `blobs` must be the bench's own, one store per
  * database: [[prune]] removes each of its blobs no trajectory names, as a file area's sweep does
  * by its files, so two such owners of one store would each remove the other's.
  */
final class BenchRows(blobs: Blobs^) {

  /** `trial` recorded; then every trial but the newest [[BenchRows.Kept]] recorded removed with
    * its trajectory's row, never its blob ([[prune]]). [[BenchRows.Recorded.Repeat]], writing
    * nothing, when `trial`'s session has a trial already. Waits on any other record or prune
    * until the transaction ends.
    */
  def record(trial: BenchRows.Trial)(using Tx^): Either[StoreError, BenchRows.Recorded] =
    BenchRows.begun(trial).map(_.fold(BenchRows.Recorded.Repeat)(_ => BenchRows.Recorded.Fresh(0)))

  /** As `record(trial)`, with `transcript` kept as its trajectory, `clipped` when the transcript
    * ran past what was received, kept at `label` joined with this transaction's floor (ADR 0030:
    * what a transaction writes is kept at least where it writes); on a repeat `transcript` is not
    * kept. `Left(Invalid)`, recording nothing, when `transcript` holds more than
    * [[BenchRows.Longest]] bytes; `Left(DatabaseError)` when it cannot be kept.
    */
  def record(trial: BenchRows.Trial, transcript: Pending^, clipped: Boolean, label: Label)(using
      Tx^
  ): Either[StoreError, BenchRows.Recorded] =
    if (transcript.bytes > BenchRows.Longest)
      Left(
        StoreError.Invalid(
          s"a trajectory holds at most ${BenchRows.Longest} bytes, not ${transcript.bytes}"
        )
      )
    else {
      BenchRows.begun(trial).flatMap {
        case None => Right(BenchRows.Recorded.Repeat)
        case Some(id) =>
          for {
            _ <- transcript.keep().left.map(e => StoreError.DatabaseError(s"its transcript: $e"))
            _ <- BenchRows.trajectoryRow(id, transcript.sha256, transcript.bytes, clipped, label)
          } yield BenchRows.Recorded.Fresh(0)
      }
    }

  /** Every trial kept, oldest recorded first. */
  def trials()(using Tx^): Either[StoreError, Vector[BenchRows.Trial]] =
    SqlStatements
      .many("SELECT * FROM grit.bench_trials ORDER BY id")(_ => ())(BenchRows.trialOf)
      .flatMap(SqlStatements.valid)

  /** `session`'s trial's trajectory, when it has one this transaction reads. */
  def trajectory(session: SessionId)(using Tx^): Either[StoreError, Option[BenchRows.Trajectory]] =
    SqlBenchRows
      .trajectory(Harness.value(session.harness), session.local)(BenchRows.trajectoryOf)
      .flatMap(found => SqlStatements.valid(found.toVector))
      .map(_.headOption)
}

object BenchRows {

  /** 60: the most trials kept. */
  val Kept: Int = 60

  /** 32 MiB: the most bytes of a transcript a trajectory keeps. */
  val Longest: Long = 32L * 1024 * 1024

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
    * `None`: it made no edit); whether the task's checks passed; and how many lines its diff
    * differs from the task's known diff by (`None`: it left none).
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
      distance: Option[Long]
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
        distance: Option[Long]
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
            distance
          )
        )
    }
  }

  /** A trial's trajectory as kept: its harness's transcript as it wrote it, the blob `sha256` of
    * `bytes` bytes, `clipped` when the transcript ran past [[Longest]]; read by whom `label`
    * clears.
    */
  final case class Trajectory private[dbos] (
      sha256: Sha256,
      bytes: Long,
      clipped: Boolean,
      label: Label
  ) extends caps.Pure {

    /** [[Integrity.Member]], every trajectory's: the repository a trial works in is its
      * operator's own, as the coding tools' results are.
      */
    def integrity: Integrity = Integrity.Member
  }

  enum Recorded {

    /** Recorded, and `removed` older trials removed past [[Kept]]. */
    case Fresh(removed: Int)
    case Repeat
  }

  /** The advisory lock every record and prune takes until its transaction ends. */
  private val Lock = 0x67726974_62656e63L

  private def lock()(using Tx^): Either[StoreError, Unit] =
    SqlStatements.one("SELECT pg_advisory_xact_lock(?)")(_.setLong(1, Lock))(_ => ()).map(_ => ())

  /** Under the bench's lock, `trial`'s row inserted, its id; `None` when its session has one. */
  private def begun(trial: Trial)(using Tx^): Either[StoreError, Option[Long]] =
    lock().flatMap(_ => trialRow(trial))

  /** `trial`'s row inserted, its id; `None` when its session has one. */
  private def trialRow(trial: Trial)(using Tx^): Either[StoreError, Option[Long]] =
    SqlStatements.one(
      """INSERT INTO grit.bench_trials
        |       (harness, local, task, model, input_tokens, cache_read_tokens, cache_write_tokens,
        |        output_tokens, cost_usd, calls, wall_ms, read_before_edit, calls_before_edit,
        |        passed, distance)
        |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
      ps.setBigDecimal(9, u.costUsd.map(_.bigDecimal).orNull)
      ps.setInt(10, trial.calls)
      ps.setLong(11, trial.wall.toMillis)
      ps.setObject(12, trial.readBeforeEdit.map(Long.box).orNull)
      ps.setObject(13, trial.callsBeforeEdit.map(Int.box).orNull)
      ps.setBoolean(14, trial.passed)
      ps.setObject(15, trial.distance.map(Long.box).orNull)
    }(_.getLong("id"))

  /** The trajectory row of trial `id`, at `label` joined with the transaction's floor. */
  private def trajectoryRow(id: Long, sha: Sha256, bytes: Long, clipped: Boolean, label: Label)(
      using tx: Tx^
  ): Either[StoreError, Unit] = {
    val kept = label.join(Tx.clearance(tx).floor)
    SqlStatements
      .update(
        s"""INSERT INTO grit.bench_trajectories (trial_id, sha256, bytes, clipped, classification_id)
           |VALUES (?, ?, ?, ?, ${SqlLabels.Interned})""".stripMargin
      ) { ps =>
        ps.setLong(1, id)
        ps.setString(2, sha.hex)
        ps.setLong(3, bytes)
        ps.setBoolean(4, clipped)
        SqlLabels.bind(ps, 5, kept)
      }
      .map(_ => ())
  }

  private def trialOf(rs: ResultSet): Either[String, Trial] = {
    def maybeLong(c: String) = Option(rs.getObject(c, classOf[java.lang.Long])).map(_.longValue)
    for {
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
          Tokens(rs.getLong("cache_write_tokens"))
        ),
        rs.getInt("calls"),
        rs.getLong("wall_ms").millis,
        maybeLong("read_before_edit"),
        Option(rs.getObject("calls_before_edit", classOf[java.lang.Integer])).map(_.intValue),
        rs.getBoolean("passed"),
        maybeLong("distance")
      )
    } yield r
  }

  private def trajectoryOf(rs: ResultSet): Either[String, Trajectory] =
    Sha256
      .read(rs.getString("sha256"))
      .toRight(s"no digest: ${rs.getString("sha256")}")
      .map(Trajectory(_, rs.getLong("bytes"), rs.getBoolean("clipped"), SqlLabels.read(rs)))
}
