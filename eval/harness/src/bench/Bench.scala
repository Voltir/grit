package grit.eval.harness.bench

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.host.Blobs
import grit.core.id.SessionId
import grit.core.store.Jot
import grit.core.visibility.{Label, Subject}
import grit.dbos.internal.{Reader, TrialRows}

/** The bench's keeping: a trial and its transcript into [[TrialRows]] over the bench's own blobs,
  * oldest out.
  */
object Bench {

  /** How long after its last write a pending blob counts as abandoned: what a rig killed
    * mid-receive left.
    */
  val Abandoned: FiniteDuration = 1.hour

  /** Every trial `reader`'s database keeps, oldest first, read for `subject`. `Left` with the
    * cause, for the operator, when the database fails.
    */
  def trials(reader: Reader^)(subject: Subject): Either[String, Vector[TrialRows.Trial]] =
    reader.db.read(subject)(TrialRows.trials()).left.map(e => s"the trials: ${e.described}")

  /** `trial` kept in `blobs`' rows for `subject`, with `standing`, the standing text whose digest
    * is `trial.standing` ([[TrialRows.record]]), and `transcript` and `diff` as its parts,
    * labelled `label`, each read to [[TrialRows.MostBytes]] bytes and one past them; then the rows
    * pruned of what this and any earlier keep left ([[TrialRows.prune]], pending blobs
    * [[Abandoned]] by `clock`'s time). `Left` with the cause, for the operator, when the
    * database, the machine, the transcript or the diff fails, or the database refuses the trial.
    * A trial recorded before a later failure stays recorded, and the next keep prunes what this
    * one left.
    */
  def keep(jot: Jot^, blobs: Blobs^, clock: Clock^)(
      subject: Subject,
      trial: TrialRows.Trial,
      standing: Option[String],
      transcript: InputStream^,
      diff: InputStream^,
      label: Label
  ): Either[String, (TrialRows.Recorded, TrialRows.Pruned)] = {
    // Untracked mutable state, held apart from each stream so `receive`'s body may read it while
    // `receive` holds the stream. No one else can see it change: each is made in this call,
    // reaches only this call's Clip and bodies, and never leaves `keep`; its Clip sets it only
    // while its `receive` reads the stream to its end, and the bodies read it only after that, on
    // the same thread, so they read the final value.
    val transcriptClipped = new AtomicBoolean(false)
    val diffClipped = new AtomicBoolean(false)
    val rows = TrialRows(blobs)
    blobs
      .receive(Clip(transcript, TrialRows.MostBytes, transcriptClipped), TrialRows.MostBytes) {
        kept =>
          blobs
            .receive(Clip(diff, TrialRows.MostBytes, diffClipped), TrialRows.MostBytes) { changes =>
              val clipped = Set(
                Option.when(transcriptClipped.get)(TrialRows.Part.Transcript),
                Option.when(diffClipped.get)(TrialRows.Part.Diff)
              ).flatten
              jot.write(subject)(rows.record(trial, standing, kept, changes, clipped, label))
            }
            .left
            .map(e => s"the diff: $e")
      }
      .left
      .map(e => s"the transcript: $e")
      .flatMap(identity)
      .flatMap(_.left.map(e => s"the trial: ${e.described}"))
      .flatMap(r => pruned(jot, blobs, clock)(subject).map((r, _)))
  }

  /** A trial's transcript as the bench kept it: its text, and whether it was cut at
    * [[TrialRows.MostBytes]].
    */
  final case class Transcript(text: String, clipped: Boolean) extends caps.Pure

  /** `session`'s trial's transcript, read for `subject` from `reader`'s rows and `blobs`, whole;
    * `None` when the session has no trial, or kept none `subject` may read. `Left` with the
    * cause, for the operator, when the database fails, or the blob its row names is missing or
    * not UTF-8.
    */
  def trajectory(reader: Reader^, blobs: Blobs^)(
      subject: Subject,
      session: SessionId
  ): Either[String, Option[Transcript]] = {
    val rows = TrialRows(blobs)
    reader.db
      .read(subject)(rows.kept(session, TrialRows.Part.Transcript))
      .left
      .map(e => s"the transcript: ${e.described}")
      .flatMap {
        case None => Right(None)
        case Some(kept) =>
          for {
            path <- blobs
              .at(kept.sha256)
              .toRight(s"the transcript's blob ${kept.sha256.hex} is not kept")
            text <- scala.util
              .Try(
                StandardCharsets.UTF_8
                  .newDecoder()
                  .onMalformedInput(CodingErrorAction.REPORT)
                  .onUnmappableCharacter(CodingErrorAction.REPORT)
                  .decode(ByteBuffer.wrap(Files.readAllBytes(path)))
                  .toString
              )
              .toEither
              .left
              .map(e => s"the transcript's blob ${kept.sha256.hex} does not read as UTF-8: $e")
          } yield Some(Transcript(text, kept.clipped))
      }
  }

  /** `blobs`' rows pruned for `subject`, pending blobs [[Abandoned]] by `clock`'s time. */
  private def pruned(jot: Jot^, blobs: Blobs^, clock: Clock^)(
      subject: Subject
  ): Either[String, TrialRows.Pruned] = {
    val before = clock.now().minusMillis(Abandoned.toMillis)
    jot
      .write(subject)(TrialRows(blobs).prune(before))
      .left
      .map(e => s"the prune: ${e.described}")
      .flatMap(_.left.map(e => s"the prune's blobs: $e"))
  }

  /** At most `max` bytes of `in`, a bulk read taking as many of them as it asks for; `clipped`
    * set once a byte past them was read.
    */
  private[bench] final class Clip(in: InputStream^, max: Long, clipped: AtomicBoolean)
      extends InputStream {
    // A count, moved only by this stream's own reads, which one reader makes in turn: no
    // capability can pass through it.
    @caps.unsafe.untrackedCaptures
    private var taken: Long = 0L

    override def read(): Int =
      if (clipped.get) -1
      else if (taken < max) {
        val b = in.read()
        if (b >= 0) taken += 1
        b
      } else {
        clipped.set(in.read() >= 0)
        -1
      }

    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if (len == 0) 0
      else if (clipped.get) -1
      else if (taken < max) {
        // Cast: `b` is the buffer this read's caller handed it, passed on to `in.read`, which
        // fills it before returning, as InputStream's contract has it; this stream keeps no
        // reference to it, so no write to it can come from here after the call.
        val n = in.read(b.asInstanceOf[Array[Byte]], off, math.min(len.toLong, max - taken).toInt)
        if (n > 0) taken += n
        n
      } else read()
  }
}
