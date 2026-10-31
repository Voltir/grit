package grit.eval.harness.bench

import java.io.InputStream
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.host.Blobs
import grit.core.store.Jot
import grit.core.visibility.{Label, Subject}
import grit.dbos.internal.BenchRows

/** The bench's keeping: a trial and its transcript into [[BenchRows]] over the bench's own blobs,
  * oldest out.
  */
object Bench {

  /** How long after its last write a pending blob counts as abandoned: what a rig killed
    * mid-receive left.
    */
  val Abandoned: FiniteDuration = 1.hour

  /** `trial` kept in `blobs`' rows for `subject`; then the rows pruned of what this and any
    * earlier keep left ([[BenchRows.prune]], pending blobs [[Abandoned]] by `clock`'s time).
    * `Left` with the cause, for the operator, when the database or the machine fails, or the
    * database refuses the trial. A trial recorded before a later failure stays recorded, and the
    * next keep prunes what this one left.
    */
  def keep(jot: Jot^, blobs: Blobs^, clock: Clock^)(
      subject: Subject,
      trial: BenchRows.Trial
  ): Either[String, (BenchRows.Recorded, BenchRows.Pruned)] =
    jot
      .write(subject)(BenchRows(blobs).record(trial))
      .left
      .map(e => s"the trial: ${e.described}")
      .flatMap(r => pruned(jot, blobs, clock)(subject).map((r, _)))

  /** As the keep above, with `transcript` as `trial`'s trajectory, labelled `label`, read to
    * [[BenchRows.Longest]] bytes and one past them; `Left` also when the transcript fails.
    */
  def keep(jot: Jot^, blobs: Blobs^, clock: Clock^)(
      subject: Subject,
      trial: BenchRows.Trial,
      transcript: InputStream^,
      label: Label
  ): Either[String, (BenchRows.Recorded, BenchRows.Pruned)] = {
    // Untracked mutable state, held apart from the stream so `receive`'s body may read it while
    // `receive` holds the stream. No one else can see it change: it is made in this call,
    // reaches only this call's Clip and body, and never leaves `keep`; Clip sets it only while
    // `receive` reads the stream to its end, and the body reads it only after that, on the same
    // thread, so the body reads the final value.
    val clipped = new AtomicBoolean(false)
    val rows = BenchRows(blobs)
    blobs
      .receive(Clip(transcript, BenchRows.Longest, clipped), BenchRows.Longest) { pending =>
        jot.write(subject)(rows.record(trial, pending, clipped.get, label))
      }
      .left
      .map(e => s"the transcript: $e")
      .flatMap(_.left.map(e => s"the trial: ${e.described}"))
      .flatMap(r => pruned(jot, blobs, clock)(subject).map((r, _)))
  }

  /** `blobs`' rows pruned for `subject`, pending blobs [[Abandoned]] by `clock`'s time. */
  private def pruned(jot: Jot^, blobs: Blobs^, clock: Clock^)(
      subject: Subject
  ): Either[String, BenchRows.Pruned] = {
    val before = clock.now().minusMillis(Abandoned.toMillis)
    jot
      .write(subject)(BenchRows(blobs).prune(before))
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
