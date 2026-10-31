package grit.eval.harness.bench

import java.io.InputStream
import java.util.concurrent.atomic.AtomicBoolean

import grit.core.host.Blobs
import grit.core.store.Jot
import grit.core.visibility.{Label, Subject}
import grit.dbos.internal.BenchRows

/** The bench's keeping: a trial and its transcript into [[BenchRows]] over the bench's own blobs. */
object Bench {

  /** `trial` kept in `blobs`' rows for `subject`. `Left` with the cause, for the operator, when
    * the database fails or refuses the trial.
    */
  def keep(jot: Jot^, blobs: Blobs^)(
      subject: Subject,
      trial: BenchRows.Trial
  ): Either[String, BenchRows.Recorded] =
    jot.write(subject)(BenchRows(blobs).record(trial)).left.map(e => s"the trial: ${e.described}")

  /** `trial` kept in `blobs`' rows for `subject`, with `transcript` as its trajectory, labelled
    * `label`, read to [[BenchRows.Longest]] bytes and one past them. `Left` with the cause, for
    * the operator, when the database, the machine or the transcript fails, or the database
    * refuses the trial.
    */
  def keep(jot: Jot^, blobs: Blobs^)(
      subject: Subject,
      trial: BenchRows.Trial,
      transcript: InputStream^,
      label: Label
  ): Either[String, BenchRows.Recorded] = {
    // Apart from the stream: the body reads it while `receive` holds the stream.
    val clipped = new AtomicBoolean(false)
    val rows = BenchRows(blobs)
    blobs
      .receive(Clip(transcript, BenchRows.Longest, clipped), BenchRows.Longest) { pending =>
        jot.write(subject)(rows.record(trial, pending, clipped.get, label))
      }
      .left
      .map(e => s"the transcript: $e")
      .flatMap(_.left.map(e => s"the trial: ${e.described}"))
  }

  /** At most `max` bytes of `in`, read one at a time; `clipped` set once a byte past them was
    * read.
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
  }
}
