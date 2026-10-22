package grit.core.files

import java.io.InputStream

import scala.concurrent.duration.*

import grit.core.id.FileHandle
import grit.core.store.{StoreError, Tx}

/** Bytes arriving from a source, read once. */
trait Incoming {

  /** The size its source states, when it states one. */
  def declared: Option[Long]

  /** `body` over a stream of its bytes, open only within it; `Left` with the source's words, in
    * plain words for the model, when the source cannot give them.
    */
  def read[A](body: InputStream^ => A): Either[String, A]
}

/** The file area as an edge puts into it. */
trait Intake extends caps.SharedCapability {

  /** How long a file put now is kept: the area's expiry. */
  def keeps: FiniteDuration

  /** What a file kept here is read with: the deployment's formats, the same [[Files.formats]]. */
  def formats: Formats

  /** `incoming`'s bytes kept as a file named `name` from `origin`, opened by whoever reads
    * `item`, expiring the area's expiry after the put; then `body` over it opened, before this
    * returns. When a file put under `key` is still kept, that file, nothing fetched, `body` run
    * over it; a key whose file expired puts afresh. Refused, keeping nothing, by the area's
    * bounds ([[FileArea.Bounds]]): [[FileError.TooLarge]] past the most one file may have
    * (before a byte is read when `declared` says so); [[FileError.Full]] when the files kept and
    * this one would pass the most they may have together;
    * [[FileError.Fetch]] with the source's words; [[FileError.Disk]] or [[FileError.Store]] when
    * this machine or the database fails; [[FileError.Gone]] when the file under `key` lost its
    * bytes.
    */
  def put[A](
      key: PutKey,
      name: String,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      incoming: Incoming^
  )(body: OpenFile^ => A): Either[FileError, (FileRecord, A)]
}

/** A file a transaction was found to read, for [[Files.open]]. It has no stored form, so no
  * step records it: admit and open within one step.
  */
final case class Admitted private[grit] (record: FileRecord)

/** The file area as a turn, a job or a sync reads it. */
trait Files extends caps.SharedCapability {

  /** What a file kept here is read with: the deployment's formats, the same an edge's
    * [[Intake.formats]] read it with when it was put.
    */
  def formats: Formats

  /** `handle`'s file, when it is kept, not expired, and this transaction reads its item
    * ([[Tx.readsItem]]); otherwise [[FileError.Gone]], never saying which; [[FileError.Store]]
    * when the database fails.
    */
  def admit(handle: FileHandle)(using Tx^): Either[FileError, Admitted]

  /** `body` over `admitted`'s file opened, holding no transaction; [[FileError.Gone]] when its
    * bytes are no longer kept.
    */
  def open[A](admitted: Admitted)(body: OpenFile^ => A): Either[FileError, A]
}

object Files {

  /** How long a file is kept after its put, unless the area is built with its own: 24 hours. */
  val Expiry: FiniteDuration = 24.hours

  /** The most bytes one file may have: 100 MiB. A larger one is refused, nothing kept. */
  val MaxFile: Long = 100L * 1024 * 1024

  /** The most bytes the files kept may have together, each blob counted once: 2 GiB. A put past
    * it is refused, nothing evicted.
    */
  val MaxTotal: Long = 2L * 1024 * 1024 * 1024
}

/** Why a file was not kept or opened. [[message]] is what the model reads. */
enum FileError derives CanEqual {

  /** No longer kept, or never readable here: the area keeps files `keptFor` after their put. */
  case Gone(handle: FileHandle, keptFor: FiniteDuration)

  /** `bytes` (when known) past the `max` a file may have. */
  case TooLarge(bytes: Option[Long], max: Long)

  /** The files kept have no room for it. */
  case Full

  /** Its source did not give its bytes: `why`, in plain words. */
  case Fetch(why: String)

  /** This machine did not keep its bytes: `why`, for the operator. */
  case Disk(why: String)

  /** The database failed. */
  case Store(error: StoreError)

  def message: String = this match {
    case Gone(_, keptFor) =>
      s"That file is no longer kept (files are kept ${FileError.span(keptFor)}); " +
        "read it again from where it came from."
    case TooLarge(bytes, max) =>
      s"That file is ${bytes.fold("larger than")(b => s"${FileError.mb(b)}, over")} the " +
        s"${FileError.mb(max)} a file kept here may be."
    case Full => "Too many files are kept here just now to keep this one; try again in a while."
    case Fetch(why) => why
    case Disk(_) | Store(_) => "The file could not be kept here just now; try again in a while."
  }
}

object FileError {

  /** `bytes` in whole megabytes (MiB), rounded up: "100 MB". */
  private def mb(bytes: Long): String = s"${(bytes + (1L << 20) - 1) >> 20} MB"

  /** `d` in its largest whole unit up to hours: "for 24 hours", "for 10 minutes". */
  def span(d: FiniteDuration): String = {
    val (n, unit) =
      if (d.toHours >= 1 && d.toMinutes % 60 == 0) (d.toHours, "hour")
      else if (d.toMinutes >= 1 && d.toSeconds % 60 == 0) (d.toMinutes, "minute")
      else (d.toSeconds, "second")
    s"for $n $unit${if (n == 1) "" else "s"}"
  }
}
