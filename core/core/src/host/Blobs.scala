package grit.core.host

import java.io.InputStream
import java.nio.file.Path
import java.time.Instant

import grit.core.id.Sha256

/** Bytes kept on this machine, each blob under its [[Sha256]]: a blob is whole or absent. */
trait Blobs extends caps.SharedCapability {

  /** `in` read to its end into a pending blob, then `body` over it, which may keep it
    * ([[Pending.keep]]); a pending blob not kept is gone when this returns. Nothing is kept, and
    * `body` is not run: [[BlobError.TooLarge]] when `in` holds more than `max` bytes (read to
    * one past `max`, no further); [[BlobError.Source]] when `in` fails; [[BlobError.Failed]]
    * when the machine refuses.
    */
  def receive[A](in: InputStream^, max: Long)(body: Pending^ => A): Either[BlobError, A]

  /** Where `sha256`'s blob is, read-only; `None` when it is not kept. */
  def at(sha256: Sha256): Option[Path]

  /** Every blob kept. */
  def kept(): Either[BlobError, Vector[Sha256]]

  /** `sha256`'s blob removed; nothing when it is not kept. */
  def remove(sha256: Sha256): Either[BlobError, Unit]

  /** Every blob kept but those in `named` removed, how many. `Left` when the machine fails
    * partway: the blobs removed before it stay removed, the rest stay kept.
    */
  final def removeAllBut(named: Set[Sha256]): Either[BlobError, Int] =
    kept().flatMap(
      _.filterNot(named)
        .foldLeft[Either[BlobError, Int]](Right(0))((n, sha) =>
          n.flatMap(k => remove(sha).map(_ => k + 1))
        )
    )

  /** Every pending blob last written before `before` removed, how many: what a receive cut short
    * by a crash left. One still being written is newer.
    */
  def clearPending(before: Instant): Either[BlobError, Int]
}

/** Bytes received and not yet kept, valid only within the block [[Blobs.receive]] runs. */
trait Pending extends caps.SharedCapability {

  /** Their digest. */
  def sha256: Sha256

  /** How many. */
  def bytes: Long

  /** Kept as [[sha256]]'s blob, at once whole; a blob already kept under it is left as it is. */
  def keep(): Either[BlobError, Unit]
}

/** Why bytes were not kept. */
enum BlobError derives CanEqual {

  /** More than `max` bytes arrived. */
  case TooLarge(max: Long)

  /** The bytes' source failed partway: `why`, its own words. */
  case Source(why: String)

  /** The machine refused: `why`, its own words. */
  case Failed(why: String)
}
