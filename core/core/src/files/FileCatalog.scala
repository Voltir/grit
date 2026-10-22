package grit.core.files

import java.time.Instant

import grit.core.id.{FileHandle, Sha256}
import grit.core.store.{StoreError, Tx}

/** What the file area keeps, as rows: one a put, naming its blob by digest. Expired rows are
  * kept until the collector forgets them.
  */
trait FileCatalog {

  /** The area's lock, held until the transaction ends: one put's keeping, or one sweep, at a
    * time.
    */
  def lock()(using Tx^): Either[StoreError, Unit]

  /** The file put under `key`, expired or not. */
  def keyed(key: PutKey)(using Tx^): Either[StoreError, Option[FileRecord]]

  /** `handle`'s file, expired or not. */
  def get(handle: FileHandle)(using Tx^): Either[StoreError, Option[FileRecord]]

  /** Every file kept, expired or not. */
  def all()(using Tx^): Either[StoreError, Vector[FileRecord]]

  /** The size of each blob a file names, each once. */
  def blobs()(using Tx^): Either[StoreError, Map[Sha256, Long]]

  /** A file kept under a new handle, as the arguments say, and put under `key`, which any file
    * put under it before gives up.
    */
  def insert(
      key: PutKey,
      name: String,
      bytes: Long,
      sha256: Sha256,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      expires: Instant
  )(using Tx^): Either[StoreError, FileRecord]

  /** `handle`'s row deleted; nothing when there is none. */
  def forget(handle: FileHandle)(using Tx^): Either[StoreError, Unit]
}
