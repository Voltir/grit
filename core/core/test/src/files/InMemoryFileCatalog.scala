package grit.core.files

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.id.{EdgeName, FileHandle, Sha256}
import grit.core.store.{StoreError, Tx}

/** An in-memory [[FileCatalog]] for tests, keeping [[FilesContract]] under a [[FileArea]]. It
  * ignores the `Tx`, and its lock holds nothing: its tests put one at a time.
  */
final class InMemoryFileCatalog extends FileCatalog {

  // Replaced whole by its own calls, on the test's one thread; nothing else holds it.
  @caps.unsafe.untrackedCaptures
  private var rows = VectorMap.empty[FileHandle, (Option[PutKey], FileRecord)]

  // Moved only by `insert`, on the test's one thread.
  @caps.unsafe.untrackedCaptures
  private var next = 1L

  def lock()(using Tx^): Either[StoreError, Unit] = Right(())

  def keyed(key: PutKey)(using Tx^): Either[StoreError, Option[FileRecord]] =
    Right(rows.values.collectFirst { case (Some(k), r) if k == key => r })

  def snapshots(edge: EdgeName, ref: String, revision: String)(using
      Tx^
  ): Either[StoreError, Vector[FileRecord]] =
    Right(rows.values.map(_._2).toVector.filter { r =>
      r.origin.edge == edge && r.origin.ref == ref && r.origin.revision.contains(revision)
    })

  def get(handle: FileHandle)(using Tx^): Either[StoreError, Option[FileRecord]] =
    Right(rows.get(handle).map(_._2))

  def all()(using Tx^): Either[StoreError, Vector[FileRecord]] = Right(
    rows.values.map(_._2).toVector
  )

  def blobs()(using Tx^): Either[StoreError, Map[Sha256, Long]] =
    Right(rows.values.map((_, r) => r.sha256 -> r.bytes).toMap)

  def insert(
      key: PutKey,
      name: String,
      bytes: Long,
      sha256: Sha256,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      expires: Instant
  )(using Tx^): Either[StoreError, FileRecord] =
    FileHandle.of(next).toRight(StoreError.Invalid(s"handle $next")).map { handle =>
      next += 1
      val record = FileRecord(handle, name, bytes, sha256, claimed, origin, item, expires)
      rows = rows
        .map { case (h, (k, r)) => h -> (k.filter(_ != key), r) } + (handle -> (Some(key), record))
      record
    }

  def forget(handle: FileHandle)(using Tx^): Either[StoreError, Unit] = {
    rows = rows - handle
    Right(())
  }
}
