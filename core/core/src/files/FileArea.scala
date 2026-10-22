package grit.core.files

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.host.{BlobError, Blobs}
import grit.core.id.FileHandle
import grit.core.retention.Target
import grit.core.store.{Jot, StoreError, Tombstones, Tx}
import grit.core.visibility.Subject

/** The file area (ADR 0045): files listed in `catalog`, their bytes in `blobs`, each put
  * written in a short transaction of `jot`'s, dated by `clock`, read with `formats`, kept as
  * `bounds` says.
  */
final class FileArea(
    catalog: FileCatalog,
    blobs: Blobs^,
    jot: Jot^,
    clock: Clock^,
    val formats: Formats,
    val bounds: FileArea.Bounds = FileArea.Bounds.Default
) extends Files,
      Intake {

  def keeps: FiniteDuration = bounds.expiry

  def put[A](
      key: PutKey,
      name: String,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      incoming: Incoming^
  )(body: OpenFile^ => A): Either[FileError, (FileRecord, A)] =
    jot
      .write(Subject.Public)(kept(key, origin, item))
      .left
      .map(FileError.Store(_))
      .flatMap {
        case Some(record) => Right(record)
        case None =>
          incoming.declared.filter(_ > bounds.maxFile) match {
            case Some(size) => Left(FileError.TooLarge(Some(size), bounds.maxFile))
            case None => fetched(key, name, claimed, origin, item, incoming)
          }
      }
      .flatMap(record => opened(record)(body).map(record -> _))

  def admit(handle: FileHandle)(using Tx^): Either[FileError, Admitted] =
    catalog.get(handle).left.map(FileError.Store(_)).flatMap {
      case Some(r) if live(r) && Tx.readsItem(r.item.item.place, r.item.readers) =>
        Right(Admitted(r))
      case _ => Left(gone(handle))
    }

  def open[A](admitted: Admitted)(body: OpenFile^ => A): Either[FileError, A] =
    opened(admitted.record)(body)

  /** One sweep of the area at its clock's time, under its lock: each file expired or whose bytes
    * are lost marked for deletion in `tombstones` ([[Target.File]]; the collector forgets it),
    * each blob no file names removed, and each pending blob last written [[FileArea.Abandoned]]
    * ago or more (what a crash left) cleared. `Left` with the cause, for the operator, when the
    * database or this machine fails.
    */
  def sweep(tombstones: Tombstones): Either[String, FileArea.Swept] = {
    val now = clock.now()
    jot
      .write(Subject.Public) {
        for {
          _ <- catalog.lock()
          files <- catalog.all()
          marked <- files
            .filter(r => !r.expires.isAfter(now) || blobs.at(r.sha256).isEmpty)
            .foldLeft[Either[StoreError, Vector[FileHandle]]](Right(Vector.empty)) { (acc, r) =>
              acc.flatMap(done =>
                tombstones
                  .write(Target.File(r.handle), now)
                  .map(fresh => if (fresh) done :+ r.handle else done)
              )
            }
          // Under the lock, so no put is keeping a blob its row does not name yet.
          named = files.map(_.sha256).toSet
          removed = blobs.kept().flatMap { kept =>
            kept
              .filterNot(named)
              .foldLeft[Either[BlobError, Int]](Right(0))((n, sha) =>
                n.flatMap(k => blobs.remove(sha).map(_ => k + 1))
              )
          }
        } yield (marked, removed)
      }
      .left
      .map(e => s"the file area could not be swept: $e")
      .flatMap { (done: (Vector[FileHandle], Either[BlobError, Int])) =>
        val (marked, removed) = done
        for {
          r <- removed.left.map(e => s"the file area's blobs could not be removed: $e")
          c <- blobs
            .clearPending(now.minusMillis(FileArea.Abandoned.toMillis))
            .left
            .map(e => s"the file area's pending blobs could not be cleared: $e")
        } yield FileArea.Swept(marked, r, c)
      }
  }

  /** `incoming`'s bytes received and kept, and its row written under the area's lock, unless a
    * live file was put under `key` meanwhile, which is answered instead.
    */
  private def fetched(
      key: PutKey,
      name: String,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      incoming: Incoming^
  ): Either[FileError, FileRecord] =
    incoming
      .read { in =>
        blobs.receive(in, bounds.maxFile) { pending =>
          // The blob is kept under the lock, before its row, so a sweep (which takes the lock)
          // never removes it as named by no row, and a row is never written without its blob.
          jot.write(Subject.Public) {
            for {
              _ <- catalog.lock()
              again <- kept(key, origin, item)
              stored <- catalog.blobs()
              kept <- again match {
                case Some(r) => Right(Right(r))
                case None
                    if !stored.contains(pending.sha256) &&
                      stored.values.sum + pending.bytes > bounds.maxTotal =>
                  Right(Left(FileError.Full))
                case None =>
                  pending.keep() match {
                    case Left(e) => Right(Left(FileArea.unkept(e)))
                    case Right(()) =>
                      catalog
                        .insert(
                          key,
                          name,
                          pending.bytes,
                          pending.sha256,
                          claimed,
                          origin,
                          item,
                          clock.now().plusMillis(bounds.expiry.toMillis)
                        )
                        .map(Right(_))
                  }
              }
            } yield kept
          }
        }
      }
      .left
      .map(FileError.Fetch(_))
      .flatMap(_.left.map(FileArea.unkept(_)))
      .flatMap(_.left.map(FileError.Store(_)))
      .flatten

  /** `body` over `record`'s file opened; gone when its bytes are not kept. */
  private def opened[A](record: FileRecord)(body: OpenFile^ => A): Either[FileError, A] =
    blobs
      .at(record.sha256)
      .toRight(gone(record.handle))
      .flatMap(path =>
        OpenFile
          .reading(path, record.name, record.claimed, clock)(body)
          .left
          .map(_ => gone(record.handle))
      )

  /** The live file a put under `key`, of `origin` at `item`, answers: the one put under `key`,
    * else the latest of the same snapshot (`origin`'s edge, ref and revision, when it has one)
    * kept at the same item whose bytes are still here; `None` when there is neither.
    */
  private def kept(key: PutKey, origin: FileOrigin, item: FileItem)(using
      Tx^
  ): Either[StoreError, Option[FileRecord]] =
    catalog.keyed(key).flatMap {
      case Some(r) if live(r) => Right(Some(r))
      case _ =>
        origin.revision
          .fold(Right(Vector.empty))(catalog.snapshots(origin.edge, origin.ref, _))
          .map(_.filter(r => live(r) && r.item == item && blobs.at(r.sha256).nonEmpty).lastOption)
    }

  private def live(record: FileRecord): Boolean = record.expires.isAfter(clock.now())

  private def gone(handle: FileHandle): FileError = FileError.Gone(handle, bounds.expiry)
}

object FileArea {

  /** How long a file is kept after its put, the most bytes one may have, and the most the files
    * kept may have together, each blob counted once.
    */
  final case class Bounds(expiry: FiniteDuration, maxFile: Long, maxTotal: Long)

  object Bounds {

    /** [[Files.Expiry]], [[Files.MaxFile]] and [[Files.MaxTotal]]. */
    val Default: Bounds = Bounds(Files.Expiry, Files.MaxFile, Files.MaxTotal)
  }

  /** What one [[FileArea.sweep]] did: the files it newly marked, oldest first; how many blobs it
    * removed; how many pending blobs it cleared.
    */
  final case class Swept(marked: Vector[FileHandle], removed: Int, cleared: Int)

  /** How long ago a pending blob was last written for a sweep to clear it: an hour. A download
    * still arriving writes more often than this.
    */
  val Abandoned: FiniteDuration = 1.hour

  /** Why bytes were not kept, as a put answers it. */
  private def unkept(e: BlobError): FileError = e match {
    case BlobError.TooLarge(max) => FileError.TooLarge(None, max)
    case BlobError.Source(_) =>
      FileError.Fetch("The file's source stopped sending it partway; try again.")
    case BlobError.Failed(why) => FileError.Disk(why)
  }
}
