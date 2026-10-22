package grit.core.files

import scala.concurrent.duration.FiniteDuration

import grit.core.clock.Clock
import grit.core.host.{BlobError, Blobs}
import grit.core.id.FileHandle
import grit.core.store.{Jot, Tx}
import grit.core.visibility.Subject

/** The file area (ADR 0045): files listed in `catalog`, their bytes in `blobs`, each put
  * written in a short transaction of `jot`'s, dated by `clock`, kept as `bounds` says.
  */
final class FileArea(
    catalog: FileCatalog,
    blobs: Blobs^,
    jot: Jot^,
    clock: Clock^,
    val bounds: FileArea.Bounds = FileArea.Bounds.Default
) extends Files,
      Intake {

  def put[A](
      key: PutKey,
      name: String,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      incoming: Incoming^
  )(body: OpenFile^ => A): Either[FileError, (FileRecord, A)] =
    jot
      .write(Subject.Public)(catalog.keyed(key))
      .left
      .map(FileError.Store(_))
      .flatMap {
        case Some(record) if live(record) => Right(record)
        case _ =>
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
              again <- catalog.keyed(key)
              stored <- catalog.blobs()
              kept <- again.filter(live) match {
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

  /** Why bytes were not kept, as a put answers it. */
  private def unkept(e: BlobError): FileError = e match {
    case BlobError.TooLarge(max) => FileError.TooLarge(None, max)
    case BlobError.Source(_) =>
      FileError.Fetch("The file's source stopped sending it partway; try again.")
    case BlobError.Failed(why) => FileError.Disk(why)
  }
}
