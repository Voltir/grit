package grit.core.files

import grit.core.id.FileHandle
import grit.core.store.Tx

/** An area that keeps no file, for tests that read none: every admit is [[FileError.Gone]]. */
class NoFiles extends Files {
  val formats: Formats = Formats(_ => Vector.empty)
  def admit(handle: FileHandle)(using Tx^): Either[FileError, Admitted] =
    Left(FileError.Gone(handle, Files.Expiry))
  def open[A](admitted: Admitted)(body: OpenFile^ => A): Either[FileError, A] =
    Left(FileError.Gone(admitted.record.handle, Files.Expiry))
}
