package grit.core.id

/** A file kept in grit's file area: one put, of one snapshot of its bytes, numbered across the
  * area and never reused. Written `f<n>`, as the model sees it and a step records it. At least 1.
  */
opaque type FileHandle = Long

object FileHandle {

  /** `n` as a handle, or `None` below 1. */
  def of(n: Long): Option[FileHandle] = Option.when(n >= 1)(n)

  /** The handle written `s` (`f41`); `None` for any other text. */
  def read(s: String): Option[FileHandle] =
    Option
      .when(s.startsWith("f"))(s.drop(1))
      .filter(_.forall(_.isDigit))
      .flatMap(_.toLongOption)
      .flatMap(of)

  def value(h: FileHandle): Long = h

  extension (h: FileHandle) {

    /** As the model sees it: `f41`. */
    def written: String = s"f$h"
  }

  given CanEqual[FileHandle, FileHandle] = CanEqual.derived
}
