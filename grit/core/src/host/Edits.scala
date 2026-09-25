package grit.core.host

/** Changing files in the checkout. Each change replaces the whole file atomically, so a
  * change cut short leaves the old file or the new one, never a mix. A path that is a link
  * is refused ([[HostError.Outside]]) when it leads outside the checkout or to a secrets
  * file.
  */
trait Edits extends caps.SharedCapability {

  /** Makes `text` the whole of the file `path`, creating it, and any missing parent
    * directory, when it does not exist. Fails when `path` is a directory, or a parent is a
    * file.
    */
  def write(path: RelPath, text: String): Either[HostError, Unit]

  /** Applies `edits` to the existing file `path`, as [[Replace.onto]] says, and writes the
    * result. Fails, changing nothing, as `onto` does, or when `path` is missing, a
    * directory, over [[Workspace.MaxFileBytes]] or not UTF-8.
    */
  def edit(path: RelPath, edits: Seq[Replace]): Either[EditError, Edited]
}
