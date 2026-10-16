package grit.mcp.drive

/** A Drive file's, folder's or shared drive's id, as Drive writes it: letters, digits, `-` and
  * `_`, at most [[FileId.MaxLength]].
  */
opaque type FileId = String

object FileId {

  /** The longest id read: 128 characters. */
  val MaxLength: Int = 128

  /** `id` as an id, or why not, in words the model reads: blank, longer than [[MaxLength]], or
    * holding any other character (a `/` or a space included).
    */
  def of(id: String): Either[String, FileId] =
    if (id.isEmpty) Left("a Drive file's id is blank")
    else if (id.length > MaxLength) Left(s"a Drive file's id is at most $MaxLength characters")
    else if (!id.forall(c => c.isLetterOrDigit && c < 128 || c == '-' || c == '_'))
      Left(s"a Drive file's id holds only letters, digits, - and _, not $id")
    else Right(id)

  def value(id: FileId): String = id

  given CanEqual[FileId, FileId] = CanEqual.derived
}
