package grit.core.document

/** A reference a document holds to something outside it, in its plugin's own terms: `kind`,
  * lower-case words and digits joined by `-`, and `ref`, 1 to 8192 characters, none of them
  * NUL. Core keeps and finds them ([[DocumentShelf.referring]]); what they mean is the plugin's.
  */
final case class DocRef private (kind: String, ref: String) extends caps.Pure

object DocRef {

  /** `kind` and `ref` as a reference, or why not. */
  def of(kind: String, ref: String): Either[String, DocRef] = {
    val chars = ref.codePointCount(0, ref.length)
    if (!Kind.matches(kind))
      Left(s"a reference's kind is lower-case words and digits joined by '-': $kind")
    else if (chars < 1 || chars > Longest)
      Left(s"a reference is 1 to $Longest characters, not $chars")
    else if (ref.contains('\u0000')) Left("a reference holds no NUL")
    else Right(new DocRef(kind, ref))
  }

  /** 256: the most references one version holds. */
  val Most: Int = 256

  private val Longest = 8192
  private val Kind = "[a-z0-9]+(-[a-z0-9]+)*".r
}
