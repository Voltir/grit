package grit.core.document

import grit.core.id.ClaimHolder

/** A reference a document holds to something outside it, in its plugin's own terms: `kind`,
  * lower-case words and digits joined by `-`, and `ref`, 1 to 8192 characters, none of them
  * NUL. Core keeps and finds them ([[DocumentShelf.referring]]); what they mean is the plugin's,
  * except a [[DocRef.brief]]'s.
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

  /** The ref a document holds when it is `holder`'s brief: what a window working as `holder` is
    * shown first at each boundary ([[DocumentSearch.briefOf]]). Search never finds a document
    * holding one.
    */
  def brief(holder: ClaimHolder): DocRef = new DocRef(Brief, ClaimHolder.value(holder))

  /** `brief-for`: the kind of a [[brief]], the one kind whose meaning is core's. */
  val Brief: String = "brief-for"

  /** 9,500: the most characters a version holding a [[brief]] holds, as Claude Code's hook
    * takes, so a session of either harness opens on the same text.
    */
  val BriefChars: Int = 9500

  /** Why `text` may not be kept holding `refs`: they hold a [[brief]] and it is over
    * [[BriefChars]] characters. None when it may.
    */
  def overlong(refs: Set[DocRef], text: DocText): Option[String] = {
    val chars = DocText.value(text).length
    Option.when(refs.exists(_.kind == Brief) && chars > BriefChars)(
      s"a brief holds at most $BriefChars characters, not $chars"
    )
  }

  /** 256: the most references one version holds. */
  val Most: Int = 256

  private val Longest = 8192
  private val Kind = "[a-z0-9]+(-[a-z0-9]+)*".r
}
