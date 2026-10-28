package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.place.Place
import grit.core.visibility.{Integrity, Label}

/** What a document says: not blank, at most [[DocText.Most]] bytes as UTF-8. */
opaque type DocText = String

object DocText {

  /** 65,536: the most bytes, as UTF-8, a document's text holds. */
  val Most: Int = 65536

  /** `text`, or why not: blank, or over [[Most]] bytes. */
  def of(text: String): Either[String, DocText] = {
    val bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
    if (text.isBlank) Left("a document's text is not blank")
    else if (bytes > Most) Left(s"a document's text is at most $Most bytes as UTF-8, not $bytes")
    else Right(text)
  }

  def value(t: DocText): String = t
}

/** How many windows held a version, and when one last did: while none has, when it was
  * written.
  */
final case class Placement(count: Long, lastPlaced: Instant)

/** How many turns followed a version's pointer, at least one, and when one last did. A
  * version never followed has none ([[Document.following]] is `None`): unlike [[Placement]],
  * which counts a version as placed when written so the bound can rank it, nothing reads a
  * follow that never happened.
  */
final case class Following private (count: Long, last: Instant)

object Following {

  /** `count` follows, the last at `last`; `None` when `count` is under 1. */
  def of(count: Long, last: Instant): Option[Following] =
    Option.when(count >= 1)(new Following(count, last))
}

/** A version holding something, of `plugin`'s document under `key` at `label`, the label it is
  * kept at (ADR 0030), believed at `integrity` (ADR 0036), as its writer gave it: `text`, what
  * a window shows and search ranks; `data`, its plugin's own,
  * never shown or searched; `pointer`, the call that reads what it stands for, if any; kept at
  * `place`; current from `written`.
  */
final case class Document(
    version: DocumentVersionId,
    plugin: PluginName,
    key: DocKey,
    label: Label,
    integrity: Integrity,
    place: Place,
    text: DocText,
    data: ujson.Value,
    pointer: Option[Pointer],
    written: Instant,
    placement: Placement,
    following: Option[Following]
)

object Document {

  /** 32,768: the most bytes a document's data holds as compact JSON (stored, it may grow by
    * half, under the table's 65,536).
    */
  val DataMost: Int = 32768

  /** Why `data` may not be a document's: over [[DataMost]] bytes as compact JSON; none when it
    * may.
    */
  def oversized(data: ujson.Value): Option[String] = {
    val bytes = data.render().getBytes(java.nio.charset.StandardCharsets.UTF_8).length
    Option.when(bytes > DataMost)(
      s"a document's data is at most $DataMost bytes as compact JSON, not $bytes"
    )
  }
}

/** What a write did, and `kept`, the label its key's document is kept at: the one a read finds
  * it under ([[DocumentShelf.current]]).
  */
enum Written {

  /** The current version already held the same place, text, data, pointer, integrity and refs:
    * nothing was written.
    */
  case Unchanged(current: DocumentVersionId, kept: Label)

  /** `version` was written and is current; `withdrew`, each key and label whose document was
    * withdrawn to keep within the bound.
    */
  case Versioned(version: DocumentVersionId, kept: Label, withdrew: Vector[(DocKey, Label)])

  def kept: Label
}

/** One version of a key at a label, as its history lists it ([[DocumentShelf.versions]]). */
enum DocumentVersion {

  /** A version holding something. */
  case Holding(document: Document)

  /** A withdrawal of the key, written at `written`: it holds nothing. */
  case Withdrawal(version: DocumentVersionId, written: Instant)
}
