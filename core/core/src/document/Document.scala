package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.visibility.Label

/** What a document says: not blank. */
opaque type DocText = String

object DocText {

  /** `text`, or why not: blank. */
  def of(text: String): Either[String, DocText] =
    Either.cond(!text.isBlank, text, "a document's text is not blank")

  def value(t: DocText): String = t
}

/** How many windows held a version, and when one last did: while none has, when it was
  * written.
  */
final case class Placement(count: Long, lastPlaced: Instant)

/** A version holding something, of `plugin`'s document under `key` at `label`, the label it is
  * kept at (ADR 0030): `text`, what a window shows and search ranks; `data`, its plugin's own,
  * never shown or searched; kept at `place`; current from `written`.
  */
final case class Document(
    version: DocumentVersion,
    plugin: PluginName,
    key: DocKey,
    label: Label,
    place: Place,
    text: DocText,
    data: ujson.Value,
    written: Instant,
    placement: Placement
)

/** What a write did, and `kept`, the label its key's document is kept at: the one a read finds
  * it under ([[DocumentShelf.current]]).
  */
enum Written {

  /** The current version already held the same place, text and data: nothing was written. */
  case Unchanged(current: DocumentVersion, kept: Label)

  /** `version` was written and is current; `withdrew`, each key and label whose document was
    * withdrawn to keep within the bound.
    */
  case Versioned(version: DocumentVersion, kept: Label, withdrew: Vector[(DocKey, Label)])

  def kept: Label
}
