package grit.core.files

import java.time.Instant

import grit.core.id.{CallSlot, EdgeName, FileHandle, Sha256}
import grit.core.visibility.{Provenance, Readers}

/** Where a file came from: `edge`, and its own `ref` for it (a Drive file's id, a message's
  * part); the source's `revision` when it keeps one (a web page or an attachment has none); and
  * when it was fetched.
  */
final case class FileOrigin(edge: EdgeName, ref: String, revision: Option[String], fetched: Instant)

/** Who may open a file: as one item of a service's result is read ([[grit.core.store.Tx.readsItem]]). */
final case class FileItem(item: Provenance, readers: Readers)

/** A file as the area keeps it: `claimed`, its source's media type, a hint ([[Kind.sniff]]
  * decides); `expires`, when it stops being kept.
  */
final case class FileRecord(
    handle: FileHandle,
    name: String,
    bytes: Long,
    sha256: Sha256,
    claimed: Option[String],
    origin: FileOrigin,
    item: FileItem,
    expires: Instant
)

/** What a put is made under: a put again under it answers the file the first one kept. */
opaque type PutKey = String

object PutKey {

  /** A hosted tool call's: its slot's key, so the call run again after a crash puts nothing new. */
  def of(slot: CallSlot): PutKey = slot.key

  /** The key written `s`, as a store keeps it. */
  private[grit] def read(s: String): PutKey = s

  def value(k: PutKey): String = k

  given CanEqual[PutKey, PutKey] = CanEqual.derived
}
