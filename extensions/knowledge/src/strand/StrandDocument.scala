package grit.knowledge.strand

import java.time.{Instant, LocalDate, ZoneOffset}

import grit.core.document.{DocText, Document}
import grit.core.id.{ConversationId, DocKey}
import grit.core.stitch.{Strand, StrandDigest}
import grit.knowledge.card.CardSummary

/** A strand's document: its key, its text and its data. */
object StrandDocument {

  /** 50: the most member conversations a document's data names. */
  val MostMembers: Int = 50

  /** `strand:{root}`. */
  def key(root: ConversationId): DocKey =
    // A conversation id is short text with no control character: DocKey.of never refuses it.
    DocKey
      .of(s"strand:${ConversationId.value(root)}")
      .fold(why => throw new IllegalArgumentException(why), identity)

  /** "{n} threads in this room, {said} messages, from {since} to {touched}." (days in UTC; "1
    * thread" and "1 message" in the singular), a blank line, the summary, then after a blank
    * line `Terms: ` and its terms separated by `, ` when it has any.
    */
  def text(seen: Strand.Digested, summary: CardSummary): DocText = {
    val head = s"${counted(seen.members.size, "thread")} in this room, " +
      s"${counted(seen.said, "message")}, from ${day(seen.since)} to ${day(seen.touched)}."
    val terms = Option.when(summary.terms.nonEmpty)(s"Terms: ${summary.terms.mkString(", ")}")
    // The head is never blank, so neither is the text.
    DocText
      .of((Vector(head, summary.summary) ++ terms).mkString("\n\n"))
      .fold(why => throw new IllegalArgumentException(why), identity)
  }

  /** `{"root", "room", "members": [...], "digest", "said", "since", "touched"}`: the place in its
    * written form, at most [[MostMembers]] members, instants ISO-8601.
    */
  def data(seen: Strand.Digested): ujson.Obj = ujson.Obj(
    "root" -> ConversationId.value(seen.root),
    "room" -> seen.room.written,
    "members" -> ujson.Arr.from(seen.members.take(MostMembers).map(ConversationId.value)),
    "digest" -> StrandDigest.value(seen.digest),
    "said" -> seen.said,
    "since" -> seen.since.toString,
    "touched" -> seen.touched.toString
  )

  /** The root and digest `document`'s data holds, or why not, naming its key. */
  def held(document: Document): Either[String, (ConversationId, StrandDigest)] = {
    val named = s"the document ${DocKey.value(document.key)}"
    for {
      o <- document.data.objOpt.toRight(s"$named holds no object")
      root <- o.get("root").flatMap(_.strOpt).toRight(s"$named names no root")
      digest <- o
        .get("digest")
        .flatMap(_.strOpt)
        .toRight(s"$named holds no digest")
        .flatMap(StrandDigest.read(_).left.map(why => s"$named: $why"))
    } yield (ConversationId(root), digest)
  }

  private def counted(n: Int, one: String): String = if (n == 1) s"1 $one" else s"$n ${one}s"

  private def day(at: Instant): String = LocalDate.ofInstant(at, ZoneOffset.UTC).toString
}
