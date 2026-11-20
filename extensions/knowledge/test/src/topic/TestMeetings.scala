package grit.knowledge.topic

import java.time.{Instant, LocalDate}

import scala.concurrent.duration.*

import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  Document,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.id.{ConversationId, PluginName}
import grit.core.place.Place
import grit.core.visibility.{Clearance, Integrity, Label}
import grit.dbos.sql.TestTx
import grit.knowledge.ref.TopicKey
import grit.prose.form.{Block, Doc, Span, Text}

/** Meeting topics kept as a topics run keeps them, for the suites that read them back. */
object TestMeetings {

  private def right[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  val Name: PluginName = right(PluginName.of("topics"))

  val Terms: DocumentTerms =
    right(DocLabel.of("topic").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 500)))

  def doc(text: String): Doc[TopicNote] =
    Doc(Vector(Block.Paragraph(Text(Vector(Span.plain(text))))))

  /** A meeting at `state` with `who`, led by `lead`, with a passage on each of `talked`. */
  def meeting(state: Meeting.State, who: Vector[String], lead: String, talked: LocalDate*): Topic =
    Topic.of(
      doc(lead),
      talked.toVector.map(d => Passage(d, Lineage.of(Vector.empty), doc(s"Talked of on $d."))),
      Topic.Kind.Meeting(Meeting.of(state, who))
    )

  /** `t` with each of its passages written from a strand of `room`. */
  def talkedIn(t: Topic, room: Place): Topic =
    Topic.of(
      t.lead,
      t.passages.map(p =>
        p.copy(from =
          Lineage.of(
            Vector(
              Lineage.Tag.Strand(
                ConversationId(s"strand-${p.day}"),
                room,
                grit.core.stitch.StrandDigest.of(Vector.empty)
              )
            )
          )
        )
      ),
      t.kind
    )

  /** A subject led by `lead`, with a passage on `talked`. */
  def subject(lead: String, talked: LocalDate): Topic =
    Topic.of(doc(lead), Vector(Passage(talked, Lineage.of(Vector.empty), doc("Said."))))

  /** The text a topic's document holds. */
  def text(t: Topic): String = DocText.value(right(Topic.text(t)))

  /** A store holding each of `topics`, keyed by its name, at its label and integrity, written
    * in the order given.
    */
  final class Kept(topics: (String, Topic, Label, Integrity)*) {
    val docs = new InMemoryDocuments
    val keeper: DocumentKeeper = docs.keeper(Name, Terms)

    topics.zipWithIndex.foreach { case ((name, t, label, integrity), i) =>
      val key = right(TopicKey.strand(ConversationId(name)))
      keeper
        .write(
          TopicKey.doc(key),
          label,
          integrity,
          Place.Everywhere,
          right(Topic.text(t)),
          Topic.write(t),
          None,
          Instant.EPOCH.plusSeconds(i.toLong)
        )(using TestTx.fake(Clearance.of(label)))
        .fold(e => sys.error(e.toString), _ => ())
    }

    /** Every current document, as a reader cleared at `label` reads them. */
    def read(label: Label = Label.Public): Vector[Document] =
      docs
        .shelf(Name)
        .newest(100)(using TestTx.fake(Clearance.of(label)))
        .fold(e => sys.error(e.toString), identity)
  }
}
