package grit.knowledge.topic

import grit.core.id.ConversationId
import grit.core.review.{DocumentReview, ReviewWord, Reviews}
import grit.knowledge.ref.TopicKey
import grit.prose.form.{Block, Doc, Mark, Span, Text}

/** A topic put up for review as its prompt shows it: whether the day it was put up for opened
  * it, its lead as plain prose, the strands that day's passage was written from by root, in the
  * order it names them, and the titles of the cards that day attached.
  */
final case class TopicPrompt(
    review: DocumentReview,
    opened: Boolean,
    lead: Doc[Nothing],
    strands: Vector[ConversationId],
    cards: Vector[String]
) extends caps.Pure

object TopicPrompt {

  /** `reviews`, a topics plugin's, each read as its prompt, each verdict kept as its word
    * ([[TopicVerdict]]); one whose document is no topic's, or holds no day's review, waits.
    */
  def reviews(reviews: Reviews[DocumentReview, ReviewWord]): Reviews[TopicPrompt, TopicVerdict] =
    Reviews.as(reviews)(read, _.review, word)

  /** `p`'s prompt: the question with "new today" or "it grew today", the lead, each strand as
    * "thread n" linked by `link` of its root (plain where `link` gives none), and each card's
    * title. It shows nothing a strand's messages said.
    */
  def doc(p: TopicPrompt, link: ConversationId => Option[String]): Doc[Nothing] = {
    val asked = Block.Paragraph(
      Text(
        Vector(
          Span("Is this one topic, told right?", Set(Mark.Strong)),
          Span.plain(if (p.opened) " New today." else " It grew today.")
        )
      )
    )
    val threads = Option.when(p.strands.nonEmpty)(
      Block.Paragraph(
        Text.of(
          Span.plain("From today's threads: ") +: p.strands.zipWithIndex.flatMap { (root, i) =>
            val name = s"thread ${i + 1}"
            Option.when(i > 0)(Span.plain(", ")).toVector :+
              link(root).fold(Span.plain(name))(href => Span(name, Set(Mark.Link(href))))
          }
        )
      )
    )
    val cards =
      Option.when(p.cards.nonEmpty)(
        Block.Paragraph(Text.plain(s"Cards: ${p.cards.mkString(", ")}"))
      )
    Doc((asked +: p.lead.blocks) ++ threads ++ cards)
  }

  private def read(r: DocumentReview): Option[TopicPrompt] =
    for {
      key <- TopicKey.read(r.key)
      held <- TopicWrite.read(key, r.document.data).toOption
      today <- held.today
    } yield TopicPrompt(
      r,
      today.opened,
      held.topic.lead.unnoted,
      held.topic.passages
        .filter(_.day == today.day)
        .flatMap(_.from.tags)
        .collect { case Lineage.Tag.Strand(root, _, _) => root }
        .distinct,
      today.cards
    )

  private def word(v: TopicVerdict): ReviewWord =
    // Each is lower-case letters alone, a word ReviewWord.of takes, so it never throws.
    ReviewWord.of(v.toString.toLowerCase).fold(why => throw new IllegalStateException(why), w => w)
}
