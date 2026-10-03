package grit.core.recipe

import java.time.Instant

import grit.core.id.EntryId

/** A message as a pool may show it: its entry, its speaker's name as shown, when it was said,
  * and its words.
  */
private[recipe] final case class Spoken(entry: EntryId, speaker: String, at: Instant, text: String)

/** An exchange stitching offered: its opening message, its record's headline when it has one,
  * and whether the thread the input is for follows it.
  */
private[recipe] final case class Offered(opening: Spoken, record: Option[String], followed: Boolean)

/** What a pool chooses among for one message: messages said in its room (`said`), its
  * author's (`byAuthor`), and the exchanges its conversation's opening was offered
  * (`offered`), in the order offered.
  */
private[recipe] final case class Candidates(
    said: Vector[Spoken],
    byAuthor: Vector[Spoken],
    offered: Vector[Offered]
)
