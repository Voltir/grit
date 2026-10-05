package grit.core.document

import scala.concurrent.duration.*

/** What heads a plugin's documents where a window shows one: one line, not blank. */
opaque type DocLabel = String

object DocLabel {

  /** `text`, or why not: blank, or holding a line break or other control character. */
  def of(text: String): Either[String, DocLabel] =
    if (text.isBlank) Left("a document label is not blank")
    else if (text.exists(_.isControl))
      Left("a document label is one line, with no control character")
    else Right(text)

  def value(l: DocLabel): String = l
}

/** How far a plugin's document scores are scaled before they rank beside entries' in one
  * pool: positive and finite. Their index's scale is its own (ADR 0005), so 1 is no claim of
  * parity.
  */
opaque type DocWeight = Double

object DocWeight {

  /** `w`, or why not: not positive, or not finite. */
  def of(w: Double): Either[String, DocWeight] =
    Either.cond(w > 0 && !w.isInfinite, w, s"a document weight is positive and finite, not $w")

  def value(w: DocWeight): Double = w

  /** 1: scores as the document index gives them. */
  val Unscaled: DocWeight = 1.0
}

/** How a plugin's documents are kept and drawn on (ADR 0028): `label` heads each where a
  * window shows one; `weight` scales each search score before it ranks beside entries; a
  * version that stopped being current is deleted `retention` after (under the raw window, a
  * window rebuilt while its entries are kept may lack it); past `bound` current documents, a
  * write withdraws the least recently placed.
  */
final case class DocumentTerms private (
    label: DocLabel,
    weight: DocWeight,
    retention: FiniteDuration,
    bound: Int
)

object DocumentTerms {

  /** These terms, or why not: `retention` under a minute, `bound` under 1. */
  def of(
      label: DocLabel,
      weight: DocWeight,
      retention: FiniteDuration,
      bound: Int
  ): Either[String, DocumentTerms] =
    if (retention < 1.minute)
      Left(s"documents are kept a minute or more after they stop being current, not $retention")
    else if (bound < 1) Left(s"a plugin's documents are bounded at 1 or more, not $bound")
    else Right(new DocumentTerms(label, weight, retention, bound))
}
