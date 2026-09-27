package grit.lifecycle.transcript

import grit.core.period.Ground

/** A period's transcript as the closing writer reads it: each line under a label, `u` for
  * the person's words, `a` for the assistant's, `t` for a tool call and its result,
  * numbered through the period (ADR 0018).
  */
final case class Labelled private (lines: Vector[Labelled.Line]) {

  /** Its last lines, whole, that fit in `chars` characters as [[text]] renders them. */
  def within(chars: Int): Labelled = {
    val kept = lines.reverseIterator
      .scanLeft((0, Option.empty[Labelled.Line])) { case ((used, _), l) =>
        // Each line after the first costs the blank line before it.
        (used + l.rendered.length + 2, Some(l))
      }
      .drop(1)
      .takeWhile { case (used, _) => used - 2 <= chars }
      .flatMap(_._2)
      .toVector
      .reverse
    Labelled(kept)
  }

  /** Its lines, one per row, each `[{label}] …`, a blank line between. */
  def text: String = lines.map(_.rendered).mkString("\n\n")

  /** The ground `cited` labels give: `Person` when any names a person's line, else `Tool`
    * when any names a tool line whose result succeeded, else `Claimed`. A tool line whose
    * result is an error (failed, declined, interrupted, gone) grounds nothing, and a label
    * this transcript does not have counts for nothing.
    */
  def ground(cited: Vector[String]): Ground = {
    val named = cited.map(_.trim.toLowerCase).toSet
    val sources = lines.filter(l => named.contains(l.label)).map(_.source)
    if (sources.contains(Labelled.Source.Person)) Ground.Person
    else if (sources.contains(Labelled.Source.Tool(succeeded = true))) Ground.Tool
    else Ground.Claimed
  }
}

object Labelled {

  /** Who or what wrote a line: the person, stating or asking, the assistant, or a tool,
    * whose result `succeeded` or was an error.
    */
  enum Source {
    case Person

    /** The person asking, not stating: a question grounds nothing. */
    case Asked
    case Assistant
    case Tool(succeeded: Boolean)
  }

  /** A line: its `label` and what it `rendered` as, `[{label}] …`, and who wrote it. */
  final case class Line private[transcript] (label: String, source: Source, rendered: String)

  /** The transcript of `lines`, in order. */
  private[transcript] def of(lines: Vector[Line]): Labelled = Labelled(lines)
}
