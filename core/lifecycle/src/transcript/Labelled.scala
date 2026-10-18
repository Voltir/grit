package grit.lifecycle.transcript

import grit.core.id.EntryId
import grit.core.period.{Cited, Ground}
import grit.core.visibility.Integrity

/** A period's transcript as the closing writer reads it: each line under a label, `u` for
  * a person's words to grit, `h` for a person's heard where grit listens, `a` for the
  * assistant's, `t` for a tool call and its result, numbered through the period (ADR 0018).
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
    * this transcript does not have counts for nothing. A heard line, or one below
    * [[Integrity.Outside]], supports nothing: `None` when every label that names a line
    * names one of those, so what rests on them alone is not kept.
    */
  def ground(cited: Vector[String]): Option[Ground] = {
    val sources = named(cited)
    val supporting = sources
      .filter(l => l.source != Labelled.Source.Heard && !l.integrity.belowOutside)
      .map(_.source)
    if (sources.nonEmpty && supporting.isEmpty) None
    else if (supporting.contains(Labelled.Source.Person)) Some(Ground.Person)
    else if (
      supporting.exists {
        case Labelled.Source.Tool(succeeded, _) => succeeded
        case _ => false
      }
    ) Some(Ground.Tool)
    else Some(Ground.Claimed)
  }

  /** The entries and tools the lines `labels` name in this transcript ([[Cited]]); a label it
    * does not have names nothing.
    */
  def cited(labels: Vector[String]): Cited = {
    val lines = named(labels)
    Cited.of(
      lines.map(_.entry),
      lines.collect { case Labelled.Line(_, _, Labelled.Source.Tool(_, Some(tool)), _, _) =>
        tool
      }.toSet
    )
  }

  /** Its lines `labels` name, in order, any case. */
  private def named(labels: Vector[String]): Vector[Labelled.Line] = {
    val wanted = labels.map(_.trim.toLowerCase).toSet
    lines.filter(l => wanted.contains(l.label))
  }
}

object Labelled {

  /** Who or what wrote a line: a person speaking to grit, a person heard where grit
    * listens, the assistant, or a tool, whose result `succeeded` or was an error, and whose
    * call named it `tool` (as the model sent it; `None` when the call is not among the
    * period's entries).
    */
  enum Source {
    case Person
    case Heard
    case Assistant
    case Tool(succeeded: Boolean, tool: Option[String])
  }

  /** A line: its `label`, the `entry` it was rendered from, what it `rendered` as,
    * `[{label}] …`, who wrote it, and the `integrity` its row is believed at.
    */
  final case class Line private[transcript] (
      label: String,
      entry: EntryId,
      source: Source,
      rendered: String,
      integrity: Integrity
  )

  /** The transcript of `lines`, in order. */
  private[transcript] def of(lines: Vector[Line]): Labelled = Labelled(lines)
}
