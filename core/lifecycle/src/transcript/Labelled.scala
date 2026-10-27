package grit.lifecycle.transcript

import grit.core.id.EntryId
import grit.core.message.Cut
import grit.core.period.{Cited, Ground}
import grit.core.prompt.{Fenced, Tag}
import grit.core.visibility.Integrity

/** A period's transcript as the closing writer reads it: each line under a label, `u` for
  * a person's words to grit, `h` for a person's heard where grit listens, `a` for the
  * assistant's, `t` for a tool call and its result, numbered through the period (ADR 0018).
  */
final case class Labelled private (lines: Vector[Labelled.Line]) {

  /** Its lines that fit in `chars` characters as [[fenced]] holds them, a blank line between,
    * the fence's own tags not counted: while they do not, each tool line's result longer than
    * [[PeriodTranscript.ToolChars]] cut back to that, earliest first; then its last lines,
    * whole, that fit. So a long result is cut before an earlier line is dropped.
    */
  def within(chars: Int): Labelled = {
    val size = lines.map(_.length).sum + 2 * (lines.size - 1).max(0)
    val (cut, _) = lines.foldLeft((Vector.empty[Labelled.Line], size - chars)) {
      case ((done, over), l) =>
        l.result.map(r => (r, Cut.head(r.text, PeriodTranscript.ToolChars))) match {
          case Some((r, short)) if over > 0 && short.length < r.text.length =>
            (
              done :+ l.copy(result = Some(r.copy(text = short))),
              over - (r.text.length - short.length)
            )
          case _ => (done :+ l, over)
        }
    }
    val kept = cut.reverseIterator
      .scanLeft((0, Option.empty[Labelled.Line])) { case ((used, _), l) =>
        // Each line after the first costs the blank line before it.
        (used + l.length + 2, Some(l))
      }
      .drop(1)
      .takeWhile { case (used, _) => used - 2 <= chars }
      .flatMap(_._2)
      .toVector
      .reverse
    Labelled(kept)
  }

  /** Its lines fenced in `<transcript>`, each as [[Labelled.Line.parts]], a blank line between. */
  def fenced: Fenced =
    Fenced(
      Tag.Transcript,
      Vector.empty,
      lines.zipWithIndex.flatMap { (l, i) =>
        Vector.fill(i.min(1))(Fenced.Part.Words("")) ++ l.parts
      }
    )

  /** [[fenced]], rendered. */
  def text: String = fenced.rendered

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
    else if (sources.exists(l => Labelled.supports(l.source, l.integrity))) Some(Ground.Tool)
    else Some(Ground.Claimed)
  }

  /** The entries and tools the lines `labels` name in this transcript ([[Cited]]); a label it
    * does not have names nothing.
    */
  def cited(labels: Vector[String]): Cited = {
    val lines = named(labels)
    Cited.of(
      lines.map(_.entry),
      lines.collect { case Labelled.Line(_, _, Labelled.Source.Tool(_, Some(tool)), _, _, _) =>
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

  /** A tool line's result: its `text`, from the call a person is shown as `call`. */
  final case class Result(call: String, text: String)

  /** A line: its `label`, the `entry` it was rendered from, what it `said`, `[{label}] …` (a
    * tool line's call as shown), a tool line's `result`, who wrote it, and the `integrity` its
    * row is believed at.
    */
  final case class Line private[transcript] (
      label: String,
      entry: EntryId,
      source: Source,
      said: String,
      result: Option[Result],
      integrity: Integrity
  ) {

    /** `said`, material, then a tool line's `result` ([[Fenced.toolOutput]]). */
    def parts: Vector[Fenced.Part] =
      Fenced.Part.Material(said) +:
        result.map(r => Fenced.Part.In(Fenced.toolOutput(r.call, r.text))).toVector

    /** Its characters as [[Labelled.fenced]] holds its parts, a newline between: what
      * [[Labelled.within]] counts.
      */
    private[transcript] def length: Int = Fenced(Tag.Transcript, Vector.empty, parts).inside.length
  }

  /** Whether a tool line from `source`, believed at `integrity`, is one [[Labelled.ground]]
    * counts as a tool's support: its result succeeded, and it is believed at Outside or above.
    * False for every other line.
    */
  def supports(source: Source, integrity: Integrity): Boolean = source match {
    case Source.Tool(succeeded, _) => succeeded && !integrity.belowOutside
    case _ => false
  }

  /** The transcript of `lines`, in order. */
  private[transcript] def of(lines: Vector[Line]): Labelled = Labelled(lines)
}
