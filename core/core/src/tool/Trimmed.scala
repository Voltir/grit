package grit.core.tool

import grit.core.host.Clipped
import grit.core.visibility.Part

/** A cut result's `head` and the `parts` kept of it ([[Outcome.Done]]'s text and parts),
  * within [[Clipped]]'s limits whatever a reader keeps of them ([[Outcome.readable]]);
  * `parts` is `None` when not one fits.
  */
final case class Trimmed private (head: String, parts: Option[Vector[Part]])

object Trimmed {

  /** `head` then `parts`, each on lines of its own, fitted to [[Clipped]]'s limits with room
    * kept for [[Outcome.NoneShown]] after `head`: every part when they fit; else the most
    * leading parts that do, after `head` with a hint saying some were left out, never how
    * many, since a reader may be shown fewer; else `head` alone, cut as [[Clipped.head]] cuts
    * it, with that hint, and no parts.
    */
  def of(head: String, parts: Vector[Part]): Trimmed = {
    // A reader keeping none of the parts is shown the head and NoneShown in their place, so
    // parts are kept only where that fits too.
    def roomy(top: String): Boolean = Clipped.fits(s"$top\n${Outcome.NoneShown}")
    def shown(top: String, k: Int): String = (top +: parts.take(k).map(_.text)).mkString("\n")
    val hinted = Clipped.head(
      head,
      {
        case None =>
          Some(
            "Some of this answer was left out to fit; call again asking for fewer, or for " +
              "the next page, to see the rest."
          )
        case kept => Clipped.cutHint(kept)
      }
    )
    if (roomy(head) && Clipped.fits(shown(head, parts.size))) new Trimmed(head, Some(parts))
    else {
      val k =
        if (!roomy(hinted.show)) 0
        else Clipped.most(parts.size - 1)(k => shown(hinted.show, k))
      if (k == 0) new Trimmed(hinted.show, None)
      else new Trimmed(hinted.show, Some(parts.take(k)))
    }
  }
}
