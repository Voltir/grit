package grit.turn

import grit.core.context.Width
import grit.core.recipe.ServiceOffer
import grit.core.tool.{ToolName, ToolSetId}

/** What a turn's `offer` step decided of its shape under its deployment's recipe
  * ([[grit.core.recipe.TurnRecipe]]): its window's `width`; `whole`, the tool set it would have
  * been offered had nothing been withheld, kept by id; and each service it takes tools from, in
  * order: its workspace's, then each it reaches.
  */
final case class TurnShape(width: Width, whole: ToolSetId, services: Vector[TurnShape.Took])

object TurnShape {

  /** How a service's tools come to a turn. */
  enum Via {

    /** The service is its conversation's workspace ([[grit.core.place.WorksIn]]). */
    case Workspace

    /** It reaches the service besides ([[grit.core.place.Reaches]]). */
    case Reached
  }

  /** A service as `offer`ing decided it, how its tools came (`via`), and `tools`: those the
    * turn took from its edge's advert, which are in its set unless the service is withheld
    * ([[ServiceOffer.withheld]]), and then in `whole` alone.
    */
  final case class Took(offer: ServiceOffer, via: Via, tools: Vector[ToolName])
}
