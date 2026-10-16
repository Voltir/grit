package grit.slack.edge

import grit.core.admin.PlaceNames

/** The slash command a deployment registered for its Slack app, `/name`; any other is ignored.
  * `places` names the places its words may name by a service's links or ids
  * ([[grit.core.admin.Command.read]]).
  */
final case class SlackCommand private (name: String, places: PlaceNames) {

  /** This command, its words naming places as `names` names them. */
  def naming(names: PlaceNames): SlackCommand = new SlackCommand(name, names)
}

object SlackCommand {

  /** `raw` trimmed, naming no place by a service's link ([[PlaceNames.None]]), or why not: not
    * `/` then 1–32 of a–z, 0–9, `-`, `_`.
    */
  def of(raw: String): Either[String, SlackCommand] = {
    val name = raw.trim
    Either.cond(
      name.matches("/[a-z0-9_-]{1,32}"),
      new SlackCommand(name, PlaceNames.None),
      s"$name: a slash command is / then 1 to 32 of a-z, 0-9, - and _"
    )
  }
}
