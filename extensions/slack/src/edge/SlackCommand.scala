package grit.slack.edge

/** The slash command a deployment registered for its Slack app, `/name`; any other is ignored. */
final case class SlackCommand private (name: String)

object SlackCommand {

  /** `raw` trimmed, or why not: not `/` then 1–32 of a–z, 0–9, `-`, `_`. */
  def of(raw: String): Either[String, SlackCommand] = {
    val name = raw.trim
    Either.cond(
      name.matches("/[a-z0-9_-]{1,32}"),
      new SlackCommand(name),
      s"$name: a slash command is / then 1 to 32 of a-z, 0-9, - and _"
    )
  }
}
