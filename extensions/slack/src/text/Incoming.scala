package grit.slack.text

import grit.slack.event.UserId

/** A person's message as Slack sends it, as grit stores it. */
object Incoming {

  /** `raw`, a message's text as Slack sends it, as grit stores it: `<@U…>` as `@` and the name
    * `names` gives (the label Slack sent, else the id, when it gives none), grit's own mention
    * (`bot`) removed; `<#C…|name>` as `#name`; `<!here>`, `<!channel>`, `<!everyone>` as `@here`,
    * `@channel`, `@everyone`, and another `<!…|label>` as its label; `<url|label>` as `label
    * (url)` and `<url>` as `url`; then `&lt;`, `&gt;` and `&amp;` undone; trimmed.
    */
  def text(raw: String, bot: UserId, names: UserId -> Option[String]): String =
    Tag
      .replaceAllIn(
        raw,
        m => scala.util.matching.Regex.quoteReplacement(tag(m.group(1), bot, names))
      )
      .replace("&lt;", "<")
      .replace("&gt;", ">")
      .replace("&amp;", "&")
      .trim

  /** Slack's `<…>` markup: a mention, a channel, a broadcast or a link. */
  private val Tag = "<([^<>]+)>".r

  /** What one `<inside>` becomes. */
  private def tag(inside: String, bot: UserId, names: UserId -> Option[String]): String = {
    val (target, label) = inside.indexOf('|') match {
      case -1 => (inside, None)
      case i => (inside.take(i), Some(inside.drop(i + 1)))
    }
    if (target.startsWith("@")) {
      val user = UserId(target.drop(1))
      if (user == bot) "" else "@" + names(user).orElse(label).getOrElse(target.drop(1))
    } else if (target.startsWith("#")) "#" + label.getOrElse(target.drop(1))
    else if (target.startsWith("!"))
      target.drop(1) match {
        case b @ ("here" | "channel" | "everyone") => s"@$b"
        case other => label.getOrElse(other)
      }
    else label.fold(target)(l => s"$l ($target)")
  }
}
