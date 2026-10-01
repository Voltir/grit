package grit.slack.text

import grit.slack.event.{ChannelId, UserId}

/** A person's message as Slack sends it, as grit stores it. */
object Incoming {

  /** `raw`, a message's text as Slack sends it, as grit stores it: `<@U…>` as `@` and the name
    * `names` gives (the label Slack sent, else the id, when it gives none), grit's own mention
    * (`bot`) removed; `<#C…|name>` as `#name`, and `<#C…|>` or `<#C…>` as `#` and the name
    * `channels` gives, else the id; `<!here>`, `<!channel>`, `<!everyone>` as `@here`,
    * `@channel`, `@everyone`, and another `<!…|label>` as its label; `<url|label>` as `label
    * (url)` and `<url>` as `url`; then `&lt;`, `&gt;` and `&amp;` undone; trimmed.
    */
  def text(
      raw: String,
      bot: UserId,
      names: UserId -> Option[String],
      channels: ChannelId -> Option[String]
  ): String =
    Tag
      .replaceAllIn(
        raw,
        m => scala.util.matching.Regex.quoteReplacement(tag(m.group(1), bot, names, channels))
      )
      .replace("&lt;", "<")
      .replace("&gt;", ">")
      .replace("&amp;", "&")
      .trim

  /** Each channel link in `raw` (`<#C…|name>`, `<#C…|>`, `<#C…>`), in order, with the name it
    * carries; `None` when it carries none or an empty one.
    */
  def channels(raw: String): Vector[(ChannelId, Option[String])] =
    Tag.findAllMatchIn(raw).flatMap(m => channel(m.group(1))).toVector

  /** The channel `inside` links to, and the name it carries; `None` when it is not a channel. */
  private def channel(inside: String): Option[(ChannelId, Option[String])] =
    Option.when(inside.startsWith("#")) {
      val (target, label) = split(inside)
      (ChannelId(target.drop(1)), label.filter(_.nonEmpty))
    }

  /** `inside`'s target, and its label after the first `|`, if any. */
  private def split(inside: String): (String, Option[String]) =
    inside.indexOf('|') match {
      case -1 => (inside, None)
      case i => (inside.take(i), Some(inside.drop(i + 1)))
    }

  /** Slack's `<…>` markup: a mention, a channel, a broadcast or a link. */
  private val Tag = "<([^<>]+)>".r

  /** What one `<inside>` becomes. */
  private def tag(
      inside: String,
      bot: UserId,
      names: UserId -> Option[String],
      channels: ChannelId -> Option[String]
  ): String = {
    val (target, label) = split(inside)
    if (target.startsWith("@")) {
      val user = UserId(target.drop(1))
      if (user == bot) "" else "@" + names(user).orElse(label).getOrElse(target.drop(1))
    } else if (target.startsWith("#")) {
      val id = target.drop(1)
      "#" + label.filter(_.nonEmpty).orElse(channels(ChannelId(id))).getOrElse(id)
    } else if (target.startsWith("!"))
      target.drop(1) match {
        case b @ ("here" | "channel" | "everyone") => s"@$b"
        case other => label.getOrElse(other)
      }
    else label.fold(target)(l => s"$l ($target)")
  }
}
