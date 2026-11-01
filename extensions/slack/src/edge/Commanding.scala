package grit.slack.edge

import grit.core.admin.Command
import grit.core.clock.Clock
import grit.core.edge.{Attesting, EdgeStores, RealmSource}
import grit.core.identity.Account
import grit.core.store.Origin
import grit.slack.client.Slack
import grit.slack.event.{ChannelId, Events, TeamId, UserId}

/** The slash command an edge answers ([[SlackEdge.command]]), its asker checked through `source`;
  * `requiet` is told when a command made a room quiet or not.
  */
private[slack] final class Commanding(
    slack: Slack,
    stores: EdgeStores^,
    clock: Clock^,
    said: String => Unit,
    source: RealmSource^{slack, said},
    requiet: () => Unit
) {
  import SlackEdge.*

  /** One slash command's payload, as [[SlackEdge.command]] says. */
  def command(registered: SlackCommand)(payload: String): Unit =
    Events.command(payload) match {
      case Left(why) => said(s"slack: a slash command grit cannot read: $why")
      case Right(c) if c.command != registered.name =>
        said(s"slack: a slash command not grit's, ${c.command}, ignored")
      case Right(c) =>
        val answer = SlackAccounts.account(c.team, c.user) match {
          case Left(why) =>
            said(s"slack: ${c.command} not run: its asker cannot be named: $why")
            NotAttested
          case Right(asker) =>
            stores.attesting.before(source, asker) match {
              case Left(e) =>
                said(s"slack: ${c.command} not run: ${Account.written(asker)} unchecked: $e")
                NotAttested
              case Right(_) =>
                val room =
                  if (c.direct) Origin.Direct(asker, "").room
                  else Origin.channel(TeamId.value(c.team), ChannelId.value(c.channel))
                // A mention as Slack escapes it, `<@U…|name>`, is read as the user alone, and a
                // link, `<https://…|label>`, as the link alone.
                val words = Linked.replaceAllIn(
                  Mentioned.replaceAllIn(c.text, m => s"<@${m.group(1)}>"),
                  m => scala.util.matching.Regex.quoteReplacement(m.group(1))
                )
                def person(word: String): Option[Account] = word match {
                  case Mentioned(user) => SlackAccounts.account(c.team, UserId(user)).toOption
                  case _ => None
                }
                Command.read(words, person, registered.places) match {
                  case Left(why) => why
                  case Right(command) =>
                    stores.administration.run(asker, room, command, clock.now()) match {
                      case Right(answered) =>
                        command match {
                          case Command.Quiet(_) => requiet()
                          case _ => ()
                        }
                        answered.text
                      case Left(e) =>
                        said(s"slack: ${c.command} not run: $e")
                        NotRun
                    }
                }
            }
        }
        slack
          .respond(c.answerAt, answer)
          .left
          .foreach(e => said(s"slack: the answer to ${c.command} not shown: $e"))
    }
}
