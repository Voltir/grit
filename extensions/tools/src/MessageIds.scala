package grit.tools

import java.time.Instant

import grit.core.clock.Utc
import grit.core.id.{ConversationId, EntryId, Href}
import grit.core.identity.Principal
import grit.core.message.Message
import grit.core.place.Place
import grit.core.recipe.RoomReads
import grit.core.stitch.Said
import grit.core.store.{Askers, EntryStore, Payload, Principals, StoreError, Tx}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, Retry, Tool, ToolName, ToolSpec}

/** `message_ids`: the id grit keeps this conversation under, and the asker's first or last
  * message in this room, by its entry id, its conversation, when it was said and the link its
  * edge recorded it with. Each call reads in one transaction of the reads its turn hands it
  * ([[grit.core.tool.Hosted.reading]]), so it finds only what the turn may read; the room is
  * that transaction's own ([[grit.core.visibility.Clearance.own]]), every conversation in it
  * counted.
  */
object MessageIds {

  /** Its tool's name, `message_ids`. */
  val Name: ToolName = ToolName("message_ids")

  /** Which message: the asker's first or last in the room. */
  enum Pointed {
    case First, Last
  }

  /** Where it reads: the turn's own entries, the room's messages, and their authors' links. */
  final case class Stores(entries: EntryStore, rooms: RoomReads, principals: Principals)

  /** The answer when the store cannot be read: it names nothing of what failed. */
  val Unread: String = "I could not read the messages just now. Ask again in a moment."

  /** `message_ids` over `stores`, the asker as `askers` resolves them: the asker's message is
    * read up to the one the turn answers, so a message said since is not among them. Its answer
    * is believed at the message's integrity when it shows one. A store it cannot read is an
    * error [[Outcome]], [[Unread]].
    */
  def tool(stores: Stores, askers: Askers): Tool[Pointed] =
    described.reading((pointed, at, reads) =>
      reads
        .read { (tx: Tx^) ?=>
          val conversation = at.turn.conversationId
          val intro = s"This conversation's id is ${ConversationId.value(conversation)}."
          Tx.clearance(tx).own match {
            case None =>
              Right((s"$intro It belongs to no room, so I can't look for other messages.", None))
            case Some(own) =>
              for {
                mine <- stores.entries.ofTurn(at.turn)
                asker <- askers.of(at.turn)
                found <- (asker, mine.minByOption(_.seq)) match {
                  case (Some(Principal.Person(person, _)), Some(answered)) =>
                    // Past the answered message by a microsecond, the store's resolution.
                    val until = answered.createdAt.plusNanos(1000)
                    (pointed match {
                      case Pointed.First =>
                        stores.rooms.firstBy(own.room, person, Instant.EPOCH, until, Set.empty, 1)
                      case Pointed.Last =>
                        stores.rooms.saidBy(own.room, person, Instant.EPOCH, until, Set.empty, 1)
                    }).map(s => Some(s.headOption))
                  case _ => Right(None)
                }
                answer <- found match {
                  case None =>
                    Right(
                      (
                        s"$intro I can't tell which messages here are the person's, so I can't " +
                          "look for theirs.",
                        None
                      )
                    )
                  case Some(None) =>
                    Right(
                      (
                        s"$intro grit keeps no such message of theirs ${here(own.room)}: older " +
                          "messages are deleted after a while.",
                        None
                      )
                    )
                  case Some(Some(said)) =>
                    stores.principals
                      .speakers(Vector(said.entry.id))
                      .map(s =>
                        (
                          s"$intro\n${shownAs(pointed, own.room, said, s.link(said.entry.id))}",
                          Some(said.entry.integrity)
                        )
                      )
                }
              } yield answer
          }
        }
        .fold(
          (_: StoreError) => Outcome.Failed(Unread),
          (text, integrity) => Outcome.Done(text, integrity)
        )
    )

  /** Where "here" is, in words: a direct message's room, a directory's, or a channel's. */
  private def here(room: Place): String =
    if (room.direct) "in their direct messages with you (every thread counted)"
    else if (room.directory.isDefined) "in this directory (every session counted)"
    else "in this channel (every thread counted)"

  /** `said`, the message `pointed` names, as the answer gives it. */
  private def shownAs(pointed: Pointed, room: Place, said: Said, link: Option[Href]): String = {
    val which = pointed match {
      case Pointed.First => "first"
      case Pointed.Last => "last"
    }
    val text = said.entry.payload match {
      case Payload.Heard(t) => t
      case Payload.Message(Message.User(t)) => t
      case _ => ""
    }
    val begins = text.trim.replaceAll("\\s+", " ")
    val shown = if (begins.length > 80) s"${begins.take(80)}…" else begins
    Vector(
      s"Of their messages ${here(room)}, the $which grit still keeps:",
      s"entry id: ${EntryId.value(said.entry.id)}",
      s"conversation id: ${ConversationId.value(said.conversation)}",
      s"said: ${Utc.toDayMinute(said.entry.createdAt)}",
      link.fold("link: none; the app it came from (Slack, say) gives no link to it")(l =>
        s"link: ${Href.value(l)}"
      ),
      s"it begins: \"$shown\""
    ).mkString("\n")
  }

  /** What the model is told of it. */
  private val described: Hosted[Pointed] =
    Hosted(
      ToolSpec(
        Name,
        "The id grit keeps this conversation under, and the person's first or last message " +
          "here: its id, when it was said, and a link that opens it where it was said, when the " +
          "app it came from has one. Here means the whole room: in a channel, the channel and " +
          "every thread in it; in a direct message, all of the person's direct messages with " +
          "you. Call it when the person asks for a message's id or link, or this conversation's.",
        Args
          .of(
            (message =
              Field.oneOf(
                "Which of the person's messages here: first or last (the last is usually the one " +
                  "you are answering).",
                "first",
                "last"
              )
            )
          )
          .map(a => if (a.message == "first") Pointed.First else Pointed.Last),
        retry = Retry.Rerun,
        action = Some("look up a message's id or link")
      ),
      Gate.Free,
      {
        case Pointed.First => "first"
        case Pointed.Last => "last"
      }
    )
}
