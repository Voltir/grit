package grit.tools

import java.time.Instant

import grit.core.clock.Utc
import grit.core.id.{ConversationId, EntryId, Href}
import grit.core.identity.Principal
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Place
import grit.core.recipe.RoomReads
import grit.core.store.{Askers, Entry, EntrySearch, EntryStore, Payload, Principals, StoreError, Tx}
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSpec
}
import grit.core.visibility.Integrity

/** `message_ids`: the id grit keeps this conversation under, and the asker's first or last
  * message in this room, or the message there whose words best match a quote, by its entry id,
  * its conversation, when it was said and the link its edge recorded it with. Each call reads
  * in one transaction of the reads its turn hands it ([[grit.core.tool.Hosted.reading]]), so
  * it finds only what the turn may read; the room is
  * that transaction's own ([[grit.core.visibility.Clearance.own]]), every conversation in it
  * counted.
  */
object MessageIds {

  /** Its tool's name, `message_ids`. */
  val Name: ToolName = ToolName("message_ids")

  /** Which message: the asker's first or last in the room, or the one, anyone's, whose words
    * best match `words`.
    */
  enum Pointed {
    case First, Last
    case Quoted(words: String)
  }

  /** Where it reads: the turn's own entries, the room's messages and its search, and their
    * authors' links.
    */
  final case class Stores(
      entries: EntryStore,
      rooms: RoomReads,
      search: EntrySearch,
      principals: Principals
  )

  /** 5: the best matches a quote reads, of which the first message is the answer (a closing
    * among them is passed over).
    */
  val Matches: Int = 5

  /** The answer when the store cannot be read: it names nothing of what failed. */
  val Unread: String = "I could not read the messages just now. Ask again in a moment."

  /** `message_ids` over `stores`, the asker as `askers` resolves them: a message is read up to
    * the one the turn answers, so one said since is not among them; a quote is matched as
    * [[grit.core.store.EntrySearch.room]] ranks, over the room's whole history, the first message
    * of its best [[Matches]] the answer: a person's, to grit or heard, or grit's reply, never a
    * closing. Its answer is believed at the message's integrity when it shows one. A store it
    * cannot read is an error [[Outcome]], [[Unread]].
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
                // Past the answered message by a microsecond, the store's resolution.
                until = mine.minByOption(_.seq).map(_.createdAt.plusNanos(1000))
                found <- (pointed, asker, until) match {
                  case (Pointed.Quoted(words), _, Some(until)) =>
                    quoted(stores, own.room, words, until).map(Some(_))
                  case (Pointed.First, Some(Principal.Person(person, _)), Some(until)) =>
                    stores.rooms
                      .firstBy(own.room, person, Instant.EPOCH, until, Set.empty, 1)
                      .map(s => Some(s.headOption.map(_.entry)))
                  case (Pointed.Last, Some(Principal.Person(person, _)), Some(until)) =>
                    stores.rooms
                      .saidBy(own.room, person, Instant.EPOCH, until, Set.empty, 1)
                      .map(s => Some(s.headOption.map(_.entry)))
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
                        (pointed match {
                          case Pointed.Quoted(_) =>
                            s"$intro grit keeps no message ${here(own.room)} that matches those " +
                              "words: older messages are deleted after a while."
                          case Pointed.First | Pointed.Last =>
                            s"$intro grit keeps no such message of theirs ${here(own.room)}: " +
                              "older messages are deleted after a while."
                        }),
                        None
                      )
                    )
                  case Some(Some(entry)) =>
                    stores.principals
                      .speakers(Vector(entry.id))
                      .map(s =>
                        (
                          s"$intro\n${shownAs(pointed, own.room, entry, s.link(entry.id))}",
                          Some(entry.integrity)
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

  /** The first message among the best [[Matches]] of `words` in `room` said before `until`. */
  private def quoted(stores: Stores, room: Place, words: String, until: Instant)(using
      Tx^
  ): Either[StoreError, Option[Entry]] =
    stores.search
      .room(room, Instant.EPOCH, until, words, Matches)
      .flatMap(hits =>
        hits.foldLeft[Either[StoreError, Vector[Entry]]](Right(Vector.empty))((read, hit) =>
          read.flatMap(es => stores.entries.get(hit.id).map(es ++ _))
        )
      )
      .map(_.find(e => text(e).isDefined))

  /** The words of `entry` when it is a message: a person's, or grit's reply or post. */
  private def text(entry: Entry): Option[String] = entry.payload match {
    case Payload.Heard(t) => Some(t)
    case Payload.Message(Message.User(t)) => Some(t)
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      Some(blocks.collect { case AssistantBlock.Text(t) => t }.mkString("\n"))
    case Payload.Posted(t) => Some(t)
    case _ => None
  }

  /** `entry`, the message `pointed` names, as the answer gives it. */
  private def shownAs(pointed: Pointed, room: Place, entry: Entry, link: Option[Href]): String = {
    val which = pointed match {
      case Pointed.First => s"Of their messages ${here(room)}, the first grit still keeps:"
      case Pointed.Last => s"Of their messages ${here(room)}, the last grit still keeps:"
      case Pointed.Quoted(_) => s"Of the messages ${here(room)}, the one that best matches:"
    }
    val begins = text(entry).getOrElse("").trim.replaceAll("\\s+", " ")
    val shown = if (begins.length > 80) s"${begins.take(80)}…" else begins
    Vector(
      which,
      s"entry id: ${EntryId.value(entry.id)}",
      s"conversation id: ${ConversationId.value(entry.conversationId)}",
      s"said: ${Utc.toDayMinute(entry.createdAt)}",
      // None is either a message recorded before links were kept or one from an edge that links
      // none; nothing tells which, so the line claims neither.
      link.fold(
        "link: none kept for it (grit keeps none for messages from before links were recorded, " +
          "or from an app that gives none)"
      )(l => s"link: ${Href.value(l)}"),
      s"it begins: \"$shown\""
    ).mkString("\n")
  }

  /** What the model is told of it. */
  private val described: Hosted[Pointed] =
    Hosted(
      ToolSpec(
        Name,
        "The id grit keeps this conversation under, and a message here: the person's first or " +
          "last, or the one whose words best match a quote; its id, when it was said, and a " +
          "link that opens it where it was said, when the app it came from has one. Here means " +
          "the whole room: in a channel, the channel and every thread in it; in a direct " +
          "message, all of the person's direct messages with you. Call it when the person asks " +
          "for a message's id or link, or this conversation's.",
        Args
          .of(
            (
              message = Field.oneOf(
                "Which message here: the person's first or last (the last is usually the one " +
                  "you are answering), or quoted: the one, anyone's, whose words best match " +
                  "`quote`.",
                "first",
                "last",
                "quoted"
              ),
              quote = Field.text("Words from the message, for quoted.").optional
            )
          )
          .refine(a =>
            (a.message, a.quote.map(_.trim).filter(_.nonEmpty)) match {
              case ("quoted", Some(words)) => Right(Pointed.Quoted(words))
              case ("quoted", None) =>
                Left(ArgsError.Missing("quote", "the words to look for, for quoted"))
              case (_, Some(_)) =>
                Left(ArgsError.Refused("quote", "`quote` is only for `message` quoted."))
              case ("first", None) => Right(Pointed.First)
              case (_, None) => Right(Pointed.Last)
            }
          ),
        retry = Retry.Rerun,
        action = Some("look up a message's id or link")
      ),
      Gate.Free,
      {
        case Pointed.First => "first"
        case Pointed.Last => "last"
        case Pointed.Quoted(words) => s"quoted \"$words\""
      },
      // Not grit's own words: its "it begins" line quotes a person's message, anyone's under
      // `quoted`.
      Believed.At(Integrity.Untrusted)
    )
}
