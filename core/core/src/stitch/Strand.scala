package grit.core.stitch

import java.time.Instant

import grit.core.id.ConversationId
import grit.core.place.Place
import grit.core.store.Entry
import grit.core.visibility.{Integrity, Label}

/** `conversation`'s first message continues the exchange `root` began. */
final case class Link(conversation: ConversationId, root: ConversationId)

/** An entry of a conversation other than the reader's, with the place it was said at. */
final case class Said(conversation: ConversationId, place: Place, entry: Entry)

object Strand {

  /** The strand of `conversation` among `links`: the root it follows (itself when it follows
    * none) and every conversation that follows that root. One hop: a follower of a follower is
    * not in it.
    */
  def of(conversation: ConversationId, links: Vector[Link]): Set[ConversationId] = {
    val root = links.find(_.conversation == conversation).fold(conversation)(_.root)
    links.filter(_.root == root).map(_.conversation).toSet + root
  }

  /** What a reader is shown of its strand ([[Along.read]]): the root's `opening` message when
    * the root is another conversation and its first message is still kept; what the other
    * members `said` in the range read, oldest first (the opening among them when it was said
    * in range); the members whose first message is `gone` (their raw entries purged), shown by
    * their record instead; and every member but the reader (`conversations`), whether or not
    * anything of theirs is shown.
    */
  final case class Read(
      opening: Option[Said],
      said: Vector[Said],
      gone: Vector[ConversationId],
      conversations: Set[ConversationId]
  ) {

    /** Every entry it shows: the opening, then what was said, each once. */
    def shown: Vector[grit.core.store.Entry] =
      (opening.toVector ++ said).map(_.entry).distinctBy(_.id)

    /** Every conversation it shows messages of. */
    def members: Vector[ConversationId] =
      (opening.map(_.conversation).toVector ++ said.map(_.conversation)).distinct
  }

  object Read {
    val empty: Read = Read(None, Vector.empty, Vector.empty, Set.empty)
  }

  /** A strand as it stood `asOf`, by ids and counts alone ([[StrandShelf]]): its `room` and the
    * label in force there; its `root` and `members`, the root first then its followers by
    * first message, each holding a message said before `asOf` that is still kept and its
    * reader reads; how many messages they hold (`said`) and how many characters of them a
    * reader is shown, at most [[Stitching.MessageChars]] of each (`chars`); when the first and
    * last were said; and their [[StrandDigest]].
    */
  final case class Seen(
      room: Place,
      roomLabel: Label,
      root: ConversationId,
      members: Vector[ConversationId],
      said: Int,
      chars: Int,
      since: Instant,
      touched: Instant,
      digest: StrandDigest,
      asOf: Instant
  ) extends caps.Pure

  object Seen {

    /** `seen` as a step records it: `{"room", "roomLabel", "root", "members": [...], "said",
      * "chars", "since", "touched", "digest", "asOf"}`, a place and a label in their written
      * forms, instants ISO-8601. Ids and counts alone: no message's text.
      */
    def write(seen: Seen): ujson.Value = ujson.Obj(
      "room" -> seen.room.written,
      "roomLabel" -> Label.written(seen.roomLabel),
      "root" -> ConversationId.value(seen.root),
      "members" -> ujson.Arr.from(seen.members.map(ConversationId.value)),
      "said" -> seen.said,
      "chars" -> seen.chars,
      "since" -> seen.since.toString,
      "touched" -> seen.touched.toString,
      "digest" -> StrandDigest.value(seen.digest),
      "asOf" -> seen.asOf.toString
    )

    /** A strand [[write]] wrote, or why `v` is not one. */
    def read(v: ujson.Value): Either[String, Seen] = {
      def field(key: String): Either[String, ujson.Value] =
        v.objOpt.flatMap(_.get(key)).toRight(s"a strand has no '$key'")
      def text(key: String): Either[String, String] =
        field(key).flatMap(_.strOpt.toRight(s"a strand's '$key' is not text"))
      def count(key: String): Either[String, Int] =
        field(key).flatMap(_.numOpt.map(_.toInt).toRight(s"a strand's '$key' is not a number"))
      def instant(key: String): Either[String, Instant] =
        text(key).flatMap(t =>
          scala.util.Try(Instant.parse(t)).toOption.toRight(s"a strand's '$key' is no instant")
        )
      for {
        room <- text("room").flatMap(Place.read)
        label <- text("roomLabel").flatMap(Label.read)
        root <- text("root")
        members <- field("members").flatMap(
          _.arrOpt
            .flatMap(a => Option.when(a.forall(_.strOpt.nonEmpty))(a.toVector.flatMap(_.strOpt)))
            .toRight("a strand's 'members' are not text")
        )
        said <- count("said")
        chars <- count("chars")
        since <- instant("since")
        touched <- instant("touched")
        digest <- text("digest").flatMap(StrandDigest.read)
        asOf <- instant("asOf")
      } yield Seen(
        room,
        label,
        ConversationId(root),
        members.map(ConversationId(_)),
        said,
        chars,
        since,
        touched,
        digest,
        asOf
      )
    }
  }

  /** A strand's messages as an excerpt shows them, and the integrity they are believed at
    * together ([[StrandExcerpts]]).
    */
  final case class Shown(excerpt: Stitching.Excerpt, integrity: Integrity)
}
