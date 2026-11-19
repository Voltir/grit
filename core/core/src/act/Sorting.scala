package grit.core.act

import scala.annotation.tailrec

import grit.core.classify.{Answer, Ask, ClassifierError, Criterion, Decision, Request, StateJson}
import grit.core.document.{DocText, Document, DocumentSearch}
import grit.core.id.{DocumentVersionId, PluginName}
import grit.core.message.Cut
import grit.core.stitch.StrandExcerpts
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Integrity
import grit.std.either.Traversal.sequence

/** How a sort ([[Dreaming.sort]]) is asked, and what its answer reads as: `R`. Its classifier
  * is sent the state `{"item": {"kind", "text"}}`, `kind` `discussion` or `document` and
  * `text` what the sort read of its item, so its instructions say "this discussion" or "this
  * document". Made only by [[Sorting.choice]] and [[Sorting.each]].
  */
sealed trait Sorting[C <: caps.Pure, R] extends caps.Pure {

  /** The most characters of a discussion shown. */
  def chars: Int

  /** The most characters of a document's text shown, its head cut at a word: the item's, and
    * each candidate's, cut further only when the request would otherwise pass
    * [[Request.MostChars]] ([[Sorting.fitted]]).
    */
  def documentChars: Int

  /** The most candidates one sort by it asks about. */
  def most: Int

  /** The questions about `asked`, each candidate with its text, in the order asked; `None` when
    * `asked` is empty. Their answers read without the texts: a recorded answer reads back by
    * the candidates alone.
    */
  private[act] def questions(asked: Vector[(C, String)]): Option[Ask[Sorting.Shown, R]]
}

object Sorting {

  /** 64: the most candidates one choice is asked over. */
  val Chosen: Int = 64

  /** 40: the most questions one sort asks together. */
  val Most: Int = 40

  /** What a sort is asked among, each candidate by its plugin's value and its document's
    * version: the `pinned`, in order, then at most `fill` of `pool`, those whose text best
    * matches the item's by the documents' index ([[DocumentSearch.rank]]); in all, at most
    * the sorting's [[Sorting.most]], the pinned first. Of either, only a document the sort
    * reads is asked about; none of `pool` when `fill` is not positive.
    */
  final case class Among[C <: caps.Pure](
      pinned: Vector[(C, DocumentVersionId)],
      pool: Vector[(C, DocumentVersionId)],
      fill: Int
  ) extends caps.Pure {

    /** The value of the candidate whose document is `version`, the pinned first. */
    def of(version: DocumentVersionId): Option[C] =
      (pinned ++ pool).collectFirst { case (c, v) if v == version => c }
  }

  /** One choice among at most `most` candidates, each keyed by its number from 1 in the order
    * asked and described by its text's head, then `first`, `second` and `rest`: a [[Decision]]
    * over all of them. `Left` when `most` is under 1 or over [[Chosen]], or when two of
    * `first`, `second`, `rest` share a key or one is a number.
    */
  def choice[C <: caps.Pure](
      instructions: String,
      chars: Int,
      documentChars: Int,
      most: Int,
      first: Criterion[C],
      second: Criterion[C],
      rest: Criterion[C]*
  ): Either[String, Sorting[C, Decision[C]]] = {
    val fixed = first +: second +: rest.toVector
    for {
      _ <- Either.cond(
        most >= 1 && most <= Chosen,
        (),
        s"a choice is among 1 to $Chosen, not $most"
      )
      _ <- fixed
        .find(c => numbered(c.key))
        .map(c => Ask.DuplicateKey(c.key))
        .toLeft(())
        .flatMap(_ => Ask.choice[Shown, C](instructions, first, second, rest*))
        .left
        .map(d => s"a choice's fixed key ${d.key} is a number or repeats another")
    } yield Choosing(instructions, chars, documentChars, most, fixed)
  }

  /** A yes/no per candidate, in one request, asked of at most `nearest`: each question
    * `instructions`, a blank line, then the candidate's text's head; `yes` and `no` what each
    * answer means. Read as each asked candidate with its probability of yes, likeliest first,
    * ties in the order asked. `Left` when `nearest` is under 1 or over [[Most]].
    */
  def each[C <: caps.Pure](
      instructions: String,
      yes: String,
      no: String,
      chars: Int,
      documentChars: Int,
      nearest: Int
  ): Either[String, Sorting[C, Vector[Decision.Weight[C]]]] =
    Either.cond(
      nearest >= 1 && nearest <= Most,
      Each(instructions, yes, no, chars, documentChars, nearest),
      s"a sort asks of 1 to $Most candidates, not $nearest"
    )

  /** [[choice]]'s sorting. */
  private[grit] final case class Choosing[C <: caps.Pure] private[Sorting] (
      instructions: String,
      chars: Int,
      documentChars: Int,
      most: Int,
      fixed: Vector[Criterion[C]]
  ) extends Sorting[C, Decision[C]] {
    private[act] def questions(asked: Vector[(C, String)]): Option[Ask[Shown, Decision[C]]] = {
      val all = asked.zipWithIndex.map { case ((c, text), i) =>
        Criterion(c, (i + 1).toString, Some(text))
      } ++ fixed
      all match {
        case a +: b +: more =>
          // Numbered keys are distinct, and choice refused a fixed key that is a number or
          // repeats one, so no two keys here are the same.
          Ask.choice[Shown, C](instructions, a, b, more*).toOption
        case _ => None
      }
    }
  }

  /** [[each]]'s sorting. */
  private[grit] final case class Each[C <: caps.Pure] private[Sorting] (
      instructions: String,
      yes: String,
      no: String,
      chars: Int,
      documentChars: Int,
      most: Int
  ) extends Sorting[C, Vector[Decision.Weight[C]]] {
    private[act] def questions(
        asked: Vector[(C, String)]
    ): Option[Ask[Shown, Vector[Decision.Weight[C]]]] =
      asked.map((_, text) =>
        Ask.yesNo[Shown](s"$instructions\n\n$text", Some(yes), Some(no))
      ) match {
        case first +: rest =>
          val values = asked.map(_._1)
          // Read by the answers' kinds alone, so a refusal quotes no candidate's text.
          Some(Ask.all(first, rest).readBy { answers =>
            if (answers.size != values.size)
              Left(
                ClassifierError.Unreadable(s"${answers.size} answers to ${values.size} questions")
              )
            else
              sequence(answers.zip(values).map {
                case (Answer.YesNo(p), c) => Right(Decision.Weight(c, p))
                case _ => Left(ClassifierError.Unreadable("a sort's yes/no answered otherwise"))
              }).map(_.sortBy(w => -w.probability))
          })
        case _ => None
      }
  }

  /** A sort's item as its classifier is sent it ([[Sorting]]). */
  private[grit] final case class Shown(kind: String, text: String)

  private[grit] given StateJson[Shown] =
    StateJson.instance(s => ujson.Obj("item" -> ujson.Obj("kind" -> s.kind, "text" -> s.text)))

  /** What a sort read and asks: its item as `shown` and the `questions` about it; the versions
    * `asked` about, in the order asked; and `level`, the meet of `integrity` and of everything
    * read.
    */
  private[grit] final case class Posing[R](
      shown: Shown,
      questions: Ask[Shown, R],
      asked: Vector[DocumentVersionId],
      level: Integrity
  ) {

    /** What the classifier is sent. */
    def request: Request = Request.of(shown, questions)

    def posed: Posed[R] = Posed.judge(shown, questions)
  }

  /** What a sort of `item` among `among` by `sorting` reads in its transaction, starting from
    * `integrity`: the item through `excerpts` (a discussion within `chars`) or `documents` (a
    * document's head within `documentChars`), then the candidates [[Among]] names that
    * `documents` reads and whose plugin is one of `readable`, each its text's head as
    * [[fitted]] cuts it; why it asks nothing, when it does not.
    */
  private[grit] def read[C <: caps.Pure, R](
      item: Dreaming.Item,
      among: Among[C],
      sorting: Sorting[C, R],
      excerpts: StrandExcerpts,
      documents: DocumentSearch,
      readable: Vector[PluginName],
      integrity: Integrity
  )(using Tx^): Either[StoreError, Either[Unasked, Posing[R]]] = {
    def head(t: DocText) = Cut.head(DocText.value(t), sorting.documentChars)
    val shown: Either[StoreError, Either[Unasked, (Shown, Integrity)]] = item match {
      case Dreaming.Item.Discussion(seen) =>
        excerpts
          .excerpt(seen, sorting.chars)
          .map(
            _.map(s => (Shown("discussion", s.excerpt.text), s.integrity))
              .toRight(Unasked.Changed)
          )
      case Dreaming.Item.Document(version) =>
        documents
          .labelled(Vector(version))
          .map(_.collectFirst {
            case (d, _) if readable.contains(d.plugin) =>
              (Shown("document", head(d.text)), d.integrity)
          }.toRight(Unasked.Unread))
    }
    def valued(ds: Vector[Document], of: Vector[(C, DocumentVersionId)]) =
      ds.filter(d => readable.contains(d.plugin))
        .flatMap(d => of.collectFirst { case (c, v) if v == d.version => (c, d) })
    shown.flatMap {
      case Left(why) => Right(Left(why))
      case Right((it, believed)) =>
        for {
          labelled <- documents.labelled(among.pinned.map(_._2))
          pinned = valued(labelled.map(_._1), among.pinned).take(sorting.most)
          room = among.fill.min(sorting.most - pinned.size)
          named = among.pinned.map(_._2).toSet
          pool = among.pool.filterNot((_, v) => named.contains(v))
          hits <-
            if (room < 1) Right(Vector.empty)
            else documents.rank(pool.map(_._2), it.text, room)
        } yield {
          val asked = pinned ++ valued(hits.map(_.document), pool)
          fitted(sorting, it, asked.map((c, d) => (c, head(d.text)))).map(qs =>
            Posing(
              it,
              qs,
              asked.map(_._2.version),
              asked.map(_._2.integrity).foldLeft(integrity.meet(believed))(_.meet(_))
            )
          )
        }
    }
  }

  /** The questions `sorting` asks about `shown` and `asked`, each candidate's text as given
    * unless it passes an equal share of what [[Request.MostChars]] leaves once the request
    * without them is counted ([[Request.size]]), as JSON escapes it; one that passes is cut to
    * that share. So the request never holds more, and one within the limit is sent as it is.
    * `Unread` when `sorting` asks nothing of `asked`, or there is no room left for one
    * character of each.
    */
  private[act] def fitted[C <: caps.Pure, R](
      sorting: Sorting[C, R],
      shown: Shown,
      asked: Vector[(C, String)]
  ): Either[Unasked, Ask[Shown, R]] =
    sorting.questions(asked.map((c, _) => (c, ""))).toRight(Unasked.Unread).flatMap { bare =>
      val share =
        if (asked.isEmpty) 0
        else (Request.MostChars - Request.size(Request.of(shown, bare))) / asked.size
      if (asked.nonEmpty && share < 1) Left(Unasked.Unread)
      else
        sorting.questions(asked.map((c, t) => (c, cut(t, share)))).toRight(Unasked.Unread)
    }

  /** `text`'s head, as [[Cut.head]] cuts it, within `share` characters as JSON escapes it. */
  private def cut(text: String, share: Int): String = {
    def escaped(t: String) = ujson.Str(t).render().length - 2
    @tailrec def within(t: String): String = escaped(t) - share match {
      case over if over > 0 => within(Cut.head(t, t.length - over))
      case _ => t
    }
    within(Cut.head(text, share))
  }

  /** Why a sort asks nothing: its item's messages are not the ones its scan digested
    * (`Changed`), or its item is a document not read, or no candidate is left to ask about
    * while its sorting asks of candidates alone, or no room is left for their text within the
    * classifier's limit (`Unread`).
    */
  private[grit] enum Unasked {
    case Changed, Unread
  }

  /** The questions a sort's recorded answers about `asked`, in the order asked, read by. */
  private[grit] def reading[C <: caps.Pure, R](
      sorting: Sorting[C, R],
      asked: Vector[C]
  ): Option[Ask[Shown, R]] = sorting.questions(asked.map((_, "")))

  /** Whether `key` is all digits, as a candidate's is. */
  private def numbered(key: String): Boolean = key.nonEmpty && key.forall(_.isDigit)
}
