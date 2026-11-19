package grit.core.act

import grit.core.classify.{Answer, Ask, ClassifierError, Criterion, Decision, Request, StateJson}
import grit.core.document.{DocText, DocumentSearch}
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

  /** The most characters of a document's text shown, its head cut at a word. */
  def documentChars: Int

  /** The candidates asked about, by their place in `texts` (each candidate's text as read), in
    * the order asked, given `item`, the item's text.
    */
  private[act] def nearest(item: String, texts: Vector[String]): Vector[Int]

  /** The questions about `asked`, each candidate with its text, in the order asked; `None` when
    * `asked` is empty. Their answers read without the texts: a recorded answer reads back by
    * the candidates alone.
    */
  private[act] def questions(asked: Vector[(C, String)]): Option[Ask[Sorting.Shown, R]]
}

object Sorting {

  /** 40: the most questions one sort asks together. */
  val Most: Int = 40

  /** One choice among the candidates read, each keyed by its number from 1 in the order read
    * and described by its text's head, then `first`, `second` and `rest`: a [[Decision]] over
    * all of them. `Left` when two of `first`, `second`, `rest` share a key or one is a number.
    */
  def choice[C <: caps.Pure](
      instructions: String,
      chars: Int,
      documentChars: Int,
      first: Criterion[C],
      second: Criterion[C],
      rest: Criterion[C]*
  ): Either[Ask.DuplicateKey, Sorting[C, Decision[C]]] = {
    val fixed = first +: second +: rest.toVector
    fixed
      .find(c => numbered(c.key))
      .map(c => Ask.DuplicateKey(c.key))
      .toLeft(())
      .flatMap(_ => Ask.choice[Shown, C](instructions, first, second, rest*))
      .map(_ => Choosing(instructions, chars, documentChars, fixed))
  }

  /** A yes/no per candidate, in one request, asked of the `nearest` candidates read whose text
    * the item's matches best (BM25 over the candidates read, ties in their order): each
    * question `instructions`, a blank line, then the candidate's text's head; `yes` and `no`
    * what each answer means. Read as each asked candidate with its probability of yes,
    * likeliest first. `Left` when `nearest` is under 1 or over [[Most]].
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
      fixed: Vector[Criterion[C]]
  ) extends Sorting[C, Decision[C]] {
    private[act] def nearest(item: String, texts: Vector[String]): Vector[Int] =
      texts.indices.toVector

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
    private[act] def nearest(item: String, texts: Vector[String]): Vector[Int] =
      Bm25.ranked(item, texts).take(most)

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
    * document's head within `documentChars`), and each of `among` that `documents` reads and
    * whose plugin is one of `readable`, its head within `documentChars`; why it asks nothing,
    * when it does not.
    */
  private[grit] def read[C <: caps.Pure, R](
      item: Dreaming.Item,
      among: Vector[(C, DocumentVersionId)],
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
    shown.flatMap {
      case Left(why) => Right(Left(why))
      case Right((it, believed)) =>
        documents.labelled(among.map(_._2)).map { ds =>
          val read = ds.collect {
            case (d, _) if readable.contains(d.plugin) =>
              among.collectFirst { case (c, v) if v == d.version => (c, d) }
          }.flatten
          val asked = sorting.nearest(it.text, read.map((_, d) => head(d.text))).flatMap(read.lift)
          sorting
            .questions(asked.map((c, d) => (c, head(d.text))))
            .map(qs =>
              Posing(
                it,
                qs,
                asked.map(_._2.version),
                asked.map(_._2.integrity).foldLeft(integrity.meet(believed))(_.meet(_))
              )
            )
            .toRight(Unasked.Unread)
        }
    }
  }

  /** Why a sort asks nothing: its item's messages are not the ones its scan digested
    * (`Changed`), or its item is a document not read, or no candidate is left to ask about
    * while its sorting asks of candidates alone (`Unread`).
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

  /** Okapi BM25 (k1 1.2, b 0.75) of a query over a few texts, as the spike that chose sorting
    * measured it: words are runs of letters and digits, lower-cased, of two characters or more,
    * and not [[Stop]] words.
    */
  private object Bm25 {

    /** Each of `texts`' places, best match of `query` first, ties in their order. */
    def ranked(query: String, texts: Vector[String]): Vector[Int] = {
      val docs = texts.map(words)
      val counts = docs.map(_.groupMapReduce(identity)(_ => 1)(_ + _))
      val n = docs.size.toDouble
      val mean = if (docs.isEmpty) 1.0 else (docs.map(_.size).sum.toDouble / n).max(1.0)
      val df = counts.flatMap(_.keys).groupMapReduce(identity)(_ => 1)(_ + _)
      val q = words(query)
      counts
        .zip(docs.map(_.size))
        .map { (c, length) =>
          q.map(w =>
            c.get(w).fold(0.0) { f =>
              val d = df.getOrElse(w, 0).toDouble
              val idf = math.log(1 + (n - d + 0.5) / (d + 0.5))
              idf * f * (K1 + 1) / (f + K1 * (1 - B + B * length / mean))
            }
          ).sum
        }
        .zipWithIndex
        .sortBy((score, _) => -score)
        .map(_._2)
    }

    private val K1 = 1.2
    private val B = 0.75

    private def words(text: String): Vector[String] =
      "[\\p{L}\\p{N}]+".r
        .findAllIn(text.toLowerCase)
        .filter(w => w.length > 1 && !Stop.contains(w))
        .toVector

    private val Stop: Set[String] =
      ("a an the and or of to in on for with at by from is are was were be been it its this that " +
        "these those as but not no we i you he she they them our your their his her has have had " +
        "do does did so if then than there here what which who whom when where why how all any " +
        "each can could will would should may might just also about into over after before up " +
        "down out more most some such only own same very s t").split(' ').toSet
  }
}
