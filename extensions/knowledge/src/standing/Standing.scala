package grit.knowledge.standing

import java.time.ZoneOffset

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, Document, DocumentKeeper, DocumentTerms}
import grit.core.either.Traversal.traverse
import grit.core.id.{ConversationId, DocKey, LineId, PeriodSeq}
import grit.core.period.{Change, Ground, Line, Section}
import grit.core.plugin.DocumentPosting
import grit.core.store.{ClosedPeriod, StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Label

/** What each earned closing ([[grit.core.period.CloseReason.shownElsewhere]]) of a conversation
  * settled, kept as one document (ADR 0028) so a window elsewhere can find it once the
  * conversation is done: the closing's headline ([[grit.core.period.Closing.headline]]), then
  * each Standing line it added on a person's or a tool's ground whose add
  * ([[grit.core.period.Change.Added]]) names none of the tools left out
  * ([[Standing.posting]]); a closing adding none keeps nothing. At the closed conversation's
  * label, believed at its closing's integrity, placed at its conversation, written at that
  * close. When a later closing of the conversation, earned or not, resolves, drops or evicts
  * one of its lines, the document is written again at that close without it, at the integrity
  * it had, or withdrawn when none of its lines is left; a line carried unchanged writes
  * nothing. A closing purged before its plugin was first posted keeps nothing. No model call.
  */
object Standing {

  /** Label "what a conversation settled", unscaled, kept 30 days, at most 1000 documents over
    * every label. A document's data is `{"conversation", "period", "day", "headline", "lines":
    * [{"line", "ground"}]}`: the adding closing's conversation, period, UTC day and headline,
    * and each line it keeps by id with its ground's key.
    */
  val Terms: DocumentTerms =
    // Literals both constructors accept, read when the object is first used: every test of
    // the plugin throws here if not.
    (for {
      label <- DocLabel.of("what a conversation settled")
      terms <- DocumentTerms.of(label, DocWeight.Unscaled, 30.days, 1000)
    } yield terms).fold(why => throw new IllegalStateException(why), identity)

  /** The posting of its documents, under [[Terms]], leaving out the lines whose add names one
    * of the tools in `leftOut`.
    */
  def posting(leftOut: Set[ToolName]): DocumentPosting = new Posted(leftOut.map(ToolName.value))

  /** Each closing's kept lines, but those whose add names one of the tools `leftOut`, written;
    * each document a closing takes a line from, written again without it.
    */
  private final class Posted(leftOut: Set[String]) extends DocumentPosting {
    def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using
        tx: Tx^
    ): Either[StoreError, Unit] = {
      // Posting reads and keeps at the closed conversation's label (Tx.cleared).
      val label = Tx.cleared(tx)
      val standing = closed.closing.balance.in(Section.Standing)
      val inBalance = standing.map(_.id).toSet
      // An Added line the cap then refused is not in the balance: never written.
      val added =
        if (closed.reason.shownElsewhere) closed.closing.flows.changes.collect {
          case Change.Added(l, cited)
              if inBalance(l.id) && grounded(l) && !cited.tools.exists(leftOut) =>
            l
        }
        else Vector.empty
      val gone = closed.closing.flows.changes.collect {
        case Change.Resolved(l, _) if l.section == Section.Standing => l
        case Change.Dropped(l, _) if l.section == Section.Standing => l
        case Change.Evicted(l) if l.section == Section.Standing => l
      }
      for {
        _ <- traverse(gone.map(_.since).distinct)(since =>
          for {
            k <- key(closed, since)
            now <- keeper.current(k, label)
            _ <- now.fold(Right(()))(again(keeper, k, label, _, standing, closed))
          } yield ()
        ).map(_ => ()) // a bare Vector[Unit] binding holds Vector's Mutable prefix1
        _ <-
          if (added.isEmpty) Right(())
          else {
            val made = Made(
              closed.ref.conversationId,
              closed.ref.seq,
              closed.at.atOffset(ZoneOffset.UTC).toLocalDate.toString,
              closed.closing.headline,
              added
            )
            for {
              k <- key(closed, closed.ref.seq)
              text <- made.text
              _ <- keeper.write(
                k,
                label,
                closed.integrity,
                closed.origin.place,
                text,
                made.data,
                None,
                closed.at
              )
            } yield ()
          }
      } yield ()
    }
  }

  /** `document`, under `key` at `label`, written again at `closed`'s close with only its lines
    * still in `standing`, or withdrawn when none is, or when its data does not read: a document
    * this plugin cannot read back cannot be told apart from one it no longer keeps.
    */
  private def again(
      keeper: DocumentKeeper,
      key: DocKey,
      label: Label,
      document: Document,
      standing: Vector[Line],
      closed: ClosedPeriod
  )(using Tx^): Either[StoreError, Unit] =
    Made.read(document.data, standing) match {
      case Some(made) if made.lines.nonEmpty =>
        made.text.flatMap(text =>
          keeper
            .write(
              key,
              label,
              document.integrity,
              document.place,
              text,
              made.data,
              document.pointer,
              closed.at
            )
            .map(_ => ())
        )
      case _ => keeper.withdraw(key, label, closed.at).map(_ => ())
    }

  /** Whether `l` is one a document keeps: a Standing line a person or a tool established. */
  private def grounded(l: Line): Boolean =
    l.section == Section.Standing && l.ground.exists(g => g == Ground.Person || g == Ground.Tool)

  /** A document as made: what the closing of `conversation`'s `period`, on `day`, came to, and
    * the `lines` it keeps, in the balance's order.
    */
  private final case class Made(
      conversation: ConversationId,
      period: PeriodSeq,
      day: String,
      headline: String,
      lines: Vector[Line]
  ) {
    def text: Either[StoreError, DocText] =
      DocText
        .of((headline +: lines.map(l => s"- ${l.text}")).mkString("\n"))
        .left
        .map(StoreError.Invalid(_))

    def data: ujson.Value =
      ujson.Obj(
        "conversation" -> ConversationId.value(conversation),
        "period" -> PeriodSeq.value(period).toDouble,
        "day" -> day,
        "headline" -> headline,
        "lines" -> ujson.Arr.from(
          lines.map(l =>
            ujson.Obj(
              "line" -> LineId.value(l.id),
              "ground" -> l.ground.fold(ujson.Null: ujson.Value)(g => ujson.Str(g.key))
            )
          )
        )
      )
  }

  private object Made {

    /** What `data` says, its lines those of `standing` it names; `None` when it does not read. */
    def read(data: ujson.Value, standing: Vector[Line]): Option[Made] =
      for {
        o <- data.objOpt
        conversation <- o.get("conversation").flatMap(_.strOpt)
        period <- o.get("period").flatMap(_.numOpt).flatMap(n => PeriodSeq.of(n.toLong))
        day <- o.get("day").flatMap(_.strOpt)
        headline <- o.get("headline").flatMap(_.strOpt)
        named <- o.get("lines").flatMap(_.arrOpt)
      } yield {
        val ids = named.flatMap(_.objOpt).flatMap(_.get("line")).flatMap(_.strOpt).toSet
        Made(
          ConversationId(conversation),
          period,
          day,
          headline,
          standing.filter(l => ids(LineId.value(l.id)))
        )
      }
  }

  /** `standing:{conversation}/{period}`: the document of the closing of that period. */
  private def key(closed: ClosedPeriod, period: PeriodSeq): Either[StoreError, DocKey] =
    DocKey
      .of(
        s"standing:${ConversationId.value(closed.ref.conversationId)}/${PeriodSeq.value(period)}"
      )
      .left
      .map(StoreError.Invalid(_))
}
