package grit.knowledge.plugin

import scala.concurrent.duration.*

import grit.core.context.Shown
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.id.{ConversationId, DocKey, LineId, PluginName}
import grit.core.period.{Change, Ground, Line, Section}
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.store.{ClosedPeriod, StoreError, Tx}

/** Each decision or fact a conversation settled, kept as a document of its own (ADR 0028), so a
  * window elsewhere can find it once the conversation is done: every Standing line an earned
  * closing ([[grit.core.period.CloseReason.shownElsewhere]]) adds, under its conversation and
  * line, at the closed conversation's label, believed at its closing's integrity, placed at its
  * conversation, written at that close; its text the line on its ground (a line only the
  * assistant said, as not confirmed) and what the closing came to
  * ([[grit.core.period.Closing.headline]]). Withdrawn when a later closing of its conversation
  * resolves, drops or evicts the line, whatever that closing's reason; a line carried unchanged
  * writes nothing. A line whose adding closing was purged before the plugin was first posted is
  * never kept. No model call.
  */
final class StandingDocuments(val name: PluginName) extends Plugin {

  val version: Int = 1

  override val documents: Option[Documents] = Some(StandingDocuments.Kept)
}

object StandingDocuments {

  /** Label "standing: a decision or fact a conversation settled", unscaled, kept 30 days, at
    * most 1000 documents over every label.
    */
  val Terms: DocumentTerms =
    // Literals both constructors accept, read when the object is first used: every test of
    // the plugin throws here if not.
    (for {
      label <- DocLabel.of("standing: a decision or fact a conversation settled")
      terms <- DocumentTerms.of(label, DocWeight.Unscaled, 30.days, 1000)
    } yield terms).fold(why => throw new IllegalStateException(why), identity)

  /** Its documents, under [[Terms]], posted [[Posted]]. */
  private object Kept extends Documents {
    val terms: DocumentTerms = Terms
    val posting: Option[DocumentPosting] = Some(Posted)
  }

  /** Each Standing line a closing adds, kept; each it takes out, withdrawn. */
  private object Posted extends DocumentPosting {
    def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using
        tx: Tx^
    ): Either[StoreError, Unit] = {
      // Posting reads and keeps at the closed conversation's label (Tx.cleared).
      val label = Tx.cleared(tx)
      val standing = closed.closing.balance.in(Section.Standing).map(_.id).toSet
      // An Added line the cap then refused is not in the balance: never written.
      val added =
        if (closed.reason.shownElsewhere) closed.closing.flows.changes.collect {
          case Change.Added(l) if standing(l.id) => l
        }
        else Vector.empty
      val gone = closed.closing.flows.changes.collect {
        case Change.Resolved(l, _) if l.section == Section.Standing => l
        case Change.Dropped(l, _) if l.section == Section.Standing => l
        case Change.Evicted(l) if l.section == Section.Standing => l
      }
      for {
        _ <- each(gone)(l => key(closed, l).flatMap(keeper.withdraw(_, label, closed.at)))
        _ <- each(added) { l =>
          for {
            k <- key(closed, l)
            text <- DocText.of(text(l, closed.closing.headline)).left.map(StoreError.Invalid(_))
            _ <- keeper.write(
              k,
              label,
              closed.integrity,
              closed.origin.place,
              text,
              data(closed, l),
              None,
              closed.at
            )
          } yield ()
        }
      } yield ()
    }
  }

  /** `standing:{conversation}/{line}`: a line's id is the same in every conversation. */
  private def key(closed: ClosedPeriod, l: Line): Either[StoreError, DocKey] =
    DocKey
      .of(
        s"standing:${ConversationId.value(closed.ref.conversationId)}/${LineId.value(l.id)}"
      )
      .left
      .map(StoreError.Invalid(_))

  /** The line on its ground, then what its closing came to. */
  private def text(l: Line, headline: String): String = {
    val said = l.ground match {
      case Some(Ground.Claimed) => Shown.unconfirmed(l.text)
      case Some(Ground.Tool) => s"${l.text} (a tool's result showed it)"
      case Some(Ground.Person) => s"${l.text} (a person said so)"
      // A Standing line always has a ground; a line without one is shown as it reads.
      case None => l.text
    }
    s"$said\nFrom a conversation that came to: $headline"
  }

  private def data(closed: ClosedPeriod, l: Line): ujson.Value =
    ujson.Obj(
      "conversation" -> ConversationId.value(closed.ref.conversationId),
      "line" -> LineId.value(l.id),
      "ground" -> l.ground.fold(ujson.Null: ujson.Value)(g => ujson.Str(g.key))
    )

  private def each[A](as: Vector[A])(f: A => Either[StoreError, ?]): Either[StoreError, Unit] =
    as.foldLeft[Either[StoreError, Unit]](Right(()))((done, a) =>
      done.flatMap(_ => f(a).map(_ => ()))
    )
}
