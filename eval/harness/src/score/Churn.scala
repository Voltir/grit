package grit.eval.harness.score

import grit.core.id.WorkflowId
import grit.eval.harness.capture.{Called, CaseId, Fields, Reading, Said, TurnCase}
import grit.eval.harness.label.ChurnPairs
import grit.eval.harness.stats.{Estimate, Proportion}

/** How many hosted calls turns made before reading what they needed, each question asked once
  * before a change and once after it ([[ChurnPairs]]).
  *
  * @param indexed
  *   over the pairs both of whose turns made an indexed read, the calls before the before-turn's
  *   first less the after-turn's, clustered by question: positive when the after-turn read
  *   sooner; `None` under two such questions
  * @param useful
  *   as `indexed`, by each turn's first useful read
  * @param indexedApart
  *   as `indexed`, over the pairs neither of whose turns is confounded
  * @param usefulApart
  *   as `useful`, over the pairs neither of whose turns is confounded
  * @param followed
  *   of the after-turns that made an indexed read, those whose first followed a document their
  *   window held, clustered by question
  */
final case class Churn(
    pairs: Vector[Churn.Pair],
    indexed: Option[Estimate],
    useful: Option[Estimate],
    indexedApart: Option[Estimate],
    usefulApart: Option[Estimate],
    followed: Proportion
)

object Churn {

  /** How a turn read, counting the hosted calls before each read: every call of its tool loop
    * but those of its own `topic` tool, in the order made.
    */
  enum Reads {

    /** It made no indexed read ([[grit.eval.harness.capture.Reading.Indexed]]). */
    case Unread

    /** Its first indexed read came after `before` calls and `followed` a document its window
      * held; its first useful one ([[grit.eval.harness.capture.Reading.useful]]) after `useful`
      * calls, `None` when none was.
      */
    case Read(before: Int, followed: Boolean, useful: Option[Int])
  }

  object Reads {

    /** How `t` read. */
    def of(t: TurnCase): Reads = {
      val calls = t.rounds.flatMap(_.calls).filterNot(_.tool == Called.Topic)
      def first(p: Reading => Boolean) = calls.indexWhere(c => p(c.reading)) match {
        case -1 => None
        case i => Some(i)
      }
      first {
        case Reading.Indexed(_, _) => true
        case Reading.Other => false
      }.fold(Unread) { i =>
        val followed = calls
          .lift(i)
          .exists(_.reading match {
            case Reading.Indexed(f, _) => f
            case Reading.Other => false
          })
        Read(i, followed, first(_.useful))
      }
    }
  }

  /** One side of a pair: the message that asked, the turn that answered it, how it read, and
    * whether it is `confounded`: its window held a part of another thread of the pairs, so it
    * may have read the other side's answer.
    */
  final case class Side(at: CaseId, workflow: WorkflowId, reads: Reads, confounded: Boolean)

  /** A question's two turns. */
  final case class Pair(question: String, before: Side, after: Side)

  /** `pairs` over `turns`, each message's turn the first that answers it; why not, naming the
    * question and the message, when no turn answers one of its messages.
    */
  def of(turns: Vector[TurnCase], pairs: ChurnPairs): Either[String, Churn] = {
    def answering(question: String, side: String, at: CaseId) =
      turns
        .find(_.said == Said.Slack(at))
        .toRight(s"churn: question $question: no turn answers its $side message ${at.written}")
    for {
      found <- Fields.each(pairs.questions.toVector) { (q, p) =>
        for {
          before <- answering(q, "before", p.before)
          after <- answering(q, "after", p.after)
        } yield (q, p, before, after)
      }
    } yield {
      val threads = found.flatMap((_, _, b, a) => Vector(b.conversation, a.conversation)).toSet
      def side(at: CaseId, t: TurnCase) = Side(
        at,
        t.workflow,
        Reads.of(t),
        t.window.exists(
          _.parts.exists(p => p.conversation != t.conversation && threads.contains(p.conversation))
        )
      )
      val paired = found.map((q, p, b, a) => Pair(q, side(p.before, b), side(p.after, a)))
      def difference(ps: Vector[Pair])(calls: Reads => Option[Int]) =
        Estimate.clustered(ps.flatMap { p =>
          for {
            b <- calls(p.before.reads)
            a <- calls(p.after.reads)
          } yield p.question -> (b - a).toDouble
        })
      val apart = paired.filterNot(p => p.before.confounded || p.after.confounded)
      val first: Reads => Option[Int] = {
        case Reads.Read(before, _, _) => Some(before)
        case Reads.Unread => None
      }
      val useful: Reads => Option[Int] = {
        case Reads.Read(_, _, at) => at
        case Reads.Unread => None
      }
      Churn(
        paired,
        difference(paired)(first),
        difference(paired)(useful),
        difference(apart)(first),
        difference(apart)(useful),
        Proportion.of(
          paired.flatMap(p =>
            p.after.reads match {
              case Reads.Read(_, followed, _) => Some(p.question -> followed)
              case Reads.Unread => None
            }
          )
        )
      )
    }
  }
}
