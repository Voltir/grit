package grit.eval.harness.score

import grit.core.either.Traversal
import grit.core.id.WorkflowId
import grit.eval.harness.capture.{Call, Called, CaseId, Reading, Said, TurnCase}
import grit.eval.harness.label.{ChurnQuestions, Gold}
import grit.eval.harness.stats.{Estimate, Proportion}

/** How many hosted calls turns made before reading what they needed, and whether their replies
  * were right, each question asked once before a change and once after it, in two rounds of
  * [[ChurnQuestions]].
  *
  * @param indexed
  *   over the pairs both of whose turns made an indexed read, the calls before the before-turn's
  *   first less the after-turn's, clustered by question: positive when the after-turn read
  *   sooner; `None` under two such questions
  * @param useful
  *   as `indexed`, by each turn's first useful read
  * @param indexedClean
  *   as `indexed`, over the pairs both of whose sides are clean ([[Churn.Side.clean]])
  * @param usefulClean
  *   as `useful`, over the pairs both of whose sides are clean
  * @param followed
  *   of the after-turns that made an indexed read, those whose first followed a document their
  *   window held, clustered by question
  * @param correctness
  *   how the pairs' replies held their questions' gold; `None` when they were not graded
  */
final case class Churn(
    pairs: Vector[Churn.Pair],
    indexed: Option[Estimate],
    useful: Option[Estimate],
    indexedClean: Option[Estimate],
    usefulClean: Option[Estimate],
    followed: Proportion,
    correctness: Option[Churn.Correctness]
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
      val calls = hosted(t)
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

  /** How a turn's reply held its question's gold. */
  enum Grade {

    /** Not graded: no gold was read. */
    case Ungraded

    /** The turn has no reply: wrong. */
    case Unreplied

    case Graded(matched: Gold.Matched)
  }

  /** A graded turn, best first: correct after 0 calls, correct after more (fewer is better),
    * wrong having read, worst wrong having never read. Two wrong turns that both read are equal,
    * whatever their calls. The ordering's greater is the better.
    */
  enum Rank {
    case Correct(calls: Int)
    case Wrong(read: Boolean)
  }

  object Rank {
    given Ordering[Rank] = Ordering.by {
      case Correct(calls) => (1, -calls)
      case Wrong(read) => (0, if (read) 1 else 0)
    }
  }

  /** One side of a pair: the message that asked, the turn that answered it, how it read, its
    * `calls` (every hosted call of its tool loop but its own topic tool's), and how its reply
    * held its question's gold. It is `confounded` when its window held a part of another thread
    * of the pairs, so it may have read the other side's answer; `echoed` when it held a part of
    * the thread of another asking of its own question, so it may have read its own old answer.
    */
  final case class Side(
      at: CaseId,
      workflow: WorkflowId,
      reads: Reads,
      calls: Int,
      confounded: Boolean,
      echoed: Boolean,
      grade: Grade
  ) {

    /** Neither confounded nor echoed. */
    def clean: Boolean = !confounded && !echoed

    /** Its rank; `None` when it was not graded. */
    def rank: Option[Rank] = {
      def wrong = Rank.Wrong(reads != Reads.Unread)
      grade match {
        case Grade.Ungraded => None
        case Grade.Unreplied => Some(wrong)
        case Grade.Graded(m) => Some(if (m.correct) Rank.Correct(calls) else wrong)
      }
    }
  }

  /** A question's two turns. */
  final case class Pair(question: String, before: Side, after: Side)

  /** How a comparison's replies held their gold.
    *
    * @param before
    *   the share of the before-turns correct, clustered by question
    * @param after
    *   the share of the after-turns correct, clustered by question
    * @param toCorrect
    *   over the pairs both of whose turns were correct, the before-turn's calls less the
    *   after-turn's, clustered by question: positive when the after-turn needed fewer; `None`
    *   under two such questions
    * @param better
    *   over every pair, 1 when the after-turn ranks above the before-turn ([[Rank]]), −1 when
    *   below, 0 when equal, clustered by question: positive when after answered better; `None`
    *   under two questions
    * @param toCorrectClean
    *   as `toCorrect`, over the pairs both of whose sides are clean
    * @param betterClean
    *   as `better`, over the pairs both of whose sides are clean
    */
  final case class Correctness(
      before: Proportion,
      after: Proportion,
      toCorrect: Option[Estimate],
      better: Option[Estimate],
      toCorrectClean: Option[Estimate],
      betterClean: Option[Estimate]
  )

  /** The first of `turns` that answers the message `at`. */
  def answering(turns: Vector[TurnCase], at: CaseId): Option[TurnCase] =
    turns.find(_.said == Said.Slack(at))

  /** `questions`' rounds `before` and `after` compared over `turns`, each message's turn the
    * first that answers it ([[answering]]); each turn's reply graded as `graded` holds it by
    * workflow, a paired turn it lacks unreplied, every turn ungraded when `graded` is `None`.
    * Why not, naming the rounds, when no question was asked in both ([[ChurnQuestions.paired]]),
    * or naming the question and the message, when no turn answers one of its paired messages.
    */
  def of(
      turns: Vector[TurnCase],
      questions: ChurnQuestions,
      before: String,
      after: String,
      graded: Option[Map[WorkflowId, Gold.Matched]]
  ): Either[String, Churn] = {
    def found(question: String, side: String, at: CaseId) =
      answering(turns, at)
        .toRight(s"churn: question $question: no turn answers its $side message ${at.written}")
    for {
      pairs <- questions.paired(before, after)
      found <- Traversal.traverse(pairs) { (q, p) =>
        for {
          b <- found(q, "before", p.before)
          a <- found(q, "after", p.after)
        } yield (q, p, b, a)
      }
    } yield {
      val threads = found.flatMap((_, _, b, a) => Vector(b.conversation, a.conversation)).toSet
      // The threads of each question's every asking, as far as the capture answers them.
      val askings = questions.questions.map { (q, c) =>
        q -> c.asked.values.flatMap(answering(turns, _)).map(_.conversation).toSet
      }
      def side(q: String, at: CaseId, t: TurnCase) = {
        val parts = t.window.fold(Vector.empty)(_.parts).filter(_.conversation != t.conversation)
        Side(
          at,
          t.workflow,
          Reads.of(t),
          hosted(t).size,
          parts.exists(p => threads.contains(p.conversation)),
          parts.exists(p => askings.get(q).exists(_.contains(p.conversation))),
          graded.fold(Grade.Ungraded)(_.get(t.workflow).fold(Grade.Unreplied)(Grade.Graded(_)))
        )
      }
      val paired = found.map((q, p, b, a) => Pair(q, side(q, p.before, b), side(q, p.after, a)))
      def difference(ps: Vector[Pair])(of: Pair => Option[Double]) =
        Estimate.clustered(ps.flatMap(p => of(p).map(p.question -> _)))
      def calls(read: Reads => Option[Int])(p: Pair) =
        for {
          b <- read(p.before.reads)
          a <- read(p.after.reads)
        } yield (b - a).toDouble
      val clean = paired.filter(p => p.before.clean && p.after.clean)
      val first: Reads => Option[Int] = {
        case Reads.Read(before, _, _) => Some(before)
        case Reads.Unread => None
      }
      val useful: Reads => Option[Int] = {
        case Reads.Read(_, _, at) => at
        case Reads.Unread => None
      }
      val toCorrect: Pair => Option[Double] = p =>
        (p.before.rank, p.after.rank) match {
          case (Some(Rank.Correct(b)), Some(Rank.Correct(a))) => Some((b - a).toDouble)
          case _ => None
        }
      val better: Pair => Option[Double] = p =>
        for {
          b <- p.before.rank
          a <- p.after.rank
        } yield summon[Ordering[Rank]].compare(a, b).sign.toDouble
      def correct(s: Pair => Side) = Proportion.of(paired.flatMap { p =>
        s(p).rank.map(r =>
          p.question -> (r match {
            case Rank.Correct(_) => true
            case Rank.Wrong(_) => false
          })
        )
      })
      Churn(
        paired,
        difference(paired)(calls(first)),
        difference(paired)(calls(useful)),
        difference(clean)(calls(first)),
        difference(clean)(calls(useful)),
        Proportion.of(
          paired.flatMap(p =>
            p.after.reads match {
              case Reads.Read(_, followed, _) => Some(p.question -> followed)
              case Reads.Unread => None
            }
          )
        ),
        graded.map(_ =>
          Correctness(
            correct(_.before),
            correct(_.after),
            difference(paired)(toCorrect),
            difference(paired)(better),
            difference(clean)(toCorrect),
            difference(clean)(better)
          )
        )
      )
    }
  }

  /** `t`'s hosted calls: every call of its tool loop but its own topic tool's, in order. */
  private def hosted(t: TurnCase): Vector[Call] =
    t.rounds.flatMap(_.calls).filterNot(_.tool == Called.Topic)
}
