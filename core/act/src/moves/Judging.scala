package grit.act.moves

import grit.core.act.{Allowance, MoveName}
import grit.core.classify.{
  Answered,
  Answers,
  Ask,
  ClassifierError,
  ClearedClassifier,
  Request,
  StateJson
}
import grit.core.durable.{Flow, Journaled, Step}
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.route.Routed
import grit.core.spend.Budget
import grit.core.visibility.Subject

/** An engine workflow's judgment as a move (ADR 0034): its classifier asks admitted against the
  * day's spend and their costs kept in the ledger, as a job's are, its request read in its own
  * step.
  */
object Judging {

  /** The turn a judgment's cost is charged to, the workflow it is kept under, and the subject
    * each admitting read is made for.
    */
  final case class Charged(turn: TurnRef, workflow: WorkflowId, subject: Subject)

  /** What a judgment's workflow made of its input: made with no classifier asked, or a request
    * and what it makes of the answers ([[Pose.asking]]).
    */
  sealed trait Pose[+B]

  object Pose {

    /** `made` with no classifier asked. */
    final case class Made[+B](made: B) extends Pose[B]

    /** `questions` about `state`, asked once of `classifier` about what was read under
      * `routed`; `read` makes the judgment from its answers, or from why there are none:
      * `Unavailable`, sending nothing, when the day's spend reached its cap ("The day's spend
      * reached its cap, so no model was asked."), the spend cannot be read, or `classifier`'s
      * service is not cleared for `routed` (as [[ClearedClassifier.asking]] answers). An
      * answered call's cost is kept by the judgment's record step.
      */
    def asking[S: StateJson, T, B](
        classifier: ClearedClassifier^,
        routed: Routed,
        state: S,
        questions: Ask[S, T]
    )(read: Either[ClassifierError, Answered[T]] -> B): Pose[B]^{classifier} =
      Asking(
        classifier,
        routed,
        Request.of(state, questions),
        a => read(a.flatMap(questions.answered))
      )

    private[Judging] final case class Asking[+B](
        classifier: ClearedClassifier^,
        routed: Routed,
        request: Request,
        read: Either[ClassifierError, Answers] -> B
    ) extends Pose[B]
  }

  /** What a judgment came to: what its workflow `made` of it, and whether its classifier's cost
    * was recorded (`Left`, why not; `Right` when it was, or when no call was answered).
    */
  final case class Judged[+B](made: B, kept: Either[String, Unit])

  /** The judgment `name` of the workflow input `I`: `ask:{name}`, recording what `pose` made of
    * the input, its classifier asked at most once, the ask admitted first by `budget`'s daily
    * cap against what every conversation spent that day; then, when the classifier answered,
    * `ask:{name}:record`, its cost recorded for `charged`'s turn, under its workflow. A store
    * that fails the record is `kept`'s `Left`, never a throw.
    */
  def flow[R, I, B](name: MoveName, charged: I -> Charged)(
      moves: (r: R^) -> MovesEnv^{r},
      budget: (r: R^) -> Budget
  )(pose: (r: R^) -> I ->{r} Pose[B]^{r})(using
      Journaled[B]
  ): Flow[R, I, Nothing, Judged[B]] = {
    given Journaled[Posed[B]] = MovesJournal.posed[B]
    val made: Step[R, Posing[I], Posed[B]] =
      Step[R, Posing[I], Posed[B]](Act.Asks.move)(r =>
        p =>
          pose(r)(p.in) match {
            case Pose.Made(b) => Posed(b, Vector.empty)
            case Pose.Asking(cleared, routed, request, read) =>
              val env: MovesEnv^{r} = moves(r)
              val allowance: Allowance = Allowance.Daily(budget(r))
              val answers: Either[ClassifierError, Answers] =
                Act.admission(env.records, env.db, env.clock, allowance, p.charged.subject) match {
                  case Left(e) =>
                    Left(
                      ClassifierError.Unavailable(s"the day's spend is unread: ${Act.describe(e)}")
                    )
                  case Right((false, _, _)) => Left(ClassifierError.Unavailable(Act.Capped))
                  case Right((true, _, _)) => cleared.asking(routed).answers(request)
                }
              val calls = answers.toOption.toVector.map(a =>
                Act.classified(env.records, request, a, cleared.upstream)
              )
              Posed(read(answers), calls)
          }
      )
    Flow
      .pass[I]
      .map(i => Posing(name, charged(i), i))
      .andThen(Flow.step(made).keeping)
      .map((p, posed) =>
        if (posed.calls.isEmpty) Right(Judged(posed.made, Right(())))
        else {
          val c = p.charged
          Left(Act.Spent(p.name, c.turn, c.workflow, c.subject, posed.calls, posed.made))
        }
      )
      .andThen(
        Flow.either(
          (Flow.step(Act.recorded): Flow[MovesEnv, Act.Spent[B], Nothing, Either[String, Unit]])
            .over[R](moves)
            .keeping
            .map((s, kept) => Judged(s.made, kept)),
          Flow.pass[Judged[B]]
        )
      )
  }

  /** A judgment's input: the judgment `name`, charged as `charged`, of the workflow input `in`. */
  private final case class Posing[+I](name: MoveName, charged: Charged, in: I) extends Act.Move

  /** What a judgment's `ask:{n}` recorded: what its workflow `made` of it, and the cost of each
    * call its classifiers answered, in order.
    */
  private[moves] final case class Posed[+B](made: B, calls: Vector[AskMade.Call])
}
