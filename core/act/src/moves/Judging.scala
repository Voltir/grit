package grit.act.moves

import scala.concurrent.duration.{Duration, DurationLong, FiniteDuration}

import grit.core.act.{Allowance, MoveName}
import grit.core.classify.{
  Answered,
  Answers,
  Ask,
  ClassifierError,
  ClearedClassifier,
  Reply,
  Request,
  StateJson
}
import grit.core.durable.{Flow, Journaled, Step}
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.model.Pins
import grit.core.provider.{ModelRequest, Models}
import grit.core.route.Routed
import grit.core.spend.Budget
import grit.core.visibility.{Label, Subject}

/** An engine workflow's judgment as a move (ADR 0034): its classifier or model asks admitted
  * against the day's spend and their costs kept in the ledger, as a job's are, its request read
  * in its own step.
  */
object Judging {

  /** The turn a judgment's cost is charged to, the workflow it is kept under, and the subject
    * each admitting read is made for.
    */
  final case class Charged(turn: TurnRef, workflow: WorkflowId, subject: Subject)

  /** What a judgment's workflow made of its input: made with no classifier or model asked, or a
    * request and what it makes of the answers ([[Pose.asking]]) or of the reply
    * ([[Pose.writing]]).
    */
  sealed trait Pose[+B]

  object Pose {

    /** `made` with no classifier asked. */
    final case class Made[+B](made: B) extends Pose[B]

    /** `questions` about `state`, asked once of `classifier` about what was read under
      * `routed`; `read` makes the judgment from its answers, or from why there are none, and
      * from how long the call took on the moves' clock (zero when the ask was refused):
      * `Unavailable`, sending nothing, when the day's spend reached its cap ("The day's spend
      * reached its cap, so no model was asked."), the spend cannot be read, or `classifier`'s
      * service is not cleared for `routed` (as [[ClearedClassifier.asking]] answers). Every
      * call that replied is charged, however its answers read, by the judgment's record step.
      */
    def asking[S: StateJson, T, B](
        classifier: ClearedClassifier^,
        routed: Routed,
        state: S,
        questions: Ask[S, T]
    )(read: (Either[ClassifierError, Answered[T]], FiniteDuration) -> B): Pose[B]^{classifier} =
      Asking(
        classifier,
        routed,
        Request.of(state, questions),
        (a, took) => read(a.flatMap(questions.answered), took)
      )

    private[Judging] final case class Asking[+B](
        classifier: ClearedClassifier^,
        routed: Routed,
        request: Request,
        read: (Either[ClassifierError, Answers], FiniteDuration) -> B
    ) extends Pose[B]

    /** `request`, asked once of the first of `pins` cleared for what was read under `routed`,
      * through `models`; `read` makes the judgment from its reply, or from why there is none:
      * the day's spend reached its cap ("The day's spend reached its cap, so no model was
      * asked."), the spend cannot be read ("the day's spend is unread: {cause}"), or no pin is
      * cleared ("no model cleared for {label}"), each sending nothing; or the provider's
      * failure, its cause. Every reply is charged, however it reads, by the judgment's record
      * step, at the request's estimate.
      */
    def writing[B](models: Models^, routed: Routed, pins: Pins, request: ModelRequest)(
        read: Either[String, Message.Assistant] -> B
    ): Pose[B]^{models} =
      Writing(models, routed, pins, request, read)

    private[Judging] final case class Writing[+B](
        models: Models^,
        routed: Routed,
        pins: Pins,
        request: ModelRequest,
        read: Either[String, Message.Assistant] -> B
    ) extends Pose[B]
  }

  /** What a judgment came to: what its workflow `made` of it, and whether its classifier's or
    * model's cost was recorded (`Left`, why not; `Right` when it was, or when no call was
    * answered).
    */
  final case class Judged[+B](made: B, kept: Either[String, Unit])

  /** The judgment `name` of the workflow input `I`: `ask:{name}`, recording what `pose` made of
    * the input, its classifier or model asked at most once, the ask admitted first by
    * `budget`'s daily cap against what every conversation spent that day; then, when it answered,
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
            case Pose.Made(b) => Posed(b, Vector.empty, None)
            case Pose.Asking(cleared, routed, request, read) =>
              val env: MovesEnv^{r} = moves(r)
              val asked: (Either[ClassifierError, Reply], FiniteDuration) =
                admitted(env, budget(r), p.charged.subject) match {
                  case Left(why) => (Left(ClassifierError.Unavailable(why)), Duration.Zero)
                  case Right(()) =>
                    val start = env.clock.millis()
                    val got = cleared.asking(routed).replied(request)
                    (got, (env.clock.millis() - start).millis)
                }
              val (reply, took) = asked
              val calls = reply.toOption.toVector.map(a =>
                Act.classified(env.records, request, a, cleared.upstream)
              )
              Posed(
                read(reply.flatMap(_.answers), took),
                calls,
                Some(routed.classification.label)
              )
            case Pose.Writing(models, routed, pins, request, read) =>
              val env: MovesEnv^{r} = moves(r)
              val reply: Either[String, Message.Assistant] = for {
                _ <- admitted(env, budget(r), p.charged.subject)
                cleared <- routed
                  .cleared(pins)
                  .toRight(s"no model cleared for ${Label.written(routed.classification.label)}")
                got <- models.provider(cleared.first).complete(request).left.map(_.cause)
              } yield got
              val calls = reply.toOption.toVector.map(m =>
                AskMade.Call(m.model, m.usage, env.records.estimator.request(request), m.upstream)
              )
              Posed(read(reply), calls, Some(routed.classification.label))
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

  /** What a judgment's recorded `ask:{name}` step `output` says its workflow made; `Left` when
    * it does not read as one.
    */
  def made[B](output: String)(using Journaled[B]): Either[String, B] =
    MovesJournal.posed[B].decode(output).map(_.made)

  /** Whether `budget`'s daily cap admits an ask for `subject` now; `Left`, why not. */
  private def admitted(env: MovesEnv^, budget: Budget, subject: Subject): Either[String, Unit] =
    Act.admission(env.records, env.db, env.clock, Allowance.Daily(budget), subject) match {
      case Left(e) => Left(s"the day's spend is unread: ${Act.describe(e)}")
      case Right((false, _, _)) => Left(Act.Capped)
      case Right((true, _, _)) => Right(())
    }

  /** A judgment's input: the judgment `name`, charged as `charged`, of the workflow input `in`. */
  private final case class Posing[+I](name: MoveName, charged: Charged, in: I) extends Act.Move

  /** What a judgment's `ask:{n}` recorded: what its workflow `made` of it, and the cost of each
    * call its classifiers or models answered, in order; `label`, the label of what its pose was
    * routed under, `None` for one made with no classifier or model asked.
    */
  private[moves] final case class Posed[+B](
      made: B,
      calls: Vector[AskMade.Call],
      label: Option[Label]
  ) extends Asked
}
