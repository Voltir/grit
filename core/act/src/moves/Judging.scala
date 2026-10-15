package grit.act.moves

import grit.core.act.{Allowance, MoveName}
import grit.core.classify.{Classifier, ClearedClassifier}
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

  /** What a judgment's step asks through, captured by the environment its step was handed. */
  trait Asks {

    /** `cleared`, asking about what was read under `routed`, each call asked once and admitted
      * first: `Unavailable`, sending nothing, when the day's spend reached its cap ("The day's
      * spend reached its cap, so no model was asked."), the spend cannot be read, or `cleared`'s
      * service is not cleared for `routed` (as [[ClearedClassifier.asking]] answers); otherwise
      * its answers, each answered call's cost kept by the judgment's record step.
      */
    def classifier(cleared: ClearedClassifier^, routed: Routed): Classifier^
  }

  /** What a judgment came to: what its workflow `made` of it, and whether its classifier's cost
    * was recorded (`Left`, why not; `Right` when it was, or when no call was answered).
    */
  final case class Judged[+B](made: B, kept: Either[String, Unit])

  /** The judgment `name` of the workflow input `I`: `ask:{name}`, recording what `pose` makes of
    * the input through the [[Asks]] it is handed, each ask admitted by `budget`'s daily cap
    * against what every conversation spent that day; then, when a call was answered,
    * `ask:{name}:record`, its cost recorded for `charged`'s turn, under its workflow. A store
    * that fails the record is `kept`'s `Left`, never a throw.
    */
  def flow[R, I, B](name: MoveName, charged: I -> Charged)(
      moves: (r: R^) -> MovesEnv^{r},
      budget: (r: R^) -> Budget
  )(pose: (r: R^) -> (I, Asks^) ->{r} B)(using
      Journaled[B]
  ): Flow[R, I, Nothing, Judged[B]] = {
    given Journaled[Posed[B]] = MovesJournal.posed[B]
    val made: Step[R, Posing[I], Posed[B]] =
      Step[R, Posing[I], Posed[B]](Act.Asks.move)(r =>
        p => {
          val env: MovesEnv^{r} = moves(r)
          val allowance: Allowance = Allowance.Daily(budget(r))
          // Written only by the classifiers this step hands out, read once they are done.
          @caps.unsafe.untrackedCaptures
          var calls = Vector.empty[AskMade.Call]
          val asks: Asks^{env} = new Asks {
            def classifier(cleared: ClearedClassifier^, routed: Routed): Classifier^ =
              Act.admission(env.records, env.db, env.clock, allowance, p.charged.subject) match {
                case Left(e) => Classifier.none(s"the day's spend is unread: ${Act.describe(e)}")
                case Right((false, _, _)) => Classifier.none(Act.Capped)
                case Right((true, _, _)) =>
                  cleared.at(routed) match {
                    case Left(_) => cleared.asking(routed)
                    case Right(c) =>
                      Classifier.around(c)((request, ask) => {
                        val got = ask()
                        got.foreach(a =>
                          calls = calls :+ Act.classified(env.records, request, a, cleared.upstream)
                        )
                        got
                      })
                  }
              }
          }
          // `asks` reaches `r` only through the reads and the clock it hands its classifiers,
          // which mutate nothing of `r`; `pose` holds `r` too, and the two run in turn on this
          // step's one thread, so neither sees the other's use.
          val b = caps.unsafe.unsafeAssumeSeparate(pose(r)(p.in, asks))
          Posed(b, calls)
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
