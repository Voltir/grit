package grit.turn

import grit.act.moves.Judging
import grit.act.moves.Judging.Pose
import grit.core.act.MoveName
import grit.core.context.{
  AssemblyError,
  AssemblyRequest,
  Packing,
  Prepared,
  Querying,
  Results,
  Retrieved,
  Window
}
import grit.core.durable.{Flow, Step}
import grit.core.model.Pins
import grit.core.period.Probability
import grit.core.route.Routed
import grit.core.visibility.Subject

import TurnJournal.given
import TurnSteps.{Env, Placed}
import TurnRelevance.Verdict

/** The turn's window drawn in recorded steps, from its placed message: `ask:query`, its search
  * query written as a move of its own; on a written query, `search`, then `ask:relevance-0` to
  * `ask:relevance-2`, each judging a batch of what it found ([[TurnRelevance]]); then
  * `assemble`, the window packed with what they admitted. A turn resumed between them repeats
  * none of the calls before.
  */
private[turn] object TurnAssembly {

  /** The patch the stages shipped under: a history holding `assemble` where its marker would
    * be is replayed through the one step that drew the window whole.
    */
  val Patch = "assembly-steps"

  /** From the placed message to its window, or why there is none. */
  def flow: Flow[Env, Placed, Nothing, Either[TurnFailure, Window]] =
    query
      .andThen(
        Flow.branch((queried: (Placed, Either[TurnFailure, Querying])) =>
          queried match {
            case (_, Left(f)) => Left(Left(f))
            case (p, Right(d: Querying.Drawn)) => Left(Right((p, Packing.Drawn(d))))
            case (p, Right(w: Querying.Written)) => Right((p, w))
          }
        )(Flow.pass[Either[TurnFailure, (Placed, Packing)]], sifted)
      )
      .andThen(
        Flow.either(
          Flow.pass[TurnFailure].map(f => Left(f): Either[TurnFailure, Window]),
          Flow.step(assemble)
        )
      )

  /** What a candidate the search found, judged as a batch of the turn's, came to so far. */
  private final case class Sifting(
      at: Placed,
      written: Querying.Written,
      results: Results,
      verdicts: Vector[(Retrieved, Verdict)]
  )

  /** `ask:query`: the turn's query written, or why the window is drawn with none. */
  private def query: Flow[Env, Placed, Nothing, (Placed, Either[TurnFailure, Querying])] =
    Judging
      .flow[Env, Placed, Either[TurnFailure, Querying]](MoveName("query"), charged)(
        r => r.env.moves,
        r => r.env.routing.budget
      )(r => p => writing(r.env, p))
      .keeping
      .map((p: Placed, judged: Judging.Judged[Either[TurnFailure, Querying]]) => (p, judged.made))

  /** On a written query, `search` then each batch judged: what the window packs, or why the
    * search failed.
    */
  private def sifted
      : Flow[Env, (Placed, Querying.Written), Nothing, Either[TurnFailure, (Placed, Packing)]] = {
    val none: Flow[Env, Sifting, Nothing, Sifting] = Flow.pass[Sifting]
    val judged = TurnRelevance.Names.indices.foldLeft(none)(
      (done: Flow[Env, Sifting, Nothing, Sifting], n: Int) => done.andThen(batch(n))
    )
    Flow
      .step(search)
      .keeping
      .map((in: (Placed, Querying.Written), found: Either[TurnFailure, Results]) =>
        found.map(Sifting(in._1, in._2, _, Vector.empty))
      )
      .andThen(
        Flow.either(
          Flow.pass[TurnFailure].map(f => Left(f): Either[TurnFailure, (Placed, Packing)]),
          judged.map((s: Sifting) =>
            Right(
              (s.at, Packing.Searched(s.written, TurnRelevance.admitted(s.results, s.verdicts)))
            )
          )
        )
      )
  }

  /** `ask:relevance-{n}`, the batch numbered `n` judged, its verdicts added to what came
    * before.
    */
  private def batch(n: Int): Flow[Env, Sifting, Nothing, Sifting] =
    Judging
      .flow[Env, Sifting, Verdicts](
        TurnRelevance.Names.lift(n).getOrElse(MoveName("relevance")),
        s => charged(s.at)
      )(
        r => r.env.moves,
        r => r.env.routing.budget
      )(r => s => judging(r.env, s, n))
      .keeping
      .map((s: Sifting, judged: Judging.Judged[Verdicts]) =>
        s.copy(verdicts = s.verdicts ++ judged.made)
      )

  private type Verdicts = Vector[(Retrieved, Verdict)]

  /** A turn's asks are charged to it, under its workflow, admitted by a read made for it. */
  private def charged(p: Placed): Judging.Charged =
    Judging.Charged(p.turn, p.turn.workflowId, Subject.Turn(p.turn))

  /** What `p`'s window is asked for. */
  private def request(p: Placed): AssemblyRequest = AssemblyRequest(p.turn, p.at.offer.width)

  private def failed(error: AssemblyError): TurnFailure = error match {
    case AssemblyError.Store(e) => TurnFailure.Assembly(TurnFailure.describe(e))
  }

  /** The `ask:query` body: the assembler's preparation read as the route bounds it; when it
    * asks, its request written under the route's query pin, as cleared for what was read.
    */
  private def writing(env: TurnEnv^, p: Placed): Pose[Either[TurnFailure, Querying]]^{env} = {
    p.at.routed.picked(env) match {
      case Left(f) => Pose.Made(Left(f))
      case Right(pins) =>
        val db = pins.reads(env.db)
        env.assembler.prepare(request(p))(using db) match {
          case Left(e) => Pose.Made(Left(failed(e)))
          case Right(Prepared.Drawn(drawn)) => Pose.Made(Right(drawn))
          case Right(Prepared.Asking(asked, written)) =>
            pins.query match {
              case None => Pose.Made(Right(written(Left(Prepared.NoPin))))
              case Some(pin) =>
                Routed
                  .reading(db, Subject.Turn(p.turn)) { (routed, _) =>
                    Pose.writing[Either[TurnFailure, Querying]](
                      env.models,
                      routed,
                      Pins.one(pin),
                      asked
                    )(reply => Right(written(reply)))
                  }
                  .fold(e => Pose.Made(Left(TurnFailure.ofStore(e))), identity)
            }
        }
    }
  }

  /** The `search` step: what the turn's written query finds, read as its route bounds it, the
    * first [[TurnRelevance.Recorded]] kept and the rest counted beyond them.
    */
  private val search: Step[Env, (Placed, Querying.Written), Either[TurnFailure, Results]] =
    Step[Env, (Placed, Querying.Written), Either[TurnFailure, Results]](TurnSteps.Names.search)(r =>
      (p, written) => {
        val env = r.env
        p.at.routed
          .picked(env)
          .flatMap(pins =>
            env.assembler
              .search(request(p), written)(using pins.reads(env.db))
              .left
              .map(failed)
          )
          .map(found =>
            Results(
              found.ranked.take(TurnRelevance.Recorded),
              found.beyond + (found.ranked.size - TurnRelevance.Recorded).max(0)
            )
          )
      }
    )

  /** The `ask:relevance-n` body: batch `n` of what was found, briefed as the route bounds it,
    * asked of the turn's classifier when it is cleared for what was read; each candidate
    * unjudged when nothing can be asked.
    */
  private def judging(env: TurnEnv^, s: Sifting, n: Int): Pose[Verdicts]^{env} = {
    val batch = TurnRelevance.batches(s.results).lift(n).fold(Vector.empty[Retrieved])(_._2)
    def unjudged(why: String): Pose[Verdicts] =
      Pose.Made(batch.map(found => found -> Verdict.Unjudged(why)))
    if (batch.isEmpty) Pose.Made(Vector.empty)
    else
      s.at.at.routed.picked(env) match {
        case Left(f) => unjudged(TurnFailure.reason(f))
        case Right(pins) =>
          val db = pins.reads(env.db)
          Routed
            .reading(db, Subject.Turn(s.at.turn)) { (routed, _) =>
              env.assembler.briefs(request(s.at), batch, TurnRelevance.Chars)(using
                routed.bound(db)
              ) match {
                case Left(e) => unjudged(TurnFailure.reason(failed(e)))
                case Right(briefs) =>
                  val state = TurnRelevance.state(briefs)
                  TurnRelevance.ask(state) match {
                    case None => unjudged(TurnRelevance.Gone)
                    case Some(questions) =>
                      Pose.asking[TurnRelevance.State, Vector[Probability], Verdicts](
                        env.classifier,
                        routed,
                        state,
                        questions
                      )((answered, _) => TurnRelevance.verdicts(batch, briefs, answered))
                  }
              }
            }
            .fold(e => unjudged(TurnFailure.describe(e)), identity)
      }
  }

  /** `assemble`: the turn's window, packed as its steps decided, read as its route bounds it. */
  private val assemble: Step[Env, (Placed, Packing), Either[TurnFailure, Window]] =
    Step[Env, (Placed, Packing), Either[TurnFailure, Window]](TurnSteps.Names.assemble)(r =>
      (p, packing) => {
        val env = r.env
        p.at.routed
          .picked(env)
          .flatMap(pins =>
            env.assembler
              .assemble(request(p), packing)(using pins.reads(env.db))
              .left
              .map(failed)
          )
      }
    )
}
