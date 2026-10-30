package grit.turn

import grit.act.moves.Judging
import grit.act.moves.Judging.Pose
import grit.core.act.MoveName
import grit.core.context.{
  AssemblyError,
  AssemblyRequest,
  CarriedWindow,
  Packing,
  Prepared,
  Projection,
  Querying,
  Results,
  Retrieved,
  Window
}
import grit.core.durable.{Flow, Step}
import grit.core.id.EntryId
import grit.core.model.Pins
import grit.core.period.Probability
import grit.core.route.Routed
import grit.core.store.{Db, StoreError, Tx}
import grit.core.visibility.Subject

import TurnJournal.given
import TurnSteps.{Env, Placed, Windowed}
import TurnRelevance.Verdict

/** The turn's window drawn in recorded steps, from its placed message: `ask:needs`, whether its
  * message needs a search ([[TurnNeeds]]); unless it ends there, `ask:query`, its search query
  * written as a move of its own; on a written query, `search`, then `ask:relevance-0` to
  * `ask:relevance-2`, each judging a batch of what it found ([[TurnRelevance]]); then, unless a
  * heard turn ends there, `assemble`, the window packed with what they admitted. A turn resumed
  * between them repeats none of the calls before.
  */
private[turn] object TurnAssembly {

  /** From the placed message to its window: `carried`, the window record it is drawn against
    * ([[TurnSteps.Env.carried]]), then the steps that draw it, `assemble` recording its
    * projection; under a history from before [[Carrying]], those steps alone, `assemble`
    * recording the whole window. `Right(Left(root))` when the needs gate rejected a heard
    * message, or no candidate of a heard turn's search grounds a draft
    * ([[TurnRelevance.grounds]]), so none is drawn; `Left` why there is none.
    */
  def flow: Flow[Env, Placed, Nothing, Drawn[Windowed]] =
    Flow.patch(Carrying)(
      drawing(whole).map(_.map(_.map(Windowed.Whole(_)))),
      Flow.patch(CarryReasons)(
        drawnAgainst(unreasoned)(identity),
        drawnAgainst(carried)(_.head)
      )
    )

  /** The window drawn against the record `read` names (`head` of its output), or afresh when
    * it names none: `Left` why there is none when `read` fails.
    */
  private def drawnAgainst[A](read: Step[Env, Placed, Either[TurnFailure, A]])(
      head: A -> Option[EntryId]
  ): Flow[Env, Placed, Nothing, Drawn[Windowed]] =
    Flow
      .step(read)
      .keeping
      .andThen(
        Flow.branch((r: (Placed, Either[TurnFailure, A])) =>
          r._2.map(a => r._1.copy(carried = head(a))).left.map(f => Left(f))
        )(
          Flow.pass[Drawn[Windowed]],
          drawing(projected).keeping
            .map((p, drawn) => drawn.map(_.map(Windowed.Carried(p.carried, _))))
        )
      )

  /** The patch under which `carried` records why a window was drawn afresh (ADR 0004); a
    * history from before it records only the record drawn against, or none.
    */
  val CarryReasons: String = "carry-reasons"

  /** The patch under which a turn reads the window it carries before its first ask (ADR 0004,
    * ADR 0052); a history from before it draws every window whole.
    */
  val Carrying: String = "carried-window"

  /** From the placed message to what `assemble` records of its window, as [[flow]] says. */
  private def drawing[A](
      assemble: Step[Env, (Placed, Packing), Either[TurnFailure, A]]
  ): Flow[Env, Placed, Nothing, Drawn[A]] =
    needs.andThen(
      Flow.branch((judged: (Placed, Verdict)) =>
        TurnNeeds.after(judged._2, judged._1.at.offer.root) match {
          case TurnNeeds.After.Ends(root) => Left(Right(Left(root)))
          case TurnNeeds.After.Searches => Right((judged._1, true))
          case TurnNeeds.After.Unsearched => Right((judged._1, false))
        }
      )(Flow.pass[Drawn[A]], drawn(assemble))
    )

  /** A window, or what it is packed from (`A`); a heard turn's root when it ends unasked; or why
    * there is none.
    */
  private type Drawn[A] = Either[TurnFailure, Either[TurnNeeds.Unasked, A]]

  /** `ask:needs`: the needs gate's verdict on the placed message ([[TurnNeeds]]). */
  private def needs: Flow[Env, Placed, Nothing, (Placed, Verdict)] =
    Judging
      .flow[Env, Placed, Verdict](MoveName("needs"), charged)(
        r => r.env.moves,
        r => r.env.routing.budget
      )(r => p => needing(r.env, p))
      .keeping
      .map((p: Placed, judged: Judging.Judged[Verdict]) => (p, judged.made))

  /** From the placed message, searching when `true`, to its window, its root when it ends
    * unasked, or why there is none.
    */
  private def drawn[A](
      assemble: Step[Env, (Placed, Packing), Either[TurnFailure, A]]
  ): Flow[Env, (Placed, Boolean), Nothing, Drawn[A]] =
    query
      .andThen(
        Flow.branch((queried: (Placed, Either[TurnFailure, Querying])) =>
          queried match {
            case (_, Left(f)) => Left(Left(f))
            case (p, Right(d: Querying.Drawn)) => Left(Right(Right((p, Packing.Drawn(d)))))
            case (p, Right(w: Querying.Written)) => Right((p, w))
          }
        )(Flow.pass[Drawn[(Placed, Packing)]], sifted)
      )
      .andThen(
        Flow.branch((packed: Drawn[(Placed, Packing)]) =>
          packed match {
            case Right(Right(packing)) => Right(packing)
            case Right(Left(root)) => Left(Right(Left(root)))
            case Left(f) => Left(Left(f))
          }: Either[Drawn[A], (Placed, Packing)]
        )(Flow.pass[Drawn[A]], Flow.step(assemble).map(_.map(Right(_))))
      )

  /** What a candidate the search found, judged as a batch of the turn's, came to so far. */
  private final case class Sifting(
      at: Placed,
      written: Querying.Written,
      results: Results,
      verdicts: Vector[(Retrieved, Verdict)]
  )

  /** `ask:query`: the turn's query written when it searches, or why the window is drawn with
    * none.
    */
  private def query
      : Flow[Env, (Placed, Boolean), Nothing, (Placed, Either[TurnFailure, Querying])] =
    Judging
      .flow[Env, (Placed, Boolean), Either[TurnFailure, Querying]](
        MoveName("query"),
        (p, _) => charged(p)
      )(
        r => r.env.moves,
        r => r.env.routing.budget
      )(r => (p, searching) => writing(r.env, p, searching))
      .keeping
      .map((at: (Placed, Boolean), judged: Judging.Judged[Either[TurnFailure, Querying]]) =>
        (at._1, judged.made)
      )

  /** On a written query, `search` then each batch judged: what the window packs, the root of a
    * heard turn that ends unasked, or why the search failed.
    */
  private def sifted: Flow[Env, (Placed, Querying.Written), Nothing, Drawn[(Placed, Packing)]] = {
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
          Flow.pass[TurnFailure].map(f => Left(f): Drawn[(Placed, Packing)]),
          judged
            .andThen(quiet)
            .map((ended: Either[TurnNeeds.Unasked, Sifting]) =>
              Right(
                ended.map(s =>
                  (s.at, Packing.Searched(s.written, TurnRelevance.admitted(s.results, s.verdicts)))
                )
              )
            )
        )
      )
  }

  /** After the last batch, a heard turn none of whose candidates grounds a draft
    * ([[TurnRelevance.grounds]]) ends with its root; any other goes on. A history recorded
    * before the patch `heard-quiet-exit` goes on whatever its verdicts (ADR 0004).
    */
  private def quiet: Flow[Env, Sifting, Nothing, Either[TurnNeeds.Unasked, Sifting]] =
    Flow.patch("heard-quiet-exit")(
      Flow.pass[Sifting].map(s => Right(s): Either[TurnNeeds.Unasked, Sifting]),
      Flow
        .pass[Sifting]
        .map(s =>
          if (TurnRelevance.grounds(s.verdicts)) Right(s)
          else TurnNeeds.unasked(s.at.at.offer.root).toLeft(s)
        )
    )

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

  /** What `p`'s window is asked for, read through `db`: drawn against the window its `carried`
    * step read ([[CarriedWindow.chain]]), or afresh when it read none.
    */
  private def request(env: TurnEnv^, db: Db^, p: Placed): Either[AssemblyError, AssemblyRequest] =
    p.carried
      .fold[Either[StoreError, CarriedWindow]](Right(CarriedWindow.Empty))(head =>
        db.read(Subject.Turn(p.turn))(
          CarriedWindow.chain(head)(env.records.entries.get).map(CarriedWindow.of)
        )
      )
      .map(AssemblyRequest(p.turn, p.at.offer.width, _))
      .left
      .map(AssemblyError.Store(_))

  private def failed(error: AssemblyError): TurnFailure = error match {
    case AssemblyError.Store(e) => TurnFailure.Assembly(e.described)
  }

  /** The `ask:needs` body: asked of the turn's classifier, cleared for what was read, about its
    * message and thread ([[TurnNeeds.State]]) read as the route bounds them, when its window
    * could find anything by searching; unjudged, asking nothing, when it could not
    * ([[TurnNeeds.NothingToSearch]]) or nothing can be asked.
    */
  private def needing(env: TurnEnv^, p: Placed): Pose[Verdict]^{env} = {
    def unjudged(why: String): Pose[Verdict] = Pose.Made(Verdict.Unjudged(why))
    p.at.routed.picked(env) match {
      case Left(f) => unjudged(TurnFailure.reason(f))
      case Right(pins) =>
        val db = pins.reads(env.db)
        // `ask:query` reads `prepare` again: a read, never a call, and `Prepared.Asking` holds a
        // function, so it cannot be recorded here and passed on.
        request(env, db, p).flatMap(env.assembler.prepare(_)(using db)) match {
          case Left(e) => unjudged(TurnFailure.reason(failed(e)))
          case Right(Prepared.Drawn(_)) => unjudged(TurnNeeds.NothingToSearch)
          case Right(Prepared.Asking(_, _)) if pins.query.isEmpty =>
            unjudged(TurnNeeds.NothingToSearch)
          case Right(Prepared.Asking(_, _)) =>
            val turn = p.turn
            Routed
              .reading(db, Subject.Turn(turn)) { (routed, reads) =>
                reads.read { (tx: Tx^) ?=>
                  for {
                    all <- env.records.entries.list(turn.conversationId)
                    strand <- TurnSteps.strandOf(
                      env.stitching,
                      env.hosting.conversations,
                      turn,
                      all,
                      env.clock.now()
                    )
                    named <- env.records.principals.speakers((all ++ strand.shown).map(_.id))
                  } yield TurnNeeds.state(
                    TurnJudge.told(
                      all,
                      TurnShown.own(all, turn),
                      strand,
                      env.stitching.tuning.strandChars,
                      named
                    )
                  )
                } match {
                  case Left(e) => unjudged(e.described)
                  case Right(state) =>
                    Pose.asking[TurnNeeds.State, Probability, Verdict](
                      env.classifier,
                      routed,
                      state,
                      TurnNeeds.ask
                    )((answered, _) => Verdict.read(answered.map(_.value), TurnNeeds.Pass))
                }
              }
              .fold(e => unjudged(e.described), identity)
        }
    }
  }

  /** The `ask:query` body: the assembler's preparation read as the route bounds it; when it
    * asks and the turn searches, its request written under the route's query pin, as cleared
    * for what was read; when it does not search, the window drawn with no query, noted
    * [[TurnNeeds.Unneeded]].
    */
  private def writing(
      env: TurnEnv^,
      p: Placed,
      searching: Boolean
  ): Pose[Either[TurnFailure, Querying]]^{env} = {
    p.at.routed.picked(env) match {
      case Left(f) => Pose.Made(Left(f))
      case Right(pins) =>
        val db = pins.reads(env.db)
        request(env, db, p).flatMap(env.assembler.prepare(_)(using db)) match {
          case Left(e) => Pose.Made(Left(failed(e)))
          case Right(Prepared.Drawn(drawn)) => Pose.Made(Right(drawn))
          case Right(Prepared.Asking(_, written)) if !searching =>
            Pose.Made(Right(written(Left(TurnNeeds.Unneeded))))
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
          .flatMap { pins =>
            val db = pins.reads(env.db)
            request(env, db, p)
              .flatMap(env.assembler.search(_, written)(using db))
              .left
              .map(failed)
          }
          .map(found =>
            Results(
              found.ranked.take(TurnRelevance.Recorded),
              found.beyond + (found.ranked.size - TurnRelevance.Recorded).max(0)
            )
          )
      }
    )

  /** The `ask:relevance-n` body: batch `n` of what was found, given judge text as the route bounds it,
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
              request(env, db, s.at).flatMap(
                env.assembler.judgeText(_, batch, TurnRelevance.Chars)(using routed.bound(db))
              ) match {
                case Left(e) => unjudged(TurnFailure.reason(failed(e)))
                case Right(judgeText) =>
                  val state = TurnRelevance.state(judgeText)
                  TurnRelevance.ask(state) match {
                    case None => unjudged(TurnRelevance.Gone)
                    case Some(questions) =>
                      Pose.asking[TurnRelevance.State, Vector[Probability], Verdicts](
                        env.classifier,
                        routed,
                        state,
                        questions
                      )((answered, _) => TurnRelevance.verdicts(batch, judgeText, answered))
                  }
              }
            }
            .fold(e => unjudged(e.described), identity)
      }
  }

  /** `assemble`: the turn's projection, packed as its steps decided against the window its
    * `carried` step read, read as its route bounds it.
    */
  private val projected: Step[Env, (Placed, Packing), Either[TurnFailure, Projection]] =
    Step[Env, (Placed, Packing), Either[TurnFailure, Projection]](TurnSteps.Names.assemble)(r =>
      (p, packing) => projecting(r.env, p, packing)
    )

  /** `assemble` as a turn recorded it before windows were carried: the window alone. */
  private val whole: Step[Env, (Placed, Packing), Either[TurnFailure, Window]] =
    Step[Env, (Placed, Packing), Either[TurnFailure, Window]](TurnSteps.Names.assemble)(r =>
      (p, packing) => projecting(r.env, p, packing).map(_.window)
    )

  /** The `assemble` body: `p`'s projection, packed as `packing` says. */
  private def projecting(
      env: TurnEnv^,
      p: Placed,
      packing: Packing
  ): Either[TurnFailure, Projection] =
    p.at.routed
      .picked(env)
      .flatMap { pins =>
        val db = pins.reads(env.db)
        request(env, db, p)
          .flatMap(env.assembler.assemble(_, packing)(using db))
          .left
          .map(failed)
      }

  /** `carried` ([[TurnSteps.Env.carried]]). */
  private val carried: Step[Env, Placed, Either[TurnFailure, TurnBasis]] =
    Step[Env, Placed, Either[TurnFailure, TurnBasis]](TurnSteps.Names.carried)(r =>
      p => r.carried(p)
    )

  /** `carried` as recorded before [[CarryReasons]]: the record drawn against, or none. */
  private val unreasoned: Step[Env, Placed, Either[TurnFailure, Option[EntryId]]] =
    Step[Env, Placed, Either[TurnFailure, Option[EntryId]]](TurnSteps.Names.carried)(r =>
      p => r.carried(p).map(_.head)
    )
}
