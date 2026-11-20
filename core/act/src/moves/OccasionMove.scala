package grit.act.moves

import grit.core.act.{Acting, MoveName, Occasioned}
import grit.core.document.DocumentKeeper
import grit.core.durable.{Flow, Journaled, Step, StepName}
import grit.core.id.{DocumentVersionId, PluginName, TurnRef, WorkflowId}
import grit.core.inbox.OccasionInbox
import grit.core.occasion.Occasion
import grit.core.place.Place
import grit.core.speech.{SpeechJson, SpeechStore, Unprompted}
import grit.core.visibility.Integrity

/** What a keeping job's occasions are made with ([[grit.core.act.Keeping.occasion]]): `inbox`,
  * which records each as a turn of its own; `speech`, where its decision is kept; and
  * `unprompted`, what grit speaks within and the start of the turn it drafts.
  */
final case class Occasioning(inbox: OccasionInbox^, speech: SpeechStore, unprompted: Unprompted^)

/** An occasion move's steps: `occasion:{name}`, which reads it and records its entry;
  * `occasion:{name}:decide`, which decides it; `occasion:{name}:start`, which starts its turn. A
  * stored form: a run in flight replays by them.
  */
private[moves] object OccasionMove {

  /** The occasion move `name` of `occasion`, the acting plugin `plugin`'s, believed at
    * `integrity`, read against `keeper`'s documents.
    */
  final case class Move(
      name: MoveName,
      acting: Acting,
      plugin: PluginName,
      occasion: Occasion,
      integrity: Integrity,
      keeper: DocumentKeeper
  ) extends Act.Acted

  /** `move`'s occasion, recorded as the first entry of `turn`, its room's reply address `to`. */
  final case class Recorded(move: Move, turn: TurnRef, to: String) extends Act.Move {
    def name: MoveName = move.name
  }

  private val Found: StepName[Act.Move] =
    StepName.keyed[Act.Move]("occasion")(m => MoveName.value(m.name))
  private val Decided: StepName[Act.Move] =
    StepName.keyed[Act.Move]("occasion")(m => s"${MoveName.value(m.name)}:decide")
  private val Started: StepName[Act.Move] =
    StepName.keyed[Act.Move]("occasion")(m => s"${MoveName.value(m.name)}:start")

  /** Read at the run's floor ([[Occasioned.found]]), then recorded through the inbox, dated by
    * the step's clock: the turn its entry began and its reply address, or what it ended as.
    */
  private val found: Step[KeepingEnv, Move, Either[String, Either[Occasioned, (TurnRef, String)]]] =
    Step[KeepingEnv, Move, Either[String, Either[Occasioned, (TurnRef, String)]]](Found)(env =>
      m => {
        val (subject, at) = (m.acting.subject, env.moves.clock.now())
        val o = env.occasions
        env.moves.db
          .read(subject)(
            Occasioned.found(m.occasion, m.keeper, o.speech, o.unprompted.speaking, m.integrity, at)
          )
          .left
          .map(_.described)
          .flatMap {
            case Left(ended) => Right(Left(ended))
            case Right((origin, to)) =>
              o.inbox
                .occasion(origin, m.plugin, m.occasion, at, m.integrity)
                .left
                .map(_.toString)
                .map(turn => Right((turn, to)))
          }
      }
    )(using MovesJournal.kept(using recorded))

  /** Decided in one transaction with its record ([[Occasioned.decided]]), dated by the step's
    * clock: what the move ended as, or `Right` when its turn is to start.
    */
  private val decided: Step[KeepingEnv, Recorded, Either[String, Either[Occasioned, Unit]]] =
    Step.transact[KeepingEnv, Recorded, Either[String, Either[Occasioned, Unit]]](
      Decided,
      _.move.acting.subject
    )(env =>
      r =>
        Occasioned
          .decided(
            r.move.occasion,
            r.turn,
            r.to,
            r.move.integrity,
            env.occasions.unprompted.speaking,
            env.occasions.unprompted.budget,
            env.occasions.speech,
            env.moves.records.spending,
            env.moves.clock.now()
          )
          .left
          .map(_.described)
    )(using MovesJournal.kept(using decision))

  /** Its turn started; or, when it could not be, its decision settled failed, with why, as a
    * heard message's is ([[grit.core.speech.Outcome.Failed]]).
    */
  private val started: Step[KeepingEnv, Recorded, Either[String, Occasioned]] =
    Step[KeepingEnv, Recorded, Either[String, Occasioned]](Started)(env =>
      r =>
        env.occasions.unprompted.start(r.turn) match {
          case Right(()) => Right(Occasioned.Started(r.turn))
          case Left(why) =>
            env.jot
              .write(r.move.acting.subject)(
                Occasioned.unstarted(r.turn, why, env.occasions.speech, env.moves.clock.now())
              )
              .left
              .map(_.described)
        }
    )(using MovesJournal.kept(using occasioned))

  /** What [[grit.core.act.Keeping.occasion]] says it does: what came of the move, or why the
    * store failed, either way as its last step left it. Defined after its steps, which the object
    * initialises in order.
    */
  val flow: Flow[KeepingEnv, Move, Either[String, Occasioned], Either[String, Occasioned]] =
    Flow
      .decide(found)((m, o) =>
        o match {
          case Left(why) => Left(Left(why))
          case Right(Left(ended)) => Left(Right(ended))
          case Right(Right((turn, to))) => Right(Recorded(m, turn, to))
        }
      )
      .andThen(
        Flow.decide(decided)((r, d) =>
          d match {
            case Left(why) => Left(Left(why))
            case Right(Left(ended)) => Left(Right(ended))
            case Right(Right(())) => Right(r)
          }
        )
      )
      .andThen(Flow.step(started))

  /** `{"started":turn}`, `{"held":silence,"turn"?:turn}`, `{"already":true}`,
    * `{"mislabelled":place}`, `{"unread":version}` or `{"unstarted":turn,"why":why}`, each turn
    * by its workflow id.
    */
  private[moves] given occasioned: Journaled[Occasioned] = Journaled.json(
    {
      case Occasioned.Started(turn) => ujson.Obj("started" -> id(turn))
      case Occasioned.Held(turn, why) =>
        val o = ujson.Obj("held" -> SpeechJson.writeSilence(why))
        turn.foreach(t => o("turn") = id(t))
        o
      case Occasioned.Already => ujson.Obj("already" -> true)
      case Occasioned.Mislabelled(room) => ujson.Obj("mislabelled" -> room.written)
      case Occasioned.Unread(about) =>
        ujson.Obj("unread" -> ujson.Num(DocumentVersionId.value(about).toDouble))
      case Occasioned.Unstarted(turn, why) => ujson.Obj("unstarted" -> id(turn), "why" -> why)
    },
    v =>
      v.objOpt.toRight("occasion: expected an object").flatMap { o =>
        def str(k: String) = o.get(k).flatMap(_.strOpt)
        (
          str("started"),
          o.get("held"),
          str("mislabelled"),
          o.get("unread"),
          str("unstarted")
        ) match {
          case (Some(t), _, _, _, _) => turn(t).map(Occasioned.Started(_))
          case (_, Some(why), _, _, _) =>
            for {
              silence <- SpeechJson.readSilence(why)
              t <- str("turn")
                .fold[Either[String, Option[TurnRef]]](Right(None))(turn(_).map(Some(_)))
            } yield Occasioned.Held(t, silence)
          case (_, _, Some(room), _, _) => Place.read(room).map(Occasioned.Mislabelled(_))
          case (_, _, _, Some(n), _) =>
            n.numOpt
              .flatMap(d => DocumentVersionId.of(d.toLong))
              .toRight("occasion: unread is not a document version")
              .map(Occasioned.Unread(_))
          case (_, _, _, _, Some(t)) =>
            for {
              u <- turn(t)
              why <- str("why").toRight("occasion: unstarted has no why")
            } yield Occasioned.Unstarted(u, why)
          case _ if o.get("already").flatMap(_.boolOpt).contains(true) => Right(Occasioned.Already)
          case _ =>
            Left("occasion: expected started, held, already, mislabelled, unread or unstarted")
        }
      }
  )

  /** `{"ended":occasioned}` or `{"recorded":turn,"to":address}`. */
  private given recorded: Journaled[Either[Occasioned, (TurnRef, String)]] = Journaled.json(
    {
      case Left(ended) => ujson.Obj("ended" -> ujson.read(occasioned.encode(ended)))
      case Right((t, to)) => ujson.Obj("recorded" -> id(t), "to" -> to)
    },
    v =>
      v.objOpt.toRight("occasion: expected an object").flatMap { o =>
        (o.get("ended"), o.get("recorded").flatMap(_.strOpt), o.get("to").flatMap(_.strOpt)) match {
          case (Some(ended), _, _) => occasioned.decode(ended.render()).map(Left(_))
          case (_, Some(t), Some(to)) => turn(t).map(r => Right((r, to)))
          case _ => Left("occasion: expected ended, or recorded and to")
        }
      }
  )

  /** `{"ended":occasioned}` or `{"drafting":true}`. */
  private given decision: Journaled[Either[Occasioned, Unit]] = Journaled.json(
    {
      case Left(ended) => ujson.Obj("ended" -> ujson.read(occasioned.encode(ended)))
      case Right(()) => ujson.Obj("drafting" -> true)
    },
    v =>
      v.objOpt.toRight("occasion: expected an object").flatMap { o =>
        o.get("ended") match {
          case Some(ended) => occasioned.decode(ended.render()).map(Left(_))
          case None if o.get("drafting").flatMap(_.boolOpt).contains(true) => Right(Right(()))
          case None => Left("occasion: expected ended or drafting")
        }
      }
  )

  private def id(turn: TurnRef): String = WorkflowId.value(turn.workflowId)

  private def turn(id: String): Either[String, TurnRef] =
    TurnRef.fromWorkflowId(WorkflowId(id)).toRight(s"occasion: $id is not a turn")
}
