package grit.act.phase

import scala.concurrent.duration.*

import grit.core.act.{Acting, Delegation}
import grit.core.agent.Subagent
import grit.core.durable.{Flow, Journaled, Step, StepName}
import grit.core.flow.{Mint, Tainted}
import grit.core.id.{CallSlot, ConversationId, Sha256, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.inbox.{InboxError, SpawnInbox, SpawnReply, TurnEnding}
import grit.core.json.Fields
import grit.core.message.{Cost, SpendCap}
import grit.core.place.{Place, Repository, WorkName}
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Integrity, Label}

/** What a spawn's steps act through: `spawns`, where its child is recorded, started, ended and
  * read.
  */
final case class SpawnEnv(spawns: SpawnInbox^)

/** What a spawn's steps are recorded as. */
final case class SpawnSteps[I](
    narrow: StepName[I],
    spawn: StepName[I],
    await: StepName[I],
    ended: StepName[I],
    read: StepName[I]
)

/** How a spawn ended, as its parent reads it. */
enum Delegated extends caps.Pure {

  /** It answered; its reply as the parent's transaction read it. */
  case Replied(child: TurnRef, reply: Tainted[String], spent: Cost)

  /** Ended at its spend cap or its round limit; `reply` is what it said, if anything. */
  case Limited(child: TurnRef, reply: Option[Tainted[String]], spent: Cost)

  case Refused(why: Delegation.Refused)

  /** Its turn had already spawned `most` children ([[Spawning.PerTurn]]). */
  case OverLimit(most: Int)

  /** No child was made or started: the inbox refused or failed, `why` saying so in words a person
    * may be shown.
    */
  case Unspawned(why: String)

  /** Not ended within [[Spawning.Within]]. */
  case Unanswered(child: TurnRef)

  /** Ended, its reply unread: above what the parent's transaction may read, or that transaction
    * knows more than the request's `bound`.
    */
  case Unread(child: TurnRef)

  /** Ended without replying, or its reply could not be read; `why` says which. */
  case Failed(child: TurnRef, why: String)
}

/** The steps that make a child turn and wait for its reply, over the [[SpawnInbox]] a run hands
  * them, which the turn and a rehearsal share. Their records are in this object's forms alone:
  * no caller has a failure form of its own to record, as [[Calling]]'s have.
  */
object Spawning {

  /** How long each wait for a child's ring lasts before its end is read: 5 min. */
  val Every: FiniteDuration = 5.minutes

  /** How long a parent waits for its child in all: 30 min, six of [[Every]]. */
  val Within: FiniteDuration = 30.minutes

  /** The most children one turn spawns: 2. */
  val PerTurn: Int = 2

  private val Rungs: Int = (Within / Every).toInt

  /** A child of type `as`, asked for at `call` by the parent acting as `parent` in the
    * conversation begun at `origin`, briefed with `brief`, believed at `briefed`; recorded
    * through `by`, and read within `bound`, as [[Waiting.bound]] is.
    */
  final case class Request private (
      call: CallSlot,
      parent: Acting,
      origin: Origin,
      as: Subagent,
      brief: String,
      briefed: Integrity,
      by: Account,
      bound: Option[Label]
  ) extends caps.Pure

  object Request {

    /** The request, or `None` when `call` is not of `parent`'s turn. */
    def of(
        call: CallSlot,
        parent: Acting,
        origin: Origin,
        as: Subagent,
        brief: String,
        briefed: Integrity,
        by: Account,
        bound: Option[Label]
    ): Option[Request] =
      Option.when(call.turn == parent.turn)(
        new Request(call, parent, origin, as, brief, briefed, by, bound)
      )
  }

  /** `asked`'s child, made in its parent's room (a coding type's, its repository) and waited for.
    * Refused before any step when that room is a direct message's; then, in the parent's
    * transaction, `OverLimit`, or as [[Delegation.narrowed]] refuses it, at what the parent's
    * model has read (the transaction's floor joined with `bound`), its spend counting the caps
    * its earlier children were granted; made and started once however often it is rerun; its
    * end taken from its ring ([[SpawnInbox.returned]]), else read after each [[Every]] that
    * passes without one, up to [[Within]]; its reply and spend read in the
    * parent's transaction. At most 21 steps. In a rehearsal, whose waits return at once and whose
    * children run only after it returns, `Unanswered`. [[PerTurn]] and the cap count hold only
    * while one turn's spawns run one after another: each is counted in one step and made in the
    * next, so two run at once both count the children before either.
    */
  def spawn[I](steps: SpawnSteps[I], asked: I -> Request): Flow[SpawnEnv, I, Nothing, Delegated] = {
    val direct: Flow[Any, I, Delegated, I] = Flow.fromEither { i =>
      val room = roomOf(asked(i))
      if (room.direct) Left(Delegated.Refused(Delegation.Refused.Direct(room))) else Right(i)
    }
    val narrow =
      Step.transact[SpawnEnv, I, Narrowing](steps.narrow, i => asked(i).parent.subject)(env =>
        i => narrowing(env.spawns, asked(i))
      )
    val made = Step[SpawnEnv, Narrowed[I], Made](steps.spawn.on(_.in))(env =>
      n => making(env.spawns, asked(n.in), n.at)
    )
    val ended = Step[SpawnEnv, Child[I], Either[String, Option[TurnEnding]]](
      steps.ended.on(_.in)
    )(env => c => env.spawns.ended(c.child).left.map(e => InboxError.told(e)))
    val read = Step.transact[SpawnEnv, Ended[I], Read](
      steps.read.on(_.in),
      e => asked(e.in).parent.subject
    )(env => e => reading(env.spawns, asked(e.in).bound, e.child))
    val unrung: Flow[SpawnEnv, Child[I], Ended[I], Child[I]] =
      Flow.decide(ended) {
        case (c, Right(Some(ending))) => Left(Ended(c.in, c.child, ending))
        case (c, _) => Right(c)
      }
    // A ring carries the child's ending, sent from inside its workflow before that workflow's
    // status says it ended, so `ended` is read only when the wait timed out or its message is
    // no ending.
    val rung: Flow[SpawnEnv, Child[I], Ended[I], Child[I]] =
      Flow
        .recv[Child[I]](steps.await.on(_.in), c => SpawnInbox.topic(c.child), _ => Every)
        .keeping
        .andThen(
          Flow.branch((at: (Child[I], Option[String])) =>
            at._2.flatMap(TurnEnding.read).map(Ended(at._1.in, at._1.child, _)).toLeft(at._1)
          )(Flow.fromEither[Ended[I], Ended[I], Child[I]](Left(_)), unrung)
        )
    val waited: Flow[SpawnEnv, Child[I], Ended[I], Delegated] =
      (1 until Rungs)
        .foldLeft(rung)((f, _) => f.andThen(rung))
        .map(c => Delegated.Unanswered(c.child))
    val answered: Flow[SpawnEnv, Ended[I], Nothing, Delegated] =
      Flow.decide(read)((e, r) => Right(delegated(asked(e.in).as.spendCap, e, r)))
    val whole: Flow[SpawnEnv, I, Delegated, Delegated] =
      direct
        .andThen(Flow.decide(narrow) {
          case (i, Narrowing.Go(at)) => Right(Narrowed(i, at))
          case (_, Narrowing.Refused(why)) => Left(Delegated.Refused(why))
          case (_, Narrowing.OverLimit(most)) => Left(Delegated.OverLimit(most))
          case (_, Narrowing.Failed(why)) => Left(Delegated.Unspawned(why))
        })
        .andThen(Flow.decide(made) {
          case (n, Made.Child(child)) => Right(Child(n.in, child))
          case (_, Made.Above(at, room)) =>
            Left(Delegated.Refused(Delegation.Refused.Above(at, room)))
          case (_, Made.Unmade(why)) => Left(Delegated.Unspawned(why))
        })
        .andThen(Flow.attempt(waited))
        .andThen(Flow.either(answered, Flow.pass[Delegated]))
    Flow.attempt(whole).map(_.merge)
  }

  /** The room a child of `r` is made in: its type's repository, or its parent's room. */
  private def roomOf(r: Request): Place = r.as.tools match {
    case Subagent.Tools.Coding(_, repository, _) => repository.place
    case Subagent.Tools.Services(_) => r.origin.room
  }

  /** `s{round}-{index}-{h}`, `h` the first 16 hex digits of the SHA-256 of the parent turn's
    * `conversation:seq`: unique across parents, at most 40 characters, starting with a letter.
    */
  private def nameOf(call: CallSlot): Either[String, WorkName] = {
    val turn = call.turn
    val hash = Sha256
      .text(s"${ConversationId.value(turn.conversationId)}:${TurnSeq.value(turn.turnSeq)}")
      .hex
      .take(16)
    WorkName.of(s"s${call.round}-${call.index}-$hash")
  }

  /** What the parent's transaction decides of `r`. */
  private def narrowing(inbox: SpawnInbox^, r: Request)(using Tx^): Narrowing = {
    val at = r.bound.fold(Tx.classification.label)(Tx.locked(_).label)
    val parent = r.call.turn
    inbox.spent(parent).flatMap(own => inbox.granted(parent).map(caps => (own, caps))) match {
      case Left(e) => Narrowing.Failed(e.described)
      case Right((_, caps)) if caps.size >= PerTurn => Narrowing.OverLimit(PerTurn)
      case Right((own, caps)) =>
        Delegation
          .narrowed(
            r.parent,
            r.as,
            r.briefed,
            parentAt = at,
            room = Tx.roomLabel(roomOf(r)),
            origin = r.origin,
            spent = caps.foldLeft(own)((c, cap) => c + Cost.Exact(cap.usd))
          )
          .fold(Narrowing.Refused(_), _ => Narrowing.Go(at))
    }
  }

  /** `r`'s child recorded and started, classified `at`. */
  private def making(inbox: SpawnInbox^, r: Request, at: Label): Made =
    nameOf(r.call).fold(
      Made.Unmade(_),
      name =>
        inbox.spawned(
          Origin.Spawned(r.call.turn, name, roomOf(r)),
          r.as.agent,
          r.brief,
          r.as.spendCap,
          Delegation.believed(r.parent, r.briefed),
          at,
          r.by
        ) match {
          case Right(child) => Made.Child(child)
          case Left(InboxError.Above(classification, room)) => Made.Above(classification, room)
          case Left(e) => Made.Unmade(InboxError.told(e))
        }
    )

  /** `child`'s reply and spend, read in this transaction within `bound`. */
  private def reading(inbox: SpawnInbox^, bound: Option[Label], child: TurnRef)(using Tx^): Read =
    Calling.bounded(bound) match {
      case Left(e) => Read.Beyond(e.described)
      case Right(()) =>
        (for {
          reply <- inbox.reply(child)
          spent <- inbox.spent(child)
        } yield Read.Kept(reply, spent)).fold(e => Read.Failed(e.described), identity)
    }

  /** What a child capped at `cap`, ended as `e`, comes to, its reply and spend `r`. */
  private def delegated(cap: SpendCap, e: Ended[?], r: Read): Delegated = r match {
    case Read.Beyond(_) => Delegated.Unread(e.child)
    case Read.Failed(why) => Delegated.Failed(e.child, why)
    case Read.Kept(SpawnReply.Unread, _) => Delegated.Unread(e.child)
    case Read.Kept(reply, spent) =>
      val said = reply match {
        case SpawnReply.Said(text, believed) => Some(Mint.tainted(text, believed))
        case SpawnReply.Silent | SpawnReply.Unread => None
      }
      (e.ending, said) match {
        case (TurnEnding.Answered, Some(text)) => Delegated.Replied(e.child, text, spent)
        case (TurnEnding.AtLimit, _) => Delegated.Limited(e.child, said, spent)
        case (_, _) if !cap.admits(spent) => Delegated.Limited(e.child, said, spent)
        case (TurnEnding.Answered, None) => Delegated.Failed(e.child, NoReply)
        case (TurnEnding.Otherwise, _) => Delegated.Failed(e.child, Unfinished)
      }
  }

  private val NoReply = "it ended saying nothing"
  private val Unfinished = "it ended without finishing its task"

  private final case class Narrowed[I](in: I, at: Label)
  private final case class Child[I](in: I, child: TurnRef)
  private final case class Ended[I](in: I, child: TurnRef, ending: TurnEnding)

  /** What the parent's transaction decided: go, at what its model has read, or why not. */
  private enum Narrowing extends caps.Pure {
    case Go(at: Label)
    case Refused(why: Delegation.Refused)
    case OverLimit(most: Int)
    case Failed(why: String)
  }

  /** What became of the child's record and start. */
  private enum Made extends caps.Pure {
    case Child(child: TurnRef)
    case Above(classification: Label, room: Label)
    case Unmade(why: String)
  }

  /** What the parent's transaction read of the child once it ended. */
  private enum Read extends caps.Pure {
    case Kept(reply: SpawnReply, spent: Cost)
    case Beyond(why: String)
    case Failed(why: String)
  }

  // The recorded forms. A run in flight reads back what an earlier build wrote, so a form here is
  // never changed, only added to. SpawningTests pins each form its runs reach, and replays it;
  // none reaches a narrowing's or a read's `store`, a read's `beyond`, a spawn's `above` or
  // `unspawned`, or a `notWorking` or `direct` refusal, which are unpinned.

  private def cost(c: Cost): ujson.Value = c match {
    case Cost.Exact(usd) => ujson.Obj("exact" -> usd.bigDecimal.toPlainString)
    case Cost.AtLeast(usd) => ujson.Obj("atLeast" -> usd.bigDecimal.toPlainString)
  }

  private def costOf(v: ujson.Value): Either[String, Cost] = {
    val f = Fields("a cost", v)
    f.decimal("exact")
      .map(Cost.Exact(_))
      .orElse(f.decimal("atLeast").map(Cost.AtLeast(_)))
  }

  private def capOf(f: Fields, k: String): Either[String, SpendCap] =
    f.decimal(k).flatMap(SpendCap.of)

  private def turn(t: TurnRef): ujson.Value =
    ujson.Obj(
      "conversation" -> ConversationId.value(t.conversationId),
      "turn" -> ujson.Num(TurnSeq.value(t.turnSeq).toDouble)
    )

  private def turnOf(v: ujson.Value): Either[String, TurnRef] = {
    val f = Fields("a turn", v)
    for {
      c <- f.str("conversation")
      s <- f.long("turn")
    } yield TurnRef(ConversationId(c), TurnSeq(s))
  }

  private def label(f: Fields, k: String): Either[String, Label] = f.str(k).flatMap(Label.read)
  private def integrity(f: Fields, k: String): Either[String, Integrity] =
    f.str(k).flatMap(Integrity.read)

  private def refused(r: Delegation.Refused): ujson.Value = r match {
    case Delegation.Refused.Below(needs, has) =>
      ujson.Obj(
        "below" -> ujson.Obj(
          "needs" -> Integrity.written(needs),
          "has" -> Integrity.written(has)
        )
      )
    case Delegation.Refused.Above(classification, room) =>
      ujson.Obj(
        "above" -> ujson.Obj(
          "classification" -> Label.written(classification),
          "room" -> Label.written(room)
        )
      )
    case Delegation.Refused.NotWorking(repository) =>
      ujson.Obj("notWorking" -> repository.name)
    case Delegation.Refused.OverCap(asked, cap, spent) =>
      ujson.Obj(
        "overCap" -> ujson.Obj(
          "asked" -> asked.usd.bigDecimal.toPlainString,
          "cap" -> cap.usd.bigDecimal.toPlainString,
          "spent" -> cost(spent)
        )
      )
    case Delegation.Refused.Direct(room) => ujson.Obj("direct" -> room.written)
  }

  private def refusedOf(v: ujson.Value): Either[String, Delegation.Refused] = {
    val f = Fields("a refusal", v)
    def in(k: String) = f.obj(k).map(Fields(k, _))
    in("below")
      .flatMap(o =>
        for {
          needs <- integrity(o, "needs")
          has <- integrity(o, "has")
        } yield Delegation.Refused.Below(needs, has)
      )
      .orElse(
        in("above").flatMap(o =>
          for {
            c <- label(o, "classification")
            r <- label(o, "room")
          } yield Delegation.Refused.Above(c, r)
        )
      )
      .orElse(f.str("notWorking").flatMap(Repository.of).map(Delegation.Refused.NotWorking(_)))
      .orElse(
        in("overCap").flatMap(o =>
          for {
            asked <- capOf(o, "asked")
            cap <- capOf(o, "cap")
            spent <- o.field("spent").flatMap(costOf)
          } yield Delegation.Refused.OverCap(asked, cap, spent)
        )
      )
      .orElse(f.str("direct").flatMap(Place.read).map(Delegation.Refused.Direct(_)))
      .left
      .map(_ => s"recorded: no refusal: $v")
  }

  /** `{"go":{"at":l}}`, `{"refused":r}`, `{"overLimit":n}` or `{"store":w}`. */
  private given Journaled[Narrowing] = Journaled.json(
    {
      case Narrowing.Go(at) => ujson.Obj("go" -> ujson.Obj("at" -> Label.written(at)))
      case Narrowing.Refused(why) => ujson.Obj("refused" -> refused(why))
      case Narrowing.OverLimit(most) => ujson.Obj("overLimit" -> ujson.Num(most.toDouble))
      case Narrowing.Failed(why) => ujson.Obj("store" -> why)
    },
    v => {
      val f = Fields("a spawn's narrowing", v)
      f.obj("go")
        .flatMap(o => label(Fields("go", o), "at"))
        .map(Narrowing.Go(_))
        .orElse(f.field("refused").flatMap(refusedOf).map(Narrowing.Refused(_)))
        .orElse(f.int("overLimit").map(Narrowing.OverLimit(_)))
        .orElse(f.str("store").map(Narrowing.Failed(_)))
        .left
        .map(_ => s"recorded: no narrowing: $v")
    }
  )

  /** `{"child":t}`, `{"above":{"classification":l,"room":l}}` or `{"unspawned":w}`. */
  private given Journaled[Made] = Journaled.json(
    {
      case Made.Child(child) => ujson.Obj("child" -> turn(child))
      case Made.Above(classification, room) =>
        ujson.Obj(
          "above" -> ujson.Obj(
            "classification" -> Label.written(classification),
            "room" -> Label.written(room)
          )
        )
      case Made.Unmade(why) => ujson.Obj("unspawned" -> why)
    },
    v => {
      val f = Fields("a spawn", v)
      f.field("child")
        .flatMap(turnOf)
        .map(Made.Child(_))
        .orElse(
          f.obj("above").flatMap { o =>
            val a = Fields("above", o)
            for {
              c <- label(a, "classification")
              r <- label(a, "room")
            } yield Made.Above(c, r)
          }
        )
        .orElse(f.str("unspawned").map(Made.Unmade(_)))
        .left
        .map(_ => s"recorded: no spawn: $v")
    }
  )

  private val endings: Vector[(Option[TurnEnding], String)] =
    (None -> "running") +: TurnEnding.values.toVector.map(e => Some(e) -> TurnEnding.word(e))

  /** `{"ok":e}`, `e` one of `running`, `answered`, `atLimit`, `otherwise`; or `{"failed":w}`. */
  private given Journaled[Either[String, Option[TurnEnding]]] = Journaled.outcome(
    e => ujson.Str(endings.collectFirst { case (`e`, s) => s }.getOrElse("running")),
    v =>
      v.strOpt
        .flatMap(s => endings.collectFirst { case (e, `s`) => e })
        .toRight(s"recorded: no ending: $v")
  )

  /** `{"said":{"text":t,"integrity":i},"spent":c}`, `{"silent":true,"spent":c}`,
    * `{"unread":true,"spent":c}`, `{"beyond":w}` or `{"store":w}`.
    */
  private given Journaled[Read] = Journaled.json(
    {
      case Read.Kept(SpawnReply.Said(text, believed), spent) =>
        ujson.Obj(
          "said" -> ujson.Obj("text" -> text, "integrity" -> Integrity.written(believed)),
          "spent" -> cost(spent)
        )
      case Read.Kept(SpawnReply.Silent, spent) =>
        ujson.Obj("silent" -> true, "spent" -> cost(spent))
      case Read.Kept(SpawnReply.Unread, spent) =>
        ujson.Obj("unread" -> true, "spent" -> cost(spent))
      case Read.Beyond(why) => ujson.Obj("beyond" -> why)
      case Read.Failed(why) => ujson.Obj("store" -> why)
    },
    v => {
      val f = Fields("a spawn's reply", v)
      def kept(reply: SpawnReply) = f.field("spent").flatMap(costOf).map(Read.Kept(reply, _))
      f.obj("said")
        .flatMap { o =>
          val s = Fields("said", o)
          for {
            text <- s.str("text")
            believed <- integrity(s, "integrity")
          } yield SpawnReply.Said(text, believed)
        }
        .flatMap(kept)
        .orElse(f.bool("silent").flatMap(_ => kept(SpawnReply.Silent)))
        .orElse(f.bool("unread").flatMap(_ => kept(SpawnReply.Unread)))
        .orElse(f.str("beyond").map(Read.Beyond(_)))
        .orElse(f.str("store").map(Read.Failed(_)))
        .left
        .map(_ => s"recorded: no reply read: $v")
    }
  )
}
