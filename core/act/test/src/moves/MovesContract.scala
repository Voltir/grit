package grit.act.moves

import java.time.Instant

import grit.core.act.MovesFixtures.{replied, shown}
import grit.core.act.{
  Asked,
  Called,
  Keeping,
  MoveError,
  MoveKind,
  MoveLimits,
  MoveName,
  Moves,
  MovesFixtures,
  Posed
}
import grit.core.classify.{Answer as Judged, Ask, StateJson}
import grit.core.document.{DocText, Document, DocumentKeeper}
import grit.core.durable.Journaled
import grit.core.flow.{Tainted, TestTainted}
import grit.core.id.DocKey
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.{Namespace, Place, Service}
import grit.core.provider.ModelRequest
import grit.core.schema.JsonSchema
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.core.visibility.{Label, Level}

import utest.*

/** The rules every [[Moves]] keeps, run against [[DurableMoves]] and against the fake jobs'
  * tests use, `grit.core.act.ScriptedMoves`: a move's result as its world gives it; a name made
  * once, by either kind, is `Repeated` after, whatever the move came to; past its kind's limit a
  * move is `OverLimit`; a refused move is not made and counts against no limit; an ask whose
  * store failed was made, and counts; a JSON reply its schema refuses is asked for once more,
  * and a model's failure is `Model` in the same words. And every [[Keeping]]'s: a keep whose
  * body is `Left` keeps none of its writes, is `Store` in its words, and its name is spent; a
  * keep writes at no less than its floor; keeps are not limited.
  */
abstract class MovesContract extends TestSuite {
  import MovesContract.*

  /** What `use` returns, given moves within `limits` in a world where a text ask is answered
    * [[Answer]]; a JSON ask's model replies each of `shapes` in turn, across the run, the last
    * again once they run out (`{"answer": Answer}`, [[Shaped]], when there are none); a
    * judgment's classifier answers `judgment`; and a call of [[Tool]] at [[Probe]] is done with
    * [[Read]] at [[Floor]]; when `broken`, every ask's store fails instead.
    */
  def within[A <: caps.Pure](
      limits: MoveLimits,
      broken: Boolean = false,
      shapes: Vector[ujson.Value] = Vector(),
      judgment: Judged = Judged.YesNo(Yes)
  )(use: Moves^ -> A): A

  /** What `use` returns, given moves as [[within]]'s within `limits`, whose keeps keep a
    * plugin's documents in transactions opened at [[Floor]]; and that plugin's current documents
    * afterwards, as a run at [[Floor]] reads them, most recently written first.
    */
  def keeping[A <: caps.Pure](limits: MoveLimits)(use: Keeping^ -> A): (A, Vector[Document])

  val tests = Tests {
    test(
      "an ask is answered as its world answers, and a call done with its edge's answer, each at the world's floor"
    ) {
      val got = within(limits(1, 1)) { m =>
        Seen(
          Vector(
            m.ask(name("a"), posed(Posed.Text(Request))).map(a => s"${text(a)} at ${a.at}"),
            called(m, "c")
          )
        )
      }
      got ==> Seen(Vector(Right(s"$Answer at $Floor"), Right(s"Done($Read,$Floor)")))
    }

    test("a name made once, by either kind, is Repeated after, and the move is not made") {
      val got = within(limits(2, 2)) { m =>
        Seen(Vector(asked(m, "a"), called(m, "a"), asked(m, "a"), called(m, "b"), asked(m, "b")))
      }
      got ==> Seen(
        Vector(
          Right(Answer),
          Left(MoveError.Repeated(name("a"))),
          Left(MoveError.Repeated(name("a"))),
          Right(s"Done($Read,$Floor)"),
          Left(MoveError.Repeated(name("b")))
        )
      )
    }

    test("an ask whose store failed spends its name") {
      val got = within(limits(2, 0), broken = true) { m =>
        Seen(Vector(asked(m, "a"), asked(m, "a")))
      }
      got.moves.map(_.left.map(kind)) ==> Vector(Left("Store"), Left("Repeated(a)"))
    }

    test("past its kind's limit a move is OverLimit; a refused move counts against no limit") {
      val got = within(limits(2, 1)) { m =>
        Seen(
          Vector(
            asked(m, "a"),
            asked(m, "a"),
            asked(m, "b"),
            asked(m, "c"),
            called(m, "d"),
            called(m, "e")
          )
        )
      }
      got ==> Seen(
        Vector(
          Right(Answer),
          Left(MoveError.Repeated(name("a"))),
          Right(Answer),
          Left(MoveError.OverLimit(MoveKind.Ask, 2)),
          Right(s"Done($Read,$Floor)"),
          Left(MoveError.OverLimit(MoveKind.Call, 1))
        )
      )
    }

    test(
      "a JSON ask is answered as its world answers, counts against asks, and shares its names with text asks"
    ) {
      val got = within(limits(2, 0)) { m =>
        Seen(Vector(shaped(m, "a"), asked(m, "a"), asked(m, "b"), shaped(m, "b"), shaped(m, "c")))
      }
      got ==> Seen(
        Vector(
          Right(s"$Shaped at $Floor"),
          Left(MoveError.Repeated(name("a"))),
          Right(Answer),
          Left(MoveError.Repeated(name("b"))),
          Left(MoveError.OverLimit(MoveKind.Ask, 2))
        )
      )
    }

    test(
      "a judgment is judged as its world judges at the world's floor, counts against judgments while asks remain, and shares its names with asks"
    ) {
      val limited =
        MoveLimits.of(2, 0, 1).fold(e => throw new java.lang.AssertionError(e), identity)
      val got = within(limited) { m =>
        Seen(
          Vector(
            judged(m, "a"),
            asked(m, "a"),
            shaped(m, "b"),
            judged(m, "b"),
            judged(m, "c"),
            asked(m, "c")
          )
        )
      }
      got ==> Seen(
        Vector(
          Right(s"$Yes at $Floor"),
          Left(MoveError.Repeated(name("a"))),
          Right(s"$Shaped at $Floor"),
          Left(MoveError.Repeated(name("b"))),
          Left(MoveError.OverLimit(MoveKind.Judge, 1)),
          Right(Answer)
        )
      )
    }

    test("a judgment whose store failed spends its name and counts against judgments") {
      val limited =
        MoveLimits.of(0, 0, 1).fold(e => throw new java.lang.AssertionError(e), identity)
      val got = within(limited, broken = true) { m =>
        Seen(Vector(judged(m, "a"), judged(m, "a"), judged(m, "b")))
      }
      got.moves.map(_.left.map(kind)) ==>
        Vector(Left("Store"), Left("Repeated(a)"), Left("OverLimit(Judge,1)"))
    }

    test("a JSON reply its schema refuses is asked for once more, and read when that conforms") {
      val got = within(limits(1, 0), shapes = Vector(Unanswered, Answered)) { m =>
        Seen(Vector(shaped(m, "a")))
      }
      got ==> Seen(Vector(Right(s"$Shaped at $Floor")))
    }

    test("a JSON reply refused twice is Model, naming the last refusal, and spends its name") {
      // A third reply would conform: a second repair would read it.
      val got = within(limits(2, 0), shapes = Vector(Unanswered, Unanswered, Answered)) { m =>
        Seen(Vector(shaped(m, "a"), shaped(m, "a")))
      }
      got ==> Seen(
        Vector(
          Left(
            MoveError.Model(
              "its arguments do not match its schema: answer: expected a string, got 1"
            )
          ),
          Left(MoveError.Repeated(name("a")))
        )
      )
    }

    test("a judgment whose answers do not read is Model, in the classifier's words") {
      val choice = Judged.Choice("yes", Vector(Judged.Weight("yes", 1.0)), 1.0)
      val got = within(limits(1, 0), judgment = choice) { m => Seen(Vector(judged(m, "a"))) }
      got ==> Seen(
        Vector(
          Left(
            MoveError.Model(
              "the classifier's answers do not read: \"Is `text` urgent?\": answered a choice to a yes/no"
            )
          )
        )
      )
    }

    test("an ask whose store failed counts against its kind's limit") {
      val got = within(limits(1, 0), broken = true) { m =>
        Seen(Vector(asked(m, "a"), asked(m, "b")))
      }
      got.moves.map(_.left.map(kind)) ==> Vector(Left("Store"), Left("OverLimit(Ask,1)"))
    }

    test("under zero limits no move is made") {
      val got = within(MoveLimits.Zero) { m => Seen(Vector(asked(m, "a"), called(m, "a"))) }
      got ==> Seen(
        Vector(
          Left(MoveError.OverLimit(MoveKind.Ask, 0)),
          Left(MoveError.OverLimit(MoveKind.Call, 0))
        )
      )
    }

    test(
      "a keep whose body is Left keeps none of its writes, is Store in its words, and its name is spent"
    ) {
      val (got, docs) = keeping(MoveLimits.Zero) { m =>
        val refused = m.keep[Unit, Kept](name("a"), From) { (_, keeper, at) =>
          write(keeper, "k", Label.Public, at).flatMap(_ => Left(StoreError.Invalid("not today")))
        }
        val again = m.keep(name("a"), From)((_, keeper, at) => write(keeper, "k", Label.Public, at))
        val other = m.keep(name("b"), From)((_, keeper, at) => write(keeper, "b", Label.Public, at))
        Seen(Vector(refused, again, other).map(_.map(_.label)))
      }
      (got, docs.map(d => DocKey.value(d.key))) ==> (
        Seen(
          Vector(
            Left(MoveError.Store("not today")),
            Left(MoveError.Repeated(name("a"))),
            Right(Label.written(Floor))
          )
        ),
        Vector("b")
      )
    }

    test(
      "a keep from what is believed less than Outside is Below: its body does not run, nothing is written, and its name is spent"
    ) {
      val (got, docs) = keeping(MoveLimits.Zero) { m =>
        val low = m.keep(name("a"), TestTainted.at((), Integrity.Untrusted))((_, keeper, at) =>
          write(keeper, "a", Label.Public, at)
        )
        val again = m.keep(name("a"), From)((_, keeper, at) => write(keeper, "a", Label.Public, at))
        Seen(Vector(low, again).map(_.map(_.label)))
      }
      (got, docs) ==> (
        Seen(
          Vector(Left(MoveError.Below(Integrity.Untrusted)), Left(MoveError.Repeated(name("a"))))
        ),
        Vector()
      )
    }

    test(
      "a keep's body is given what it keeps from, and its documents are believed at no more than that"
    ) {
      val (_, docs) = keeping(MoveLimits.Zero) { m =>
        Seen(
          Vector(
            m.keep(name("o"), TestTainted.at("outside", Integrity.Outside))((k, keeper, at) =>
              write(keeper, k, Label.Public, at)
            ),
            m.keep(name("a"), TestTainted.at("authority", Integrity.Authority))((k, keeper, at) =>
              write(keeper, k, Label.Public, at)
            )
          ).map(_.map(_.label))
        )
      }
      docs.map(d => (DocKey.value(d.key), Integrity.written(d.integrity))).sorted ==>
        Vector(("authority", "member"), ("outside", "outside"))
    }

    test("an ask's reply is believed as what it was posed from, whatever its shape") {
      given StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))
      val got = within(limits(2, 0)) { m =>
        Seen(
          Vector(
            m.ask(name("t"), TestTainted.at(Posed.Text(Request), Integrity.Outside))
              .map(a => Integrity.written(a.reply.integrity)),
            m.ask(
              name("j"),
              TestTainted.at(
                Posed.json(Request.system, Request.messages, AnswerSchema),
                Integrity.Untrusted
              )
            ).map(a => Integrity.written(a.reply.integrity)),
            m.ask(
              name("k"),
              TestTainted.at(
                Posed.judge(State, Ask.yesNo[String]("Is `text` urgent?", None, None)),
                Integrity.Authority
              )
            ).map(a => Integrity.written(a.reply.integrity))
          )
        )
      }
      got ==> Seen(Vector(Right("outside"), Right("untrusted"), Right("authority")))
    }

    test(
      "a call's answer is believed at the meet of what its service's results are and what its arguments are"
    ) {
      val got = within(limits(0, 2)) { m =>
        Seen(
          Vector(Integrity.Authority, Integrity.Outside).zipWithIndex.map((i, n) =>
            m.call(name(s"c$n"), Probe, Tool, TestTainted.at(Args, i)).map {
              case Called.Done(text, _) => Integrity.written(text.integrity)
              case other => shown(other)
            }
          )
        )
      }
      got ==> Seen(Vector(Right(Integrity.written(ProbeOutput)), Right("outside")))
    }

    test("a keep writes at no less than its floor: below it at the floor, above it as asked") {
      val above = Label.at(Level.Restricted)
      val (got, docs) = keeping(MoveLimits.Zero) { m =>
        Seen(
          Vector(
            m.keep(name("low"), From)((_, keeper, at) => write(keeper, "low", Label.Public, at)),
            m.keep(name("high"), From)((_, keeper, at) => write(keeper, "high", above, at))
          ).map(_.map(_.label))
        )
      }
      (got, docs.map(d => (DocKey.value(d.key), Label.written(d.label)))) ==> (
        Seen(Vector(Right(Label.written(Floor)), Right(Label.written(above)))),
        Vector(("low", Label.written(Floor)))
      )
    }

    test("keeps are not limited, and count against no other kind's limit") {
      val (got, docs) = keeping(limits(1, 0)) { m =>
        val kept = Vector("a", "b", "c").map(n =>
          m.keep(name(n), From)((_, keeper, at) => write(keeper, n, Label.Public, at)).map(_.label)
        )
        Seen(kept ++ Vector(asked(m, "d"), called(m, "e")))
      }
      (got, docs.map(d => DocKey.value(d.key)).sorted) ==> (
        Seen(
          Vector.fill(3)(Right(Label.written(Floor))) ++ Vector(
            Right(Answer),
            Left(MoveError.OverLimit(MoveKind.Call, 0))
          )
        ),
        Vector("a", "b", "c")
      )
    }

    test("a keep named as an earlier ask is Repeated, and so is an ask or call named as a keep") {
      val (got, docs) = keeping(limits(2, 1)) { m =>
        val first = asked(m, "a")
        val keptA = m.keep(name("a"), From)((_, keeper, at) => write(keeper, "a", Label.Public, at))
        val keptB = m.keep(name("b"), From)((_, keeper, at) => write(keeper, "b", Label.Public, at))
        Seen(Vector(first, keptA.map(_.label), keptB.map(_.label), asked(m, "b"), called(m, "b")))
      }
      (got, docs.map(d => DocKey.value(d.key))) ==> (
        Seen(
          Vector(
            Right(Answer),
            Left(MoveError.Repeated(name("a"))),
            Right(Label.written(Floor)),
            Left(MoveError.Repeated(name("b"))),
            Left(MoveError.Repeated(name("b")))
          )
        ),
        Vector("b")
      )
    }
  }
}

object MovesContract {

  /** The label a keep wrote at, journaled as its written form. */
  final case class Kept(label: String) extends caps.Pure
  object Kept {
    given Journaled[Kept] =
      Journaled.json(k => ujson.Str(k.label), v => v.strOpt.map(Kept(_)).toRight("no label"))
  }

  /** Where a keep's documents are placed. */
  val Here: Place = Place.under(Namespace.Task, Vector("notes"))

  /** Writes `k`'s own name as its text under `k` at `label` through `keeper`, at `at`: the
    * label it was kept at.
    */
  def write(keeper: DocumentKeeper, k: String, label: Label, at: Instant)(using
      Tx^
  ): Either[StoreError, Kept] =
    keeper
      .write(
        DocKey.of(k).fold(e => throw new java.lang.AssertionError(e), identity),
        label,
        Integrity.Member,
        Here,
        DocText.of(k).fold(e => throw new java.lang.AssertionError(e), identity),
        ujson.Obj(),
        None,
        at
      )
      .map(w => Kept(Label.written(w.kept)))

  /** What a run's moves came to, each an ask's text or a call's result. */
  final case class Seen(moves: Vector[Either[MoveError, String]]) extends caps.Pure

  /** The ask `n` of [[Request]] through `m`: its text. */
  def asked(m: Moves^, n: String): Either[MoveError, String] =
    m.ask(name(n), posed(Posed.Text(Request))).map(text)

  /** The JSON ask `n` of [[Request]]'s system text and messages, held to [[AnswerSchema]],
    * through `m`: its conforming JSON's text and the label it was read at.
    */
  def shaped(m: Moves^, n: String): Either[MoveError, String] =
    m.ask(name(n), posed(Posed.json(Request.system, Request.messages, AnswerSchema)))
      .map(a => s"${replied(a).text} at ${a.at}")

  /** The judgment `n`, whether [[State]] is urgent, through `m`: its probability of yes and the
    * label it was read at.
    */
  def judged(m: Moves^, n: String): Either[MoveError, String] = {
    given StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))
    m.ask(name(n), posed(Posed.judge(State, Ask.yesNo[String]("Is `text` urgent?", None, None))))
      .map(a => s"${replied(a)} at ${a.at}")
  }

  /** The state every judgment is about. */
  val State = "the build is red"

  /** What a yes/no judgment is answered: its probability of yes. */
  val Yes: Double = MovesFixtures.Yes

  /** The call `n` of [[Tool]] at [[Probe]] through `m`: its result. */
  def called(m: Moves^, n: String): Either[MoveError, String] =
    m.call(name(n), Probe, Tool, TestTainted.at(Args, Believed)).map(shown)

  /** What an ask is answered. */
  val Answer = "hello"

  /** What a JSON ask is answered, as its conforming JSON's text. */
  val Shaped = """{"answer":"hello"}"""

  /** [[Shaped]], as a model replies it. */
  val Answered: ujson.Value = ujson.Obj("answer" -> Answer)

  /** A reply [[AnswerSchema]] refuses. */
  val Unanswered: ujson.Value = ujson.Obj("answer" -> 1)

  /** `{"answer": string}`. */
  val AnswerSchema: JsonSchema = JsonSchema
    .read(
      ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("answer" -> ujson.Obj("type" -> "string")),
        "required" -> ujson.Arr("answer"),
        "additionalProperties" -> false
      )
    )
    .fold(e => throw new java.lang.AssertionError(e.message), identity)

  /** What a call of [[Tool]] at [[Probe]] is answered. */
  val Read = "read"

  /** The least label every transaction of the world is opened at: above public, so a move that
    * reports public whatever its floor is seen.
    */
  val Floor: Label = Label.at(Level.Internal)

  val Probe: Service =
    Service.of("probe").fold(e => throw new java.lang.AssertionError(e), identity)

  val Tool: ToolName = ToolName("probe_read")

  val Args: ujson.Obj = ujson.Obj("path" -> "a")

  /** What every move is made from is believed at, unless a test says: [[Integrity.Member]]. */
  val Believed: Integrity = Integrity.Member

  /** `p`, posed from what is believed at [[Believed]]. */
  def posed[R](p: Posed[R]): Tainted[Posed[R]] = TestTainted.at(p, Believed)

  /** What a keep keeps from unless a test says: nothing, at [[Believed]]. */
  val From: Tainted[Unit] = TestTainted.at((), Believed)

  /** What [[Probe]]'s results are believed at: [[Integrity.Member]]. */
  val ProbeOutput: Integrity = Integrity.Member

  val Request: ModelRequest = ModelRequest("Say hello.", Vector(Message.User("hi")))

  def name(n: String): MoveName =
    MoveName.of(n).fold(e => throw new java.lang.AssertionError(e), identity)

  def limits(asks: Int, calls: Int): MoveLimits =
    MoveLimits.of(asks, calls).fold(e => throw new java.lang.AssertionError(e), identity)

  def text(a: Asked[Message.Assistant]): String =
    replied(a).blocks.collect { case AssistantBlock.Text(t) => t }.mkString

  /** A move error's case name, and its name when it is `Repeated`. */
  def kind(e: MoveError): String = e match {
    case MoveError.Repeated(n) => s"Repeated(${MoveName.value(n)})"
    case MoveError.Store(_) => "Store"
    case other => other.toString
  }
}
