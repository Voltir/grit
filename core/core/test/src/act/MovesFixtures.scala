package grit.core.act

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.durable.InMemoryDurable
import grit.core.flow.Trusted
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, Ladder, ModelId, ModelRef, Pinned, Policy, Upstream}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, TokenEstimator, ToolUse}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Recorded, Subject, Visibility}
import grit.dbos.sql.TestTx

/** What the moves' tests and a job's run's tests both stand their moves on: the models, the
  * catalog, the estimator and the database a move reads through.
  */
object MovesFixtures {

  /** What a call came to, as the moves' tests and recorded runs' replies write it:
    * `Done(text,label)`, `Failed(why)` or `Interrupted`, nothing said of what it is believed at
    * or of the facts it kept.
    */
  def shown(called: Called): String = called match {
    case Called.Done(answer, at) => s"Done(${Trusted.asIs(answer).extract.text},$at)"
    case other => other.toString
  }

  /** `reply`, as believed. */
  def replied[R](asked: Asked[R]): R = Trusted.asIs(asked.reply).extract

  /** Models whose every provider answers `answer`, at a priced cost, counting its calls and
    * keeping each request in [[requests]] and the pair it was pinned to in [[pairs]]; required to call a tool, it calls the request's first
    * tool `calling` times (answering `answer` in text when that is 0, as a model may), each
    * call's arguments the next of `shapes` (the last again once they run out;
    * `{"answer": answer}` when there are none), the `i`th call of its `n`th answer `call-{n}`,
    * or `call-{n}-{i}` from its second. With `crash`, the process dies inside the first call.
    * Each reply reports `upstream` as who served it; a text answer stops as `stop` says, and is
    * `answer`, or from its second the next of `later` (the last again once they run out).
    */
  final class Answering(
      answer: String,
      crash: Boolean = false,
      shapes: Vector[ujson.Value] = Vector(),
      calling: Int = 1,
      upstream: Option[String] = None,
      stop: StopReason = StopReason.EndTurn,
      later: Vector[String] = Vector()
  ) extends Models {
    // Written and read only on the test's own thread, through the calls the moves make on it.
    @caps.unsafe.untrackedCaptures
    var calls = 0
    @caps.unsafe.untrackedCaptures
    var requests: Vector[ModelRequest] = Vector()
    @caps.unsafe.untrackedCaptures
    var pairs: Vector[ModelRef] = Vector()
    @caps.unsafe.untrackedCaptures
    private var armed = crash
    @caps.unsafe.untrackedCaptures
    private var shaped = 0
    @caps.unsafe.untrackedCaptures
    private var texted = 0
    private def text(): AssistantBlock = {
      val all = answer +: later
      val t = all.lift(texted).orElse(all.lastOption).getOrElse(answer)
      texted += 1
      AssistantBlock.Text(t)
    }
    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ = new Provider {
      def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
        calls += 1
        requests = requests :+ r
        pairs = pairs :+ pinned.assignment.ref
        if (armed) { armed = false; throw new InMemoryDurable.Crash }
        val (blocks, stopped) = r.use match {
          case ToolUse.Required if calling > 0 =>
            val tool = r.tools.headOption.fold("none")(_.name)
            val made = (0 until calling).toVector.map { i =>
              val args = shapes
                .lift(shaped)
                .orElse(shapes.lastOption)
                .getOrElse(ujson.Obj("answer" -> answer))
              shaped += 1
              val id = if (i == 0) s"call-$calls" else s"call-$calls-$i"
              AssistantBlock.ToolCall(ToolCallId(id), tool, ujson.read(args.render()))
            }
            (made, StopReason.ToolUse)
          case ToolUse.Required =>
            (Vector(text()), stop)
          case ToolUse.Auto | ToolUse.Off =>
            (Vector(text()), stop)
        }
        Right(
          Message.Assistant(
            blocks,
            stopped,
            Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
            "test/summary",
            upstream
          )
        )
      }
    }
  }

  /** A classifier answering each request with the next of `script` (the last again once they
    * run out), counting its calls and keeping each request's state in [[states]]. With no
    * script, it answers each question of its kind, at [[Judged]]'s cost: a choice its first key,
    * wholly; a yes/no [[Yes]]; a score its first level, wholly.
    */
  final class Judging(script: Vector[Either[ClassifierError, Answers]] = Vector())
      extends Classifier {
    // Written and read only on the test's own thread, through the calls the moves make on it.
    @caps.unsafe.untrackedCaptures
    var calls = 0
    @caps.unsafe.untrackedCaptures
    var states: Vector[ujson.Value] = Vector()
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      val i = calls
      calls += 1
      states = states :+ state
      script
        .lift(i)
        .orElse(script.lastOption)
        .getOrElse(Right(Answers(questions.map(fitting), Judged, "test/judge")))
    }
  }

  /** What an unscripted [[Judging]] answers a yes/no: its probability of yes. */
  val Yes: Double = 0.75

  /** What every call of a [[Judging]] costs. */
  val Judged: Usage = Usage(Tokens(300), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.000013")))

  private def fitting(q: Question): Answer = q match {
    case c: Question.Choice =>
      val weights =
        c.keys.zipWithIndex.map((k, i) => Answer.Weight(k.name, if (i == 0) 1.0 else 0.0))
      Answer.Choice(c.keys.headOption.fold("")(_.name), weights, 1.0)
    case _: Question.YesNo => Answer.YesNo(Yes)
    case s: Question.Score =>
      Answer.Score(0, s.levels.indices.toVector.map(i => if (i == 0) 1.0 else 0.0), 1.0)
  }

  /** The summaries' pair, `test/summary` served by `test` alone: a party a world clears. */
  val Summary: ModelRef =
    ModelRef(
      ModelId.of("test/summary").getOrElse(throw new java.lang.AssertionError("summary")),
      Upstream.of("test")
    )

  /** A dreaming job's summaries' pair, `test/dream` served by `test` alone: a party a world
    * clears.
    */
  val Dream: ModelRef =
    ModelRef(
      ModelId.of("test/dream").getOrElse(throw new java.lang.AssertionError("dream")),
      Upstream.of("test")
    )

  /** Each role pinned to `test/<role>`, 1024 tokens out, the summaries to [[Summary]] and a
    * dreaming job's to [[Dream]].
    */
  val TestCatalog: Catalog = {
    def role(name: String) =
      Assignment(
        ModelRef(
          ModelId.of(s"test/$name").getOrElse(throw new java.lang.AssertionError(name)),
          None
        ),
        1024,
        None
      )
    val summary = Assignment(Summary, 1024, None)
    val policy = Policy.one(role("turn"), summary, role("query"), summary)
    Catalog.of(policy.copy(dream = Ladder.one(Assignment(Dream, 1024, None))), Vector.empty)
  }

  /** A token for each character of a message's or prompt's written form. */
  object PerChar extends TokenEstimator {
    def message(message: Message): Tokens = Tokens(message.toString.length.toLong)
    def system(prompt: String): Tokens = Tokens(prompt.length.toLong)
  }

  /** Every read in one transaction opened at `clearance`, whatever its subject, under
    * `visibility` and what `recorded` holds.
    */
  final class FakeDb(
      clearance: Clearance = Clearance.of(Label.Public),
      visibility: Visibility = Visibility.Shipped,
      recorded: Recorded = Recorded.Empty
  ) extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(clearance, visibility, recorded))
  }
}
