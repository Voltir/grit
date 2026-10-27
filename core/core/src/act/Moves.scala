package grit.core.act

import java.time.Instant

import grit.core.classify.{Ask, Request, StateJson}
import grit.core.document.{
  Declassified,
  DocLabel,
  DocText,
  DocumentKeeper,
  DocumentSearch,
  PolicyApplication
}
import grit.core.durable.Journaled
import grit.core.files.PieceSearch
import grit.core.flow.{Extracted, Shape, Tainted, Trusted}
import grit.core.id.{DocumentVersionId, PluginName}
import grit.core.message.{Cut, Message}
import grit.core.need.Needs
import grit.core.place.Service
import grit.core.prompt.{Fenced, Tag}
import grit.core.provider.ModelRequest
import grit.core.sanitise.{IntegrityTree, Reason, Suspicions, Verdict, Vetted}
import grit.core.schema.{Conforming, JsonSchema, Typed}
import grit.core.screen.{Bench, Screen, Screened}
import grit.core.stitch.{Strand, StrandExcerpts, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Outcome, ToolName}
import grit.core.visibility.{DeclassificationPolicy, Label}

/** What an ask poses, and what its reply reads as: `R`. Each shape is answered by its own
  * model and recorded as that model replied; `R` is read from the record on every run.
  */
sealed trait Posed[R]

object Posed {

  /** `request`, answered by the model in its own words. */
  final case class Text(request: ModelRequest) extends Posed[Message.Assistant]

  /** `system` then `messages`, answered as JSON that `reply`'s schema accepts and `reply`
    * reads. The model is offered one tool, [[ReplyTool]], whose parameters are the schema, and
    * must call it; the call's arguments are the reply. A reply that does not read (no such
    * call, a refusal by the schema, or by `reply`) is shown to the model with why and asked
    * again, up to [[Json.Repairs]] times. Every call is recorded, and all are admitted once,
    * before the first. A message holding a tool call may be refused by the provider, since no
    * other tool is offered.
    */
  final case class Json[T](system: String, messages: Vector[Message], reply: Typed[T])
      extends Posed[T]

  object Json {

    /** 1: how many times a reply that does not read is asked again, so up to two model calls. */
    val Repairs: Int = 1
  }

  /** Questions about a state, judged by the deployment's classifier with a probability for
    * each option or level, and read as `questions` reads them ([[grit.core.classify.Ask.read]]).
    * `request` is what the classifier is sent: the state as its
    * [[grit.core.classify.StateJson]] writes it, and `questions`' questions. The classifier is
    * cleared as the service it calls (ADR 0031): only what that clearance dominates is sent to it.
    * Made by [[judge]].
    */
  final case class Judgment[T] private[Posed] (request: Request, questions: Ask[?, T])
      extends Posed[T]

  /** `questions` about `state`. */
  def judge[S: StateJson, T](state: S, questions: Ask[S, T]): Posed[T] =
    Judgment(Request.of(state, questions), questions)

  /** `system` then `messages`, answered as JSON `schema` accepts. */
  def json(system: String, messages: Vector[Message], schema: JsonSchema): Posed[Conforming] =
    Json(system, messages, Typed.json(schema))

  /** `reply`: the one tool a [[Json]] ask offers. */
  val ReplyTool: String = "reply"
}

/** An ask's reply, and the most what it holds can be: what the ask was made from was read at
  * no more than `at`. The reply is believed as what it was posed from is.
  */
final case class Asked[R](reply: Tainted[R], at: Label)

/** A call's answer as a planner reads it: `text`, as the model would read it, and `stated`,
  * the facts of what it kept, as its service's extension wrote them.
  */
final case class Answer(text: String, stated: Outcome.Stated) extends caps.Pure

/** What a call came to. Never a person's decline or silence: a call whose tool asks first is
  * not sent under [[Gates.Closed]], and a planner's acting is closed.
  */
enum Called {

  /** The edge ran it: `answer` is what it answered, holding nothing above `at`, the label in
    * force at its service's place joined with those at the places of the parts it kept, and
    * believed at most as what its service returns is
    * ([[grit.core.visibility.Visibility.outputOf]]) and as its arguments are.
    *
    * `at` labels what the service gave, which may sit below the caller's floor, and nothing
    * checks it against the floor, so it may sit above it too. It does not cover the request:
    * an answer whose text echoes its arguments back holds them, and they were written at the
    * caller's floor. A reader that keeps `answer`, or anything made from it, keeps it at `at`
    * joined with its own floor.
    */
  case Done(answer: Tainted[Answer], at: Label)

  /** It did not run, or failed: `why`. Nothing was sent when no edge serves the place's tool,
    * the tool is not advertised there or asks first, or the acting may not read from that place,
    * send it the arguments, or write where they name (ADR 0031), or its arguments are believed
    * less than its tool requires ([[grit.core.tool.CallError.Withheld]]); no edge claimed it within
    * `grit.act.phase.Calling.ServeWithin`; or the tool failed.
    */
  case Failed(why: String)

  /** An edge claimed it and did not answer within `grit.act.phase.Calling.RunWithin` more: it
    * may have partly run.
    */
  case Interrupted
}

/** A planner's moves (ADR 0034), each made under its run's [[Acting]] at most once, as durable
  * steps named for it: a run resumed after a crash gets each move's result back without making
  * it again. So a run makes the same moves, by name, with the same inputs, in the same order,
  * every time: its code between moves reads only its `JobRun` and its moves' results, and a move
  * whose input differs on a rerun is [[MoveError.Diverged]]. Any move is refused, and not made,
  * when its name was used earlier in the run ([[MoveError.Repeated]]) or its kind's limit is
  * reached ([[MoveError.OverLimit]]).
  */
trait Moves {

  /** One reply to `posed`, from the model its shape names: for [[Posed.Text]] and
    * [[Posed.Json]], the first of the models the catalog in force assigns to summaries
    * (`grit.core.model.Policy.summary`) that is cleared for what the run's conversation knows,
    * within its output budget; for [[Posed.Judgment]], the deployment's classifier, asked
    * again while it is unavailable, as a provider is. Each model's or classifier's call's cost
    * is recorded once, under the run's conversation, with who served it. Admitted once, before
    * its first call (a [[Posed.Json]] repair is not admitted again). [[MoveError.Capped]] when
    * the allowance does not admit it, and [[MoveError.Uncleared]] when no summary model (or
    * the classifier's service) is cleared for what the conversation knows (ADR 0031): in
    * both, nothing is called. [[MoveError.Model]] when no catalog reads (for a text or JSON
    * ask), the provider or classifier fails after its retries, or the reply does not read
    * ([[Posed.Json]]: after its repairs). Its reply is believed as `posed` is.
    */
  def ask[R](name: MoveName, posed: Tainted[Posed[R]]): Either[MoveError, Asked[R]]

  /** `tool`, named as a model is offered it (an MCP server's tool under its server's prefix),
    * called with `arguments` at `service`'s place (ADR 0017), for the acting's principal, with
    * the retry the serving edge advertises for it. A tool that writes outside grit names its
    * destination in `arguments`' `to`, as a model's call does ([[grit.core.tool.Writes.Field]]).
    * Every refusal is a [[Called.Failed]], never a [[MoveError]]: one whose `arguments` are
    * believed less than the tool requires ([[grit.core.tool.ToolSet.Entry.requires]]) is
    * failed with [[grit.core.tool.CallError.Withheld]]'s words, and nothing is sent.
    */
  def call(
      name: MoveName,
      service: Service,
      tool: ToolName,
      arguments: Tainted[ujson.Obj]
  ): Either[MoveError, Called]

  /** `question` of `from`, asked as a JSON ask of [[Extraction]] is ([[ask]]), its reply read
    * in `shape` ([[Extraction.reply]]): the answer, or `None` when `from` does not say. The
    * answer is trusted at the acting's integrity ([[Acting.integrity]]) met with
    * [[grit.core.visibility.Integrity.Member]] ([[Extraction.Node]]), whatever `from` is
    * believed at: an `Authority`
    * run's is `Member`, never higher. Made, limited and refused as an ask, [[MoveError.Model]]
    * when the reply does not read in `shape` after its repair; and [[MoveError.Model]], no move
    * made, when `shape` writes no schema (a reversed range).
    */
  def extract(
      name: MoveName,
      from: Tainted[String],
      question: String,
      shape: Shape
  ): Either[MoveError, Option[Trusted[Extracted]]]

  /** `text`, a `source` (what kind of document it is, in a few words, such as `file`), through
    * the integrity tree ([[IntegrityTree.Node]]): raised from below `Outside` to it when the tree
    * finds nothing, otherwise as it came, with the tree's reasons. The tree's questions
    * ([[grit.core.sanitise.Suspicions.questions]]) are asked as a judgment ([[ask]]), counted
    * and refused as one, and only when their answer could raise `text`: when it is believed
    * below `Outside` and no flag fires ([[grit.core.sanitise.HardFlags]]); otherwise no move is
    * made. A classifier that is unavailable, uncleared or answers what does not read keeps
    * `text` as it came, its move made; any other refusal is the judgment's. Never fails on what
    * `text` holds. [[MoveError.Undeclared]] when the run's program did not declare the tree
    * ([[IntegrityTree.Node]]), and no move is made.
    */
  final def vet(
      name: MoveName,
      text: Tainted[String],
      source: String
  ): Either[MoveError, Vetted] =
    tree match {
      case None => Left(MoveError.Undeclared(IntegrityTree.Node.described.name))
      case Some(raise) =>
        val unasked = Vetted.apply.tupled(raise(text.map(raw => (raw, None))))
        unasked.verdict match {
          case Verdict.Kept(Vector(Reason.Unjudged)) =>
            ask(
              name,
              text.map(raw => Posed.judge(Suspicions.State(source, raw), Suspicions.questions))
            ) match {
              case Right(asked) =>
                val answered = text.zip(asked.reply).map((raw, s) => (raw, Some(s)))
                Right(Vetted.apply.tupled(raise(answered)))
              case Left(MoveError.Model(_) | MoveError.Uncleared(_)) => Right(unasked)
              case Left(error) => Left(error)
            }
          case _ => Right(unasked)
        }
    }

  /** The integrity tree, as the run's program declared it ([[grit.core.durable.Flow.program]]'s
    * `within`): `None` when it did not.
    */
  protected def tree: Option[IntegrityTree.Raising^]
}

/** [[Moves]], and the acting plugin's own documents and pieces of files. */
trait Keeping extends Moves {

  /** `body` over `from`'s value and its plugin's documents, `at` the instant the move is made,
    * in one transaction opened for the acting's subject and committed with the move's record:
    * so written at no less than its floor, and believed at no more than `from` is
    * ([[DocumentKeeper.believedAtMost]]). [[MoveError.Below]] when `from` is believed less than
    * [[grit.core.visibility.Integrity.Outside]]: `body` does not run. [[MoveError.Store]] when
    * the store fails, another run wrote one of the same keys meanwhile, or `body` returns
    * `Left`: nothing it wrote is kept. Either way the move counts as made.
    */
  def keep[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
      body: (I, DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A]

  /** What `from` asks, done to the acting plugin's pieces of files at the run's floor, in one
    * transaction for the acting's subject, believed at no more than `from` is. For
    * [[Piecing.Read]]: the file admitted as the acting's subject reads it
    * ([[grit.core.files.Files.admit]]), then opened and read by the area's formats holding no
    * transaction, cut by [[grit.core.files.Pieces.of]] and kept ([[grit.core.files.PieceKeeper.keep]]); or,
    * gone or read by no format, [[Pieced.Unread]]. The transaction is not the move's record's,
    * so a rerun cut short by a crash keeps the same pieces again. [[MoveError.Below]] when `from`
    * is believed less than [[grit.core.visibility.Integrity.Outside]]: nothing is read or
    * written. [[MoveError.Store]] when the store fails: nothing is written. Either way the move
    * counts as made.
    */
  def piece(name: MoveName, from: Tainted[Piecing]): Either[MoveError, Pieced]
}

/** [[Keeping]], the strands of the rooms its run reads, by ids and counts, and its plugin's
  * needs: what a [[grit.core.job.DreamingJob]]'s run is handed. Their messages' text, and a
  * document's text it is to read, reach a model only through [[summarise]] and
  * [[declassify]], read in their own steps, and never the job's code, which is handed only the
  * links in them
  * ([[grit.core.stitch.StrandShelf.links]]).
  */
trait Dreaming extends Keeping {

  /** As [[keep]], `body` also reading `strands` and the services of its plugin's `needs`
    * ([[grit.core.plugin.Plugin.needs]]), each over its plugin's documents, in the keep's
    * transaction. What it returns is believed as `from` is, whatever it read: a shelf giving
    * ids and counts alone ([[StrandShelf]], a need's text-free service) keeps every text a
    * model is to read on [[summarise]]'s route, where its integrity is met.
    */
  def scan[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
      body: (I, DocumentKeeper, StrandShelf, Needs, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A]

  /** As [[ask]] of a [[Posed.Json]] of `summary`'s system text ([[Dreaming.told]]), then one
    * user message ([[Dreaming.shown]]): what
    * each of `seen`'s members said before its `asOf`, as [[grit.core.stitch.Stitching.excerpt]]
    * shows them within `summary`'s `chars`, strand by strand; then the text of each of `shown`,
    * its head within `summary`'s `documentChars` cut at a word, in their order, each under its
    * plugin's label; read in this move's own step, so only the ask's record is kept. Asked of
    * the catalog's `dream` role ([[grit.core.model.Policy]]). Its reply is believed at the meet
    * of `summary`, every message read and every document shown: a guest's message or a
    * document believed less caps it. One of `shown` not read (no longer kept, above the
    * transaction's clearance, or neither the acting plugin's own nor one of its needs') is
    * left out, and [[Dreaming.Summarised.shown]] lists those that were shown. `Right(None)`,
    * asking nothing, when a strand's messages are not the ones `seen` digested (one was
    * removed or said since its scan). The move counts as made either way.
    */
  def summarise[T](
      name: MoveName,
      seen: Vector[Strand.Digested],
      shown: Vector[DocumentVersionId],
      summary: Tainted[Dreaming.Summary[T]]
  ): Either[MoveError, Option[Dreaming.Summarised[T]]]

  /** `source`, a document of the acting plugin or one of its needs, rewritten under `applied`
    * from the text of its file at `revision` (as its plugin's pieces of it name it,
    * [[grit.core.files.PiecedFile.revision]]), and screened. It is asked as a [[Posed.Text]] of
    * [[Declassifying.request]], the policy's text and the file's alone, read in this move's own
    * step (`ask:{name}`), of the catalog's `dream` role; then the rewrite alone is screened by
    * [[Declassifying.screen]] of the policy's checks, read again in its own step
    * (`ask:{name}-check`); when a floor answer fires ([[Screened.floorFires]]), the source's
    * text by [[Declassifying.Baseline]], once for both attempts, in its own (`ask:{name}-source`);
    * then lowered by the declassification the run's program declared ([[Declassifying.Node]])
    * when nothing fired against the source's floor ([[Screened.fired]]). When only floor
    * questions or present signals fired ([[Declassifying.Again]]), the rewrite is asked once more
    * (`ask:{name}-retry`, as [[Declassifying.request]] of the draft and what fired) and screened
    * again (`ask:{name}-retry-check`). [[Declassifying.Outcome]]: `Made`, of the attempt that
    * passed; `Kept` when an absent signal fired, or anything fired on the retry, or the model
    * wrote [[Declassifying.Nothing]] (no check asked);
    * `Unapplied`, asking nothing more, when any step reads nothing, with why
    * ([[Declassifying.Unread]]: `Unchecked`, before the rewrite, when its policy version has no
    * passing bench of its checks as they are now, [[Declassifying.onBench]]). Each ask is
    * counted, limited and refused as an ask or a judgment is ([[ask]]), and a screen refused is
    * that refusal, never a declassification. A rewrite stopped at its length limit, or blank, is
    * neither a rewrite nor a withholding: [[MoveError.Model]] saying which
    * ([[Declassifying.written]]), no check asked. [[MoveError.Undeclared]], no move made, when
    * the program did not declare the declassification.
    */
  final def declassify(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication
  ): Either[MoveError, Declassifying.Outcome] =
    declassifier match {
      case None => Left(MoveError.Undeclared(Declassified.declassification.described.name))
      case Some(raise) =>
        // One attempt: its rewrite (`again` for the retry), its screen, and the source's floor
        // when its floor fires and `known`, the source's floor as an earlier attempt read it,
        // is none.
        def attempt(
            at: Declassified.Attempt,
            again: Option[Declassifying.Again],
            known: Option[Screen.Baseline]
        ): Either[MoveError, Declassifying.Outcome] = {
          val asked = again match {
            case None => Right(name)
            case Some(_) => Declassifying.retried(name).left.map(MoveError.Model(_))
          }
          asked.flatMap(rewrite(_, source, revision, applied, again)).flatMap { rewritten =>
            Trusted.asIs(rewritten).extract match {
              case Declassifying.Rewritten.Unapplied(why) =>
                Right(Declassifying.Outcome.Unapplied(why))
              case Declassifying.Rewritten.Withheld => Right(Declassifying.Outcome.Kept(None))
              case Declassifying.Rewritten.Made(text, from, by) =>
                screen(name, source, revision, applied, text, at).flatMap {
                  case Left(why) => Right(Declassifying.Outcome.Unapplied(why))
                  case Right(screened) =>
                    val against
                        : Either[MoveError, Either[Declassifying.Unread, Option[Screen.Baseline]]] =
                      if (!screened.floorFires || known.nonEmpty) Right(Right(known))
                      else baseline(name, source, revision, applied).map(_.map(Some(_)))
                    against.flatMap {
                      case Left(why) => Right(Declassifying.Outcome.Unapplied(why))
                      case Right(base) =>
                        val fired = screened.fired(base)
                        raise(
                          (
                            rewrite = text,
                            source = from,
                            applied = applied,
                            screened = screened,
                            baseline = base,
                            attempt = at
                          )
                        ) match {
                          case Some(d) => Right(Declassifying.Outcome.Made(rewritten.map(_ => d)))
                          // The rewrite's source is no longer the application's document.
                          case None if fired.passes =>
                            Right(Declassifying.Outcome.Unapplied(Declassifying.Unread.Lapsed))
                          case None =>
                            val retry = at match {
                              case Declassified.Attempt.First =>
                                Declassifying.Again.of(text, by, fired)
                              case Declassified.Attempt.Second => None
                            }
                            retry.fold(
                              Right(Declassifying.Outcome.Kept(Some(screened.judged(base))))
                            )(a => attempt(Declassified.Attempt.Second, Some(a), base))
                        }
                    }
                }
            }
          }
        }
        attempt(Declassified.Attempt.First, None, None)
    }

  /** `policy`'s examples benched ([[grit.core.screen.Bench]]): `ask:{name}-{i}` for each `i`
    * from 0, each reading the version's checks again in its own step and screening its
    * example `i` by their signals alone, until one finds none past the last; then, every ask
    * answered, the verdict kept as the version's bench (`keep:{name}`,
    * [[DocumentKeeper.keepBenched]]) under the screening every ask read
    * ([[Declassifying.screening]]), and `Kept`. `Unbenched`, keeping nothing: `Unreadable`
    * when its checks do not read, `Lapsed` when they are not read here or changed between its
    * asks. Each ask is counted, limited and refused as a judgment is ([[ask]]): a refused or
    * failed ask is that refusal, keeping nothing, as a keep that fails is
    * [[MoveError.Store]]; the version is benched again next time.
    */
  final def bench(
      name: MoveName,
      policy: DeclassificationPolicy
  ): Either[MoveError, Declassifying.Benching] = {
    // Its asks from `at`, each read, until one past the last: every one asked, or why not.
    def asks(
        at: Int,
        done: Vector[Tainted[Declassifying.Examined]]
    ): Either[MoveError, Either[Declassifying.Unbenched, Vector[Tainted[Declassifying.Examined]]]] =
      Declassifying
        .examined(name, at)
        .left
        .map(MoveError.Model(_))
        .flatMap(example(_, policy, at))
        .flatMap {
          case Left(why) => Right(Left(why))
          case Right(asked) =>
            if (Trusted.asIs(asked).extract.example.isEmpty) Right(Right(done :+ asked))
            else asks(at + 1, done :+ asked)
        }
    asks(0, Vector.empty).flatMap {
      case Left(why) => Right(Declassifying.Benching.Unbenched(why))
      case Right(asked) =>
        val read = asked.map(Trusted.asIs(_).extract)
        (asked.headOption, read.map(_.screening).distinct) match {
          case (Some(first), Vector(screening)) =>
            val benched = Bench.verdict(read.flatMap(_.example.map(e => (e.kind, e.screened))))
            val row = asked
              .foldLeft(first.map(_ => ()))((all, a) => all.zip(a).map(_ => ()))
              .map(_ => new Declassified.BenchRow(policy, screening, benched))
            import Declassifying.benchedRecord
            keep(name, row)((r, keeper, at) => keeper.keepBenched(r, at).map(_ => r.benched))
              .map(Declassifying.Benching.Kept(_))
          // Its checks changed between its asks: what it asked is of no one screening.
          case _ => Right(Declassifying.Benching.Unbenched(Declassifying.Unbenched.Lapsed))
        }
    }
  }

  /** The declassification, as the run's program declared it ([[Declassifying.raise]]): `None`
    * when it did not.
    */
  protected def declassifier: Option[Declassifying.Raising^]

  /** [[declassify]]'s rewrite `name`, its first, or with `again` its retry
    * ([[Declassifying.request]]): what it read and the model wrote, believed at the meet of the
    * run, its source and the reply; [[Declassifying.Rewritten.Unapplied]] when it read nothing
    * and asked nothing.
    */
  protected def rewrite(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication,
      again: Option[Declassifying.Again]
  ): Either[MoveError, Tainted[Declassifying.Rewritten]]

  /** [[declassify]]'s screen of `rewrite`, its `attempt` ([[Declassifying.checked]]), read again
    * as its rewrite's was: its answers; why not, when it read nothing and asked nothing.
    */
  protected def screen(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication,
      rewrite: DocText,
      attempt: Declassified.Attempt
  ): Either[MoveError, Either[Declassifying.Unread, Screened]]

  /** [[declassify]]'s screen of its source's text as the rewrite was shown it
    * ([[Declassifying.sourced]]), the floor alone, read again as its rewrite's was: the
    * [[Screen.Baseline]] a rewrite's floor answers are excused against; why not, when it read
    * nothing and asked nothing.
    */
  protected def baseline(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication
  ): Either[MoveError, Either[Declassifying.Unread, Screen.Baseline]]

  /** [[bench]]'s ask `name`: `policy`'s example `at` as [[Declassifying.exhibit]] reads it in
    * its own step, screened, believed at the run's integrity; why not, when it read nothing and
    * asked nothing.
    */
  protected def example(
      name: MoveName,
      policy: DeclassificationPolicy,
      at: Int
  ): Either[MoveError, Either[Declassifying.Unbenched, Tainted[Declassifying.Examined]]]
}

object Dreaming {

  /** What a dreaming run reads: its scans, `strands` and, given its plugin, that plugin's
    * `needs`; its summaries, their strands' messages through `excerpts` and their documents
    * through `documents`; `pieces`, the kept files' pieces its declassifies read.
    */
  final case class Reads(
      strands: StrandShelf,
      needs: PluginName -> Needs,
      excerpts: StrandExcerpts,
      documents: DocumentSearch,
      pieces: PieceSearch
  )

  /** A summary's reply, and the versions of `shown` it was shown, in their order. */
  final case class Summarised[T](asked: Asked[T], shown: Vector[DocumentVersionId])

  /** A summary's ask: `system`; the most characters of each strand shown (`chars`) and of each
    * document (`documentChars`); and the reply it reads as.
    */
  final case class Summary[T](system: String, chars: Int, documentChars: Int, reply: Typed[T])

  /** The user message a summary is asked in: each of `excerpts` fenced in `<discussion>`, then
    * each of `documents` in `<document label="{label}">`, its text's head within
    * `documentChars` ([[grit.core.message.Cut.head]], cut before it is fenced), a blank line
    * between each ([[Fenced]]).
    */
  def shown(
      excerpts: Vector[String],
      documents: Vector[(DocLabel, DocText)],
      documentChars: Int
  ): String =
    (excerpts.map(Fenced.in(Tag.Discussion, _)) ++ documents.map((label, text) =>
      Fenced.in(
        Tag.Document,
        Cut.head(DocText.value(text), documentChars),
        "label" -> DocLabel.value(label)
      )
    )).map(_.rendered).mkString("\n\n")

  /** A summary's system text as it is asked, `system` then a blank line and [[Fenced.Rule]]:
    * what the material [[shown]] fences is.
    */
  def told(system: String): String = system + "\n\n" + Fenced.Rule
}
