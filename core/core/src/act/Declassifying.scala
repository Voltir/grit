package grit.core.act

import grit.core.classify.Ask
import grit.core.document.{Declassified, DocText, DocumentKeeper, DocumentSearch, PolicyApplication}
import grit.core.durable.{Flow, Journaled, Steps}
import grit.core.either.Traversal
import grit.core.files.{PieceSearch, Pieces}
import grit.core.flow.{Raise, Tainted}
import grit.core.id.{DocumentVersionId, PluginName, Sha256}
import grit.core.message.{AssistantBlock, Cut, Message, StopReason}
import grit.core.period.Probability
import grit.core.prompt.{Fenced, Tag}
import grit.core.provider.ModelRequest
import grit.core.screen.{Bench, Benched, Checks, Fired, Screen, Screened, Signal}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{DeclassificationPolicy, Integrity}

/** What [[Dreaming.declassify]] asks and comes to: the rewrite's text for the model, the check's
  * for the classifier, and the declassification it is lowered by.
  */
object Declassifying {

  /** The rewrite's system prompt, the first part of [[request]]'s: the writer's role, how the
    * fenced `<document>` in the user message is to be read, and what to reply. It names no
    * compartment and no declassification: the model reads only the policy's instructions (which
    * [[request]] appends, fenced) and the group a document was written for.
    */
  val Rewrite: String =
    """You prepare documents written for one group so that a wider audience can read them. The people responsible for the material gave you the instructions below: what must be removed before it is shared, and what to write in its place. Follow them. What you write will be read as an ordinary document by people who must not learn what was removed.
      |
      |The document is in the user's message, inside <document> tags.
      |
      |Write only the rewritten document, without tags, as plain statements of fact a colleague would read: no preamble, no instructions to the reader, and no mention of the instructions, of rewriting, or of anything left out. Where the instructions tell you what to say about a person or a thing, write it as a fact about them, never as a direction to anyone.
      |
      |Remove everything the instructions say must be removed, including anything about what kind of material this is or how it came about when they say so, such as that it is a joke, a test, a game or a draft. Do not hint at what you removed.
      |
      |Keep the rest of the document as it is.
      |
      |If nothing of the document may be shared under the instructions, reply with exactly NOTHING.""".stripMargin

  /** 8,000: the most characters of its source the rewrite is shown, cut at a word
    * ([[grit.core.message.Cut.head]]).
    */
  val SourceChars: Int = 8000

  /** What the model replies when nothing of its source may be shared: then no check is asked. */
  val Nothing: String = "NOTHING"

  /** The name of the check of the declassifying move `name`'s `attempt`: `{name}-check`, or
    * `{name}-retry-check` for the second; or why not, when that is over [[MoveName]]'s length.
    */
  def checked(name: MoveName, attempt: Declassified.Attempt): Either[String, MoveName] =
    attempt match {
      case Declassified.Attempt.First => MoveName.of(s"${MoveName.value(name)}-check")
      case Declassified.Attempt.Second => MoveName.of(s"${MoveName.value(name)}-retry-check")
    }

  /** The name of the screen of the declassifying move `name`'s source: `{name}-source`; or why
    * not, when that is over [[MoveName]]'s length.
    */
  def sourced(name: MoveName): Either[String, MoveName] =
    MoveName.of(s"${MoveName.value(name)}-source")

  /** The name of the second rewrite of the declassifying move `name`: `{name}-retry`; or why
    * not, when that is over [[MoveName]]'s length.
    */
  def retried(name: MoveName): Either[String, MoveName] =
    MoveName.of(s"${MoveName.value(name)}-retry")

  /** The name of the bench `name`'s ask of its example `at`: `{name}-{at}`; or why not, when
    * that is over [[MoveName]]'s length.
    */
  def examined(name: MoveName, at: Int): Either[String, MoveName] =
    MoveName.of(s"${MoveName.value(name)}-$at")

  /** A second attempt: the first's `draft`, the model that wrote it, `by`, and what its screen
    * `fired`: floor and present only.
    */
  final case class Again private[act] (draft: DocText, by: String, fired: Fired) extends caps.Pure

  object Again {

    /** A retry of `draft`, which `by` wrote, when `fired` holds floor or present failures and no
      * absent one.
      */
    def of(draft: DocText, by: String, fired: Fired): Option[Again] =
      if (fired.passes || fired.absent.nonEmpty) None else Some(Again(draft, by, fired))
  }

  /** 64: the most pieces of its file a rewrite reads ([[grit.core.files.PieceSearch.file]]),
    * well past [[SourceChars]] for pieces near [[grit.core.files.Pieces.Target]].
    */
  val SourcePieces: Int = 64

  /** The rewrite's request. Its system text is [[Rewrite]], then `shown.instructions` fenced in
    * `<instructions>` tags, which the policy's people wrote and the model must follow, then
    * [[Fenced.Rule]] ([[Fenced.told]]). Its one
    * user message is `shown.text`, fenced in `<document>` tags, which is material only. Each
    * closing tag of its fence inside either text, in any case, is escaped ([[Fenced]]), so
    * neither can end its own fence. For
    * `again`, a retry, its draft follows as the model's own turn, then one user message, the
    * [[feedback]] naming what fired of [[screen]] of `shown.checks`, never a score.
    */
  def request(shown: Shown, again: Option[Again]): ModelRequest = {
    val first = framed(shown.instructions, shown.text)
    again.fold(first) { a =>
      val draft = Message.said(DocText.value(a.draft), a.by)
      val told = Message.User(feedback(screen(shown.checks).failed(a.fired)))
      first.copy(messages = first.messages ++ Vector(draft, told))
    }
  }

  /** "Your rewrite was not accepted: it {each, '; and it '}. Write the document again. The check
    * is an imperfect test of the real goal. Don't reword things just to get past it: write so
    * that a reader genuinely could not infer these things.", each of `failed` in order, a floor
    * question by its `named`, a signal as "does not state {infer}".
    */
  def feedback(failed: Vector[Either[Screen.Fixed, Signal]]): String =
    "Your rewrite was not accepted: it " +
      failed.map(_.fold(_.named, s => s"does not state ${s.infer}")).mkString("; and it ") +
      ". Write the document again. The check is an imperfect test of the real goal. Don't " +
      "reword things just to get past it: write so that a reader genuinely could not infer " +
      "these things."

  /** [[request]]'s framing of `instructions` and of `source`, which is already cut: the system
    * text and the user message, each fenced.
    */
  private def framed(instructions: String, source: String): ModelRequest =
    ModelRequest(
      Fenced.told(Rewrite + "\n\n" + Fenced.in(Tag.Instructions, instructions).rendered),
      Vector(Message.User(Fenced.in(Tag.Document, source).rendered))
    )

  /** The text [[request]] adds to its system text and user messages, as a model is shown it: the
    * system text, then each user message's text, framed around placeholders that each name a
    * closing tag, so the fences and their escaping are all in it. [[MovesJournal]]'s rewrite key
    * hashes it, so a change to any of that changes every key.
    */
  val Template: Vector[String] = {
    val placeholder = framed("</instructions>", "</document>")
    placeholder.system +: placeholder.messages.collect { case Message.User(t) => t }
  }

  /** What `reply` rewrote, its text blocks joined and trimmed; `None` when that is [[Nothing]].
    * Why it is no rewrite, when it did not end as a whole reply ([[StopReason.EndTurn]]; one
    * stopped at its length limit is cut short), or is blank.
    */
  def written(reply: Message.Assistant): Either[String, Option[DocText]] = {
    val text = reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
    reply.stop match {
      case StopReason.MaxTokens => Left(CutShort)
      case StopReason.EndTurn if text == Nothing => Right(None)
      case StopReason.EndTurn => DocText.of(text).map(Some(_)).left.map(_ => Blank)
      case _ => Left(Unended)
    }
  }

  private val CutShort = "its rewrite stopped at its reply's length limit, so it was cut short"

  private val Blank = "its rewrite came back empty"

  private val Unended = "its rewrite did not end as a whole reply"

  /** The floor every declassification's screen asks first, about the rewrite as `text`:
    * whether it refers to itself as a rewrite or to its instructions (`meta`), then whether its
    * writer speaks of what it shares or leaves out (`first`); each `named` as a retry tells the
    * writer.
    */
  val Floor: Vector[Screen.Fixed] = Vector(Meta, Speaks)

  /** The source's [[Floor]] alone, asked about the text its rewrite was shown: what a rewrite's
    * floor answers are excused against.
    */
  val Baseline: Ask[Screen.Text, Screen.Baseline] = Screen.baseline(Meta, Vector(Speaks))

  private lazy val Meta = Screen.Fixed(
    "Does `text` refer to itself as a rewrite, version or edit of another document, or to " +
      "instructions it was given?",
    "refers to itself as a rewrite or its instructions"
  )

  private lazy val Speaks = Screen.Fixed(
    "Does `text` speak in the first person singular, as 'I', about what it shares, includes " +
      "or leaves out?",
    "speaks as its writer about what it shares or leaves out"
  )

  /** The screen a rewrite under `checks` is asked: a bench's ([[Bench.screen]]), [[Floor]]
    * first.
    */
  def screen(checks: Checks): Screen = Bench.screen(checks).copy(floor = Floor)

  /** The screening of `checks`, what a bench of them is kept under: a digest of their canonical
    * form ([[Checks.canonical]]) with [[Floor]]'s questions, the signals' two templates
    * ([[Screen.absent]], [[Screen.present]]) and the two thresholds ([[Screen.Infers]],
    * [[Screen.States]]); not the retry's wording, which a bench never asks.
    */
  def screening(checks: Checks): Declassified.Screening =
    Declassified.Screening.of(
      Sha256.text(
        ujson.write(
          ujson.Obj(
            "checks" -> ujson.read(checks.canonical),
            "floor" -> ujson.Arr.from(Floor.map(f => ujson.Str(f.question))),
            "absent" -> Screen.absent("{infer}"),
            "present" -> Screen.present("{infer}"),
            "infers" -> Probability.value(Screen.Infers),
            "states" -> Probability.value(Screen.States)
          )
        )
      )
    )

  /** Where a policy version stands on its bench, as one transaction reads it. */
  enum OnBench extends caps.Pure {

    /** Its checks are not read here: not cleared for its label, no such version, or a
      * revocation.
      */
    case Unread

    /** No bench is kept under the [[screening]] of its checks as they are now: none is, its
      * checks changed since, or they do not read.
      */
    case Stale

    /** Its bench under its current screening, passing or not. */
    case Kept(benched: Benched)
  }

  /** `policy` on its bench: the one rule [[read]] refuses under (anything but a passing `Kept`
    * is [[Unread.Unchecked]]) and [[unbenched]] lists by.
    */
  def onBench(keeper: DocumentKeeper, policy: DeclassificationPolicy)(using
      Tx^
  ): Either[StoreError, OnBench] =
    keeper.checks(policy).flatMap {
      case None => Right(OnBench.Unread)
      case Some(read) =>
        read.checks match {
          case Left(_) => Right(OnBench.Stale)
          case Right(checks) =>
            keeper.benched(policy).map {
              case Some((at, benched)) if at == screening(checks) => OnBench.Kept(benched)
              case _ => OnBench.Stale
            }
        }
    }

  /** Whether `on` lets a version be rewritten under: a passing bench under its current checks. */
  private def passed(on: OnBench): Boolean = on match {
    case OnBench.Kept(benched) => benched.passed
    case OnBench.Unread | OnBench.Stale => false
  }

  /** The versions of `over` due a bench this run: each of [[DocumentKeeper.benchable]]'s that
    * is not `Kept` [[onBench]].
    */
  def unbenched(keeper: DocumentKeeper, over: DeclassificationPolicy.Over)(using
      Tx^
  ): Either[StoreError, Vector[DeclassificationPolicy]] =
    keeper
      .benchable(over)
      .flatMap(versions =>
        Traversal
          .traverse(versions)(p => onBench(keeper, p).map(on => (p, on)))
          .map(_.collect { case (p, OnBench.Unread | OnBench.Stale) =>
            p
          })
      )

  /** What a screening step screens, read in its own step by `reading`: a rewrite's `attempt`,
    * `text`, by [[screen]]; or the source's text as the rewrite was shown it, by [[Floor]] alone
    * (its [[Screen.Baseline]]).
    */
  enum Of {
    case Rewrite(reading: Reading, text: DocText, attempt: Declassified.Attempt)
    case Source(reading: Reading)

    /** A bench's example `at` of `policy`'s checks, as [[exhibit]] reads it through `keeper`. */
    case Example(keeper: DocumentKeeper, policy: DeclassificationPolicy, at: Int)
  }

  /** Why a bench's ask screened nothing. */
  enum Unbenched extends caps.Pure {

    /** Its checks do not read: `why`, by position. */
    case Unreadable(why: Checks.Unreadable)

    /** Its checks are not read here (not cleared for its label, no such version, or a
      * revocation), or changed between the bench's asks.
      */
    case Lapsed
  }

  /** What a bench came to: `Kept`, its verdict kept as its version's bench; or `Unbenched`,
    * keeping none.
    */
  enum Benching extends caps.Pure {
    case Kept(benched: Benched)
    case Unbenched(why: Declassifying.Unbenched)
  }

  /** A bench's keep's record: `{"passed", "examples"}`, its examples as [[Benched.write]]
    * writes them, read back by [[Benched.read]].
    */
  given benchedRecord: Journaled[Benched] = Journaled.json(
    b => ujson.Obj("passed" -> b.passed, "examples" -> Benched.write(b)),
    v =>
      for {
        o <- v.objOpt.toRight("bench: expected an object")
        passed <- o.get("passed").flatMap(_.boolOpt).toRight("bench: no passed")
        examples <- o.get("examples").toRight("bench: no examples")
        benched <- Benched.read(passed, examples)
      } yield benched
  )

  /** A bench's example as its ask reads it, of the checks under `screening`: its `kind`, its
    * `text` and the `screen` it is asked by (the checks' signals alone, [[Bench.screen]]); or
    * `None` past the last.
    */
  final case class Exhibit(
      screening: Declassified.Screening,
      example: Option[(kind: Benched.Kind, text: Screen.Text, screen: Screen)]
  ) extends caps.Pure

  /** What a bench's ask came to, of the checks under `screening`: its example's `kind` and its
    * answers, `screened`; or `None` past the last.
    */
  final case class Examined(
      screening: Declassified.Screening,
      example: Option[(kind: Benched.Kind, screened: Screened)]
  ) extends caps.Pure

  /** `policy`'s example `at` (accept then refuse, from 0), as one transaction reads its checks
    * through `keeper`; or why not.
    */
  def exhibit(keeper: DocumentKeeper, policy: DeclassificationPolicy, at: Int)(using
      Tx^
  ): Either[StoreError, Either[Unbenched, Exhibit]] =
    keeper.checks(policy).map {
      case None => Left(Unbenched.Lapsed)
      case Some(read) =>
        read.checks match {
          case Left(why) => Left(Unbenched.Unreadable(why))
          case Right(checks) =>
            Right(
              Exhibit(
                screening(checks),
                Bench
                  .example(checks, at)
                  .map(e => (kind = e.kind, text = e.text, screen = Bench.screen(checks)))
              )
            )
        }
    }

  /** What a rewrite of `source`, its file at `revision`, under `applied` reads, through
    * `documents`, `pieces` and `keeper`, as one of `readable`'s plugins.
    */
  final case class Reading(
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication,
      documents: DocumentSearch,
      pieces: PieceSearch,
      keeper: DocumentKeeper,
      readable: Vector[PluginName]
  )

  /** Why a rewrite read nothing to rewrite, and `why` in words. */
  enum Unread(val why: String) extends caps.Pure {

    /** Its source is not kept, holds nothing, is not read, is not one of `readable`'s or has no
      * pointer; or `applied` is not the application in force for it, or its policy's text and
      * checks are not read ([[DocumentKeeper.declassifying]]).
      */
    case Lapsed extends Unread("its source, or the application in force for it, changed")

    /** Its policy version in force has no passing bench of its checks as they are now
      * ([[onBench]]), as when they do not read: nothing is rewritten under
      * it. Which fault or example, the bench says.
      */
    case Unchecked extends Unread("its policy version in force is not benched, or failed its bench")

    /** Its source's plugin keeps no pieces of its file at `revision` at its label. */
    case Unpieced extends Unread("its source's file has no pieces at the revision it names")
  }

  /** What a rewrite is made from, as one transaction reads it: its source, the text of its file
    * at `reading.revision` ([[grit.core.files.Pieces.text]] of at most [[SourcePieces]] pieces,
    * cut at [[SourceChars]] at a word, [[grit.core.message.Cut.head]]) and its policy's text and
    * checks; or why not.
    */
  def read(reading: Reading)(using Tx^): Either[StoreError, Either[Unread, Shown]] =
    reading.documents.read(Vector(reading.source)).flatMap { found =>
      found
        .find(d => reading.readable.contains(d.plugin))
        .flatMap(d => d.pointer.map((d, _))) match {
        case None => Right(Left(Unread.Lapsed))
        case Some((d, pointer)) =>
          reading.keeper.declassifying(d.label, pointer).flatMap { inForce =>
            inForce.filter(_.applied == reading.applied).flatMap(_.policy) match {
              case None => Right(Left(Unread.Lapsed))
              case Some((_, Left(_))) => Right(Left(Unread.Unchecked))
              case Some((instructions, Right(checks))) =>
                onBench(reading.keeper, reading.applied.policy).flatMap { on =>
                  if (!passed(on)) Right(Left(Unread.Unchecked))
                  else
                    reading.pieces
                      .file(d.plugin, pointer, reading.revision, d.label, SourcePieces)
                      .map { pieces =>
                        if (pieces.isEmpty) Left(Unread.Unpieced)
                        else
                          Right(
                            Shown(
                              Declassified.Source(
                                d.version,
                                d.plugin,
                                d.key,
                                d.place,
                                d.label,
                                pointer,
                                reading.revision
                              ),
                              Cut.head(Pieces.text(pieces.map(_.piece)), SourceChars),
                              instructions,
                              checks,
                              pieces.foldLeft(d.integrity)((at, p) => at.meet(p.integrity))
                            )
                          )
                      }
                }
            }
          }
      }
    }

  /** A rewrite's `source`, its file's `text` as the model is shown it, its policy's
    * `instructions` and the `checks` it is screened by ([[screen]]), and the `integrity` the
    * text is believed at (its source's met with its pieces').
    */
  final case class Shown(
      source: Declassified.Source,
      text: String,
      instructions: String,
      checks: Checks,
      integrity: Integrity
  )

  /** What a rewrite's ask came to, as [[Dreaming]]'s primitive hands it back. */
  enum Rewritten extends caps.Pure {

    /** The model `by` (as its reply names it) wrote `text` from `from`. */
    case Made(text: DocText, from: Declassified.Source, by: String)

    /** The model wrote [[Nothing]]. */
    case Withheld

    /** No ask was made: [[read]] read nothing, `why`. */
    case Unapplied(why: Unread)
  }

  /** What a declassifying move came to. */
  enum Outcome {

    /** `rewrite`, declassified, believed at the meet of the run, its source and the reply. */
    case Made(rewrite: Tainted[Declassified])

    /** Nothing declassified: its screen failed ([[grit.core.screen.Screened.fired]]), `judged`
      * its [[grit.core.screen.Screened.judged]]; `None` when the model wrote [[Nothing]] and no
      * check was asked.
      */
    case Kept(judged: Option[Probability])

    /** No ask, or no check, was made: its source read nothing to rewrite, `why`. */
    case Unapplied(why: Unread)
  }

  /** The declassification, as a run's program declares it ([[grit.core.durable.Flow.program]]'s
    * `within`). Not the downgrade itself: what holds it cannot make it on a step.
    */
  val Node: Flow.Node = new Flow.Node {
    def shown: String = Declassified.declassification.shown
  }

  /** [[Node]] where it was handed. */
  type Raising = Raise[
    (
        rewrite: DocText,
        source: Declassified.Source,
        applied: PolicyApplication,
        screened: Screened,
        baseline: Option[Screen.Baseline],
        attempt: Declassified.Attempt
    ),
    Option[Declassified]
  ]

  /** The declassification, when `steps`' program declared [[Node]]; `None` otherwise. */
  def raise(using steps: Steps[?]^): Option[Raising^] =
    Declassified.declassification.raiseAs(Node)
}
