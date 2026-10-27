package grit.kit.deployment

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{Keeping, Moves}
import grit.core.document.{DocLabel, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.flow.Trusted
import grit.core.id.{CallSlot, JobName, PluginName}
import grit.core.identity.Principal
import grit.core.job.{
  Declared,
  DreamingJob,
  Job,
  JobRun,
  KeepingJob,
  NotOwn,
  OwnJobs,
  PlainJob,
  ScheduleDesk
}
import grit.core.need.{Needs, PluginReads, Unneeded}
import grit.core.plugin.{
  Answered,
  DocumentPosting,
  Documents,
  Exports,
  KeepingCommand,
  PluginCommand,
  PluginRun,
  PluginTool,
  ReadingCommand
}
import grit.core.store.{Reads, StoreError, Tx}
import grit.core.tool.{Args, Believed, Field, Gate, Hosted, Outcome, ToolName, ToolSpec}
import grit.core.visibility.Compartment

/** Plugins for tests of how a deployment takes them: one that exports its documents' keys, and
  * tools that read through a service or their own documents.
  */
object TestPlugins {

  def name(s: String): PluginName = PluginName.of(s).fold(e => sys.error(e), identity)

  /** What [[Keys]] exports: the keys of its documents. */
  trait KeyList extends caps.Pure {
    def keys()(using Tx^): Either[StoreError, Vector[String]]
  }

  /** A tool named `called` whose call answers the keys `list` gives, joined by commas. */
  final class Listing(called: ToolName, list: (PluginReads, Needs) -> Either[Unneeded, KeyList])
      extends PluginTool[Int] {
    val described: Hosted[Int] =
      new Hosted(
        ToolSpec(called, "Lists keys.", Args.of((n = Field.count("How many.", 1, 9))).map(_.n)),
        Gate.Free,
        n => n.toString,
        Believed.Default
      )
    def bind(
        own: PluginReads,
        needs: Needs,
        jobs: OwnJobs
    ): Either[Unneeded | NotOwn, PluginRun[Int]] =
      list(own, needs).map { l =>
        new PluginRun[Int] {
          def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome =
            reads
              .read(l.keys())
              .fold(e => Outcome.Failed(e.toString), k => Outcome.Done(k.mkString(",")))
        }
      }
  }

  private def ownKeys(own: PluginReads): KeyList = new KeyList {
    def keys()(using Tx^): Either[StoreError, Vector[String]] =
      own.cache.newest("", 100).map(_.map(_._1))
  }

  /** Exports its own documents' keys, and offers them as the tool `called`. */
  final class Keys(val name: PluginName, called: ToolName = ToolName("keys"))
      extends Exports[KeyList] {
    val version: Int = 1
    def service(own: PluginReads): KeyList = ownKeys(own)
    override val tools: Vector[PluginTool[?]] =
      Vector(new Listing(called, (own, _) => Right(ownKeys(own))))
  }

  /** Needs `keys` when `declared`, and offers `called`, which lists `keys`' keys through it. */
  final class Reading(
      val name: PluginName,
      keys: Keys,
      declared: Boolean,
      called: ToolName = ToolName("theirs")
  ) extends grit.core.plugin.Plugin {
    val version: Int = 1
    override val needs: Vector[Exports[?]] = if (declared) Vector(keys) else Vector.empty
    override val tools: Vector[PluginTool[?]] =
      Vector(new Listing(called, (_, needs) => needs.of(keys)))
  }

  /** A job named `called`, with no parameters, replying its name. */
  final class Named(called: String) extends PlainJob[Named.None.type] {
    val name: JobName = JobName.of(called).fold(e => sys.error(e), identity)
    val version: Int = 1
    def write(params: Named.None.type): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, Named.None.type] = Right(Named.None)
    def run(run: Trusted[JobRun[Named.None.type]], moves: Moves^): String = called
  }

  object Named {
    case object None extends caps.Pure
  }

  /** A keeping job named `called`, with no parameters, replying its name and keeping nothing. */
  final class Tallying(called: String) extends KeepingJob[Named.None.type] {
    val name: JobName = JobName.of(called).fold(e => sys.error(e), identity)
    val version: Int = 1
    def write(params: Named.None.type): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, Named.None.type] = Right(Named.None)
    def run(run: Trusted[JobRun[Named.None.type]], moves: Keeping^): String = called
  }

  /** A dreaming job named `called`, with no parameters, replying its name and keeping nothing. */
  final class Dreaming(called: String) extends DreamingJob[Named.None.type] {
    val name: JobName = JobName.of(called).fold(e => sys.error(e), identity)
    val version: Int = 1
    def write(params: Named.None.type): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, Named.None.type] = Right(Named.None)
    def run(run: Trusted[JobRun[Named.None.type]], moves: grit.core.act.Dreaming^): String = called
  }

  /** Documents labelled "tallies", which nothing posts. */
  object Tallies extends Documents {
    val terms: DocumentTerms =
      DocLabel
        .of("tallies")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 10))
        .fold(e => sys.error(e), identity)
    val posting: Option[DocumentPosting] = None
  }

  /** Offers `called`, which books `job` when bound; its plugin's jobs are `jobs`. */
  final class Booking(
      val name: PluginName,
      job: Named,
      override val jobs: Vector[Job[?]],
      called: ToolName = ToolName("book")
  ) extends grit.core.plugin.Plugin {
    val version: Int = 1
    override val tools: Vector[PluginTool[?]] = Vector(new PluginTool[Int] {
      val described: Hosted[Int] =
        new Hosted(
          ToolSpec(called, "Books.", Args.of((n = Field.count("How many.", 1, 9))).map(_.n)),
          Gate.Free,
          n => n.toString,
          Believed.Default
        )
      def bind(
          own: PluginReads,
          needs: Needs,
          jobs: OwnJobs
      ): Either[Unneeded | NotOwn, PluginRun[Int]] =
        jobs
          .of(job)
          .map(_ =>
            new PluginRun[Int] {
              def run(n: Int, call: CallSlot, reads: Reads^, desk: ScheduleDesk^): Outcome =
                Outcome.Done("booked")
            }
          )
    })
  }

  /** Names `compartments`, and contributes nothing. */
  final class Naming(val name: PluginName, override val compartments: Vector[Compartment])
      extends grit.core.plugin.Plugin {
    val version: Int = 1
  }

  /** What a test command reads: one word. */
  final case class Word(word: String) extends caps.Pure

  private def saying(called: ToolName): ToolSpec[Word] =
    ToolSpec(
      called,
      "Says a word.",
      Args.of((word = Field.text("The word."))).map(a => Word(a.word))
    )

  /** A reading command named `called`, answering the word it is given. */
  final class Says(called: ToolName) extends ReadingCommand[Word] {
    val spec: ToolSpec[Word] = saying(called)
    def run(args: Word, by: Principal.Person, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = Right(Answered.Done(args.word))
  }

  /** A keeping command named `called`, answering the word it is given and keeping nothing. */
  final class Notes(called: ToolName) extends KeepingCommand[Word] {
    val spec: ToolSpec[Word] = saying(called)
    def run(args: Word, by: Principal.Person, own: DocumentKeeper, at: Instant)(using
        Tx^
    ): Either[StoreError, Answered] = Right(Answered.Done(args.word))
  }

  /** Its commands are `commands`, beside `tools` and `documents`. */
  final class Commanding(
      val name: PluginName,
      override val commands: Vector[PluginCommand[?]],
      override val tools: Vector[PluginTool[?]] = Vector.empty,
      override val documents: Option[Documents] = None
  ) extends grit.core.plugin.Plugin {
    val version: Int = 1
  }

  /** Its jobs are `jobs`, and it declares `schedules`. */
  final class Declaring(
      val name: PluginName,
      override val jobs: Vector[Job[?]],
      override val schedules: Vector[Declared[?]],
      override val documents: Option[Documents] = None
  ) extends grit.core.plugin.Plugin {
    val version: Int = 1
  }
}
