package grit.kit.deployment

import grit.core.act.Moves
import grit.core.id.{CallSlot, JobName, PluginName}
import grit.core.job.{Declared, Job, JobRun, NotOwn, OwnJobs, PlainJob, ScheduleDesk}
import grit.core.plugin.{Exports, Needs, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.{Reads, StoreError, Tx}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, ToolName, ToolSpec}
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
        n => n.toString
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
    def run(run: JobRun[Named.None.type], moves: Moves^): String = called
  }

  object Named {
    case object None extends caps.Pure
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
          n => n.toString
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

  /** Its jobs are `jobs`, and it declares `schedules`. */
  final class Declaring(
      val name: PluginName,
      override val jobs: Vector[Job[?]],
      override val schedules: Vector[Declared[?]]
  ) extends grit.core.plugin.Plugin {
    val version: Int = 1
  }
}
