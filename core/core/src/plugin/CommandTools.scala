package grit.core.plugin

import java.time.Instant

import grit.core.claim.Claims
import grit.core.clock.Clock
import grit.core.document.{
  DocRef,
  Document,
  DocumentKeeper,
  DocumentSearch,
  DocumentShelf,
  DocumentTerms,
  DocumentVersion
}
import grit.core.id.{CallSlot, DocKey, PluginName}
import grit.core.identity.Principal
import grit.core.need.{PluginDocs, PluginReads}
import grit.core.store.{Askers, StoreError, Tx}
import grit.core.tool.{Believed, Gate, Hosted, Outcome, Tool}
import grit.core.visibility.{Integrity, Label}

/** A plugin's commands as a turn's tools: the very command a person gives through an edge. */
object CommandTools {

  /** `commanded` as a tool a turn is offered under its spec, run without asking, for the person
    * the turn answers ([[CommandBy.Turn]]), dated by `clock`, in the one transaction its turn
    * opens for each call. `Failed`, running nothing, for a turn that answers no person (a job's
    * run, grit's own, one whose conversation is gone). A reading command's `Done` is believed at
    * the least integrity of the documents it read (untrusted for any read of its plugin's
    * cache), as grit's own words when it read none. A keeping command requires
    * [[Integrity.Outside]]; what it keeps is believed at no more than its call's stamp met with
    * the cap of the room the turn's transaction is opened in, and kept only when it answers
    * `Done` ([[KeepingCommand.run]]); `Failed`, keeping nothing, when that transaction is in no
    * room. A refusal is `Failed` with its reason; a store call that fails, `Failed` saying so.
    */
  def of(
      commanded: Commanded,
      reads: PluginName -> PluginReads,
      keeper: (PluginName, DocumentTerms) -> DocumentKeeper,
      claims: PluginName -> Claims,
      askers: Askers,
      clock: Clock^
  ): Tool.Offered^{clock} = commanded match {
    case Commanded.Reading(plugin, command) =>
      val tool: Tool.Offered^{clock} = reading(command, reads(plugin), askers, clock)
      tool
    case Commanded.Keeping(plugin, terms, command) =>
      val tool: Tool.Offered^{clock} =
        keeping(command, keeper(plugin, terms), claims(plugin), askers, clock)
      tool
  }

  /** What a call is answered when its turn answers no person. */
  val NoPerson: String =
    "Nothing was done: this runs only for a person who asked, and no one did here."

  /** What a keeping call is answered when its turn's transaction is in no room. */
  val NoRoom: String = "Nothing was kept: this conversation has no room to keep it for."

  private def reading[A <: caps.Pure](
      command: ReadingCommand[A],
      own: PluginReads,
      askers: Askers,
      clock: Clock^
  ): Tool[A]^{clock} =
    new Hosted(command.spec, Gate.Free, command.shown, Believed.Own).reading { (args, at, reads) =>
      reads
        .read((tx: Tx^) ?=>
          person(askers, at).flatMap {
            case None => Right(Outcome.Failed(NoPerson))
            case Some(by) =>
              // The least integrity of what the command read: a scoped local of this one call.
              var least: Option[Integrity] = None
              val note: Integrity -> Unit =
                // Proof: `note` writes `least` alone, a local of this call. It is handed only to
                // `noted`, which only `command.run` below holds, and a command is pure, so it
                // keeps nothing past its run; `least` is read once, after the run returns, on
                // this thread.
                caps.unsafe.unsafeAssumePure(i => least = Some(least.fold(i)(_.meet(i))))
              val noted = Noted.reads(own, note)
              command.run(args, by, noted, clock.now()).map(answer(_, least))
          }
        )
        .fold(failed("read"), identity)
    }

  private def keeping[A <: caps.Pure](
      command: KeepingCommand[A],
      keeper: DocumentKeeper,
      claims: Claims,
      askers: Askers,
      clock: Clock^
  ): Tool[A]^{clock} =
    Hosted.keeping(command.spec, command.shown, Integrity.Outside, Believed.Own) {
      (args, at, stamped) => (tx: Tx^) ?=>
        person(askers, at).flatMap {
          case None => Right(Outcome.Failed(NoPerson))
          case Some(by) =>
            Tx.clearance(tx).own match {
              case None => Right(Outcome.Failed(NoRoom))
              case Some(own) =>
                KeepingCommand
                  .run(command, args, by, keeper, claims, own.room, stamped, clock.now())
                  .map(answer(_, None))
            }
        }
    }

  /** Who the turn of the call `at` answers, as a command runs for them; `None` when no person. */
  private def person(askers: Askers, at: CallSlot)(using
      Tx^
  ): Either[StoreError, Option[CommandBy]] =
    askers.of(at.turn).map {
      case Some(p: Principal.Person) => Some(CommandBy.Turn(p, at.turn))
      case _ => None
    }

  private def answer(outcome: CommandOutcome, least: Option[Integrity]): Outcome = outcome match {
    case CommandOutcome.Done(text) => Outcome.Done(text, least)
    case CommandOutcome.Refused(why) => Outcome.Failed(why)
  }

  private def failed(doing: String)(error: StoreError): Outcome = error match {
    case StoreError.Invalid(why) => Outcome.Failed(s"Nothing was $doing: $why.")
    case _ => Outcome.Failed(s"Nothing could be $doing just now; try again.")
  }

  /** A plugin's reads that tell `note` the integrity of each thing they return: a document's
    * own, [[Integrity.Untrusted]] for anything of the cache.
    */
  private object Noted {
    def reads(own: PluginReads, note: Integrity -> Unit): PluginReads =
      PluginReads(docs(own.cache, note), shelf(own.documents, note))

    private def docs(cache: PluginDocs, note: Integrity -> Unit): PluginDocs = new PluginDocs {
      def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]] =
        cache.get(key).map { found => found.foreach(_ => note(Integrity.Untrusted)); found }
      def newest(prefix: String, n: Int)(using
          Tx^
      ): Either[StoreError, Vector[(String, ujson.Value)]] =
        cache.newest(prefix, n).map { found =>
          if (found.nonEmpty) note(Integrity.Untrusted)
          found
        }
    }

    private def shelf(documents: DocumentShelf, note: Integrity -> Unit): DocumentShelf =
      new DocumentShelf {
        private def noted(d: Document): Document = { note(d.integrity); d }
        def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]] =
          documents.current(key, label).map(_.map(noted))
        def asOf(key: DocKey, label: Label, at: Instant)(using
            Tx^
        ): Either[StoreError, Option[Document]] =
          documents.asOf(key, label, at).map(_.map(noted))
        def versions(key: DocKey, label: Label, n: Int)(using
            Tx^
        ): Either[StoreError, Vector[DocumentVersion]] =
          documents
            .versions(key, label, n)
            .map(_.map {
              case DocumentVersion.Holding(d) => DocumentVersion.Holding(noted(d))
              case withdrawal => withdrawal
            })
        def labels(key: DocKey)(using Tx^): Either[StoreError, Vector[Label]] =
          documents.labels(key)
        def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]] =
          documents.newest(n).map(_.map(noted))
        def referring(ref: DocRef, n: Int)(using
            Tx^
        ): Either[StoreError, Vector[Document]] =
          documents.referring(ref, n).map(_.map(noted))
        def search(query: String, limit: Int, at: Instant)(using
            Tx^
        ): Either[StoreError, Vector[DocumentSearch.Hit]] =
          documents.search(query, limit, at).map(_.map(h => h.copy(document = noted(h.document))))
      }
  }
}
