package grit.kit.run

import java.nio.file.Files

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.assembly.estimate.CharEstimate
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Console, Desks, EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.host.Instructions
import grit.core.id.{EdgeName, PrincipalId}
import grit.core.place.{Directory, Place, Repository}
import grit.core.prompt.Fragment
import grit.core.store.StoreError
import grit.core.tool.{SetRefusal, ToolSet}
import grit.edge.desk.{PlaceFragments, Server}
import grit.host.checkout.{LocalInstructions, LocalProcesses, LocalShell, Redaction}
import grit.host.git.{Branches, WorkingCopies}
import grit.kit.deployment.{Deployment, Offered, RepositoryClone}
import grit.kit.environment.Secrets
import grit.tools.SourceTools

/** The edge a process serves for its own checkout (ADR 0017), and the edges a served deployment
  * serves its repositories through.
  */
object LocalEdge {

  /** How often the instruction files are read again: 5 s. */
  val AdvertEvery: FiniteDuration = 5.seconds

  /** The edge a process serves over its checkout `directory`, for [[Kit.console]]: registered
    * at the directory's place, it offers there what `tools` runs, with the instruction files
    * `instructions` finds around the directory (estimated by [[CharEstimate]]), and runs each
    * request routed to it with `tools`, on a virtual thread, until the console closes. The
    * files are read again every [[AdvertEvery]] until the process ends, and advertised when
    * they changed, so an edit reaches the next turn without a restart. A registration or an
    * advert that fails is logged to `log` as a warning, and turns are then served by no edge
    * here; what the server says of a run, as information. When `tools` serve a repository's runs
    * ([[LocalTools.repository]]), the edge registers that repository's place too and offers
    * there what `tools` runs at it ([[LocalTools.hostedInRepository]]), with the repository's
    * own instruction file ([[LocalTools.instructionFile]]), read again as often; one that
    * cannot be read is advertised as none, and why is logged as a warning and as information
    * when it changes. `Left` names a tool `tools` cannot offer with the others.
    */
  def over(
      directory: Directory,
      tools: LocalTools^,
      instructions: Instructions^,
      log: org.slf4j.Logger
  ): Either[SetRefusal, Console.Edge^] =
    tools.hosted.flatMap(h => tools.hostedInRepository.map((h, _))) match {
      case Left(twice) => Left(twice)
      case Right((hosted, inRepository)) =>
        val repository = tools.repository.map(r => (r, inRepository))
        val edge: Console.Edge^ = desks =>
          serve(
            desks,
            PrincipalId.Local,
            Some((Place.of(directory), hosted)),
            repository,
            instructions,
            tools,
            AdvertEvery,
            log.warn(_),
            log.info(_)
          ) match {
            case Left(refused) => log.warn(s"this edge is not served: ${refused.message}")
            case Right(_) => ()
          }
        Right(edge)
    }

  /** `use` given the edge this process serves for `directory` ([[over]]): the coding tools
    * `offered` names, run there ([[LocalTools]]), and for each run of `copies`' repository in
    * its working copy, offering its branch as `pushing` does when given, with what they show
    * cleared by `redaction`, each allowlisted `run` judged by a machine guard asking `guarding`
    * and a shell read of `sources`' files answering after the line naming what reads them,
    * and the instruction files around the directory as this machine reads them; the tools closed
    * once `use` returns or
    * throws, which ends any command they left running. Why not, before `use` runs: a tool
    * `offered` names cannot be offered with the others.
    */
  def serving[A](
      directory: Directory,
      offered: Offered,
      redaction: Redaction,
      clock: Clock^,
      fresh: Fresh^,
      copies: Option[WorkingCopies^],
      pushing: Option[Pushing^],
      guarding: Either[String, Classifier^],
      sources: Option[SourceTools],
      log: org.slf4j.Logger
  )(use: Console.Edge^ => A): Either[String, A] = {
    val tools =
      new LocalTools(offered, redaction, clock, fresh, copies, guarding, sources, pushing)
    try
      over(directory, tools, new LocalInstructions(), log) match {
        case Left(refused) => Left(refused.refusing("the coding tools"))
        case Right(coding) => Right(use(coding))
      }
    finally tools.close()
  }

  /** `clone`'s working copies, git run in the process's own environment, the SSH agent's socket
    * included so a fetch reaches an SSH origin ([[LocalShell.Agent]]), with what it shows cleared
    * by `redaction`, their leftovers as `leftovers` says, deleting done branches on their
    * remote with `branches` when given, and saying what a sweep removes to `note`.
    */
  def copies(
      clone: RepositoryClone,
      redaction: Redaction,
      clock: Clock^,
      branches: Option[Branches^],
      note: String -> Unit,
      leftovers: WorkingCopies.Leftovers = WorkingCopies.Leftovers.Kept
  ): WorkingCopies^ =
    new WorkingCopies(
      clone.repository,
      clone.at,
      clone.copies,
      new LocalProcesses(clone.at, sys.env, redaction, LocalShell.Agent),
      clock,
      branches,
      note,
      leftovers
    )

  /** The edge, named [[Deployment.edgeOf]] its repository, that serves `hosted`'s repository at
    * its place: registered for grit, it offers there the coding tools `hosted.allowlist`
    * permits, run in the working copy of the run each request is routed to (a spawned child's,
    * by its name) on `grit/{work}`, each `run` judged by a machine guard asking
    * [[Launch.guarding]] of `deployment` and `secrets`,
    * with the repository's instruction file read again every
    * `every`, as [[over]]'s is; it offers no pull request. What a run leaves uncommitted is
    * committed to its branch once its copy is abandoned ([[WorkingCopies.Leftovers.Committed]]);
    * what its copies' sweeps remove, and any advert that later fails, is logged under
    * `grit.serve`. Refused at open when the clone is no directory, or its place cannot be
    * registered or first advertised. Closing it stops its server and its adverts, then ends
    * every command its tools left running.
    */
  private[run] def served(
      hosted: Deployment.Hosted,
      deployment: Deployment,
      secrets: Secrets,
      every: FiniteDuration = AdvertEvery
  ): ServedEdge =
    new ServedEdge {
      private val cloned = hosted.cloned
      def name: EdgeName = Deployment.edgeOf(cloned.repository)
      def needs: Vector[Variable] = Vector.empty
      def asks: ServedEdge.Asks = ServedEdge.Asks.Never
      def open(
          stores: EdgeStores^,
          env: Map[String, String],
          clock: Clock^,
          log: String => Unit
      ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] =
        if (!Files.isDirectory(cloned.at))
          Left(
            EdgeRefusal.Failed(
              s"the clone of ${cloned.repository.name} at ${cloned.at} is no directory"
            )
          )
        else {
          val logged = org.slf4j.LoggerFactory.getLogger("grit.serve")
          val redaction = Redaction.of(env)
          val tools = new LocalTools(
            Offered.Allowlisted(hosted.allowlist),
            redaction,
            clock,
            Fresh.random(),
            Some(
              copies(
                cloned,
                redaction,
                clock,
                None,
                line => logged.info(line),
                WorkingCopies.Leftovers.Committed
              )
            ),
            Launch.guarding(deployment, secrets),
            None,
            None
          )
          val opened = tools.hostedInRepository match {
            case Left(twice) => Left(EdgeRefusal.Failed(twice.refusing("the coding tools")))
            case Right(set) =>
              serve(
                stores.desks,
                PrincipalId.Grit,
                None,
                Some((cloned.repository, set)),
                new LocalInstructions(),
                tools,
                every,
                log,
                line => logged.info(line)
              )
          }
          opened match {
            case Left(refused) =>
              tools.close()
              Left(refused)
            case Right(serving) =>
              log(s"${cloned.repository.place.written}: serving its runs in ${cloned.copies}")
              Right(new ServedEdge.Open {
                def deliver(): Either[StoreError, Int] = Right(0)
                def close(): Unit = {
                  serving.close()
                  tools.close()
                }
              })
          }
        }
    }

  /** Registers `place` (with the tools it offers) and `repository`'s place (with its) for
    * `principal` through `desks`, advertises each, and serves their requests with `tools` until
    * closed: closing stops the server, then the adverts, which run every `every`, each place
    * advertised again when what it shows changed. Refused when the places cannot be registered,
    * or a first advert is refused; a later advert that fails is told to `warn`.
    */
  private def serve(
      desks: Desks^,
      principal: PrincipalId,
      place: Option[(Place, ToolSet)],
      repository: Option[(Repository, ToolSet)],
      instructions: Instructions^,
      tools: LocalTools^,
      every: FiniteDuration,
      warn: String => Unit,
      info: String => Unit
  ): Either[EdgeRefusal, AutoCloseable^{desks, instructions, tools, warn, info}] =
    desks.register(principal, place.map(_._1).toSet ++ repository.map(_._1.place)) match {
      case Left(e) => Left(EdgeRefusal.Failed(s"this edge could not register: ${e.why}"))
      case Right(desk) =>
        // What each place was last advertised with, and why the repository's file was not read.
        var atPlace: Option[(Vector[Fragment], Boolean)] = None
        var atRepository: Option[Vector[Fragment]] = None
        var unread: Option[String] = None
        // The refusal of the last advert, if it was refused.
        def advertised(
            at: Place,
            set: ToolSet,
            now: Vector[Fragment],
            inWorkTree: Boolean
        ): Option[String] =
          desk.advertise(at, set, now, inWorkTree) match {
            case Right(()) => None
            case Left(e) => Some(s"not advertised: ${e.why}")
          }
        // One round of adverts; why the first refused one was refused.
        def round(): Option[String] = {
          val atDirectory = place.flatMap { (p, hosted) =>
            val now =
              p.directory.fold((Vector.empty, false))(dir =>
                (
                  PlaceFragments.of(instructions.around(dir), CharEstimate),
                  instructions.inWorkTree(dir)
                )
              )
            if (atPlace.contains(now)) None
            else {
              val refused = advertised(p, hosted, now._1, now._2)
              if (refused.isEmpty) atPlace = Some(now)
              refused
            }
          }
          val atRepo = repository.flatMap { (r, inRepository) =>
            val (files, why) = tools.instructionFile match {
              case Left(e) => (Vector.empty, Some(e.message))
              case Right(file) => (file.map(PlaceFragments.repository(r, _)).toVector, None)
            }
            // Said when it changes, not each round.
            if (why != unread) {
              why.foreach { w =>
                val said = s"${r.place.written}'s instruction file is not shown to its runs: $w"
                warn(said)
                info(said)
              }
              unread = why
            }
            if (atRepository.contains(files)) None
            else {
              // A repository's place is no directory: its reach says how to work in its copy.
              val refused = advertised(r.place, inRepository, files, false)
              if (refused.isEmpty) atRepository = Some(files)
              refused
            }
          }
          atDirectory.orElse(atRepo)
        }
        round() match {
          case Some(refused) => Left(EdgeRefusal.Failed(refused))
          case None =>
            val advertising =
              if (place.exists(_._1.directory.nonEmpty) || repository.nonEmpty) {
                val named = (place.map(_._1) ++ repository.map(_._1.place)).map(_.written)
                // A daemon platform thread, so that it is listed while it lives, and never keeps
                // the process up.
                Some(
                  Thread.ofPlatform().daemon().name(s"grit-advert ${named.mkString(" ")}").start {
                    () =>
                      try
                        while (!Thread.currentThread().isInterrupted) {
                          Thread.sleep(every.toMillis)
                          round().foreach(warn)
                        }
                      catch { case _: InterruptedException => () }
                  }
                )
              } else None
            val server = new Server(desk, tools, Server.onVirtualThreads("grit-tool"), info)
            server.serve()
            Right(new AutoCloseable {
              def close(): Unit = {
                server.close()
                advertising.foreach(_.interrupt())
              }
            })
        }
    }
}
