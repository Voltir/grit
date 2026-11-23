package grit.kit.run

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.assembly.estimate.CharEstimate
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Console, Desks}
import grit.core.host.Instructions
import grit.core.id.PrincipalId
import grit.core.place.{Directory, Place, Repository}
import grit.core.prompt.Fragment
import grit.core.tool.{SetRefusal, ToolSet}
import grit.edge.desk.{PlaceFragments, Server}
import grit.host.checkout.{LocalInstructions, Redaction}
import grit.host.git.WorkingCopies
import grit.kit.deployment.Offered

/** The edge a process serves for its own checkout (ADR 0017). */
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
            Place.of(directory),
            repository,
            hosted,
            instructions,
            tools,
            log.warn(_),
            log.info(_)
          )
        Right(edge)
    }

  /** `use` given the edge this process serves for `directory` ([[over]]): the coding tools
    * `offered` names, run there ([[LocalTools]]), and for each run of `copies`' repository in
    * its working copy, offering its branch as `pushing` does when given, with what they show
    * cleared by `redaction`, and the instruction files
    * around the directory as this machine reads them; the tools closed once `use` returns or
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
      log: org.slf4j.Logger
  )(use: Console.Edge^ => A): Either[String, A] = {
    val tools = new LocalTools(offered, redaction, clock, fresh, copies, pushing)
    try
      over(directory, tools, new LocalInstructions(), log) match {
        case Left(refused) => Left(refused.refusing("the coding tools"))
        case Right(coding) => Right(use(coding))
      }
    finally tools.close()
  }

  private def serve(
      desks: Desks^,
      place: Place,
      repository: Option[(Repository, ToolSet)],
      hosted: ToolSet,
      instructions: Instructions^,
      tools: LocalTools^,
      warn: String => Unit,
      info: String => Unit
  ): Unit = {
    desks.register(PrincipalId.Local, Set(place) ++ repository.map(_._1.place)) match {
      case Left(e) => warn(s"this edge could not register: ${e.why}")
      case Right(desk) =>
        // What each place was last advertised with, and why the repository's file was not read.
        var atPlace: Option[Vector[Fragment]] = None
        var atRepository: Option[Vector[Fragment]] = None
        var unread: Option[String] = None
        def advertised(at: Place, set: ToolSet, now: Vector[Fragment]): Boolean =
          desk.advertise(at, set, now) match {
            case Right(()) => true
            case Left(e) => warn(s"not advertised: ${e.why}"); false
          }
        def round(): Unit = {
          val now =
            place.directory.fold(Vector.empty)(dir =>
              PlaceFragments.of(instructions.around(dir), CharEstimate)
            )
          if (!atPlace.contains(now) && advertised(place, hosted, now)) atPlace = Some(now)
          repository.foreach { (r, inRepository) =>
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
            if (!atRepository.contains(files) && advertised(r.place, inRepository, files))
              atRepository = Some(files)
          }
        }
        round()
        if (place.directory.nonEmpty || repository.nonEmpty) {
          val _ = Thread.ofVirtual().name("grit-advert").start { () =>
            while (true) {
              Thread.sleep(AdvertEvery.toMillis)
              round()
            }
          }
        }
        new Server(
          desk,
          tools,
          Server.onVirtualThreads("grit-tool"),
          info
        ).serve()
    }
  }
}
