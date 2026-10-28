package grit.projects.method

import grit.projects.artifact.{Artifact, Project}
import grit.projects.name.{ArtifactKey, CheckName, StateName}

/** A worked example of a method: heads as `start` holds them (and the judged artifacts' bodies),
  * then moves in order, each expected allowed or refused by the check named. `start`'s pin is not
  * read: the bench runs it under the method it is given.
  */
final case class Example(name: String, start: Project, moves: Vector[Example.Move])
    extends caps.Pure

object Example {

  /** `key` moved to `to`, expected to come out as `expect` says. */
  final case class Move(key: ArtifactKey, to: StateName, expect: Expect) extends caps.Pure

  /** What a move is expected to come out as. */
  enum Expect extends caps.Pure {

    /** It moves, and its transition's action is done. */
    case Allowed

    /** It is refused, `check` among the checks that did not hold. */
    case RefusedBy(check: CheckName)
  }
}

/** A method's worked examples replayed as a live `project_move` makes each move
  * ([[Method.move]]): the check that a method version does what its author says it does.
  */
object MethodBench {

  /** An example that came out otherwise than expected: its name, its first such move, counted
    * from 1, and the refusal that move met, or none when it was allowed.
    */
  final case class Failed(example: String, at: Int, move: Example.Move, met: Option[MoveRefusal])
      extends caps.Pure

  /** Each example whose moves under `method` came out otherwise than expected, with the first
    * that did; none when every one passes. A move allowed changes what the moves after it see;
    * a move refused changes nothing.
    */
  def failures(method: Method, examples: Vector[Example]): Vector[Failed] =
    examples.flatMap(e => failure(method, e))

  /** `example`'s first move to come out otherwise than expected, if any. */
  private def failure(method: Method, example: Example): Option[Failed] = {
    val start: Either[Failed, Vector[Artifact]] = Right(example.start.artifacts)
    example.moves.zipWithIndex
      .foldLeft(start) { case (so, (move, i)) =>
        so.flatMap { artifacts =>
          val came = method.move(artifacts, move.key, move.to)
          val failed = Failed(example.name, i + 1, move, came.left.toOption)
          (move.expect, came) match {
            case (Example.Expect.Allowed, Right(moved)) => Right(after(artifacts, moved))
            case (Example.Expect.RefusedBy(check), Left(MoveRefusal.Unheld(_, _, unheld)))
                if unheld.exists(_._1 == check) =>
              Right(artifacts)
            case _ => Left(failed)
          }
        }
      }
      .left
      .toOption
  }

  /** `artifacts` with those `moved` changed replaced. */
  private def after(artifacts: Vector[Artifact], moved: Moved): Vector[Artifact] = {
    val changed = moved.artifact +: moved.changed.toVector
    artifacts.map(a => changed.find(_.head.key == a.head.key).getOrElse(a))
  }
}
