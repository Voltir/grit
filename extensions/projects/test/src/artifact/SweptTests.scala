package grit.projects.artifact

import scala.util.Random

import grit.core.id.ClaimHolder
import grit.projects.artifact.Project.{Stay, Swept}
import grit.projects.name.{ArtifactKey, StateName}
import grit.projects.render.ProjectGen

import utest.*

/** [[Project.swept]] held to its rule over the seeded projects of [[ProjectGen]], each with a
  * seeded `due` and `claimed`, `ended` holding of `done`: what it withdraws against every set the
  * rule allows, found by trying each subset of the candidates.
  */
object SweptTests extends TestSuite {

  /** One seed's sweep, and what the rule says of it, worked out apart from [[Project.swept]]. */
  private final case class Case(
      project: Project,
      due: Set[ArtifactKey],
      claimed: Map[ArtifactKey, ClaimHolder],
      swept: Swept
  ) {
    val artifacts: Vector[Artifact] = project.artifacts
    def ended(h: ArtifactHead): Boolean = h.state == done
    def blocks(a: Artifact): Boolean = !ended(a.head) || claimed.contains(a.head.key)
    def children(k: ArtifactKey): Vector[Artifact] =
      artifacts.filter(_.head.parent.contains(k)).sortBy(_.head.key.value)
    def citers(k: ArtifactKey): Vector[Artifact] =
      artifacts.filter(a => blocks(a) && a.cites.contains(k)).sortBy(_.head.key.value)

    /** Due and ended: what the sweep is asked to withdraw. */
    val asked: Vector[Artifact] = artifacts.filter(a => due(a.head.key) && ended(a.head))
    val candidates: Set[ArtifactKey] =
      asked.map(_.head.key).filterNot(claimed.contains).toSet

    /** Whether the rule allows withdrawing exactly `s`. */
    def closed(s: Set[ArtifactKey]): Boolean =
      s.subsetOf(candidates) &&
        s.forall(k => children(k).forall(c => s(c.head.key)) && citers(k).isEmpty)

    /** Every set of candidates the rule allows. */
    def allowed: Vector[Set[ArtifactKey]] =
      candidates.subsets().filter(closed).toVector
    val gone: Set[ArtifactKey] = swept.withdrawn.gone.map(_.head.key).toSet
  }

  private val done: StateName = right(StateName.of("done"))

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new IllegalArgumentException(why), identity)

  private def sweepOf(seed: Int): Case = {
    val project = ProjectGen.project(seed)
    val r = new Random(seed * 31 + 7)
    val keys = project.artifacts.map(_.head.key)
    val due = keys.filter(_ => r.nextInt(5) != 0).toSet
    val claimed =
      keys
        .filter(_ => r.nextInt(6) == 0)
        .map(k => k -> right(ClaimHolder.of(s"h-${k.value}")))
        .toMap
    Case(
      project,
      due,
      claimed,
      Project.swept(project, due, _.state == done, claimed)
    )
  }

  private val cases: Vector[Case] = Vector.tabulate(600)(sweepOf)

  val tests: Tests = Tests {

    test("a sweep withdraws the largest set its rule allows") {
      val wrong = cases.zipWithIndex.collect {
        case (c, seed) if !c.closed(c.gone) || c.allowed.exists(s => !s.subsetOf(c.gone)) =>
          (seed, c.gone.map(_.value).toVector.sorted, c.allowed.maxByOption(_.size))
      }
      wrong.take(3) ==> Vector()
    }

    test("every other artifact asked for stays, with the first reason that holds of it") {
      val wrong = cases.zipWithIndex.flatMap { (c, seed) =>
        val expected = c.asked
          .filterNot(a => c.gone(a.head.key))
          .sortBy(_.head.key.value)
          .map { a =>
            val k = a.head.key
            val why: Option[Stay] = c.claimed.get(k).map(Stay.Claimed(_)).orElse {
              c.citers(k).map(_.head) match {
                case first +: rest => Some(Stay.Cited(first, rest))
                case _ =>
                  c.children(k).find(ch => !c.gone(ch.head.key)).map(ch => Stay.Above(ch.head))
              }
            }
            (a.head.key.value, why)
          }
        val actual = c.swept.stayed.map(s => (s.artifact.head.key.value, Some(s.why)))
        Option.when(actual != expected)((seed, actual, expected))
      }
      wrong.take(3) ==> Vector()
    }

    test(
      "what is left is the project without what went, each artifact citing it rewritten not to, and reads back as a project"
    ) {
      val wrong = cases.zipWithIndex.flatMap { (c, seed) =>
        val left = c.swept.withdrawn.project
        val rewritten = c.artifacts
          .filter(a => !c.gone(a.head.key) && a.cites.exists(c.gone))
          .map(a => (a.head.key.value, a.head.cites.filterNot(c.gone)))
          .sortBy(_._1)
        val problems = Vector(
          Option.when(
            left.artifacts.map(_.head.key).toSet != c.artifacts.map(_.head.key).toSet -- c.gone
          )("keys"),
          Option.when(
            c.swept.withdrawn.uncited.map(a => (a.head.key.value, a.head.cites)) != rewritten
          )("uncited"),
          Option.when(left.artifacts.exists(_.cites.exists(c.gone)))("still cites"),
          Option.when(Project.of(left.key, left.pin, left.artifacts) != Right(left))(
            "not a project"
          )
        ).flatten
        Option.when(problems.nonEmpty)((seed, problems))
      }
      wrong.take(3) ==> Vector()
    }

    test("sweeping what is left again, as asked before, withdraws nothing") {
      val again = cases.zipWithIndex.collect {
        case (c, seed)
            if Project
              .swept(c.swept.withdrawn.project, c.due, c.ended, c.claimed)
              .withdrawn
              .gone
              .nonEmpty =>
          seed
      }
      again.take(3) ==> Vector()
    }

    test(
      "the seeds hold each reason, a citation cycle withdrawn, a chain of two above, and a claimed citer"
    ) {
      val whys = cases.flatMap(_.swept.stayed.map(_.why))
      val cycle = cases.exists(c =>
        c.swept.withdrawn.gone.exists(a =>
          a.cites.exists(b =>
            c.gone(b) && c.artifacts.exists(o => o.head.key == b && o.cites.contains(a.head.key))
          )
        )
      )
      val chain = cases.exists { c =>
        val above = c.swept.stayed.collect { case (a, Stay.Above(ch)) => a.head.key -> ch.key }
        val keys = above.map(_._1).toSet
        above.exists((_, ch) => keys(ch))
      }
      val claimedCiter = cases.exists(c =>
        c.swept.stayed.exists {
          case (_, Stay.Cited(first, rest)) =>
            (first +: rest).exists(h => c.claimed.contains(h.key) && c.ended(h))
          case _ => false
        }
      )
      (
        whys.exists(_.isInstanceOf[Stay.Claimed]),
        whys.exists(_.isInstanceOf[Stay.Cited]),
        whys.exists(_.isInstanceOf[Stay.Above]),
        cycle,
        chain,
        claimedCiter
      ) ==> (true, true, true, true, true, true)
    }
  }
}
