package grit.tools

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.host.{AbsPath, Allowlist, HostError, Reach, ReachContract, Reached, Stashes}
import grit.core.message.{Tokens, Usage}

/** What a [[MachineGuard]] is built over in tests: a checkout laid out by a map of links, a
  * scripted stash, and a classifier answering scripted harms.
  */
object GuardFixtures {

  def abs(s: String): AbsPath =
    AbsPath.of(s).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The home directory the corpus's checkouts lie under. */
  val Home: AbsPath = abs("/home/coder")

  /** The checkout, `~/work/co`. */
  val Root: AbsPath = abs("/home/coder/work/co")

  /** The links of [[Root]]'s layout, by absolute path to what each holds: `link-out` to the
    * home directory, `link-in` to `src`, `up` to `..` ([[ReachContract]]'s).
    */
  val Links: Map[AbsPath, String] = Map(
    abs("/home/coder/work/co/link-out") -> "/home/coder",
    abs("/home/coder/work/co/link-in") -> "src",
    abs("/home/coder/work/co/up") -> ".."
  )

  /** A [[Reach]] over a file system holding no links but `links`, its checkout at `root`, read
    * as [[grit.host.checkout.LocalReach]] reads a real one ([[ReachContract]]).
    */
  final class LaidOut(root: AbsPath, links: Map[AbsPath, String]) extends Reach {
    def reached(path: String): Reached = {
      def walk(at: AbsPath, rest: List[String], hops: Int): AbsPath = rest match {
        case Nil => at
        case ("" | ".") :: more => walk(at, more, hops)
        case ".." :: more => walk(AbsPath.resolve(at, ".."), more, hops)
        case name :: more =>
          val next = AbsPath.resolve(at, name)
          links.get(next).filter(_ => hops < 40) match {
            case Some(t) =>
              walk(
                if (t.startsWith("/")) AbsPath.Root else at,
                t.split('/').toList ++ more,
                hops + 1
              )
            case None => walk(next, more, hops)
          }
      }
      val at = walk(if (path.startsWith("/")) AbsPath.Root else root, path.split('/').toList, 0)
      if (AbsPath.within(at, root)) Reached.Inside else Reached.Outside(at)
    }
  }

  /** A stash that reads as `as`. */
  final class Held(as: Either[HostError, Stashes.Held]) extends Stashes {
    def held: Either[HostError, Stashes.Held] = as
  }

  /** A guard whose classifier finds no harm in anything. */
  def harmless: MachineGuard^ = guard(Right(new Judging(Right(Vector.fill(4)(0.0)))))

  /** A clone of its own, its stash empty. */
  def ownStash: Held^ = new Held(Right(Stashes.Held(false, Some("main"), Vector.empty)))

  /** A classifier answering every request with `harms`, the four probabilities in
    * [[MachineGuard.Harms]]'s order, or failing with `error`; keeping each state it was sent.
    */
  final class Judging(answer: Either[ClassifierError, Vector[Double]]) extends Classifier {
    // Kept only for the test's assertions, on the test's own thread.
    @caps.unsafe.untrackedCaptures
    var states: Vector[ujson.Value] = Vector.empty
    // How many questions each request asked, kept likewise.
    @caps.unsafe.untrackedCaptures
    var asked: Vector[Int] = Vector.empty
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      states = states :+ state
      asked = asked :+ questions.size
      // Asked fewer than the four, it is asked the last of them.
      answer.map(ps =>
        Answers(
          ps.takeRight(questions.size).map(Answer.YesNo(_)),
          Usage(Tokens(300), Tokens.Zero, Tokens.Zero, None),
          "jev"
        )
      )
    }
  }

  /** A guard over [[Root]]'s layout, `stashes` and `classifier`, clearing `cleared` besides its
    * own.
    */
  def guard(
      classifier: Either[String, Classifier^],
      stashes: Stashes^ = ownStash,
      cleared: Vector[Allowlist.Rule] = Vector.empty
  ): MachineGuard^ =
    new MachineGuard(new LaidOut(Root, Links), stashes, classifier, Root, Some(Home), cleared)
}

/** [[GuardFixtures.LaidOut]] held to the spec [[grit.host.checkout.LocalReach]] meets. */
object LaidOutReachTests extends ReachContract {
  import GuardFixtures.*

  protected def over[A](body: (Reach^, AbsPath) => A): A =
    body(new LaidOut(Root, Links), Home)
}
