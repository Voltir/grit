package grit.core.edge

import java.time.Instant

import grit.core.id.EdgeName
import grit.core.inbox.Inbox
import grit.core.place.Place
import grit.core.review.Reviews
import grit.core.store.{Jot, Principals, StoreError}
import grit.core.visibility.Compartment

/** What an edge is given to reach the engine (ADR 0002): the inbox it hands messages to and
  * reads turns from, the people it enrolls, the replies it awaits, the messages it marks as
  * being answered while their turns run, the review prompts it posts
  * and the labels their rater gives, `jot`, the short transactions it writes those in, and
  * `desks`, where it registers places it hosts tools in.
  */
final case class EdgeStores(
    inbox: Inbox,
    principals: Principals,
    deliveries: Deliveries,
    acknowledgements: Acknowledgements,
    reviews: Reviews,
    jot: Jot,
    desks: Desks^
)

/** The name of an environment variable an edge reads, such as `SLACK_BOT_TOKEN`; never its
  * value.
  */
opaque type Variable = String

object Variable {
  def apply(name: String): Variable = name
  def value(v: Variable): String = v
}

/** Why an edge would not open, or stopped partway. None quotes a variable's value: each may be a secret. */
enum EdgeRefusal {

  /** `variable` is not set. */
  case Missing(variable: Variable)

  /** `variable` is set to something it cannot be; `why` says what it must be. */
  case Malformed(variable: Variable, why: String)

  /** The edge's service refused it, or could not be reached; `why` is its answer. */
  case Refused(why: String)

  /** The store or the service failed partway; what was done before it stays done. */
  case Failed(why: String)

  /** A line a person reads. */
  def message: String = this match {
    case Missing(v) => s"${Variable.value(v)} is not set"
    case Malformed(v, why) => s"${Variable.value(v)}: $why"
    case Refused(why) => why
    case Failed(why) => why
  }
}

object EdgeRefusal {

  /** The first of `needs` that `env` does not set, as [[EdgeRefusal.Missing]]. */
  def missing(needs: Vector[Variable], env: Map[String, String]): Option[EdgeRefusal] =
    needs.find(v => !env.contains(Variable.value(v))).map(Missing(_))
}

/** An edge a deployment serves beside its engine, as a plugin is one it posts to. */
trait ServedEdge {

  def name: EdgeName

  /** The environment variables [[open]] reads. The kit refuses to start, naming the first one
    * unset, before it opens the engine.
    */
  def needs: Vector[Variable]

  /** Whether a person on this edge can answer a tool call that asks first. A deployment that
    * offers such tools is refused when any edge it serves cannot.
    */
  def answersAsks: Boolean

  /** Whether it posts anything beyond the conversation a turn answers: a post to a channel a
    * tool call names, or one of its own about a turn, such as a review's prompt. A deployment
    * labelling any room above public is refused while an edge it serves does: nothing yet
    * checks what such a post carries against where it goes.
    */
  def postsOut: Boolean

  /** Where it posts a deployment's review prompts, when it answers a review: the kit picks only
    * messages that place may receive ([[grit.core.review.ReviewStore.candidates]]).
    */
  def reviewsAt: Option[Place] = None

  /** The compartments it names, in a label it labels anything with. A deployment is refused
    * unless its visibility declares each.
    */
  def compartments: Vector[Compartment] = Vector.empty

  /** Connects to the edge's service with `env`'s credentials and starts taking its messages
    * into `stores`, telling `log` what a person running it may want to read.
    */
  def open(
      stores: EdgeStores^,
      env: Map[String, String],
      log: String => Unit
  ): Either[EdgeRefusal, ServedEdge.Open^{stores, log, caps.any}]
}

object ServedEdge {

  /** An edge while it is served. */
  trait Open {

    /** Posts the replies of the turns finished since the last call, and puts up or takes down
      * the marks of the messages being answered, where the edge marks them; how many turns it
      * delivered. `Left` when the store could not be read: the caller calls again later.
      */
    def deliver(): Either[StoreError, Int]

    /** Stops taking messages and disconnects. Called once, last. */
    def close(): Unit
  }
}

/** What an edge would hear from one of its sources: `source`, as a person reads it (a
  * channel's name), and each thread's messages as their lengths in characters, in the order
  * they were said.
  */
final case class Unheard(source: String, threads: Vector[Vector[Int]]) {

  def messages: Int = threads.map(_.size).sum
}

/** Work an edge does once over the stores before serving: hearing what was said while grit
  * was not listening.
  */
trait CatchUp {

  def name: EdgeName

  /** The environment variables [[open]] reads, as [[ServedEdge.needs]]. */
  def needs: Vector[Variable]

  /** Connects with `env`'s credentials and reads what was said before `now` that `stores`
    * have not recorded; nothing is heard until [[CatchUp.Open.hear]].
    */
  def open(
      stores: EdgeStores^,
      env: Map[String, String],
      now: Instant,
      log: String => Unit
  ): Either[EdgeRefusal, CatchUp.Open^{stores, log, caps.any}]
}

object CatchUp {

  /** A catch-up that has read what it would hear. */
  trait Open {

    /** When the span read starts. */
    def since: Instant

    /** What it would hear, one source at a time. */
    def unheard: Vector[Unheard]

    /** Hears all of [[unheard]], each message at the time it was said, and answers none. */
    def hear(): Either[EdgeRefusal, Unit]

    /** Disconnects. Called once, last. */
    def close(): Unit
  }
}
