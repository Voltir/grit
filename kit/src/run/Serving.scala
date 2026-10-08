package grit.kit.run

import grit.core.edge.{EdgeStores, ServedEdge}
import grit.core.id.EdgeName

/** A deployment's edges while they are served: opened in order, delivered to in rounds, and
  * closed once, last.
  */
private[run] object Serving {

  /** The edges opened, as one. */
  trait Opened {

    /** Asks every edge to deliver once; an edge whose store could not be read is named to
      * `warn`, and asked again next round.
      */
    def round(warn: String => Unit): Unit

    /** Asks every edge for one look at the accounts its attester answers for
      * ([[ServedEdge.Open.attest]]); an edge whose store failed is named to `warn`, and asked
      * again at the next look.
      */
    def attest(warn: String => Unit): Unit

    /** Closes every edge, once each, the last opened first. */
    def close(): Unit
  }

  private object NoneOpened extends Opened {
    def round(warn: String => Unit): Unit = ()
    def attest(warn: String => Unit): Unit = ()
    def close(): Unit = ()
  }

  /** `first`, named `name`, opened before `rest`. */
  private final class Both(name: EdgeName, first: ServedEdge.Open^, rest: Opened^) extends Opened {
    def round(warn: String => Unit): Unit = {
      first
        .deliver()
        .left
        .foreach(e => warn(s"${EdgeName.value(name)}: replies not read: $e"))
      rest.round(warn)
    }
    def attest(warn: String => Unit): Unit = {
      first
        .attest()
        .left
        .foreach(e => warn(s"${EdgeName.value(name)}: accounts not attested: $e"))
      rest.attest(warn)
    }
    def close(): Unit = {
      rest.close()
      first.close()
    }
  }

  /** Each of `edges` opened over the stores `stores` builds for it, with `env`'s credentials,
    * in order. The first that refuses closes those already opened, and is the failure.
    */
  def open(
      edges: List[ServedEdge],
      stores: ServedEdge => EdgeStores^,
      env: Map[String, String],
      log: String => Unit
  ): Either[KitFailure, Opened^{stores, log, caps.any}] =
    edges match {
      case Nil => Right(NoneOpened)
      case edge :: others =>
        edge.open(stores(edge), env, log) match {
          case Left(refusal) => Left(KitFailure.Edge(edge.name, refusal))
          case Right(first) =>
            open(others, stores, env, log) match {
              case Left(failure) =>
                first.close()
                Left(failure)
              case Right(rest) => Right(new Both(edge.name, first, rest))
            }
        }
    }

  /** Asks `opened` to deliver, a round at a time, `pause` between rounds, until `stopped` says
    * so before a round; before each round `looking` says, it asks `opened` for a look.
    */
  def deliver(
      opened: Opened^,
      stopped: () => Boolean,
      looking: () => Boolean,
      pause: () => Unit,
      warn: String => Unit
  ): Unit =
    while (!stopped()) {
      if (looking()) opened.attest(warn)
      opened.round(warn)
      pause()
    }
}
