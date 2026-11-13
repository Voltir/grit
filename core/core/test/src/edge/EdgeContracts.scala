package grit.core.edge

import java.time.Instant

import grit.core.identity.{Account, TestAccounts}
import grit.core.place.{Namespace, Place}

/** What [[PluginCommandsContract]] and [[EdgeWatchContract]] run as, wherever they run. */
object EdgeContracts {

  /** Linked to a person wherever a contract runs. */
  val Reader: Account = TestAccounts.account("slack:T1/U-reader")

  /** Linked to no one. */
  val Stranger: Account = TestAccounts.account("slack:T1/U-stranger")

  /** A public room, by the shipped visibility. */
  val Room: Place = Place.under(Namespace.Slack, Vector("acme", "C1"))

  val At: Instant = Instant.parse("2026-11-13T12:00:00Z")
}
