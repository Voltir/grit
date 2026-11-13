package grit.commands.edge

import grit.commands.caller.Callers
import grit.commands.declare.{HoldingWatch, OwnCommand}
import grit.commands.routes.Routes
import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, RealmSource, ServedEdge, Variable}
import grit.core.id.{AttesterName, EdgeName}
import grit.core.identity.{Account, Realm}
import grit.core.place.Place
import grit.core.store.StoreError
import grit.core.tool.ToolName
import grit.edge.http.{Hosts, Listen, Listener}
import grit.mcp.wire.ServerInfo

/** The commands edge (ADR 0062): an outside harness's way to a deployment's plugins over HTTP at
  * `at`, each request in `room`. Every request is first admitted by `hosts`, or answered 403; then
  *   - `POST /command/<name>` runs the command `name` (a plugin's, or one of `own`) with its body,
  *     a JSON object, as the request's caller;
  *   - `POST /mcp` takes one MCP message: `tools/list` lists the plugins' offered commands,
  *     `tools/call` runs one as `/command` would;
  *   - `POST /wait` waits on the store for a commit, or for what `holding` reads, blocking for the
  *     whole wait.
  * A request with no caller ([[Callers.from]]) is 403; another verb 405, so no GET runs or waits
  * on anything; any other path 404. It attests `realm` as `attester`, saying each of `callers`'
  * accounts is a full member. It holds no conversation, so it asks nothing, and delivers nothing.
  */
final class CommandsEdge private (
    val name: EdgeName,
    at: Listen,
    hosts: Hosts,
    room: Place,
    realm: Realm,
    attesterName: AttesterName,
    callers: Callers,
    server: ServerInfo,
    own: Vector[OwnCommand],
    holding: Option[HoldingWatch]
) extends ServedEdge {

  def needs: Vector[Variable] = callers.needs

  def asks: ServedEdge.Asks = ServedEdge.Asks.Never

  override def attester: Option[AttesterName] = Some(attesterName)

  /** Listens at `at` once each caller's account is vouched. `Refused` when a guarding own command
    * names no offered plugin command, an added one names an offered command, or an offered
    * command cannot be listed at `/mcp`, each before any account is vouched, or when `at` cannot
    * be listened on; `Failed` when a caller's account cannot be
    * vouched. When the deployment does not trust `attester` for `realm`, nothing is vouched and
    * it listens all the same: a caller's account no person is linked to is then 403 on every
    * request ([[grit.core.edge.CommandError.NotVouched]]).
    */
  def open(
      stores: EdgeStores^,
      env: Map[String, String],
      clock: Clock^,
      log: String => Unit
  ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
    val offered = stores.commands.offered.map(_.schema.name).toSet
    for {
      opened <- callers.opened(env)
      _ <- own
        .collectFirst {
          case o if o.guards && !offered.contains(ToolName.value(o.name)) =>
            s"the own command ${ToolName.value(o.name)} guards no offered command"
          case o if !o.guards && offered.contains(ToolName.value(o.name)) =>
            s"the own command ${ToolName.value(o.name)} is added, but a plugin offers it"
        }
        .map(EdgeRefusal.Refused(_))
        .toLeft(())
      routes <- Routes
        .of(stores.commands, stores.watch, clock, room, hosts, opened, own, holding, server)
        .left
        .map(EdgeRefusal.Refused(_))
      source = opened.source(realm)
      _ <- CommandsEdge.vouched(stores, source, opened.accounts)
      listener <- Listener.open(at, (request, _) => routes.answer(request), log)
    } yield {
      log(s"${EdgeName.value(name)}: serving on http://${listener.at.shown}")
      new CommandsEdge.Serving(listener, stores, source)
    }
  }
}

object CommandsEdge {

  /** The edge, or why not: two of `own` of one name; a caller's account `realm` does not hold;
    * or, while `callers` take a request with no credential ([[Callers.uncredentialed]]), `at`
    * not loopback ([[Listen.loopback]]) or `hosts` other than [[Hosts.loopback]], since then any
    * process that reaches it is the owner.
    */
  def of(
      name: EdgeName,
      at: Listen,
      hosts: Hosts,
      room: Place,
      realm: Realm,
      attester: AttesterName,
      callers: Callers,
      server: ServerInfo,
      own: Vector[OwnCommand],
      holding: Option[HoldingWatch]
  ): Either[String, CommandsEdge] = {
    val names = own.map(_.name)
    names
      .diff(names.distinct)
      .headOption
      .map(n => s"two own commands are named ${ToolName.value(n)}")
      .orElse(
        callers.accounts
          .find(a => !realm.holds(a))
          .map(a => s"${Account.written(a)} is no account of ${realm.namespace}:${realm.within}/")
      )
      .orElse(
        Option.when(callers.uncredentialed && !at.loopback)(
          s"a request with no credential is the owner's, so the edge listens on loopback alone, not ${at.shown}"
        )
      )
      .orElse(
        Option.when(callers.uncredentialed && hosts != Hosts.loopback)(
          "a request with no credential is the owner's, so the edge admits loopback's names alone"
        )
      )
      .toLeft(
        new CommandsEdge(name, at, hosts, room, realm, attester, callers, server, own, holding)
      )
  }

  /** Each of `accounts` vouched through `source` before a request is answered; `Failed` naming
    * the first that cannot be.
    */
  private def vouched(
      stores: EdgeStores^,
      source: RealmSource,
      accounts: Vector[Account]
  ): Either[EdgeRefusal, Unit] =
    accounts.foldLeft[Either[EdgeRefusal, Unit]](Right(())) { (done, account) =>
      done.flatMap(_ =>
        stores.attesting
          .before(source, account)
          .left
          .map(why => EdgeRefusal.Failed(s"${Account.written(account)} is not vouched: $why"))
      )
    }

  /** The edge while served: `listener` until [[close]], and a look through `source` when asked. */
  private final class Serving(
      listener: Listener^,
      stores: EdgeStores^,
      source: RealmSource
  ) extends ServedEdge.Open {
    def deliver(): Either[StoreError, Int] = Right(0)
    override def attest(): Either[StoreError, Int] = stores.attesting.round(source)
    def close(): Unit = listener.close()
  }
}
