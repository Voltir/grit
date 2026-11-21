package grit.commands.edge

import grit.commands.caller.Callers
import grit.commands.declare.{HoldingWatch, OwnCommand}
import grit.commands.routes.Routes
import grit.core.clock.Clock
import grit.core.edge.Adapter
import grit.core.edge.{Attesting, EdgeRefusal, EdgeStores, RealmSource, ServedEdge, Variable}
import grit.core.id.{AttesterName, EdgeName, Harness}
import grit.core.identity.{Account, Realm}
import grit.core.place.Place
import grit.core.store.StoreError
import grit.core.tool.ToolName
import grit.edge.http.{Hosts, Listen, Listener}
import grit.mcp.wire.ServerInfo
import grit.std.collect.Collect.firstRepeat

/** The commands edge (ADR 0062): an outside harness's way to a deployment's plugins over HTTP at
  * `at`, each request in `room`. Every request is first admitted by `hosts`, or answered 403; then
  *   - `POST /command/<name>` runs the command `name` (a plugin's, or one of `own`) with its body,
  *     a JSON object (an empty body: none; any other, 400), as the request's caller, answered
  *     as [[grit.commands.routes.RouteReply.command]] says; for the harness session one
  *     `Grit-Session: <harness>:<local>` header names (none without one; two, or one naming no
  *     session, 400), whose writes of a key another session's claim works are refused
  *     ([[grit.core.plugin.KeepingCommand.run]]);
  *   - `POST /mcp` takes one MCP message: `tools/list` lists the plugins' offered commands,
  *     `tools/call` runs one as `/command` would;
  *   - `POST /wait` waits on the store for a commit, or for what `holding` reads, blocking for the
  *     whole wait, a body it does not read 400 ([[grit.commands.routes.Waited.watchOf]]);
  *   - `GET /holder?harness=&dir=[&session=&type=]` answers the claim holder a session in `dir`,
  *     or an agent of `type` that `session` starts, works as on the caller's machine
  *     ([[grit.commands.machine.ClaimHolders.of]]): the one way a claim learns it;
  *   - `GET /brief?harness=&session=&dir=&start=fresh|resume[&agent=&type=][&part=brief|digest]`
  *     answers, as text, the holder's brief, or with `part=digest` what its newest session did
  *     lately, through the deployment's session record, empty when that session last did
  *     something over 24 hours ago (`Briefing.Recent`); which start asks for a digest is the
  *     harness adapter's choice;
  *   - `POST /events?harness=` records one session's event lines, in grit's words
  *     ([[grit.commands.events.Lines.read]] gives every bound a line is refused for), as the
  *     caller, answering:
  *     - 200 `{"recorded": <lines logged>, "refused": [{"line": <its number>, "why": ...}]}`:
  *       every line read is kept, a line sent before not logged twice, and each refused line
  *       named, `recorded` 0 when every line was; a refused line is refused again as it is, so
  *       the machine drops it, saying why, rather than resending it;
  *     - 400 the post refused whole, nothing kept: no `harness`, another query parameter,
  *       grit's own harness, no line, or lines of two sessions; the machine posts each session's
  *       lines apart;
  *     - 403 nothing kept, the store refusing the caller or the session: resending as it is
  *       will not help, so the machine keeps a bounded number of bytes of lines, dropping the
  *       oldest and saying so;
  *     - 413, over [[grit.edge.http.Listener.BodyMost]], nothing kept: split the batch;
  *     - 503 nothing kept, the database not written: resending is safe;
  *     - 500 nothing kept, any other store failure: kept as on 403;
  *   - `GET /join/<harness>` answers the join script of the one of `adapters` for that harness,
  *     as text, or 404.
  * A request with no caller ([[Callers.from]]) is 401, carrying its challenge; another verb than its route's 405, so no
  * GET runs, waits on or writes anything; a query parameter its route does not read (on
  * `/command`, `/mcp`, `/wait` and `/join`, any) 400, naming it; any other path 404. It attests `realm` as `attester`,
  * saying each of `callers`' accounts is a full member. It holds no conversation, so it asks
  * nothing, and delivers nothing.
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
    holding: Option[HoldingWatch],
    adapters: Vector[Adapter]
) extends ServedEdge {

  def needs: Vector[Variable] = Vector.empty

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
        .of(
          stores.commands,
          stores.watch,
          stores.sessions,
          clock,
          room,
          hosts,
          callers,
          own,
          holding,
          adapters,
          server
        )
        .left
        .map(EdgeRefusal.Refused(_))
      source = callers.source(realm)
      _ <- CommandsEdge.vouched(stores, source, callers.accounts)
      listener <- Listener.open(at, (request, _) => routes.answer(request), log)
    } yield {
      log(s"${EdgeName.value(name)}: serving on http://${listener.at.shown}")
      new CommandsEdge.Serving(listener, stores, source)
    }
  }
}

object CommandsEdge {

  /** The edge, or why not: two of `own` of one name; two of `adapters` of one harness; a caller's
    * account `realm` does not hold; `at` not loopback ([[Listen.loopback]]), since a credential
    * crosses it in clear and any process reaching an uncredentialed edge is its owner; or, while
    * `callers` take a request with no credential ([[Callers.uncredentialed]]), `hosts` other than
    * [[Hosts.loopback]].
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
      holding: Option[HoldingWatch],
      adapters: Vector[Adapter] = Vector.empty
  ): Either[String, CommandsEdge] = {
    firstRepeat(own.map(_.name))
      .map(n => s"two own commands are named ${ToolName.value(n)}")
      .orElse(
        firstRepeat(adapters.map(_.harness))
          .map(h => s"two adapters are given for the harness ${Harness.value(h)}")
      )
      .orElse(
        callers.accounts
          .find(a => !realm.holds(a))
          .map(a => s"${Account.written(a)} is no account of ${realm.namespace}:${realm.within}/")
      )
      .orElse(
        Option.when(!at.loopback)(
          if (callers.uncredentialed)
            s"a request with no credential is the owner's, so the edge listens on loopback alone, not ${at.shown}"
          else
            s"a credential crosses the network in clear, so the edge listens on loopback alone, not ${at.shown}"
        )
      )
      .orElse(
        Option.when(callers.uncredentialed && hosts != Hosts.loopback)(
          "a request with no credential is the owner's, so the edge admits loopback's names alone"
        )
      )
      .toLeft(
        new CommandsEdge(
          name,
          at,
          hosts,
          room,
          realm,
          attester,
          callers,
          server,
          own,
          holding,
          adapters
        )
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
          .map {
            case Attesting.Unchecked.Unattested(_, why) =>
              EdgeRefusal.Failed(s"${Account.written(account)} is not vouched: $why")
            case Attesting.Unchecked.Store(error) =>
              EdgeRefusal.Failed(
                s"${Account.written(account)} could not be vouched: ${error.described}"
              )
          }
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
