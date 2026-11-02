package grit.mcp.client

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.visibility.Itemised
import grit.mcp.wire.{Answer, McpCalled, McpError, McpTool, Rpc, Skipped}

/** A server's tools as listed: `tools`, those grit may offer, each name once, in the order
  * listed; `skipped`, the others and why; `unlisted`, the names the server's allowlist gives
  * that no entry of the list has.
  */
final case class Listed private[client] (
    tools: Vector[McpTool],
    skipped: Vector[Skipped],
    unlisted: Set[String]
)

/** `server`'s tools and calls, each request sent over its transport: made by [[McpClient.apply]]
  * for Streamable HTTP.
  */
final class McpClient private (
    val server: McpServer[McpTransport],
    connection: Connection^,
    clock: Clock
) extends AutoCloseable {
  import McpClient.*

  // The list as last read, the read-only tools it lists that a lookup names, by name, and the
  // Clock.millis reading until which it is fresh. Replaced whole through the reference's own
  // atomic operations, so a reader sees one list or the other; two callers that both find it
  // stale both list, and either result is right.
  @caps.unsafe.untrackedCaptures
  private val kept = new AtomicReference[Option[(Listed, Map[String, McpTool], Long)]](None)

  /** The tools grit may offer from `server`, every page of its list, less those its allowlist
    * leaves out ([[Skipped.NotAllowed]]) and then those its scope excludes
    * ([[Skipped.OutOfScope]]): the list kept while fresh, else listed again. A list is fresh until the earliest of its pages' receipt plus
    * that page's `ttlMs`, so a page without one makes it stale at once. A re-list that fails
    * returns the kept list when there is one, else its error; [[McpError.Unreadable]] when the
    * list runs past [[McpClient.MaxPages]] pages. A re-list blocks on its server for up to
    * [[McpClient.Timeout]] a page, so up to [[McpClient.MaxPages]] times that in all.
    */
  def tools(): Either[McpError, Listed] = {
    val held = kept.get()
    held match {
      case Some((listed, _, until)) if clock.millis() < until => Right(listed)
      case _ =>
        list() match {
          case Right(read @ (listed, _, _)) =>
            kept.set(Some(read))
            Right(listed)
          case Left(e) => held.map(_._1).toRight(e)
        }
    }
  }

  /** The kept list made stale, so the next [[tools]] lists again; it is still served when
    * that fails.
    */
  def stale(): Unit = {
    kept.updateAndGet(_.map((listed, lookable, _) => (listed, lookable, Long.MinValue)))
    ()
  }

  /** `tool` called with `arguments` as the server's scope sends them
    * ([[grit.mcp.scope.McpScope.request]]): its answer as the scope shows it
    * ([[grit.mcp.scope.McpScope.shown]]), an `isError` one included, its items as the server's
    * `accounts` read the shown result: whole ([[Answer.of]]) or cut by item ([[Answer.cut]]);
    * or why none:
    * [[McpError.OutOfScope]], with nothing sent, when the scope refuses the call;
    * [[McpError.Unattributed]], with none of the answer shown, when the scope cannot hold it;
    * [[McpError.Unplaced]], with none of it shown, when `accounts` cannot place it;
    * [[McpError.NotLooked]], with the call not sent, when a lookup it needs first cannot be
    * made ([[McpServer.lookups]], whose answers the `accounts` read beside it). An
    * [[McpError.Rpc]] with code `-32602` (an unknown tool, or invalid arguments) makes the list
    * [[stale]], since the server's tools may have changed. Blocks on the server for up to
    * [[McpClient.Timeout]] a request: the call and each of its lookups.
    */
  def call(tool: McpTool, arguments: ujson.Obj): Either[McpError, Answer] = {
    val answered = for {
      sent <- server.scope.request(tool, arguments).left.map(McpError.OutOfScope(_))
      looked <- look(tool.name, sent)
      result <- exchange(Rpc.Call.CallTool(tool, sent))
      shown <- server.scope.shown(tool, result).left.map(McpError.Unattributed(_))
      answer <- server.accounts.items(McpCalled(tool.name, sent, shown, looked)) match {
        case Itemised.Whole(items) => Answer.of(shown, items)
        case cut: Itemised.Parts => Answer.cut(shown, cut)
        case Itemised.Unplaced(why) => Left(McpError.Unplaced(why))
      }
    } yield answer
    // A -32020 HeaderMismatch is not re-listed and retried, though the spec says a client
    // SHOULD (streamable-http.mdx, Client Behavior): a list with ttlMs 0, as GitHub's is, is
    // re-read on every access, so `tool` already carries the schema the server holds and a
    // retry would mirror the same headers. A server listing with a longer ttlMs may want it.
    answered match {
      case Left(McpError.Rpc(InvalidParams, _)) => stale()
      case _ => ()
    }
    answered
  }

  /** The answers of `tool`'s lookups ([[McpServer.lookups]]) for a call sending `sent`, each by
    * its tool's name; [[McpError.NotLooked]], with none sent, when `sent` lacks an argument one
    * names, and with none after it sent, when one cannot be made. A lookup's tool is read from
    * the kept list, stale or not, and the list is read only when none is kept.
    */
  private def look(tool: String, sent: ujson.Obj): Either[McpError, Map[String, ujson.Obj]] = {
    val lookups = server.lookups.getOrElse(tool, Vector.empty)
    val valued = lookups.foldLeft[Either[McpError, Vector[(Lookup, ujson.Value)]]](
      Right(Vector.empty)
    ) { (read, lookup) =>
      read.flatMap(vs =>
        sent.value
          .get(lookup.argument)
          .map(v => vs :+ (lookup -> v))
          .toRight(
            McpError.NotLooked(
              s"the call names no ${lookup.argument}, which is looked up with ${lookup.tool} first"
            )
          )
      )
    }
    def lookable(): Either[McpError, Map[String, McpTool]] = kept.get() match {
      case Some((_, held, _)) => Right(held)
      case None => tools().map(_ => kept.get().fold(Map.empty[String, McpTool])(_._2))
    }
    valued.flatMap(
      _.foldLeft[Either[McpError, Map[String, ujson.Obj]]](Right(Map.empty)) {
        case (read, (lookup, value)) =>
          def failed(e: McpError) = McpError.NotLooked(s"${lookup.tool} ${e.message}")
          for {
            looked <- read
            held <- lookable().left.map(failed)
            listed <- held
              .get(lookup.tool)
              .toRight(McpError.NotLooked(s"${lookup.tool} is not a read-only tool it lists"))
            result <- exchange(Rpc.Call.CallTool(listed, ujson.Obj(lookup.argument -> value))).left
              .map(failed)
          } yield looked + (lookup.tool -> result)
      }
    )
  }

  /** Every page of the list, from the first; the read-only tools on it that a lookup names; and
    * the reading until which it is fresh.
    */
  private def list(): Either[McpError, (Listed, Map[String, McpTool], Long)] = {
    def from(
        cursor: Option[String],
        pages: Int,
        read: Vector[(McpTool.Page, Long)]
    ): Either[McpError, Vector[(McpTool.Page, Long)]] =
      if (pages >= MaxPages)
        Left(McpError.Unreadable(s"tools/list ran past $MaxPages pages"))
      else
        exchange(Rpc.Call.ListTools(cursor)).flatMap(McpTool.page(_, server.name)).flatMap { page =>
          val received = (page, fresh(clock.millis(), page.ttlMs))
          page.next match {
            case None => Right(read :+ received)
            case next => from(next, pages + 1, read :+ received)
          }
        }
    from(None, 0, Vector.empty).map { read =>
      val pages = read.map(_._1)
      val listed = pages.flatMap(_.tools).distinctBy(_.name)
      val skipped = pages.flatMap(_.skipped)
      val (allowed, unallowed) =
        listed.partition(t => server.allow.isEmpty || server.allow.contains(t.name))
      val held = allowed.map(t => (t, server.scope.excludes(t)))
      val looked = server.lookups.values.flatten.map(_.tool).toSet
      (
        Listed(
          held.collect { case (t, None) => t },
          skipped ++ unallowed.map(t => Skipped.NotAllowed(t.name)) ++ held.flatMap(_._2),
          server.allow -- listed.map(_.name) -- skipped.flatMap(named)
        ),
        listed.filter(t => looked.contains(t.name)).map(t => t.name -> t).toMap,
        read.map(_._2).minOption.getOrElse(Long.MinValue)
      )
    }
  }

  /** Ends this client's transport: nothing for HTTP, whose every request is its own. */
  def close(): Unit = connection.close()

  /** The result of one request making `call`. */
  private def exchange(call: Rpc.Call): Either[McpError, ujson.Obj] = connection.exchange(call)
}

object McpClient {

  /** `server`'s client over Streamable HTTP: each request a POST of its own (no session)
    * carrying the bearer `authorizer` gives, timing out after [[Timeout]]. A request for which
    * it gives none is not sent, and is its [[McpError.NoToken]].
    */
  def apply(
      server: McpServer[McpTransport.Http],
      authorizer: Authorizer,
      clock: Clock
  ): McpClient^{clock} =
    new McpClient(server, new HttpConnection(server.transport, authorizer, clock), clock)

  /** How long one request may take: 60 seconds, well under the 11 minutes a turn waits on a
    * claimed request.
    */
  val Timeout: FiniteDuration = 60.seconds

  /** JSON-RPC's Invalid params, which a server answers for an unknown tool. */
  private val InvalidParams = -32602

  /** The most pages one listing reads: 100. */
  val MaxPages: Int = 100

  /** The listed tool `skipped` names, `None` for an entry with no name it can read. */
  private def named(skipped: Skipped): Option[String] = skipped match {
    case Skipped.NotReadOnly(tool) => Some(tool)
    case Skipped.BadName(tool, _) => Some(tool)
    case Skipped.BadHeader(tool, _) => Some(tool)
    case Skipped.NotAllowed(tool) => Some(tool)
    case Skipped.OutOfScope(tool) => Some(tool)
    case Skipped.DeclaresField(tool) => Some(tool)
    case Skipped.Malformed(_) => None
  }

  /** The reading until which a page received at `received` with `ttlMs` is fresh. */
  private def fresh(received: Long, ttlMs: Long): Long =
    if (ttlMs > Long.MaxValue - received) Long.MaxValue else received + ttlMs

}
