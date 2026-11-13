package grit.commands.edge

import java.io.{InputStream, OutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets.UTF_8

import grit.core.admin.InMemoryAdministration
import grit.core.document.InMemoryDocuments
import grit.core.edge.{
  Attesting,
  EdgeSessions,
  EdgeStores,
  EdgeWatch,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  InMemoryOrigins,
  PluginCommands
}
import grit.core.files.NoIntake
import grit.core.inbox.InMemoryInbox
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{TestJot, Voucher}

/** What the edge's suites share: the stores it is opened over, and plain HTTP to its socket. */
object EdgeFixtures {

  /** In-memory stores with `commands`, `sessions` and `voucher`'s attesting, and no watch. */
  def stores(
      commands: PluginCommands,
      voucher: Voucher,
      sessions: EdgeSessions = EdgeSessions.Refusing
  ): EdgeStores^ = {
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    val jot = new TestJot
    EdgeStores(
      inbox,
      InMemoryAdministration.none(),
      InMemoryJoins.none(),
      inbox.principals,
      new InMemoryDeliveries,
      inbox.ledger,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      new InMemoryOrigins(inbox.conversations),
      (new InMemoryDocuments).reviews.of,
      jot,
      new InMemoryEdges,
      new Attesting(voucher, jot, _ => ()),
      new NoIntake,
      commands = commands,
      sessions = sessions,
      watch = EdgeWatch.Refusing
    )
  }

  /** `raw`, an HTTP/1.1 request, sent to `port` on this machine: the status line and the body. */
  def sent(port: Int, raw: String): (String, String) = {
    val socket = new Socket("127.0.0.1", port)
    try {
      val out: OutputStream = socket.getOutputStream
      out.write(raw.getBytes(UTF_8))
      out.flush()
      val in: InputStream = socket.getInputStream
      val whole = new String(in.readAllBytes(), UTF_8)
      (whole.takeWhile(_ != '\r'), whole.split("\r\n\r\n", 2).lift(1).getOrElse(""))
    } finally socket.close()
  }

  /** A POST of `body` to `target` at `port`, naming `host`. */
  def post(port: Int, host: String, target: String, body: String): (String, String) =
    sent(
      port,
      s"POST $target HTTP/1.1\r\nHost: $host\r\nConnection: close\r\n" +
        s"Content-Length: ${body.getBytes(UTF_8).length}\r\n\r\n$body"
    )

  /** A GET of `target` at `port`, naming `host`. */
  def get(port: Int, host: String, target: String): (String, String) =
    sent(port, s"GET $target HTTP/1.1\r\nHost: $host\r\nConnection: close\r\n\r\n")
}
