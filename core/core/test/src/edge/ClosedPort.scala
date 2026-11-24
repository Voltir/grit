package grit.core.edge

import java.net.{InetSocketAddress, Socket}

/** A port on 127.0.0.1 that refuses every connection, for a test of a dial that fails. */
object ClosedPort {

  /** The socket holding [[port]] for the tests' run: bound, never connected or listening.
    * Never closed on purpose; the JVM's exit releases it.
    */
  private lazy val held: Socket = {
    val socket = new Socket()
    socket.bind(new InetSocketAddress("127.0.0.1", 0))
    socket
  }

  /** A port that refuses every connection and that nothing else binds while the tests run. */
  def port: Int = held.getLocalPort
}
