package grit.claudecode

import java.net.{BindException, ConnectException, ServerSocket, Socket}

import utest.*

/** [[HookFixtures.closedPort]]: a port nothing listens on, that no other suite's server can take
  * while the tests run.
  */
object ClosedPortTests extends TestSuite {

  val tests = Tests {
    test("a closed port refuses a connection, and no server can bind it") {
      val port = HookFixtures.closedPort
      assertThrows[ConnectException] { new Socket("127.0.0.1", port).close() }
      assertThrows[BindException] { new ServerSocket(port).close() }
    }
  }
}
