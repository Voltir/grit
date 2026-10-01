package grit.host

import utest.*

/** [[LocalMachine]]: this process as the operating system names it. */
object LocalMachineTests extends TestSuite {

  val tests = Tests {
    test("the identity names this process's pid and this machine's host name") {
      LocalMachine.identity() ==> grit.core.host.ProcessIdentity(
        java.net.InetAddress.getLocalHost.getHostName,
        ProcessHandle.current().pid()
      )
    }
  }
}
