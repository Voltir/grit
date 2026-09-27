package grit.host

import java.net.InetAddress

import scala.util.control.NonFatal

import grit.core.host.ProcessIdentity

/** This machine and this process, as the operating system names them. */
object LocalMachine {

  /** This process's identity: the machine's host name (`unknown` when it cannot be read) and
    * this process's pid.
    */
  def identity(): ProcessIdentity = {
    val machine =
      try InetAddress.getLocalHost.getHostName
      catch { case NonFatal(_) => "unknown" }
    ProcessIdentity(machine, ProcessHandle.current().pid())
  }
}
