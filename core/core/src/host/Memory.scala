package grit.core.host

/** How much memory this machine's processes hold, read as often as its caller asks. */
trait Memory extends caps.SharedCapability {

  /** What every process of this user working in `dir` or below it (its working directory there)
    * holds resident, together, and what the machine has available. `Left(Failed)` when the
    * machine does not say; `Left(NotFound)` or `Left(Outside)` for `dir` as for
    * [[Processes.run]].
    */
  def held(dir: RelPath): Either[HostError, Memory.Held]
}

object Memory {

  /** `resident` bytes held by the processes asked about, and `available` bytes the machine can
    * still give without swapping.
    */
  final case class Held(resident: Long, available: Long) extends caps.Pure
}
