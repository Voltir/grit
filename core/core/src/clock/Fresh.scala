package grit.core.clock

import java.util.UUID

/** Capability to make a value no one has made before. */
trait Fresh extends caps.SharedCapability {

  /** A string no other call returns, in this process or any other. */
  def nonce(): String
}

object Fresh {

  /** Nonces that are random UUIDs. */
  def random(): Fresh^ = new Fresh {
    def nonce(): String = UUID.randomUUID().toString
  }
}
