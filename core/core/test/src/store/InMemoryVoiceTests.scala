package grit.core.store

import grit.dbos.sql.TestTx

/** The voice contract, kept by the in-memory fake. */
object InMemoryVoiceTests extends VoiceContract {

  protected def fresh(): VoiceStore = new InMemoryVoiceStore

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
