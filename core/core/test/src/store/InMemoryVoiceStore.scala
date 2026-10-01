package grit.core.store

import grit.core.prompt.Voice

/** An in-memory [[VoiceStore]] for tests, keeping [[VoiceContract]]. */
final class InMemoryVoiceStore extends VoiceStore {

  // Only ever replaced by a new immutable value, as the store's row would be.
  @caps.unsafe.untrackedCaptures
  private var stored = Option.empty[Voice]

  def current()(using Tx^): Either[StoreError, Voice] = Right(stored.getOrElse(Voice.Default))

  def set(voice: Voice)(using Tx^): Either[StoreError, Unit] = {
    stored = Some(voice)
    Right(())
  }
}
