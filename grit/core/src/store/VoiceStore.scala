package grit.core.store

import grit.core.prompt.Voice

/** The voice grit talks to the person in, one for the whole database. */
trait VoiceStore {

  /** The voice set; [[Voice.Default]] when none is, or when the one stored is a name this
    * build does not know (set by another build).
    */
  def current()(using Tx^): Either[StoreError, Voice]

  /** Makes `voice` the voice. */
  def set(voice: Voice)(using Tx^): Either[StoreError, Unit]
}
