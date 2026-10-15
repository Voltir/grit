package grit.core.sanitise

/** Why the integrity tree kept a text as it came, or what it noticed of one: a flag code
  * measured ([[HardFlags]]), or what the classifier answered ([[Suspicions]]).
  */
enum Reason extends caps.Pure {

  /** Characters a reader does not see (zero-width and invisible formatting characters,
    * bidirectional controls, tag characters, the soft hyphen), or a word mixing Latin letters
    * with Cyrillic or Greek ones.
    */
  case Hidden

  /** An encoded blob: a run of at least 32 characters of base64's alphabet holding both cases
    * and a digit, or of more than 64 hex digits (so no git or SHA-256 digest is one).
    */
  case Encoded

  /** A tag grit begins its own window lines with ([[grit.core.context.SectionTag]]). */
  case Marker

  /** More than [[IntegrityTree.Bound]] characters: more than the classifier is trusted to judge. */
  case OverBound

  /** It addresses its reader with directives to carry out. */
  case Directive

  /** It claims an identity or authority over its reader. */
  case Identity

  /** It asks its reader to do something beyond reading it. */
  case Act

  /** It presses its reader with urgency, secrecy or threats. */
  case Pressure

  /** A part of it is out of place for its kind of document. */
  case OutOfPlace

  /** The classifier gave no answer it could have raised the text on: unavailable, not cleared
    * for what the text was read under, or answering what does not read.
    */
  case Unjudged
}
