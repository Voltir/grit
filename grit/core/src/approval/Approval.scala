package grit.core.approval

/** A person's answer to a gated tool call. */
enum Approval {

  /** It may run. */
  case Approved

  /** It may not, for `reason` if they gave one. */
  case Declined(reason: Option[String])

  /** No answer came before the wait ran out; the call does not run. */
  case TimedOut
}
