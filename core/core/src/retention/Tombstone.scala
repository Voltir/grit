package grit.core.retention

import java.time.Instant

/** A decision to delete `target`, made at `written`. */
final case class Tombstone(target: Target, written: Instant)
