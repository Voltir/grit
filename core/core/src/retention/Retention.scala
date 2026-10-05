package grit.core.retention

import scala.concurrent.duration.FiniteDuration

/** How long a kind of target is kept after its tombstone. */
enum Retention {
  case For(window: FiniteDuration)

  /** As its plugin's document terms declare. */
  case Declared
}
