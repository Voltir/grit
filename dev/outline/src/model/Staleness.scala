package grit.outline.model

/** Whether a source file is older than the TASTy compiled from it: `Stale` when the source is newer, `NoTasty` when there is none. */
enum Staleness {
  case Fresh
  case Stale(source: java.time.Instant, tasty: java.time.Instant)
  case NoTasty
}
