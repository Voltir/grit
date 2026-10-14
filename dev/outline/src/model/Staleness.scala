package grit.outline.model

/** Whether a source file changed since the TASTy compiled from it: `Stale` with the source's and that TASTy's mtimes, `NoTasty` when there is none, `NoSource` when the source file is missing. */
enum Staleness {
  case Fresh
  case Stale(source: java.time.Instant, tasty: java.time.Instant)
  case NoTasty
  case NoSource
}
