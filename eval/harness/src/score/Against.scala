package grit.eval.harness.score

/** Which strands each opening of a lab run is placed against. */
enum Against(val label: String) {

  /** Those its earlier openings' placings made, so one misplaced opening changes what every
    * later one is offered: a cascade, as stitching runs.
    */
  case Placings extends Against("cascading")

  /** Those the room's truth makes, each earlier opening linked as its topic says whatever it
    * was placed, so each placing is scored on its own.
    */
  case Truth extends Against("against the truth strands")
}
