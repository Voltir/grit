package grit.eval.harness.run

/** How many times a run asks each request. */
object Repeats {

  /** Jev answers one request differently from call to call, and the same input differently from
    * the call it made live: across five repeats of the same requests, each probability's
    * standard deviation was 0.004 at the median and up to 0.08, though no triage case's likeliest
    * kind changed. Three repeats, averaged, cut that deviation to about 0.05 at worst, at three
    * times one run's cost.
    */
  val Default = 3
}
