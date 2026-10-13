package grit.core.visibility

/** Test fixture: a clearance that rises once, as a conversation's can while a turn runs: `from`
  * until [[rise]], `to` after.
  */
final class RisingClearance(from: Label, to: Label) {
  // Set once, by the test that owns it.
  @caps.unsafe.untrackedCaptures
  private var risen = false

  def rise(): Unit = risen = true

  def now: Clearance = Clearance.of(if (risen) to else from)
}
