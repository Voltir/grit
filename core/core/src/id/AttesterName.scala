package grit.core.id

/** An attester's name, as a deployment's trust and the log state it (`slack`): a source the
  * deployment trusts to say who a realm's accounts are ([[grit.core.identity.Vouching]]).
  */
opaque type AttesterName = String

object AttesterName {
  def apply(value: String): AttesterName = value
  def value(name: AttesterName): String = name

  given CanEqual[AttesterName, AttesterName] = CanEqual.derived
}
