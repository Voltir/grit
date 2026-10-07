package grit.core.visibility

/** How sensitive a thing is, on core's one scale: each level is above those before it. Fixed
  * by grit; no deployment adds, replaces or renames one.
  */
enum Level {
  case Public, Internal, Confidential, Restricted
}

object Level {

  /* Its name, as a label's written form holds it, and so never a compartment's: changing one
   * changes every written label. */
  private[visibility] def name(level: Level): String = level match {
    case Public => "public"
    case Internal => "internal"
    case Confidential => "confidential"
    case Restricted => "restricted"
  }

  private[visibility] def named(name: String): Option[Level] =
    values.find(this.name(_) == name)
}
