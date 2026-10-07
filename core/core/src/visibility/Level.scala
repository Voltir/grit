package grit.core.visibility

/** How sensitive a thing is, on core's one scale: each level is above those before it. Fixed
  * by grit; no deployment adds, replaces or renames one.
  */
enum Level {
  case Public, Internal, Confidential, Restricted
}
