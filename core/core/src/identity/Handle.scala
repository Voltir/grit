package grit.core.identity

/** What a deployment calls a person it declares: lowercase letters, digits and `-`, 1 to 32. */
opaque type Handle = String

object Handle {

  /** `name` as a handle, or why not, naming the rule it breaks. */
  def of(name: String): Either[String, Handle] =
    Either.cond(
      name.length >= 1 && name.length <= 32 &&
        name.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'),
      name,
      s"a handle is 1 to 32 lowercase letters, digits or -: $name"
    )

  def value(h: Handle): String = h
}
