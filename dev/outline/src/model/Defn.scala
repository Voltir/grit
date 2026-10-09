package grit.outline.model

/** What kind of definition a TASTy entry records. */
enum Kind {
  case Class, CaseClass, Trait, Object, Enum, EnumCase, Def, Val, Var, Given, TypeAlias, Opaque
}

/** A 1-based, inclusive line range; `start` is the Scaladoc's first line when there is one. */
final case class Lines(start: Int, end: Int)

/** A type a signature names; `topLevel` names the `.tasty` file it lives in. */
final case class Ref(fullName: String, topLevel: String, inRepo: Boolean)

/** One definition read from the TASTy: `file` is repo-relative, `body` and `doc` are verbatim source text. */
final case class Defn(
    kind: Kind,
    name: String,
    fullName: String,
    file: String,
    lines: Lines,
    doc: Option[String],
    signature: String,
    body: Option[String],
    members: Vector[Defn],
    refs: Vector[Ref],
    parents: Vector[String],
    isPrivate: Boolean,
    isAbstract: Boolean
)
