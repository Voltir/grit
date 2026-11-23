package grit.core.place

/** A declared repository's place, `repository:{name}`, `name` as a [[Service]]'s. */
final case class Repository private (name: String) {
  def place: Place = Place.under(Namespace.Repository, Vector(name))
}

object Repository {

  /** `name`'s repository, or why not, naming the rule it breaks. */
  def of(name: String): Either[String, Repository] =
    Service.named("a repository's name", name).map(new Repository(_))
}
