package grit.core.place

/** An outside service's place: `service:{name}`, `name` a lowercase letter, then lowercase
  * letters, digits or `_`.
  */
final case class Service private (name: String) {

  def place: Place = Place.under(Namespace.Service, Vector(name))
}

object Service {

  /** `name`'s service, or why not, naming the rule it breaks. */
  def of(name: String): Either[String, Service] =
    if (
      name.headOption.exists(c => c >= 'a' && c <= 'z') &&
      name.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_')
    ) Right(new Service(name))
    else Left(s"a service's name is a lowercase letter, then lowercase letters, digits or _: $name")
}

/** Conversations within `within` that have no directory work in `service`: their hosted
  * calls go to the edge hosting it.
  */
final case class WorksIn(within: Place, service: Service)

object WorksIn {

  /** The service of the first of `links` whose `within` holds `place`; `None` when none does. */
  def of(links: Vector[WorksIn], place: Place): Option[Service] =
    links.find(l => place.within(l.within)).map(_.service)
}
