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

/** Conversations within `within` are also offered, on turns addressed to grit, the tools an
  * edge serves at `service`, beside their workspace's: tools that act beyond the
  * conversation, such as posting elsewhere. A turn nobody addressed, rooted on a message grit
  * heard, is never offered them.
  */
final case class Reaches(within: Place, service: Service)

object Reaches {

  /** The services of every one of `links` whose `within` holds `place`, each once, in the
    * order first linked.
    */
  def of(links: Vector[Reaches], place: Place): Vector[Service] =
    links.filter(l => place.within(l.within)).map(_.service).distinct
}
