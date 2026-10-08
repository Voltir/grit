package grit.core.admin

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.visibility.{Compartment, Label}

/** The stored JSON form of a [[Change]], as its audit row keeps it, under its
  * [[Change.kind]]. Written by hand: it is persisted data, so a rename in Scala must not change
  * it, and reading it is total.
  */
object ChangeJson {

  def write(c: Change): ujson.Value = c match {
    case Change.Relabel(room, from, to) =>
      val default = to match {
        case Change.To.Set(_) => false
        case Change.To.Default(_) => true
      }
      ujson.Obj(
        "kind" -> c.kind,
        "room" -> room.written,
        "from" -> Label.written(from),
        "to" -> Label.written(to.label),
        "default" -> default
      )
    case Change.Quiet(room, on) =>
      ujson.Obj("kind" -> c.kind, "room" -> room.written, "on" -> on)
    case Change.Clear(person, compartment) => membership(c.kind, person, compartment)
    case Change.Remove(person, compartment) => membership(c.kind, person, compartment)
  }

  /** The change `v` encodes, or why it encodes none. */
  def read(v: ujson.Value): Either[String, Change] =
    for {
      o <- v.objOpt.toRight("a change is not an object")
      kind <- str(o, "kind").left.map(why => s"a change: $why")
      c <- of(kind, o).fold(Left(s"unknown change: $kind"))(_.left.map(why => s"$kind: $why"))
    } yield c

  private type Fields = collection.Map[String, ujson.Value]

  /** The change of `kind` that `o` holds, or why not; `None` when no change is of `kind`. */
  private def of(kind: String, o: Fields): Option[Either[String, Change]] = kind match {
    case "relabel" =>
      Some(for {
        room <- str(o, "room").flatMap(Place.read)
        from <- str(o, "from").flatMap(Label.read)
        to <- str(o, "to").flatMap(Label.read)
        default <- bool(o, "default")
      } yield Change.Relabel(room, from, if (default) Change.To.Default(to) else Change.To.Set(to)))
    case "quiet" =>
      Some(for {
        room <- str(o, "room").flatMap(Place.read)
        on <- bool(o, "on")
      } yield Change.Quiet(room, on))
    case "clear" => Some(membership(o).map(Change.Clear(_, _)))
    case "remove" => Some(membership(o).map(Change.Remove(_, _)))
    case _ => None
  }

  private def membership(kind: String, person: Account, compartment: Compartment): ujson.Value =
    ujson.Obj(
      "kind" -> kind,
      "person" -> Account.written(person),
      "compartment" -> Compartment.name(compartment)
    )

  private def membership(o: Fields): Either[String, (Account, Compartment)] =
    for {
      person <- str(o, "person").flatMap(Account.read)
      compartment <- str(o, "compartment").flatMap(Compartment.of)
    } yield (person, compartment)

  private def str(o: Fields, key: String): Either[String, String] =
    o.get(key).flatMap(_.strOpt).toRight(s"no $key")

  private def bool(o: Fields, key: String): Either[String, Boolean] =
    o.get(key).flatMap(_.boolOpt).toRight(s"no $key")
}
