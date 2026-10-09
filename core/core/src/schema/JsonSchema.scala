package grit.core.schema

/** A JSON Schema in the subset every provider's strict mode accepts, so any upstream can be
  * sent it `strict` and grit's own check means what the provider's does. Read only by
  * [[JsonSchema.read]].
  */
final class JsonSchema private (root: JsonSchema.Node) extends caps.Pure {

  /** Its JSON, as a provider is sent it: rebuilt from what was read, every keyword in a fixed
    * order and each object's properties in the order read.
    */
  def json: ujson.Obj = JsonSchema.write(root)
}

object JsonSchema {

  /** `json` as a schema, or the first thing in it, in document order, that the subset
    * refuses:
    *   - the root is not `"type": "object"`;
    *   - a keyword other than `type`, `description`, `properties`, `required`,
    *     `additionalProperties`, `items`, `enum`, `minimum`, `maximum`, `minItems`,
    *     `maxItems` (`$ref`, `anyOf`, `oneOf`, `allOf`, `not`, `const`, `pattern`, `format`,
    *     `default`, `$schema` and `title` among them);
    *   - a `type` that is not one of `string`, `number`, `integer`, `boolean`, `object`,
    *     `array`, or a list of one of those and `null`;
    *   - an object without `"additionalProperties": false`, or with a property missing from
    *     `required`, or naming in `required` a property it lacks (strict mode: every property
    *     is required, and one that may be left out admits `null` instead);
    *   - an array without `items`;
    *   - a keyword on a type it does not apply to (a bound, `properties`, `required`,
    *     `additionalProperties` or `items`), or a minimum above its maximum;
    *   - an empty `enum`, or one holding a value its `type` refuses;
    *   - deeper than [[MaxDepth]] objects or arrays, more than [[MaxProperties]] properties
    *     in all, or an `enum` of more than [[MaxEnum]] values.
    */
  def read(json: ujson.Value): Either[SchemaError, JsonSchema] =
    if (!json.objOpt.exists(_.get("type").contains(ujson.Str("object")))) {
      Left(SchemaError("", "the root must be \"type\": \"object\""))
    } else {
      parse(json).map(new JsonSchema(_))
    }

  /** 5: nesting of objects and arrays below the root. */
  val MaxDepth: Int = 5

  /** 100: properties across the whole schema. */
  val MaxProperties: Int = 100

  /** 500: values in one `enum`. */
  val MaxEnum: Int = 500

  /** The JSON types a schema may name, besides `null`. */
  private[schema] enum Base(val word: String, val described: String) {
    case Text extends Base("string", "a string")
    case Number extends Base("number", "a number")
    case Whole extends Base("integer", "an integer")
    case Flag extends Base("boolean", "a boolean")
    case Object extends Base("object", "an object")
    case List extends Base("array", "an array")

    /** Whether `v`, not `null`, is of this type. */
    def admits(v: ujson.Value): Boolean = this match {
      case Text => v.strOpt.isDefined
      case Number => v.numOpt.isDefined
      case Whole => v.numOpt.exists(_.isWhole)
      case Flag => v.boolOpt.isDefined
      case Object => v.objOpt.isDefined
      case List => v.arrOpt.isDefined
    }
  }

  /** What a schema holds beyond its type, shaped by its type. */
  private[schema] enum Shape {
    case Plain
    case Bounded(minimum: Option[Double], maximum: Option[Double])
    case Properties(properties: Vector[(String, Node)])
    case Items(item: Node, minItems: Option[Int], maxItems: Option[Int])
  }

  /** A schema read: `options` are its enum's values, each written afresh when sent. */
  private[schema] final case class Node(
      base: Base,
      nullable: Boolean,
      description: Option[String],
      options: Option[Vector[ujson.Value]],
      shape: Shape
  ) {

    /** Its type as a person reads it: "an integer", "a string or null". */
    def described: String = if (nullable) s"${base.described} or null" else base.described

    /** Whether `v` is of its type, `null` included when it is nullable. */
    def admits(v: ujson.Value): Boolean = if (v.isNull) nullable else base.admits(v)
  }

  private val Keywords: Set[String] = Set(
    "type",
    "description",
    "properties",
    "required",
    "additionalProperties",
    "items",
    "enum",
    "minimum",
    "maximum",
    "minItems",
    "maxItems"
  )

  /** Which keywords apply only to which type. */
  private val OnlyOn: Vector[(String, Base)] = Vector(
    "properties" -> Base.Object,
    "required" -> Base.Object,
    "additionalProperties" -> Base.Object,
    "items" -> Base.List,
    "minItems" -> Base.List,
    "maxItems" -> Base.List
  )

  private val TypeWords: String =
    "`type` must be one of string, number, integer, boolean, object, array, " +
      "or a list of one of those and null"

  /** The most characters of a JSON value a refusal or mismatch quotes. */
  private val Shown = 100

  /** `v` written compactly, cut to [[Shown]] characters. */
  private[schema] def shown(v: ujson.Value): String = {
    val s = v.render()
    if (s.length <= Shown) s else s"${s.take(Shown - 1)}…"
  }

  /** A deep copy of `v`. */
  private[schema] def copy(v: ujson.Value): ujson.Value = ujson.read(v.render())

  /** `results`, or the first `Left` among them, the rest not run. */
  private[schema] def all[E, A](results: Iterator[Either[E, A]]^): Either[E, Vector[A]] = {
    var read = Vector.empty[A]
    var failed: Option[E] = None
    while (failed.isEmpty && results.hasNext) {
      results.next() match {
        case Left(e) => failed = Some(e)
        case Right(a) => read = read :+ a
      }
    }
    failed.toLeft(read)
  }

  /** `root` read: `seen` counts the properties met so far across the whole of it. */
  private def parse(root: ujson.Value): Either[SchemaError, Node] = {
    var seen = 0

    /** The schema `json` at `path`, `depth` objects or arrays below the root. */
    def node(json: ujson.Value, path: String, depth: Int): Either[SchemaError, Node] = {
      def refuse[A](why: String): Either[SchemaError, A] = Left(SchemaError(path, why))
      json.objOpt match {
        case None => refuse("a schema must be a JSON object")
        case Some(o) =>
          for {
            _ <- o.keys.find(k => !Keywords.contains(k)) match {
              case Some(k) => refuse(s"`$k` is not in the subset every strict mode accepts")
              case None => Right(())
            }
            typed <- o.get("type") match {
              case None => refuse("has no `type`")
              case Some(t) =>
                readType(t).toRight(SchemaError(path, s"$TypeWords; got ${shown(t)}"))
            }
            (base, nullable) = typed
            description <- o.get("description") match {
              case None => Right(None)
              case Some(d) =>
                d.strOpt.map(Some(_)).toRight(SchemaError(path, "`description` must be a string"))
            }
            _ <- OnlyOn
              .find((k, b) => o.contains(k) && b != base)
              .orElse(
                Vector("minimum", "maximum")
                  .find(o.contains)
                  .filter(_ => base != Base.Number && base != Base.Whole)
                  .map(_ -> base)
              ) match {
              case Some((k, _)) => refuse(s"`$k` does not apply to ${base.described}")
              case None => Right(())
            }
            _ <-
              if ((base == Base.Object || base == Base.List) && depth > MaxDepth) {
                refuse(s"objects and arrays nest more than $MaxDepth deep below the root")
              } else Right(())
            options <- o.get("enum") match {
              case None => Right(None)
              case Some(e) =>
                e.arrOpt.map(_.toVector) match {
                  case None => refuse("`enum` must be a list")
                  case Some(vs) if vs.isEmpty => refuse("`enum` is empty")
                  case Some(vs) if vs.size > MaxEnum =>
                    refuse(s"`enum` holds ${vs.size} values; at most $MaxEnum are allowed")
                  case Some(vs) =>
                    vs.find(v => if (v.isNull) !nullable else !base.admits(v)) match {
                      case Some(v) => refuse(s"`enum` holds ${shown(v)}, which its type refuses")
                      case None => Right(Some(vs.map(copy)))
                    }
                }
            }
            shape <- base match {
              case Base.Text | Base.Flag => Right(Shape.Plain)
              case Base.Number | Base.Whole => bounded(o, path)
              case Base.Object => properties(o, path, depth)
              case Base.List => items(o, path, depth)
            }
          } yield Node(base, nullable, description, options, shape)
      }
    }

    def bounded(
        o: collection.Map[String, ujson.Value],
        path: String
    ): Either[SchemaError, Shape] = {
      def bound(k: String): Either[SchemaError, Option[Double]] = o.get(k) match {
        case None => Right(None)
        case Some(v) => v.numOpt.map(Some(_)).toRight(SchemaError(path, s"`$k` must be a number"))
      }
      for {
        min <- bound("minimum")
        max <- bound("maximum")
        _ <- ordered(min, max, "minimum", "maximum", path)
      } yield Shape.Bounded(min, max)
    }

    def items(
        o: collection.Map[String, ujson.Value],
        path: String,
        depth: Int
    ): Either[SchemaError, Shape] = {
      def count(k: String): Either[SchemaError, Option[Int]] = o.get(k) match {
        case None => Right(None)
        case Some(v) =>
          v.numOpt
            .filter(n => n.isWhole && n >= 0 && n <= Int.MaxValue)
            .map(n => Some(n.toInt))
            .toRight(SchemaError(path, s"`$k` must be a whole number of at least 0"))
      }
      for {
        min <- count("minItems")
        max <- count("maxItems")
        _ <- ordered(min.map(_.toDouble), max.map(_.toDouble), "minItems", "maxItems", path)
        sent <- o.get("items").toRight(SchemaError(path, "an array must have `items`"))
        item <- node(sent, within(path, "items"), depth + 1)
      } yield Shape.Items(item, min, max)
    }

    def properties(
        o: collection.Map[String, ujson.Value],
        path: String,
        depth: Int
    ): Either[SchemaError, Shape] = {
      def refuse[A](why: String): Either[SchemaError, A] = Left(SchemaError(path, why))
      for {
        _ <-
          if (o.get("additionalProperties").contains(ujson.False)) Right(())
          else refuse("an object must have \"additionalProperties\": false")
        sent <- o.get("properties") match {
          case None => Right(Vector.empty)
          case Some(p) =>
            p.objOpt.map(_.toVector).toRight(SchemaError(path, "`properties` must be an object"))
        }
        required <- o.get("required") match {
          case None => Right(Vector.empty)
          case Some(r) =>
            r.arrOpt
              .map(_.toVector)
              .filter(_.forall(_.strOpt.isDefined))
              .map(_.flatMap(_.strOpt))
              .toRight(SchemaError(path, "`required` must be a list of property names"))
        }
        names = sent.map(_._1)
        _ <- names.find(n => !required.contains(n)) match {
          case Some(n) =>
            refuse(
              s"`$n` must be in `required`: under strict mode every property is, and one " +
                "that may be left out admits null instead"
            )
          case None => Right(())
        }
        _ <- required.find(n => !names.contains(n)) match {
          case Some(n) => refuse(s"`required` names `$n`, which is not a property")
          case None => Right(())
        }
        read <- all(sent.iterator.map { (name, schema) =>
          val at = within(path, s"properties.$name")
          seen += 1
          if (seen > MaxProperties) {
            Left(SchemaError(at, s"more than $MaxProperties properties in all"))
          } else node(schema, at, depth + 1).map(name -> _)
        })
      } yield Shape.Properties(read)
    }

    def ordered(
        min: Option[Double],
        max: Option[Double],
        minWord: String,
        maxWord: String,
        path: String
    ): Either[SchemaError, Unit] =
      min.zip(max).find((a, b) => a > b) match {
        case Some((a, b)) =>
          Left(
            SchemaError(
              path,
              s"`$minWord` ${shown(ujson.Num(a))} is above `$maxWord` ${shown(ujson.Num(b))}"
            )
          )
        case None => Right(())
      }

    node(root, "", 0)
  }

  /** The type `t` names, and whether it admits `null`. */
  private def readType(t: ujson.Value): Option[(Base, Boolean)] = {
    def base(s: String): Option[Base] = Base.values.find(_.word == s)
    t match {
      case ujson.Str(s) => base(s).map(_ -> false)
      case ujson.Arr(ts) =>
        ts.toVector match {
          case Vector(ujson.Str("null"), ujson.Str(s)) => base(s).map(_ -> true)
          case Vector(ujson.Str(s), ujson.Str("null")) => base(s).map(_ -> true)
          case _ => None
        }
      case _ => None
    }
  }

  /** `path` joined with `step`, a schema's path. */
  private def within(path: String, step: String): String =
    if (path.isEmpty) step else s"$path.$step"

  /** `node` as a provider is sent it. */
  private def write(node: Node): ujson.Obj = {
    val out = ujson.Obj(
      "type" ->
        (if (node.nullable) ujson.Arr(node.base.word, "null") else ujson.Str(node.base.word))
    )
    node.description.foreach(d => out("description") = d)
    node.options.foreach(vs => out("enum") = ujson.Arr.from(vs.map(copy)))
    node.shape match {
      case Shape.Plain => ()
      case Shape.Bounded(min, max) =>
        min.foreach(m => out("minimum") = m)
        max.foreach(m => out("maximum") = m)
      case Shape.Properties(properties) =>
        out("properties") = ujson.Obj.from(properties.map((n, p) => n -> write(p)))
        out("required") = ujson.Arr.from(properties.map((n, _) => ujson.Str(n)))
        out("additionalProperties") = false
      case Shape.Items(item, min, max) =>
        out("items") = write(item)
        min.foreach(m => out("minItems") = m)
        max.foreach(m => out("maxItems") = m)
    }
    out
  }
}
