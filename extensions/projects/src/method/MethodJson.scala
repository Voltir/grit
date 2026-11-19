package grit.projects.method

import grit.core.json.Fields
import grit.projects.artifact.{ArtifactJson, Project}
import grit.projects.name.{
  ArtifactRef,
  CheckName,
  Handle,
  KindName,
  MethodName,
  RoleName,
  StateName
}
import grit.std.either.Traversal.traverse

/** A method and the worked examples it is declared with, which [[MethodBench]] replays. */
final case class Declared(method: Method, examples: Vector[Example]) extends caps.Pure

/** A method and its worked examples as JSON: the form a method is declared and exported in. */
object MethodJson {

  /** `method` and its worked examples in the form a method is declared and exported in; [[read]]
    * gives them back under an equal digest.
    */
  def written(method: Method, examples: Vector[Example]): ujson.Obj =
    ujson.Obj(
      "name" -> MethodName.value(method.name),
      "version" -> method.version,
      "kinds" -> ujson.Arr.from(method.kinds.map { k =>
        val o = ujson.Obj(
          "name" -> KindName.value(k.name),
          "first" -> StateName.value(k.first),
          "states" -> strings(k.states.map(StateName.value)),
          "ends" -> strings(k.ends.toVector.map(StateName.value).sorted),
          "sections" -> strings(k.sections)
        )
        k.nesting match {
          case Nesting.Anywhere => ()
          case Nesting.Root => o("nesting") = RootNesting
          case Nesting.Under(under) =>
            o("nesting") =
              ujson.Obj(UnderNesting -> strings(under.toVector.map(KindName.value).sorted))
        }
        o
      }),
      "transitions" -> ujson.Arr.from(method.transitions.map { t =>
        unlessDefault(
          ujson.Obj(
            "kind" -> KindName.value(t.kind),
            "from" -> StateName.value(t.from),
            "to" -> StateName.value(t.to),
            "checks" -> strings(t.checks.map(CheckName.value)),
            "action" -> (t.action match {
              case TransitionAction.Nothing => ujson.Obj("kind" -> NothingKind)
              case TransitionAction.MergeDeltas(into) =>
                ujson.Obj("kind" -> MergeKind, "into" -> KindName.value(into))
            })
          ),
          "movers",
          t.movers match {
            case Movers.Anyone => None
            case Movers.Person => Some(PeopleMove)
          }
        )
      }),
      "views" -> ujson.Arr.from(method.views.map { v =>
        val o = ujson.Obj(
          "role" -> RoleName.value(v.role),
          "kinds" -> strings(v.kinds.toVector.map(KindName.value).sorted),
          "depth" -> v.depth,
          "carried" -> strings(v.carried.toVector.map(Handle.value).sorted)
        )
        if (v.covers) o("covers") = true
        o
      }),
      "checks" -> ujson.Arr.from(method.checks.sortBy(c => CheckName.value(c.name)).map { c =>
        ujson.Obj("name" -> CheckName.value(c.name), "version" -> c.version)
      }),
      "examples" -> ujson.Arr.from(examples.map { e =>
        ujson.Obj(
          "name" -> e.name,
          "start" -> ArtifactJson.projectData(e.start),
          "moves" -> ujson.Arr.from(e.moves.map { m =>
            unlessDefault(
              ujson.Obj(
                "key" -> m.key.value,
                "to" -> StateName.value(m.to),
                "expect" -> (m.expect match {
                  case Example.Expect.Allowed => ujson.Obj("kind" -> AllowedKind)
                  case Example.Expect.RefusedBy(check) =>
                    ujson.Obj("kind" -> RefusedKind, "check" -> CheckName.value(check))
                  case Example.Expect.RefusedPersonOnly => ujson.Obj("kind" -> PersonOnlyKind)
                })
              ),
              "by",
              m.by match {
                case Mover.Other => None
                case Mover.Person => Some(PersonMover)
              }
            )
          })
        )
      })
    )

  /** The method and examples `json` declares, each check found by name in `compiled`, or every
    * reason not: a field missing or malformed, a check not compiled, or what [[Method.of]]
    * refuses. It does not replay the examples: declaring does.
    */
  def read(
      json: ujson.Value,
      compiled: Vector[TransitionCheck]
  ): Either[Vector[String], Declared] = {
    val top = Fields("the method", json)
    val head = one(for {
      name <- named(top, "name")(MethodName.of)
      version <- top.int("version")
    } yield (name, version))
    val kinds = each(top, "kinds", "kind")(kind)
    val transitions = each(top, "transitions", "transition")(transition)
    val views = each(top, "views", "view")(view)
    val listed = each(top, "checks", "check")(listedIn(compiled))
    val examples = each(top, "examples", "example")(example)
    val method = (head, kinds, transitions, views) match {
      case (Right((name, version)), Right(k), Right(t), Right(v)) =>
        Method.of(name, version, k, t, v, compiled).left.map(_.map(said))
      case _ => Left(Vector.empty)
    }
    val reasons = Vector(head, kinds, transitions, views, listed, examples, method)
      .flatMap(_.left.getOrElse(Vector.empty))
      .distinct
    (method, examples) match {
      case (Right(m), Right(e)) if reasons.isEmpty => Right(Declared(m, e))
      case _ => Left(reasons)
    }
  }

  private val NothingKind = "nothing"
  private val MergeKind = "merge-deltas"
  private val AllowedKind = "allowed"
  private val RefusedKind = "refused-by"
  private val PersonOnlyKind = "refused-person-only"
  private val PeopleMove = "people"
  private val AnywhereNesting = "anywhere"
  private val RootNesting = "root"
  private val UnderNesting = "under"
  private val AnyoneMoves = "anyone"
  private val PersonMover = "person"
  private val AgentMover = "agent"

  /** `o` with `field` set to `value` when there is one: a field at its default is left out. */
  private def unlessDefault(o: ujson.Obj, field: String, value: Option[String]): ujson.Obj = {
    value.foreach(v => o(field) = v)
    o
  }

  private def strings(each: Vector[String]): ujson.Arr = ujson.Arr.from(each.map(ujson.Str(_)))

  private def one[A](read: Either[String, A]): Either[Vector[String], A] = read.left.map(Vector(_))

  /** Each element of `top`'s array `field`, none when it is absent, read by `read` as `what` and
    * its position from 1; every element's reason when any is refused.
    */
  private def each[A](top: Fields, field: String, what: String)(
      read: Fields => Either[String, A]
  ): Either[Vector[String], Vector[A]] =
    top.added(field).toOption.flatten.map(_.arrOpt) match {
      case None => Right(Vector.empty)
      case Some(None) => Left(Vector(s"${top.what}'s $field is not an array"))
      case Some(Some(elements)) =>
        val results =
          elements.toVector.zipWithIndex.map((e, i) => read(Fields(s"$what ${i + 1}", e)))
        val reasons = results.flatMap(_.left.toOption)
        if (reasons.isEmpty) Right(results.flatMap(_.toOption)) else Left(reasons)
    }

  /** `f`'s string `field` as `of` reads it, a refusal naming `f` and the field. */
  private def named[A](f: Fields, field: String)(
      of: String => Either[String, A]
  ): Either[String, A] =
    f.str(field).flatMap(of(_).left.map(why => s"${f.what}'s $field: $why"))

  /** `f`'s array `field`, empty when it is absent. */
  private def array(f: Fields, field: String): Either[String, Vector[ujson.Value]] =
    f.added(field).flatMap {
      case None => Right(Vector.empty)
      case Some(v) => v.arrOpt.map(_.toVector).toRight(s"${f.what}'s $field is not an array")
    }

  /** `f`'s array of strings `field`, each as `of` reads it; none when it is absent. */
  private def names[A](f: Fields, field: String)(
      of: String => Either[String, A]
  ): Either[String, Vector[A]] =
    array(f, field).flatMap(traverse(_) { s =>
      Fields
        .str(s"an element of ${f.what}'s $field", s)
        .flatMap(of(_).left.map(why => s"${f.what}'s $field: $why"))
    })

  private def kind(f: Fields): Either[String, ArtifactKind] =
    for {
      name <- named(f, "name")(KindName.of)
      first <- named(f, "first")(StateName.of)
      states <- names(f, "states")(StateName.of)
      ends <- names(f, "ends")(StateName.of)
      sections <- names(f, "sections")(Right(_))
      written <- f.added("nesting")
      nesting <- written match {
        case None | Some(ujson.Str(AnywhereNesting)) => Right(Nesting.Anywhere)
        case Some(ujson.Str(RootNesting)) => Right(Nesting.Root)
        case Some(o: ujson.Obj) =>
          names(Fields(s"${f.what}'s nesting", o), UnderNesting)(KindName.of)
            .flatMap(ks => Nesting.under(ks.toSet).left.map(why => s"${f.what}'s nesting: $why"))
        case Some(_) =>
          Left(
            s"${f.what}'s nesting is neither $AnywhereNesting, $RootNesting nor " +
              s"""{"$UnderNesting": [kinds]}"""
          )
      }
    } yield ArtifactKind(name, first, states, ends.toSet, sections, nesting)

  private def transition(f: Fields): Either[String, Transition] =
    for {
      kind <- named(f, "kind")(KindName.of)
      from <- named(f, "from")(StateName.of)
      to <- named(f, "to")(StateName.of)
      checks <- names(f, "checks")(CheckName.of)
      written <- f.added("action")
      action <- written.fold(Right(TransitionAction.Nothing))(a =>
        action(Fields(s"${f.what}'s action", a))
      )
      named <- f.addedStr("movers")
      movers <- named match {
        case None | Some(AnyoneMoves) => Right(Movers.Anyone)
        case Some(PeopleMove) => Right(Movers.Person)
        case Some(other) =>
          Left(s"${f.what}'s movers $other is neither $AnyoneMoves nor $PeopleMove")
      }
    } yield Transition(kind, from, to, checks, action, movers)

  private def action(f: Fields): Either[String, TransitionAction] =
    f.str("kind").flatMap {
      case NothingKind => Right(TransitionAction.Nothing)
      case MergeKind => named(f, "into")(KindName.of).map(TransitionAction.MergeDeltas(_))
      case other => Left(s"${f.what}'s kind $other is neither $NothingKind nor $MergeKind")
    }

  private def view(f: Fields): Either[String, RoleView] =
    for {
      role <- named(f, "role")(RoleName.of)
      kinds <- names(f, "kinds")(KindName.of)
      depth <- f.int("depth")
      carried <- names(f, "carried")(Handle.of)
      written <- f.added("covers")
      covers <- written match {
        case None => Right(false)
        case Some(ujson.Bool(b)) => Right(b)
        case Some(_) => Left(s"${f.what}'s covers is neither true nor false")
      }
      view <- RoleView
        .of(role, kinds.toSet, depth, carried.toSet, covers)
        .left
        .map(w => s"${f.what}: $w")
    } yield view

  /** Nothing when the check `f` names is compiled at the version it names; else why not. */
  private def listedIn(compiled: Vector[TransitionCheck])(f: Fields): Either[String, Unit] =
    for {
      name <- named(f, "name")(CheckName.of)
      version <- f.int("version")
      found <- compiled.find(_.name == name).toRight(s"check $name is not compiled")
      _ <- Either.cond(
        found.version == version,
        (),
        s"check $name is compiled at version ${found.version}, not $version"
      )
    } yield ()

  private def example(f: Fields): Either[String, Example] =
    for {
      name <- f.str("name")
      written <- f.field("start")
      start <- ArtifactJson.projectFrom(written).left.map(w => s"${f.what}'s start: $w")
      written <- array(f, "moves")
      moves <- traverse(written.zipWithIndex)((m, i) =>
        move(Fields(s"${f.what}'s move ${i + 1}", m), start)
      )
    } yield Example(name, start, moves)

  /** A move of `start`'s, its key an id or a handle of `start`'s. */
  private def move(f: Fields, start: Project): Either[String, Example.Move] =
    for {
      key <- named(f, "key")(k => ArtifactRef.read(k).flatMap(start.resolve))
      to <- named(f, "to")(StateName.of)
      expect <- f.field("expect").flatMap(e => expect(Fields(s"${f.what}'s expect", e)))
      named <- f.addedStr("by")
      by <- named match {
        case None | Some(AgentMover) => Right(Mover.Other)
        case Some(PersonMover) => Right(Mover.Person)
        case Some(other) => Left(s"${f.what}'s by $other is neither $AgentMover nor $PersonMover")
      }
    } yield Example.Move(key, to, expect, by)

  private def expect(f: Fields): Either[String, Example.Expect] =
    f.str("kind").flatMap {
      case AllowedKind => Right(Example.Expect.Allowed)
      case RefusedKind => named(f, "check")(CheckName.of).map(Example.Expect.RefusedBy(_))
      case PersonOnlyKind => Right(Example.Expect.RefusedPersonOnly)
      case other =>
        Left(s"${f.what}'s kind $other is none of $AllowedKind, $RefusedKind and $PersonOnlyKind")
    }

  /** `refusal` in words. */
  private def said(refusal: MethodRefusal): String = refusal match {
    case MethodRefusal.VersionUnderOne(version) => s"its version $version is under 1"
    case MethodRefusal.KindTwice(kind) => s"kind $kind is declared twice"
    case MethodRefusal.StateTwice(kind, state) => s"kind $kind names state $state twice"
    case MethodRefusal.RoleTwice(role) => s"role $role has two views"
    case MethodRefusal.CheckTwice(check) => s"check $check is compiled twice"
    case MethodRefusal.UndeclaredKind(kind) => s"kind $kind is not declared"
    case MethodRefusal.UndeclaredState(kind, state) => s"kind $kind declares no state $state"
    case MethodRefusal.FromEnd(kind, state) =>
      s"a transition of kind $kind leaves its end state $state"
    case MethodRefusal.NotCompiled(check) => s"check $check is not compiled"
    case MethodRefusal.Unreachable(kind, state) =>
      s"state $state of kind $kind is reached by no transition from its first"
  }
}
