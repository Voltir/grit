package grit.outline.trace

import scala.collection.mutable

import grit.outline.model.{Defn, Kind, Ref}

/** The repo types reached from `roots`, the out-of-repo types named on the way, and the repo types that could not be loaded. */
final case class Traced(types: Vector[Defn], library: Vector[String], missing: Vector[String])

object Trace {

  /** The repo types `roots` name within `depth` hops (1: the types `roots`' signatures name; 2: also those types' own), each once and none of `roots`; `load(topLevel)` gives the top-level Defns of the `.tasty` holding `topLevel`. */
  def trace(
      roots: Vector[Defn],
      depth: Int,
      load: String -> Either[String, Vector[Defn]]
  ): Traced = {
    val rootNames = roots.map(_.fullName).toSet
    val cache = mutable.Map.empty[String, Either[String, Vector[Defn]]]
    val seen = mutable.Set.from(rootNames)
    val found = mutable.ListBuffer.empty[Defn]
    val library = mutable.SortedSet.empty[String]
    val missing = mutable.SortedSet.empty[String]

    def loaded(topLevel: String): Either[String, Vector[Defn]] =
      cache.getOrElseUpdate(topLevel, load(topLevel))

    def everyDefn(ds: Vector[Defn]): Vector[Defn] =
      ds.flatMap(d => d +: everyDefn(d.members))

    // A class and its companion share a name; the class wins.
    def resolve(ref: Ref): Option[Defn] =
      loaded(ref.topLevel) match {
        case Right(tops) =>
          val candidates = everyDefn(tops).filter(_.fullName == ref.fullName)
          candidates.find(_.kind != Kind.Object).orElse(candidates.find(_ => true))
        case Left(_) => None
      }

    def expansion(d: Defn): Vector[Ref] =
      d.refs ++ d.members.filter(_.kind == Kind.EnumCase).flatMap(_.refs)

    val seeds: Vector[Ref] =
      if (depth < 1) Vector.empty
      else roots.flatMap(r => r.refs ++ r.members.flatMap(_.refs))

    (1 to depth).foldLeft(seeds) { (refs, _) =>
      val hopTypes = mutable.ListBuffer.empty[Defn]
      refs.foreach { ref =>
        if (!seen.contains(ref.fullName)) {
          seen += ref.fullName
          if (ref.inRepo) {
            resolve(ref) match {
              case Some(d) =>
                found += d
                hopTypes += d
              case None => missing += ref.fullName
            }
          } else library += ref.fullName
        }
      }
      hopTypes.toVector.flatMap(expansion)
    }

    Traced(found.toVector, library.toVector, missing.toVector)
  }
}
