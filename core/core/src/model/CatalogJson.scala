package grit.core.model

import java.time.LocalDate

/** The stored JSON forms of a [[Catalog]] (the seed file's form) and of a [[TurnProfile]].
  * Written by hand: it is persisted data, so a rename in Scala must not change it, and reading
  * it is total. An unmeasured setting is an absent key.
  */
object CatalogJson {

  /** The catalog's form, as the seed file holds it. */
  def write(c: Catalog): ujson.Value = content(c.policy, c.profiles)

  /** The catalog `v` encodes, or the first thing wrong with it, named by its path. */
  def read(v: ujson.Value): Either[String, Catalog] =
    for {
      o <- obj(v, "catalog")
      policy <- field(o, "policy", "catalog").flatMap(readPolicy)
      list <- field(o, "profiles", "catalog").flatMap(_.arrOpt.toRight("profiles is not a list"))
      profiles <- all(list.toVector.zipWithIndex.map((p, i) => readProfile(p, s"profiles[$i]")))
    } yield Catalog.of(policy, profiles)

  /** One profile's form, as the seed lists it: its pair, then each known setting. */
  def writeProfile(p: Profile): ujson.Value = profile(p)

  /** The profile `v` encodes, or the first thing wrong with it, named by its path. */
  def readProfile(v: ujson.Value): Either[String, Profile] = readProfile(v, "profile")

  /** The settings a [[Setting]] is named by, in the stored form's words. */
  val SettingNames: Vector[String] =
    Vector("strict", "replay", "names", "repairs", "afterResult", "guidance")

  /** `s`'s name and value in the stored form's words; a set of repairs is its words
    * separated by `, `, in order.
    */
  def spelled(s: Setting): (String, String) = s match {
    case Setting.Strict(v) => ("strict", StrictWords.of(v))
    case Setting.Replay(v) => ("replay", ReplayWords.of(v))
    case Setting.Names(v) => ("names", NameWords.of(v))
    case Setting.Repairs(v) => ("repairs", v.toVector.map(RepairWords.of).sorted.mkString(", "))
    case Setting.AfterResult(v) => ("afterResult", AfterWords.of(v))
    case Setting.Guidance(v) => ("guidance", GuidanceWords.of(v))
  }

  /** The words setting `name` takes, in the stored form; empty for a name that is no setting.
    * `repairs` takes a comma-separated list of its words.
    */
  def settingWords(name: String): Vector[String] = name match {
    case "strict" => StrictSchemas.values.toVector.map(StrictWords.of)
    case "replay" => ReasoningReplay.values.toVector.map(ReplayWords.of)
    case "names" => NameRepair.values.toVector.map(NameWords.of)
    case "repairs" => ArgRepair.values.toVector.map(RepairWords.of)
    case "afterResult" => AfterToolResult.values.toVector.map(AfterWords.of)
    case "guidance" => ToolGuidance.values.toVector.map(GuidanceWords.of)
    case _ => Vector.empty
  }

  /** The setting `name` (one of [[SettingNames]]) at `value`, in the stored form's words; for
    * `repairs`, a comma-separated list, empty for none. `Left` names what was wrong and the
    * words it takes.
    */
  def setting(name: String, value: String): Either[String, Setting] = {
    val v = ujson.Str(value.trim)
    name match {
      case "strict" => StrictWords.read(v, name).map(Setting.Strict(_))
      case "replay" => ReplayWords.read(v, name).map(Setting.Replay(_))
      case "names" => NameWords.read(v, name).map(Setting.Names(_))
      case "afterResult" => AfterWords.read(v, name).map(Setting.AfterResult(_))
      case "guidance" => GuidanceWords.read(v, name).map(Setting.Guidance(_))
      case "repairs" =>
        val words = value.split(",").toVector.map(_.trim).filter(_.nonEmpty)
        all(words.zipWithIndex.map((w, i) => RepairWords.read(ujson.Str(w), s"$name[$i]")))
          .map(rs => Setting.Repairs(rs.toSet))
      case other => Left(s"$other is not a setting: one of ${SettingNames.mkString(", ")}")
    }
  }

  /** The turn profile's form: its three pins. */
  def writeTurn(t: TurnProfile): ujson.Value = pins(t.turn, t.summary, t.query)

  /** The turn profile `v` encodes, or the first thing wrong with it, named by its path. */
  def readTurn(v: ujson.Value): Either[String, TurnProfile] =
    for {
      o <- obj(v, "turn profile")
      turn <- field(o, "turn", "turn profile").flatMap(readPinned(_, "turn"))
      summary <- field(o, "summary", "turn profile").flatMap(readPinned(_, "summary"))
      query <- field(o, "query", "turn profile").flatMap(readPinned(_, "query"))
    } yield TurnProfile.of(turn, summary, query)

  private[model] def content(policy: Policy, profiles: Vector[Profile]): ujson.Value =
    ujson.Obj(
      "policy" -> ujson.Obj(
        "turn" -> assignment(policy.turn),
        "summary" -> assignment(policy.summary),
        "query" -> assignment(policy.query),
        "heard" -> assignment(policy.heard)
      ),
      "profiles" -> ujson.Arr.from(profiles.map(profile))
    )

  private[model] def pins(turn: Pinned, summary: Pinned, query: Pinned): ujson.Value =
    ujson.Obj("turn" -> pinned(turn), "summary" -> pinned(summary), "query" -> pinned(query))

  /** An enum's stored words: `word` spells each case (an exhaustive match, so a new case
    * does not compile until it has a word); reading finds the case in `cases` so spelled.
    */
  private final class Words[E](cases: Vector[E], word: E => String) {
    def of(e: E): String = word(e)
    def read(v: ujson.Value, at: String): Either[String, E] =
      v.strOpt
        .flatMap(w => cases.find(word(_) == w))
        .toRight(s"$at is not one of ${cases.map(word).mkString(", ")}")
  }

  private val StrictWords: Words[StrictSchemas] = Words(
    StrictSchemas.values.toVector,
    {
      case StrictSchemas.Enforced => "enforced"
      case StrictSchemas.WhenRequired => "when-required"
      case StrictSchemas.Ignored => "ignored"
      case StrictSchemas.Rejected => "rejected"
    }
  )
  private val ReplayWords: Words[ReasoningReplay] = Words(
    ReasoningReplay.values.toVector,
    {
      case ReasoningReplay.Details => "details"
      case ReasoningReplay.Dropped => "dropped"
    }
  )
  private val NameWords: Words[NameRepair] = Words(
    NameRepair.values.toVector,
    {
      case NameRepair.AsSent => "as-sent"
      case NameRepair.HarmonyCut => "harmony-cut"
    }
  )
  private val RepairWords: Words[ArgRepair] = Words(
    ArgRepair.values.toVector,
    {
      case ArgRepair.QuotedNumber => "quoted-number"
      case ArgRepair.QuotedList => "quoted-list"
    }
  )

  /** `repair` as it is written in stored forms: `quoted-number`, `quoted-list`. */
  def argRepair(repair: ArgRepair): String = RepairWords.of(repair)

  /** The repair written `v` ([[argRepair]]'s form), or why it is none. */
  def readArgRepair(v: ujson.Value): Either[String, ArgRepair] = RepairWords.read(v, "a repair")

  private val AfterWords: Words[AfterToolResult] = Words(
    AfterToolResult.values.toVector,
    {
      case AfterToolResult.UserMessage => "user-message"
      case AfterToolResult.InLastResult => "in-last-result"
    }
  )
  private val GuidanceWords: Words[ToolGuidance] = Words(
    ToolGuidance.values.toVector,
    {
      case ToolGuidance.SchemaOnly => "schema-only"
      case ToolGuidance.SystemLines => "system-lines"
    }
  )
  private val EffortWords: Words[Effort] = Words(
    Effort.values.toVector,
    {
      case Effort.Minimal => "minimal"
      case Effort.Low => "low"
      case Effort.Medium => "medium"
      case Effort.High => "high"
      case Effort.XHigh => "xhigh"
      case Effort.Max => "max"
    }
  )

  // Writing.

  private def ref(r: ModelRef, into: ujson.Obj): ujson.Obj = {
    into("model") = ModelId.value(r.model)
    r.upstream.foreach(u => into("upstream") = Upstream.value(u))
    into
  }

  private def assignment(a: Assignment): ujson.Value = {
    val o = ref(a.ref, ujson.Obj())
    o("maxTokens") = a.maxTokens
    a.effort.foreach(e => o("effort") = EffortWords.of(e))
    o
  }

  private def repairs(set: Set[ArgRepair]): ujson.Value =
    ujson.Arr.from(set.toVector.map(RepairWords.of).sorted.map(ujson.Str(_)))

  private def profile(p: Profile): ujson.Value = {
    val o = ref(p.ref, ujson.Obj())
    def put[A](key: String, k: Known[A], word: A => ujson.Value): Unit = k match {
      case Known.Of(value, s) => o(key) = ujson.Obj("value" -> word(value), "source" -> source(s))
      case Known.Unmeasured => ()
    }
    put("strict", p.strict, v => ujson.Str(StrictWords.of(v)))
    put("replay", p.replay, v => ujson.Str(ReplayWords.of(v)))
    put("names", p.names, v => ujson.Str(NameWords.of(v)))
    put("repairs", p.repairs, repairs)
    put("afterResult", p.afterResult, v => ujson.Str(AfterWords.of(v)))
    put("guidance", p.guidance, v => ujson.Str(GuidanceWords.of(v)))
    o
  }

  private def source(s: Source): ujson.Value = s match {
    case Source.Declared(by, on) => ujson.Obj("kind" -> "declared", "by" -> by, "on" -> on.toString)
    case Source.Measured(probe, on, runs, held) =>
      ujson.Obj(
        "kind" -> "measured",
        "probe" -> probe,
        "on" -> on.toString,
        "runs" -> runs,
        "held" -> held
      )
    case Source.Advertised(on) => ujson.Obj("kind" -> "advertised", "on" -> on.toString)
  }

  private def pinned(p: Pinned): ujson.Value = {
    val s = p.settings
    ujson.Obj(
      "catalog" -> CatalogVersion.value(p.version),
      "assignment" -> assignment(p.assignment),
      "settings" -> ujson.Obj(
        "strict" -> StrictWords.of(s.strict),
        "replay" -> ReplayWords.of(s.replay),
        "names" -> NameWords.of(s.names),
        "repairs" -> repairs(s.repairs),
        "afterResult" -> AfterWords.of(s.afterResult),
        "guidance" -> GuidanceWords.of(s.guidance),
        "profiled" -> s.profiled
      )
    )
  }

  // Reading.

  private type Fields = collection.Map[String, ujson.Value]

  private def obj(v: ujson.Value, at: String): Either[String, Fields] =
    v.objOpt.toRight(s"$at is not an object")

  private def field(o: Fields, key: String, at: String): Either[String, ujson.Value] =
    o.get(key).toRight(s"$at has no $key")

  private def text(o: Fields, key: String, at: String): Either[String, String] =
    field(o, key, at).flatMap(_.strOpt.toRight(s"$at.$key is not text"))

  private def whole(o: Fields, key: String, at: String, least: Int): Either[String, Int] =
    field(o, key, at).flatMap(
      _.numOpt
        .filter(n => n.isWhole && n >= least && n <= Int.MaxValue)
        .map(_.toInt)
        .toRight(s"$at.$key is not a whole number of at least $least")
    )

  private def date(o: Fields, key: String, at: String): Either[String, LocalDate] =
    text(o, key, at).flatMap(t =>
      scala.util.Try(LocalDate.parse(t)).toOption.toRight(s"$at.$key is not a date (yyyy-mm-dd)")
    )

  private def all[A](each: Vector[Either[String, A]]): Either[String, Vector[A]] =
    each.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, e) =>
      acc.flatMap(as => e.map(as :+ _))
    )

  private def readRef(o: Fields, at: String): Either[String, ModelRef] =
    for {
      m <- text(o, "model", at)
      model <- ModelId.of(m).toRight(s"$at.model is not a model id: $m")
      upstream <- o.get("upstream") match {
        case None => Right(None)
        case Some(u) =>
          u.strOpt
            .flatMap(Upstream.of)
            .map(Some(_))
            .toRight(s"$at.upstream is not an upstream slug")
      }
    } yield ModelRef(model, upstream)

  private def readAssignment(v: ujson.Value, at: String): Either[String, Assignment] =
    for {
      o <- obj(v, at)
      r <- readRef(o, at)
      max <- whole(o, "maxTokens", at, 1)
      effort <- o.get("effort") match {
        case None => Right(None)
        case Some(e) => EffortWords.read(e, s"$at.effort").map(Some(_))
      }
    } yield Assignment(r, max, effort)

  private def readPolicy(v: ujson.Value): Either[String, Policy] =
    for {
      o <- obj(v, "policy")
      turn <- field(o, "turn", "policy").flatMap(readAssignment(_, "policy.turn"))
      summary <- field(o, "summary", "policy").flatMap(readAssignment(_, "policy.summary"))
      query <- field(o, "query", "policy").flatMap(readAssignment(_, "policy.query"))
      heard <- field(o, "heard", "policy").flatMap(readAssignment(_, "policy.heard"))
    } yield Policy(turn, summary, query, heard)

  private def readRepairs(v: ujson.Value, at: String): Either[String, Set[ArgRepair]] =
    v.arrOpt
      .toRight(s"$at is not a list")
      .flatMap(items =>
        all(items.toVector.zipWithIndex.map((w, i) => RepairWords.read(w, s"$at[$i]"))).map(_.toSet)
      )

  private def readSource(v: ujson.Value, at: String): Either[String, Source] =
    for {
      o <- obj(v, at)
      kind <- text(o, "kind", at)
      s <- kind match {
        case "declared" =>
          for { by <- text(o, "by", at); on <- date(o, "on", at) } yield Source.Declared(by, on)
        case "measured" =>
          for {
            probe <- text(o, "probe", at)
            on <- date(o, "on", at)
            runs <- whole(o, "runs", at, 1)
            held <- whole(o, "held", at, 0)
            _ <- Either.cond(held <= runs, (), s"$at.held is more than its runs")
          } yield Source.Measured(probe, on, runs, held)
        case "advertised" => date(o, "on", at).map(Source.Advertised(_))
        case other => Left(s"$at.kind is not declared, measured or advertised: $other")
      }
    } yield s

  private def readKnown[A](o: Fields, key: String, at: String)(
      value: (ujson.Value, String) => Either[String, A]
  ): Either[String, Known[A]] =
    o.get(key) match {
      case None => Right(Known.Unmeasured)
      case Some(v) =>
        for {
          k <- obj(v, s"$at.$key")
          a <- field(k, "value", s"$at.$key").flatMap(value(_, s"$at.$key.value"))
          s <- field(k, "source", s"$at.$key").flatMap(readSource(_, s"$at.$key.source"))
        } yield Known.Of(a, s)
    }

  private def readProfile(v: ujson.Value, at: String): Either[String, Profile] =
    for {
      o <- obj(v, at)
      r <- readRef(o, at)
      strict <- readKnown(o, "strict", at)(StrictWords.read)
      replay <- readKnown(o, "replay", at)(ReplayWords.read)
      names <- readKnown(o, "names", at)(NameWords.read)
      repairs <- readKnown(o, "repairs", at)(readRepairs)
      after <- readKnown(o, "afterResult", at)(AfterWords.read)
      guidance <- readKnown(o, "guidance", at)(GuidanceWords.read)
    } yield Profile(r, strict, replay, names, repairs, after, guidance)

  private def readPinned(v: ujson.Value, at: String): Either[String, Pinned] =
    for {
      o <- obj(v, at)
      version <- text(o, "catalog", at)
      a <- field(o, "assignment", at).flatMap(readAssignment(_, s"$at.assignment"))
      so <- field(o, "settings", at).flatMap(obj(_, s"$at.settings"))
      sat = s"$at.settings"
      strict <- field(so, "strict", sat).flatMap(StrictWords.read(_, s"$sat.strict"))
      replay <- field(so, "replay", sat).flatMap(ReplayWords.read(_, s"$sat.replay"))
      names <- field(so, "names", sat).flatMap(NameWords.read(_, s"$sat.names"))
      repairs <- field(so, "repairs", sat).flatMap(readRepairs(_, s"$sat.repairs"))
      after <- field(so, "afterResult", sat).flatMap(AfterWords.read(_, s"$sat.afterResult"))
      guidance <- field(so, "guidance", sat).flatMap(GuidanceWords.read(_, s"$sat.guidance"))
      profiled <- field(so, "profiled", sat).flatMap(
        _.boolOpt.toRight(s"$sat.profiled is not true or false")
      )
    } yield Pinned(
      CatalogVersion(version),
      a,
      Settings(strict, replay, names, repairs, after, guidance, profiled)
    )
}
