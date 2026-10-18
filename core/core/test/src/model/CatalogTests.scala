package grit.core.model

import java.time.LocalDate

import utest.*

object CatalogTests extends TestSuite {

  private def model(id: String): ModelId =
    ModelId.of(id).getOrElse(throw new java.lang.AssertionError(id))
  private def upstream(slug: String): Upstream =
    Upstream.of(slug).getOrElse(throw new java.lang.AssertionError(slug))

  private val oss = ModelRef(model("openai/gpt-oss-120b"), Some(upstream("cerebras/fp16")))
  private val ossOpen = ModelRef(model("openai/gpt-oss-120b"), None)
  private val flash =
    ModelRef(model("deepseek/deepseek-v4.1-flash-20260910"), Some(upstream("fireworks")))

  private val day = LocalDate.of(2026, 9, 25)
  private val nick = Source.Declared("nick", day)
  private val probe = Source.Measured("exit-task", day, 3, 3)
  private val listed = Source.Advertised(day)

  private val turnAssigned = Assignment(oss, 4096, Some(Effort.Low))
  private val summaryAssigned = Assignment(oss, 1024, None)
  private val queryAssigned = Assignment(flash, 1024, None)
  private val policy =
    Policy.one(turnAssigned, summaryAssigned, queryAssigned, Assignment(flash, 2048, None))

  private val ossProfile = Profile(
    oss,
    strict = Known.Of(StrictSchemas.Ignored, probe),
    names = Known.Of(NameRepair.HarmonyCut, probe)
  )
  private val flashProfile = Profile(
    flash,
    strict = Known.Of(StrictSchemas.Enforced, listed),
    replay = Known.Of(ReasoningReplay.Details, probe),
    repairs = Known.Of(Set(ArgRepair.QuotedNumber), nick),
    afterResult = Known.Of(AfterToolResult.InLastResult, nick),
    guidance = Known.Of(ToolGuidance.SystemLines, nick)
  )

  private val catalog = Catalog.of(policy, Vector(ossProfile, flashProfile))

  val tests = Tests {
    test("settings: an unprofiled pair behaves as grit did before profiles") {
      Settings.of(None) ==> Settings(
        StrictSchemas.Ignored,
        ReasoningReplay.Details,
        NameRepair.HarmonyCut,
        Set(ArgRepair.QuotedNumber, ArgRepair.QuotedList),
        AfterToolResult.UserMessage,
        ToolGuidance.SchemaOnly,
        profiled = false
      )
    }

    test("settings: a profile's known settings hold, and only its unmeasured ones default") {
      Settings.of(Some(flashProfile)) ==> Settings(
        StrictSchemas.Enforced,
        ReasoningReplay.Details,
        NameRepair.HarmonyCut,
        Set(ArgRepair.QuotedNumber),
        AfterToolResult.InLastResult,
        ToolGuidance.SystemLines,
        profiled = true
      )
    }

    test("known: a higher-ranked source wins whichever comes first; a tie goes to the later") {
      val declared = Known.Of(StrictSchemas.Rejected, nick)
      val measured = Known.Of(StrictSchemas.Enforced, probe)
      val advertised = Known.Of(StrictSchemas.Ignored, listed)
      declared.orOver(measured) ==> declared
      measured.orOver(declared) ==> declared
      advertised.orOver(measured) ==> measured
      measured.orOver(advertised) ==> measured
      val later = Known.Of(StrictSchemas.Ignored, Source.Measured("again", day, 5, 5))
      measured.orOver(later) ==> later
      measured.orOver(Known.Unmeasured) ==> measured
      Known.Unmeasured.orOver(measured) ==> measured
    }

    test("catalog: one profile per pair, later settings laid over earlier by rank") {
      val override1 = Profile(oss, strict = Known.Of(StrictSchemas.Rejected, nick))
      val override2 = Profile(oss, names = Known.Of(NameRepair.AsSent, listed))
      val merged = Catalog.of(policy, Vector(ossProfile, flashProfile, override1, override2))
      merged.profiles ==> Vector(
        Profile(
          oss,
          strict = Known.Of(StrictSchemas.Rejected, nick),
          names = Known.Of(NameRepair.HarmonyCut, probe)
        ),
        flashProfile
      )
    }

    test("catalog: one model at two upstreams is two pairs") {
      val open = Profile(ossOpen, strict = Known.Of(StrictSchemas.Enforced, nick))
      val c = Catalog.of(policy, Vector(ossProfile, open))
      c.profiles ==> Vector(ossProfile, open)
      c.profile(ossOpen) ==> Some(open)
    }

    test("version: the same content is the same version; any setting or policy change is another") {
      Catalog.of(policy, Vector(ossProfile, flashProfile)).version ==> catalog.version
      val oneFact = Catalog.of(
        policy,
        Vector(ossProfile.copy(names = Known.Of(NameRepair.AsSent, probe)), flashProfile)
      )
      assert(oneFact.version != catalog.version)
      assert(catalog.withPolicy(policy.copy(query = policy.turn)).version != catalog.version)
      val source = Catalog.of(
        policy,
        Vector(ossProfile.copy(strict = Known.Of(StrictSchemas.Ignored, nick)), flashProfile)
      )
      assert(source.version != catalog.version)
    }

    test(
      "pin: each role gets its assignment and its pair's settings, under the catalog's version"
    ) {
      val t = catalog.pin
      def pin(a: Assignment, p: Profile) =
        Pins.one(Pinned(catalog.version, Grade.Standard, a, Settings.of(Some(p))))
      t.turn ==> pin(turnAssigned, ossProfile)
      t.summary ==> pin(summaryAssigned, ossProfile)
      t.query ==> pin(queryAssigned, flashProfile)
    }

    test("pin: turns under one catalog share an id; a change to any one role's pin is another") {
      catalog.pin.id ==> Catalog.of(policy, Vector(ossProfile, flashProfile)).pin.id
      val t = catalog.pin
      val otherBudget =
        Pins.one(t.turn.first.copy(assignment = t.turn.first.assignment.copy(maxTokens = 1)))
      val ids = Vector(
        TurnProfile.of(otherBudget, t.summary, t.query),
        TurnProfile.of(t.turn, otherBudget, t.query),
        TurnProfile.of(t.turn, t.summary, otherBudget)
      ).map(_.id)
      ids.distinct.size ==> 3
      assert(!ids.contains(t.id))
    }

    // These pin the stored forms: the seed file is written in the catalog's, and every
    // turn's profile row in the turn profile's.
    test("the heard role pins its own assignment under the catalog, with its pair's settings") {
      catalog.heard ==> Pins.one(
        Pinned(
          catalog.version,
          Grade.Standard,
          Assignment(flash, 2048, None),
          Settings.of(Some(flashProfile))
        )
      )
      assert(catalog.withPolicy(policy.copy(heard = policy.summary)).version != catalog.version)
    }

    test(
      "the dream role pins its own ladder; stored only when it is not the summary's, and read as the summary's when absent"
    ) {
      val dreaming = catalog.withPolicy(policy.copy(dream = Ladder.one(queryAssigned)))
      val stored = CatalogJson.write(dreaming)
      (
        dreaming.dream.first.assignment,
        CatalogJson.read(stored).map(_.policy.dream),
        catalog.dream.first.assignment,
        CatalogJson.write(catalog).obj("policy").obj.contains("dream"),
        CatalogJson.read(CatalogJson.write(catalog)).map(_.version)
      ) ==> (
        queryAssigned,
        Right(Ladder.one(queryAssigned)),
        summaryAssigned,
        false,
        Right(catalog.version)
      )
    }

    test("stored form: a catalog") {
      val one = Catalog.of(
        Policy.one(
          Assignment(oss, 4096, Some(Effort.XHigh)),
          Assignment(oss, 1024, None),
          Assignment(ossOpen, 1, None),
          Assignment(flash, 2048, None)
        ),
        Vector(
          Profile(
            oss,
            strict = Known.Of(StrictSchemas.WhenRequired, probe),
            repairs = Known.Of(Set(ArgRepair.QuotedList, ArgRepair.QuotedNumber), listed)
          )
        )
      )
      CatalogJson.write(one).render() ==>
        """{"policy":{"turn":{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16","maxTokens":4096,"effort":"xhigh"},""" +
        """"summary":{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16","maxTokens":1024},""" +
        """"query":{"model":"openai/gpt-oss-120b","maxTokens":1},""" +
        """"heard":{"model":"deepseek/deepseek-v4.1-flash-20260910","upstream":"fireworks","maxTokens":2048}},""" +
        """"profiles":[{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16",""" +
        """"strict":{"value":"when-required","source":{"kind":"measured","probe":"exit-task","on":"2026-09-25","runs":3,"held":3}},""" +
        """"repairs":{"value":["quoted-list","quoted-number"],"source":{"kind":"advertised","on":"2026-09-25"}}}]}"""
    }

    test("stored form: every word reads back to what wrote it") {
      val every = Catalog.of(
        policy,
        Vector(
          flashProfile,
          ossProfile,
          Profile(
            ossOpen,
            Known.Of(StrictSchemas.Rejected, nick),
            Known.Of(ReasoningReplay.Dropped, probe),
            Known.Of(NameRepair.AsSent, listed),
            Known.Of(Set.empty, nick),
            Known.Of(AfterToolResult.UserMessage, nick),
            Known.Of(ToolGuidance.SchemaOnly, nick)
          )
        )
      )
      CatalogJson.read(ujson.read(CatalogJson.write(every).render())) ==> Right(every)
      val efforts = Effort.values.toVector.map(e =>
        catalog.withPolicy(policy.copy(turn = Ladder.one(turnAssigned.copy(effort = Some(e)))))
      )
      efforts.map(c =>
        CatalogJson.read(CatalogJson.write(c)).map(_.policy.turn.first.assignment.effort)
      ) ==>
        Effort.values.toVector.map(e => Right(Some(e)))
      val turn = every.pin
      CatalogJson.readTurn(ujson.read(CatalogJson.writeTurn(turn).render())) ==> Right(turn)
    }

    test("stored form: a turn profile") {
      val t =
        Catalog.of(policy.copy(summary = policy.turn, query = policy.turn), Vector(ossProfile)).pin
      val pinned =
        s"""{"catalog":"${CatalogVersion.value(t.turn.first.version)}",""" +
          """"assignment":{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16","maxTokens":4096,"effort":"low"},""" +
          """"settings":{"strict":"ignored","replay":"details","names":"harmony-cut","repairs":["quoted-list","quoted-number"],""" +
          """"afterResult":"user-message","guidance":"schema-only","profiled":true}}"""
      CatalogJson
        .writeTurn(t)
        .render() ==> s"""{"turn":$pinned,"summary":$pinned,"query":$pinned}"""
    }

    test("pin: each rung of a role's ladder is pinned with its grade, in the ladder's order") {
      val ladder = Ladder.of(
        Vector(
          Rung(Grade.Strong, Assignment(flash, 2048, None)),
          Rung(Grade.Light, Assignment(ossOpen, 512, None)),
          Rung(Grade.Standard, turnAssigned)
        )
      )
      val pins = ladder.map(l => Catalog.of(policy.copy(turn = l), Vector(ossProfile)).pin.turn)
      pins.map(_.all.map(p => (p.grade, p.assignment, p.settings.profiled))) ==> Right(
        Vector(
          (Grade.Strong, Assignment(flash, 2048, None), false),
          (Grade.Light, Assignment(ossOpen, 512, None), false),
          (Grade.Standard, turnAssigned, true)
        )
      )
    }

    test("a ladder has a rung, and a role's pins a pin") {
      Ladder.of(Vector.empty) ==> Left("a ladder has no rungs")
      Pins.of(Vector.empty) ==> Left("a role has no pins")
      CatalogJson.read(
        ujson.read(
          CatalogJson
            .write(catalog)
            .render()
            .replaceFirst(
              "\"turn\":\\{[^}]*\\}",
              "\"turn\":[]"
            )
        )
      ) ==> Left("policy.turn has no rungs")
    }

    test(
      "stored form: a role of several rungs is a list, and a rung off standard names its grade"
    ) {
      val strong = Rung(Grade.Strong, Assignment(flash, 2048, None))
      val graded = Catalog.of(
        Policy(
          Ladder
            .of(Vector(Rung(Grade.Standard, Assignment(ossOpen, 1, None)), strong))
            .getOrElse(throw new java.lang.AssertionError("ladder")),
          Ladder.one(summaryAssigned),
          Ladder
            .of(Vector(Rung(Grade.Light, Assignment(ossOpen, 1, None))))
            .getOrElse(throw new java.lang.AssertionError("ladder")),
          Ladder.one(summaryAssigned),
          Ladder.one(summaryAssigned)
        ),
        Vector.empty
      )
      val policy = CatalogJson.write(graded)("policy")
      policy("turn").render() ==>
        """[{"model":"openai/gpt-oss-120b","maxTokens":1},""" +
        """{"grade":"strong","model":"deepseek/deepseek-v4.1-flash-20260910","upstream":"fireworks","maxTokens":2048}]"""
      policy("query")
        .render() ==> """{"grade":"light","model":"openai/gpt-oss-120b","maxTokens":1}"""
      CatalogJson.read(ujson.read(CatalogJson.write(graded).render())) ==> Right(graded)

      val turn = graded.pin
      val stored = CatalogJson.writeTurn(turn)
      stored("turn").arrOpt.map(_.map(_.obj.get("grade").map(_.str))) ==>
        Some(Vector(None, Some("strong")))
      stored("query").obj.get("grade").map(_.str) ==> Some("light")
      CatalogJson.readTurn(ujson.read(stored.render())) ==> Right(turn)
    }

    // Written by the code before ladders: a catalog row and a kept profile row as stored then.
    // Reading either must give back the same version or id, and write the same text.
    test(
      "stored form: a catalog and a turn profile stored before ladders keep their version and id"
    ) {
      val catalogJson =
        """{"policy":{"turn":{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16","maxTokens":4096,"effort":"xhigh"},""" +
          """"summary":{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16","maxTokens":1024},""" +
          """"query":{"model":"openai/gpt-oss-120b","maxTokens":1},""" +
          """"heard":{"model":"deepseek/deepseek-v4.1-flash-20260910","upstream":"fireworks","maxTokens":2048}},""" +
          """"profiles":[{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16",""" +
          """"strict":{"value":"when-required","source":{"kind":"measured","probe":"exit-task","on":"2026-09-25","runs":3,"held":3}},""" +
          """"repairs":{"value":["quoted-list","quoted-number"],"source":{"kind":"advertised","on":"2026-09-25"}}}]}"""
      val read = CatalogJson.read(ujson.read(catalogJson))
      read.map(c => CatalogVersion.value(c.version)) ==> Right("910ee609238f662b")
      read.map(c => CatalogJson.write(c).render()) ==> Right(catalogJson)

      val pinned =
        """{"catalog":"3f0c2a91d7e4b605",""" +
          """"assignment":{"model":"openai/gpt-oss-120b","upstream":"cerebras/fp16","maxTokens":4096,"effort":"low"},""" +
          """"settings":{"strict":"ignored","replay":"details","names":"harmony-cut","repairs":["quoted-list","quoted-number"],""" +
          """"afterResult":"user-message","guidance":"schema-only","profiled":true}}"""
      val profileJson = s"""{"turn":$pinned,"summary":$pinned,"query":$pinned}"""
      val profile = CatalogJson.readTurn(ujson.read(profileJson))
      profile.map(p => TurnProfileId.value(p.id)) ==> Right("54b788bbdb8bafdd")
      profile.map(p => CatalogJson.writeTurn(p).render()) ==> Right(profileJson)
    }

    test("reading: the first thing wrong, named by its path") {
      def read(json: String) = CatalogJson.read(ujson.read(json)).map(_ => ())
      val good = CatalogJson.write(catalog).render()
      read(good.replace("\"enforced\"", "\"strictly\"")) ==>
        Left("profiles[1].strict.value is not one of enforced, when-required, ignored, rejected")
      read(good.replace("\"maxTokens\":1024", "\"maxTokens\":0")) ==>
        Left("policy.summary.maxTokens is not a whole number of at least 1")
      read(good.replace("\"runs\":3,\"held\":3", "\"runs\":3,\"held\":4")) ==>
        Left("profiles[0].strict.source.held is more than its runs")
      read(good.replace("deepseek/deepseek-v4.1-flash-20260910", "DeepSeek")) ==>
        Left("policy.query.model is not a model id: DeepSeek")
      read(good.replace("\"kind\":\"declared\"", "\"kind\":\"rumoured\"")) ==>
        Left("profiles[1].repairs.source.kind is not declared, measured or advertised: rumoured")
      read("""{"policy":{}}""") ==> Left("policy has no turn")
      read(good.replaceFirst(",\"heard\":\\{[^}]*\\}", "")) ==> Left("policy has no heard")
    }

    test("ids: model ids and upstream slugs") {
      ModelId.of("deepseek/deepseek-v4.1-flash-20260910").map(ModelId.value) ==>
        Some("deepseek/deepseek-v4.1-flash-20260910")
      ModelId.of("openai/gpt-oss-120b:batch").isDefined ==> true
      ModelId.of("gpt-oss-120b") ==> None
      ModelId.of("OpenAI/gpt-oss") ==> None
      Upstream.of("open-inference/fp8").isDefined ==> true
      Upstream.of("a/b/c") ==> None
      flash.toString ==> "deepseek/deepseek-v4.1-flash-20260910 @ fireworks"
      ossOpen.toString ==> "openai/gpt-oss-120b"
    }

    test("a model setting is a profile of its pair that knows one setting, measured by its probe") {
      val measured = ModelSetting(flash, Setting.Names(NameRepair.AsSent), "tool-probe", 5, 4)
      measured.profile(day) ==>
        Profile(
          flash,
          names = Known.Of(NameRepair.AsSent, Source.Measured("tool-probe", day, 5, 4))
        )
      ModelSetting(oss, Setting.Repairs(Set(ArgRepair.QuotedList)), "p", 1, 1)
        .profile(day)
        .repairs ==>
        Known.Of(Set(ArgRepair.QuotedList), Source.Measured("p", day, 1, 1))
    }

    test("a setting read from its stored words, or refused with the words it takes") {
      CatalogJson.setting("strict", "when-required") ==> Right(
        Setting.Strict(StrictSchemas.WhenRequired)
      )
      CatalogJson.setting("replay", "dropped") ==> Right(Setting.Replay(ReasoningReplay.Dropped))
      CatalogJson.setting("names", "as-sent") ==> Right(Setting.Names(NameRepair.AsSent))
      CatalogJson.setting("repairs", "quoted-number, quoted-list") ==>
        Right(Setting.Repairs(Set(ArgRepair.QuotedNumber, ArgRepair.QuotedList)))
      CatalogJson.setting("repairs", "") ==> Right(Setting.Repairs(Set.empty))
      CatalogJson.setting("afterResult", "in-last-result") ==> Right(
        Setting.AfterResult(AfterToolResult.InLastResult)
      )
      CatalogJson.setting("guidance", "system-lines") ==> Right(
        Setting.Guidance(ToolGuidance.SystemLines)
      )
      CatalogJson.setting("strict", "sometimes") ==>
        Left("strict is not one of enforced, when-required, ignored, rejected")
      CatalogJson.setting("repairs", "quoted-number, guess") ==>
        Left("repairs[1] is not one of quoted-number, quoted-list")
      CatalogJson.setting("speed", "fast") ==>
        Left("speed is not a setting: one of strict, replay, names, repairs, afterResult, guidance")
    }
  }
}
