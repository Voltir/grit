package grit.eval.harness.log

import java.nio.file.Files

import scala.concurrent.duration.*

import grit.core.classify.{Ask, Question}
import grit.core.message.{Tokens, Usage}
import grit.models.JevJson

import utest.*

/** Answers kept under their key: what makes two keys differ, and the files they are kept in.
  * Every value here is synthetic.
  */
object CacheTests extends TestSuite {

  private val questions: Vector[Question] = Ask.yesNo[Unit]("is it", None, None).questions

  /** Jev's wire body for `state` on `model`, as JevClassifier sends it. */
  private def body(model: String, state: String): String =
    ujson
      .write(JevJson.request(model, ujson.Obj("text" -> state), questions))

  private val cached: Cached[Vector[Weights]] = Cached(
    Vector(Weights.YesNo(0.25)),
    Usage(Tokens(1021), Tokens(1), Tokens.Zero, Some(BigDecimal("0.000042882"))),
    "jev-1.13.0",
    412.millis
  )

  val tests = Tests {
    test("a key is the same for equal inputs and differs in tag, model, repeat or one byte") {
      val key = CacheKey.of("jev", body("jev-1.13.0", "abc"), 0)
      CacheKey.of("jev", body("jev-1.13.0", "abc"), 0) ==> key
      val others = Vector(
        CacheKey.of("openrouter", body("jev-1.13.0", "abc"), 0),
        CacheKey.of("jev", body("jev-latest", "abc"), 0),
        CacheKey.of("jev", body("jev-1.13.0", "abc"), 1),
        CacheKey.of("jev", body("jev-1.13.0", "abd"), 0)
      )
      others.filter(_ == key) ==> Vector.empty
      others.distinct.size ==> others.size
    }

    // Where a key's answer is kept is the cache's on-disk form: pinned.
    test("an answer kept under a key is read back from <first two hex>/<key>.json") {
      val dir = Files.createTempDirectory("grit-cache-test")
      val cache = Cache.at[Vector[Weights]](dir)
      val key = CacheKey.of("jev", body("jev-1.13.0", "abc"), 0)
      cache.get(key) ==> Right(None)
      cache.put(key, cached) ==> Right(())
      cache.get(key) ==> Right(Some(cached))
      Files.isRegularFile(dir.resolve(key.hex.take(2)).resolve(s"${key.hex}.json")) ==> true
    }

    test("a kept file that does not read is Left, naming the key") {
      val dir = Files.createTempDirectory("grit-cache-test")
      val cache = Cache.at[Vector[Weights]](dir)
      val key = CacheKey.of("jev", body("jev-1.13.0", "abc"), 0)
      cache.put(key, cached) ==> Right(())
      Files.writeString(dir.resolve(key.hex.take(2)).resolve(s"${key.hex}.json"), "{}")
      cache.get(key) ==> Left(s"cache ${key.hex}: cached: no answer")
    }
  }
}
