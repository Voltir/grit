package grit.models

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import scala.util.control.NonFatal

import grit.core.classify.{Answers, Classifier, ClassifierError, Question}

/** [[Classifier]] over Jev (`POST /v1/systemone`), one request per call, no retries: a 429
  * or 529 comes back as `Unavailable`, for the caller to back off.
  */
final class JevClassifier(config: JevConfig) extends Classifier {

  private val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()

  protected def answer(
      state: ujson.Value,
      questions: Vector[Question]
  ): Either[ClassifierError, Answers] =
    try {
      val response = http.send(
        HttpRequest
          .newBuilder(config.endpoint)
          .timeout(config.timeout)
          .header("Authorization", s"Bearer ${config.apiKey}")
          .header("Content-Type", "application/json")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              ujson.write(JevJson.request(config.model, state, questions))
            )
          )
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      if (response.statusCode == 200)
        scala.util
          .Try(ujson.read(response.body))
          .toEither
          .left
          .map(_ => ClassifierError.Unreadable("unreadable response: not JSON"))
          .flatMap(JevJson.response(questions, _))
      else Left(JevJson.error(response.statusCode, response.body))
    } catch {
      // The message never includes the request, so never the key.
      case NonFatal(e) =>
        Left(ClassifierError.Unavailable(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }
}
