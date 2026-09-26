package grit.models

import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{Delta, ModelRequest, Provider, ToolSchema, ToolUse}

import utest.*

/** The stub's reply, its one tool call, and how it streams them. */
object StubProviderTests extends TestSuite {

  private val topic = ToolSchema("topic", "d", ujson.Obj())

  val tests = Tests {
    test("the stub streams its reply word by word, in pieces that join back to it") {
      val told = Vector.newBuilder[Delta]
      val reply = new StubProvider()
        .stream(ModelRequest("s", Vector(Message.User("a b c"))), d => told += d)
      told.result() ==> Vector("stub", " reply", " to:", " a", " b", " c").map(Delta.Text(_))
      reply.map(_.blocks) ==> Right(Vector(AssistantBlock.Text("stub reply to: a b c")))
    }

    test("the stub calls the first tool it may, with the #call: arguments, beside a line of text") {
      val note = ToolSchema("note", "d", ujson.Obj())
      val said = """hi #call:{"about":"new","name":"Knots"}"""
      val asked = Message.User(said)
      val called =
        new StubProvider().complete(ModelRequest("s", Vector(asked), Vector(topic, note)))
      called.map(_.blocks) ==> Right(
        Vector(
          AssistantBlock.Text("stub calls topic"),
          AssistantBlock.ToolCall(
            StubProvider.CallId,
            "topic",
            ujson.Obj("about" -> "new", "name" -> "Knots")
          )
        )
      )
      // Not while it may not, nor after the tool's result: then it quotes the user.
      val off = new StubProvider()
        .complete(ModelRequest("s", Vector(asked), Vector(topic), ToolUse.Off))
      off.map(_.blocks) ==> Right(Vector(AssistantBlock.Text(s"stub reply to: $said")))
      val after = new StubProvider().complete(
        ModelRequest(
          "s",
          Vector(
            asked,
            called.getOrElse(sys.error("stub")),
            Message.ToolResult(StubProvider.CallId, "noted", false)
          ),
          Vector(topic)
        )
      )
      after.map(_.blocks) ==> Right(Vector(AssistantBlock.Text(s"stub reply to: $said")))
      // Nor without the marker.
      new StubProvider()
        .complete(ModelRequest("s", Vector(Message.User("hi")), Vector(topic)))
        .map(_.blocks) ==> Right(Vector(AssistantBlock.Text("stub reply to: hi")))
      StubProvider.arguments("no marker") ==> ujson.Obj()
    }

    test("the stub tells its call after its text, as a provider that cannot stream does") {
      val asked = ModelRequest("s", Vector(Message.User("hi #call:{}")), Vector(topic))
      def told(provider: Provider) = {
        val all = Vector.newBuilder[Delta]
        val _ = provider.stream(asked, d => all += d)
        all.result()
      }
      val stub = new StubProvider()
      val plain = new Provider {
        def complete(request: ModelRequest) = stub.complete(request)
      }
      told(stub) ==> Vector[Delta](
        Delta.Text("stub"),
        Delta.Text(" calls"),
        Delta.Text(" topic"),
        Delta.Calling("topic")
      )
      told(plain) ==> Vector(Delta.Text("stub calls topic"), Delta.Calling("topic"))
    }
  }
}
