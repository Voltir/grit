package grit.core.schema

/** JSON that a [[JsonSchema]]'s check accepted, repairs made. */
final class Conforming private[schema] (written: String) extends caps.Pure {

  /** A fresh copy each call: changing it changes nothing kept. */
  def json: ujson.Value = ujson.read(written)

  /** Its compact written form. */
  def text: String = written
}
