package grit.core.recipe

/** A labelled part of a call's input that a pool fills, shown under `key`. */
enum Section(val key: String) {
  case Nearby extends Section("nearby_in_channel")
  case Exchanges extends Section("exchanges_in_channel")
}
