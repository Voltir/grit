# grit.std

What has nothing to do with grit, which every grit module may name. A thing enters under one
rule: a pure function over standard-library types, with no grit domain name in its signature,
that at least two modules call. A thing that names a grit idea (a slug, an id, a stored shape's
JSON) stays with that idea. Names no other grit module and no library.

- **`either`** -- `Traversal`: a vector of `Either`s traversed, failing at the first `Left`;
  `Proven.right`: the right of an `Either` whose `Left` the caller rules out, the one home of
  that throw.
- **`collect`** -- `Collect.firstRepeat`: the first element a sequence holds a second time.
- **`near`** -- `Near.nearest`: the strings of a set nearest one asked for, by prefix, then edit
  distance.
- **`literal`** -- `Literal.checked`: a string constant read by a type's own reader as the
  compiler runs, behind each type's `literal` (docs/capture-checking.md, "Macros").

`either`, `collect`, `near` and `literal` name nothing of each other. No source file sits at the module's root, and the test tree mirrors it.
