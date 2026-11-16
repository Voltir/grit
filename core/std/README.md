# grit.std

What has nothing to do with grit, which every grit module may name. A thing enters under one
rule: a pure function over standard-library types, with no grit domain name in its signature,
that at least two modules call. A thing that names a grit idea (a slug, an id, a stored shape's
JSON) stays with that idea. Names no other grit module and no library.

- **`either`** -- `Traversal`: a vector of `Either`s traversed, failing at the first `Left`.

No source file sits at the module's root, and the test tree mirrors it.
