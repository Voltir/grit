# grit.models

Models behind `grit.core`'s traits: `Provider`s (`StubProvider`, which calls nothing, and
`OpenRouterProvider`) and `Classifier`s (`JevClassifier`, over typesafe.ai's Jev, and
`StubClassifier`, which answers from markers in the message, for tests and the gate); and
the `Seed`, the catalog checked in at `resources/catalog.json` (`grit.core.model`'s policy and
profiles), which `OpenRouterConfig.policy` overrides per run from the environment. A
quarantine module (STYLE rule 8) once it holds an HTTP client, and subject to the classified
boundary: nothing leaves the machine unless its destination is approved.

Design: `roadmap/mechanisms/models.md`.
