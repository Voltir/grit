# grit.models

Implements `Provider` and `Classifier` (what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points)).

Models behind `grit.core`'s traits: `Provider`s (`StubProvider`, which calls nothing, and,
required to call a tool, calls its first; and `OpenRouterProvider`) and `Classifier`s
(`JevClassifier`, over typesafe.ai's Jev, which answers choices, yes/no questions and scores,
each choice and score with the confidence Jev reported; and `StubClassifier`, which answers
from markers in the message for tests and the gate, as `StubClassifier.answers` says: a
score's level by `~level:N`, 0 the first); and the `Seed`, the catalog checked in at `resources/catalog.json` (`grit.core.model`'s policy and
profiles), which `OpenRouterConfig.policy` overrides per run from the environment. A
quarantine module (STYLE rule 8) once it holds an HTTP client, and subject to the classified
boundary: nothing leaves the machine unless its destination is approved.

Design: `roadmap/mechanisms/models.md`.
