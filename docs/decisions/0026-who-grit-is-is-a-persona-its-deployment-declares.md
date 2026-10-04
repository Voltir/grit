# 0026. Who grit is, is a persona its deployment declares, not an edge's identity

Status: accepted (2026-10-04)

Context: a deployment presents grit under a name of its own: actualbest's Slack calls it
Bort. Turns in Slack were told that name, but the edge supplied it: at open, it read its
bot user's Slack name and enrolled it as the workspace's assistant, and the turn read it
back. Nothing else knew the name. Triage could not tell "Bort, …" from "Alice, …", and
`about` could not say who the assistant is. The name had to have one home, which the
prompt, `about` and triage would all read. There were two candidates:

- **The edge's identity** (Slack's bot user). Turned down. It is known only once an edge
  opens, after the deployment is declared, so nothing could check it at declaration. It
  differs by edge (a terminal and a task have none). It would tie a name triage words its
  questions with to a setting in another product.
- **A deployment's declaration.** Chosen.

Decision: `Deployment.of` takes a required `persona`, a `grit.core.persona.Persona`.

- A persona is one name: trimmed, one line, at most `Persona.MaxChars` characters. Anything
  else is refused when the `Persona` is built, never thrown. `Persona.Grit` is grit itself,
  named grit, which the reference deployment declares.
- The kit hands the persona to every turn (`TurnTooling.persona`). A Slack turn is told
  "In this workspace you are called {name}.", the words it was told before. `about` opens
  its overview with the name, and for a persona other than grit says that it is a persona
  running on grit, which the docs describe.
- Edges no longer name the assistant. The Slack edge logs its bot's display name at open,
  and enrolls nothing.
- Triage, when it asks whether a message is directed at grit, names the deployment's one
  persona in every place the deployment hears. That is the accepted scope for now.
  Per-place question sets come with personas, as do aliases and more than one persona per
  deployment.

Consequences:

- A deployment's name is checked where it is declared, and is the same in every edge, in
  `about` and in triage's words.
- A Slack bot whose display name differs from the declared name is not noticed: people
  typing its Slack name in plain text are not recognised by triage. An @-mention is the
  edge's, and is unaffected.
- Adding a field to `Persona` (instructions, places) is how personas will grow.
- Enforced by `PersonaTests` (the refusals), `TurnPromptTests` (the Slack sentence, byte
  for byte), `TurnOfferTests` (a Slack turn told the declared name), `AboutTests`, and the
  compiler: `Deployment.of` will not build without a persona.
