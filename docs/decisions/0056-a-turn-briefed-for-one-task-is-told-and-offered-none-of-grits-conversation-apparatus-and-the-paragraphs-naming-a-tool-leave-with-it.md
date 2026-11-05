# 0056. A turn briefed for one task is told and offered none of grit's conversation apparatus, and the paragraphs naming a tool leave with it

Status: accepted (2026-11-04)

Context: every turn was told and offered what a conversation with people needs: how to read
grit's labels ([record], [afar], [gap], [strand], [doc]), who may see what, and grit's own tools
for it (`about`, `clearance`, `message_ids`, `file_read`). A one-message coding task (`grit
work`) has no record, no other conversation and no recall in view, and nobody to explain grit
to; on a recorded coding turn those parts were sent on every call and never used. Turned down: a
list of tool names a deployment leaves out (stringly, and blind to the paragraphs that name
those tools); a field on the deployment's `Offer` (deployment-wide, where the recipe already
shapes each turn); a choice per root on `Shaping` (a task's turn is never heard, so a briefing
that varied by root would mean nothing).

Decision: `TurnRecipe` carries a `Briefing`, `Conversing` by default. A turn whose recipe is
`Tasked` is offered none of `TurnTooling.conversing`, and told `TurnPrompt.TaskBase` (the base
less its labels paragraph) and no `TurnPrompt.Security`; the room's own label line stays.
`grit work` runs `TurnRecipe.Tasked`; the chat, `serve` and jobs run `Shipped`. Every new tool of
grit's own is sorted into `tools` or `conversing`, and every new prompt paragraph that only a
conversation with people needs, or that names a conversing tool, is told only when
`Conversing`. Labels and clearance stay enforced at the transaction and on every keep, call and
post out (ADR 0030, 0031): only their explanation leaves.

A `Tasked` turn is sound only while it posts nowhere people read. A recipe that briefs a turn
for one task and offers it a tool that posts where people read (a Slack thread) tells it
`Security` again.

`topic` stays: `TurnShown.offered` adds it to every call outside the recorded tool set, and a
turn rebuilt from what it recorded (the eval's rebuild) cannot know the recipe it ran under, so
withholding it there would make a rebuilt request differ from the one sent. It leaves when the
briefing is recorded with the offer.

Consequences: a task's turn sends a shorter fixed prefix on every call, with the conversation's
tools and prompt unchanged byte for byte. The set and fragments differ by recipe, so their ids
do, and recorded histories keep reading theirs from the store; no step changes (ADR 0004).
Enforced by the compiler (`conversing` is `Toolbox[C]`, as `tools` is; `grit.turn.probes`'
`TurnCaptureTests`) and by `TurnOfferTests`.
