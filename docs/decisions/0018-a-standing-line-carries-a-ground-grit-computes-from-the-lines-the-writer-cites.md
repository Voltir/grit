# 0018. A Standing line carries a ground grit computes from the lines the writer cites

Status: accepted (2026-09-27)

Context: a closing's Standing lines are what later periods treat as settled (ADR 0011).
The closing writer read only the period's messages, so it could not tell a fact a tool
showed from one the assistant made up, and a real closing kept a model's invented
definitions as Standing. A prompt rule alone ("an unconfirmed claim is not Standing") did
not change what it kept, and a forged grit label in a message is believed whatever the
prompt says, so the prompt cannot guard provenance. The alternatives: a prompt rule; the
writer stating each item's source itself (the model vouching for its own claims);
dropping unconfirmed claims (losing decisions the assistant proposed and nobody
contradicted).

Decision: each Standing line records its ground: `Person`, `Tool` or `Claimed`. The
writer's transcript shows each tool call as one clipped line, and labels every line it is
shown (u, a, t). The writer cites the labels that established each Standing item, and
grit maps the citation to the ground: a person's line wins, then a tool line whose result
succeeded, otherwise Claimed. A tool line whose result is an error (failed, declined,
interrupted, gone) grounds nothing, and the writer is told to cite where the person
stated a thing, never where they asked. Grounds are computed only over the lines the
writer was shown. An uncited item or unknown label is Claimed, never dropped. A ground is
copied verbatim when the line is carried, and never changed by a touch. The window's
record lists Claimed Standing apart, as said by the assistant and not confirmed, and the
base prompt says to check it before relying on it. Closings are stored as version 3;
version-2 Standing lines read as Claimed.

Consequences: a later turn can tell what it may rely on from what it should check;
claims made while working on code will carry the same mark when code mode lands. Grounds
are only as good as the writer's citations: a citation can raise an item no higher than
the strongest real, shown, non-error line it names, so a writer that cites a question,
or a successful but empty result, can still over-ground an item. Labels are grit's
structure, not text: a message that forges a label is still one person or tool line, so
forgery can never raise a ground above what the period's real lines offer, and never
invents a line. Open lines and topics carry none; upgrading on a touch, and evicting
Claimed first, are deferred. Enforced by `Line.of`, `Edit`'s shape (a Standing line is
added only with a ground), `ClosingJsonTests`, `ClosingSummaryTests` and
`LifecycleReplayTests`.
