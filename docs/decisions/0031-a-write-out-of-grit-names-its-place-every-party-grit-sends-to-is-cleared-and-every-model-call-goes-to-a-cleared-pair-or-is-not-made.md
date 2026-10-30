# 0031. A write out of grit names its place, every party grit sends to is cleared, and every model call goes to a cleared pair or is not made

Status: accepted (2026-10-07); revised (2026-10-16)

Context: ADR 0030 filters what a transaction reads of grit's own rows and floors what it
writes there. Effects that leave grit (a post to another channel, a call to an outside
service, a model call) were not checked. Turned down: each edge checking its own posts (the
rule written once per edge, against labels an edge cannot see); a call to a service as a
write to the service's place (a two-way service is both a container and a recipient, and one
label for both makes it usable only from rooms labelled exactly as it is); clearing a service
as a person (a service is never a member of a group, never asks a turn, and a reserved
principal id could collide with one an edge enrolls); clearing a model alone, or an upstream
alone (the same model at two upstreams retains differently); a pair whose upstream the router
picks (no declaration can name it); routing by the join of the window's labels (the window is
assembled after the pair is chosen); refusing a call when its pair is not cleared (it leaves a
room dark when another cleared pair could serve it).

Decision:

- **A write names its place.** A tool that writes outside grit declares its destinations as
  places, offered to the model by name in one argument core owns, `to` (`Writes.Field`). Core
  reads `to` against the declared names, and the edge's tool is told the checked destination,
  never what the model sent.
- **No write down.** A turn writes only to a place whose label is set through grit or mapped
  explicitly by the deployment (ADR 0033), with declared compartments, at a label dominating
  its classification (`Tx.writesTo`). An unmapped place, and a quiet room, is written to by no
  one.
- **Parties.** A party (`Party`) is a Bell–LaPadula subject outside grit: an outside service
  (`Party.Service`, which also covers a classifier, cleared as the service it calls) or a
  model at one named upstream (`Party.Model`); the upstream `api.anthropic.com` is Anthropic's
  own API, reached directly rather than through the router. The deployment declares each party's clearance
  (`Cleared`); one it does not name, and a pair whose upstream the router picks, is cleared for
  public alone. Content may go to a party when its clearance dominates the content's
  classification (`Clearances.clears`, `Tx.clears`). What a transaction knows is classified at
  its floor (`Classification`), made only by reading a transaction's, never from a label a
  caller picks. Sending a call's arguments to a service is this rule (`Tx.sendsTo`).
- **A service is a place and a party.** Its place's label is what it contains: a call's
  results are read under ADR 0030's read rule (`Tx.readsFrom`). Its clearance is what may be
  sent to it. A tool that declares no destination sends its arguments to the service it runs
  at, so both checks apply.
- **Routing.** Every model call is routed or not made. Each site that calls a model or a
  classifier chooses among its cleared candidates in a transaction opened for the work, and
  when none is cleared takes its degraded path (a fixed reply, a mechanical fallback, no
  judgment), never an uncleared call. A turn routes in a step of its own (`route`), before its
  offer, and records the classification it routed for; every later read whose content a model
  is sent runs in a transaction classified at most that (`Tx.locked`), so a clearance that rose
  meanwhile fails the turn. Routing is staged: safety first, by clearance alone; then the task
  and effort pick among what remains. The one call outside routing is the operator's
  `probe_pair`, which asks first and sends grit's fixed probe text.
- **The rule is defined once**, on the transaction, and checked in core when the tools are
  offered, when a call is bound, and when its request is recorded. Sending is a
  confidentiality question; integrity (ADR 0036) is declared beside it.

Consequences:

- One service place serves rooms of several labels: a service containing internal data and
  cleared for confidential in one compartment is searched from rooms at internal and at
  confidential there, and from none in another compartment.
- A room relabelled down fails safe; relabelled up, its existing threads still write at their
  old label. A turn holds its routed classification for its run (tranquility for the turn's
  subject).
- A deployment that labels rooms above public must clear its pairs and its classifier's
  service, or calls there fail closed. A party cleared for a compartment receives everything in
  it, and the provider's retention is the deployment's to judge.
- An edge that posts elsewhere than it was told, or a tool that writes beyond its service while
  declaring nothing, is outside core's view (STYLE rule 8's boundary).
- Enforced by a capture probe (`ClassificationTests`: no classification made outside core),
  `TxTests` and `RoutingTests`.
