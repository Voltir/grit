# 0031. A write out of grit names its place, and a service is a place and a party

Status: accepted (2026-10-07)

Context: ADR 0030 filtered what a transaction reads of grit's own rows and floored what it
writes there. Effects that leave grit (a post to another channel, a call to an outside service)
were not checked, so a deployment that labelled any room above public was refused beside an
edge that posts out. Leaving each edge to check its own posts was turned down: the rule would be
written once per edge, against labels an edge cannot see. Treating a call to a service as a
write to the service's place was also turned down: a two-way service is both a container and a
recipient, and a single label for both makes it usable only from rooms labelled exactly as it is.
Clearing a service as a person, a principal with groups and grants, was turned down too: a
service is never a member of a group of people, never asks a turn and never authors an entry, and
a principal's id is a string an edge enrolls, which a reserved service id could collide with.

Decision:

- A tool that writes outside grit declares its destinations as places, each offered to the model
  by a name in one argument core owns, `to`. Core reads `to` against the declared names, and the
  edge's tool is told the checked destination, never what the model sent.
- A turn writes only to a place the deployment's rooms' labeller maps explicitly, with declared
  compartments, at a label dominating its floor, its room's label (no write down). An unmapped
  place is written to by no one, whatever the floor.
- A service is a place and a party. Its place's label is what it **contains**, given as any
  room's: a call's results are read under ADR 0030's read rule. The deployment says what it is
  **trusted with** (`trusts`, public for a service it does not name): a call sends its arguments
  only when that dominates the room's label. A tool that declares no destination sends its
  arguments to the service it runs at, so both checks apply to it.
- Grants clear people, trusts clear services, and rooms and places label content.
- The rule is defined once, on the transaction, which carries the deployment's visibility:
  `Tx.writable`, `writesTo`, `readsFrom` and `sendsTo`. It is checked in core three times: when
  the tools are offered, when a call is bound, and when its request is recorded. An edge declares
  its tools' properties and is told the checked destination.

Consequences:

- Labelled rooms and posting out coexist. One service place serves rooms of several labels: a
  service containing internal data and trusted with confidential data in one compartment is
  searched from rooms at internal and at confidential in that compartment, and from none in
  another compartment.
- A conversation's label is fixed when it is created (ADR 0030), and a place's label is the
  labeller's now. A room relabelled down fails safe, its old threads' floors staying high; a room
  relabelled up does not raise its existing threads' floors, which still write at their old,
  lower label.
- An edge that posts somewhere other than where it was told, or a tool that writes beyond its
  service while declaring nothing, is outside core's view: core trusts an edge's use of its own
  SDK and its tools' declared properties (STYLE rule 8's boundary).
- The model provider receives the whole window and is trusted with everything by the deployment;
  checking it as a party, and labels per item of a service's results, are later decisions.
- Under the default visibility every place is public and every service trusted with public, so
  nothing is refused, and every tool set with no writing tool keeps its id.
