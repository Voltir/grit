# 0037. Every party grit sends to is cleared, and every model call goes to a cleared pair or is not made

Status: accepted (2026-10-13)
Context: ADR 0031 cleared outside services for what a call sends them, but left the model
provider trusted with everything, and ADR 0034 left an ask's model and a classifier's service
unchecked: a confidential conversation went whole to whichever upstream served its pair.
Alternatives turned down: clearing a model alone, or an upstream alone (the same model at two
upstreams retains differently); clearing a classifier apart from the service it calls (two
clearances for one service); clearing a pair whose upstream the router picks (no declaration can
name it); routing by the join of the window's labels (the window is assembled after the pair is
chosen); refusing a call outright when its pair is not cleared (it leaves a room dark when
another cleared pair could serve it); and choosing the pair when a turn's models are pinned
(the pinned profile is shared, so its id would split per label).

Decision:

- **Parties are Bell–LaPadula subjects.** A party is an outside service (ADR 0031's, which also
  covers a classifier: it is cleared as the service it calls) or a model at one named upstream.
  The deployment declares each party's clearance; one it does not name is cleared for public
  alone, and a pair whose upstream is left to the router has no party, so it is cleared for
  public alone whatever is declared.
- **The rule is defined once**, on the deployment's clearances: content may go to a party when
  its clearance dominates the content's classification. What a transaction knows is classified
  at its floor; a classification is made only by reading a transaction's, never from a label a
  caller picks. Sending a call's arguments to a service is this rule.
- **Every model call is routed or not made.** Each site that calls a model or a classifier
  chooses among its cleared candidates in a transaction opened for the work, and when none is
  cleared it takes its degraded path (a fixed reply, a mechanical fallback, no judgment), never
  an uncleared call. A turn routes in a step of its own, before its offer, and records the
  classification it routed for; what the turn's window was then assembled from is checked
  against it, from the labels the window recorded, so a clearance that rose and fell meanwhile
  cannot send more than was routed.
- **Routing is staged:** safety first, by clearance alone; then, only where safety left a
  choice, the task and effort the turn calls for pick among what remains. No later stage can
  undo the first.
- Each dimension is declared and checked on its own. Integrity (ADR 0036) adds its own
  declaration beside clearances, and the rule above does not change: sending is a
  confidentiality question.

Consequences: a deployment that labels rooms above public must declare clearances for its pairs
and its classifier's service, or calls there fail closed; under the default visibility every
room is public and every check passes. A party cleared for a compartment receives everything in
it. The provider's retention is the deployment's to judge when it clears a pair; grit cannot see
it. Enforced by a capture probe (no classification made outside core), core's tests of the rule,
and, per site, a test that an uncleared party is sent nothing. Amends ADR 0031's and ADR 0034's
consequences that left the model provider and the ask unchecked.
