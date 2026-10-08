# 0032. A principal is a person; edges name accounts, and trusted realms attest who they are

Status: accepted (2026-10-07; amended the same day: no declared people, emails as attestations,
re-checked and never aged out)

Context: ADR 0021 gave each principal a kind asserted by its edge, and ADR 0030 cleared a
person by the grants of their groups. Each edge enrolled the people it saw under its own name
for them, so one person with a chat account and a directory account was two principals,
granted twice. Linking accounts is how that is fixed, and also how a clearance could be raised by
mistake: a wrong link gives one person another's grants. A deployment declaring its people in
code was tried and dropped: it is a hand-kept directory, the identity provider's job done badly,
and an engine started under the wrong declaration rewrote who held what. Identity systems that
link accounts safely do so only on an email that is verified, from a provider that is trusted,
in a domain the organisation owns, and they attach accounts rather than merge people. ADR 0031
kept services out of principals: a service is a place and a party, cleared by what the deployment
trusts it with.

Decision:

- grit delegates authentication to the realms a deployment trusts, and keeps authorisation:
  groups and grants are the deployment's, and clearance is computed in core.
- An account is how one source names someone, keyed by its realm and the source's stable id
  (a chat workspace's user id, a directory's user id); a principal is a person, or grit. Edges
  name accounts, never principals. An email address is never an account.
- What a person wrote is kept by the account it came through, and never rewritten. Who that
  person is is read whenever it is needed, so a change takes effect at the next read, for
  everything they wrote.
- Every account is first a person of its own, its home, which grit keeps for it and which holds
  no other account. A local operator with no realm stays that one person.
- A realm the deployment trusts (one source's accounts, such as one chat workspace) is attested
  by the one attester the deployment names for it: a source of who the realm's accounts are,
  which may also be where people talk (a chat workspace) or may not (a company directory, a
  login's identity provider). Trust names the attester, never a conversation edge. It says of
  each account whether it is a full member (not a guest, a member of another organisation, a
  bot, invited, suspended or deactivated), and the email its source verified. The attester
  reports what its source says; core decides what it means.
- Core alone decides when a source is asked: before each message an account writes, when the
  source reports a change, and once its last answer is a day old. What a source said stands
  until it says otherwise: nothing ends an attestation by its age, so a person away for weeks
  is still who they were. A source that cannot be asked keeps its last word, and grit says so
  each time it tries, as an alarm once an answer is a day overdue; only an account it has never
  answered for is refused, its message left for its source to send again.
- An attested email links only in a domain the deployment claims as its own; any other is not
  kept. With no domain claimed, nothing links by email, and membership still counts.
- Every account whose live attestation holds one claimed email is that email's person, which
  grit makes the first time any realm attests the address. Accounts in different realms are
  joined that way, and only that way.
- Accounts are attached, and people are never merged. An attestation moves one account between
  its home and one email's person. No person is ever deleted, and two people never hold one
  email, so a join that would need merging cannot arise. Linking on any other evidence (an
  administrator's decision, an operator with no realm) is a later decision, which must bring
  its refusals and a place to see them.
- Revocation is a lapse. An account has one attestation, and its person is computed from it,
  so a changed or withdrawn attestation returns the account at once, in the change itself,
  before anything relinks. A deployment that stops trusting a realm or a domain ends what they
  attested when it next starts serving its edges (a process that serves none, such as a
  terminal chat on the same database, ends nothing), keeping no address it no longer claims; since nothing ages out,
  that is the only way such trust ends. What was asked through an account before its link stays
  with its home; what was asked while linked stays with the email's person. While linked, the
  account still lists and cancels what it asked before, and the limit on what a person may have
  pending counts both together.
- No realm vetoes another. A source that ends its account's attestation moves that account
  alone; another realm's account stays with the email's person while its own source still says
  the address.
- Trusting a realm trusts its administrators. They decide who is a full member, and they can
  make a member with an address of their choosing; the deployment's claimed domains bound which
  addresses can link.
- Groups name accounts, as sources of membership do, and may name a realm, taking in every
  account it attests a full member. A person is in a group through any account of theirs, and
  is cleared for the join of those groups' grants.
- Every change an attestation makes to an account's person or membership is reported with what
  writing through the account was cleared for before and after, and the report holds no address.
- A direct message to grit is a room whose one member is the person writing it, spelled by
  their account, and labelled at their clearance. Only an account whose source never reissues
  its ids can have one (a chat user id, never an email address): a reissued id would give its new
  holder the room and everything said in it. The label is resolved through the account's
  current person: a conversation there is created at the clearance then, and every read of it is
  bounded by the clearance now, so a lower clearance lowers the direct message at once, and a
  thread begun above it takes no more messages. Its asker is always that person, whoever wrote
  a turn's opening. Only its person writes messages to it, and nothing is written to it from
  outside.
- What is said in a direct message is read only there, whatever a reader elsewhere is cleared
  for: the one rule by a room's identity rather than its label. Without it, a colleague cleared
  as high could retrieve it in a shared room and quote it there.
- A person may ask what they are cleared for. In a direct message the answer is in full: their
  clearance, the groups that give it, and their accounts by kind and evidence, never by name.
  Asked anywhere else, nothing about their clearance is said there: grit asks them, in one
  constant line, to write to it directly.
- Services stay out of principals (ADR 0031): no account is ever a service's.

Consequences:

- One person with accounts in two trusted realms is one person, cleared once, when both realms
  verify the same address in a claimed domain. Accounts with no shared verified address stay
  separate people.
- A wrong attestation moves one account and is undone by its source's next word, or by the
  deployment withdrawing its trust. Nothing is merged or deleted, so nothing needs unmerging.
- Risks accepted, each bounded:
  - A realm's administrators can mint a member with a chosen address. Bounded by the claimed
    domains.
  - An address reused after its holder leaves makes its new holder the address's person, with
    what that person has: pending schedules and their limit, and every earlier ask made through
    an account linked to the address. A chat workspace does not reissue an address its account
    still holds; a directory may, and the window is until its source's next answer, at most a
    day. A new person for a reused address is a later decision.
  - One human whose realms verify two different addresses is two people, and nothing detects
    it; "two people never hold one email" is true of grit's records, not of the world.
  - A source that withholds emails, or a partner from another organisation, links nothing
    across realms. That fails closed.
  - A guest conversion goes unseen until the realm reports it, the person next writes, or a day
    passes; longer while the source cannot be asked, during which grit warns and then alarms.
- A deployment can clear every full member of a workspace with one group and one grant, and a
  guest or a partner from another organisation stays at what it grants no one. A channel's
  label still comes from the deployment's labeller, which cannot tell a public channel from a
  private one; labelling a channel by its own audience is a later decision.
- A second trusted realm, a directory or a login, joins the same people under the same rules
  with no change to them. A login's issuer is named by a short name the deployment gives it,
  since a realm's name holds no `/`.
- "What am I cleared for" is answered only where the person alone reads it. An edge that cannot
  message a person directly cannot answer it.
- A clearance that falls leaves a direct message's earlier threads unread, never relabelled;
  the person starts a new thread. A group direct message has no one person's clearance and is
  not heard.
