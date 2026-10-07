# 0032. A principal is a person; edges name accounts, and accounts link to a person only on declared or vouched evidence

Status: accepted (2026-10-07)

Context: ADR 0021 gave each principal a kind asserted by its edge, and ADR 0030 cleared a
person by the grants of their groups. Each edge enrolled the people it saw under its own name
for them, so one person with a Slack account and a Google account was two principals,
granted twice, and sharing by address (a document's, a group's) could not be matched to a
person asking in Slack. Linking accounts is how that is fixed, and also how a clearance could be
raised by mistake: a wrong link gives one person another's grants. ADR 0031 kept services out of
principals: a service is a place and a party, cleared by what the deployment trusts it with.

Decision:

- An account is how one source names someone; a principal is a person, or grit. Edges name
  accounts, never principals. Every account belongs to one principal; a principal's id is
  grit's, minted when it first sees or is told of the person, and names no account.
- What a person wrote is kept by the account it came through, and never rewritten. Who that
  person is is read through the account's current link whenever it is needed, so a link made
  or undone takes effect at the next read, for everything they wrote.
- An account is linked to a person on one of three kinds of evidence. Seen for the first time,
  it is a person of its own. A deployment may declare a person by the accounts it knows them by,
  and only its declaration moves a declared account. A realm the deployment trusts (one source's
  accounts, such as one Slack workspace) may vouch, through the one edge the deployment names
  for it, for the email its source verified for a full member there; nothing else's word about
  an email is kept.
- A vouching attaches one account that is a person of its own, or an email no one holds; it never
  joins two people, which only a declaration does. A vouched link lasts while it is vouched: a
  withdrawn or changed vouching, or a realm no longer trusted, returns the account to the person
  of its own it was when first seen, which grit keeps for it. A changed vouching returns it before
  anything is relinked, so no email an account brings keeps it on another person.
- The same vouching says whether the account is a full member of the realm (not a guest, a
  member of another organisation, a bot, invited or deactivated), and that standing lasts, as
  a vouched link does, while it is vouched.
- Trusting a realm trusts its administrators: where they can change a member's email, they can
  link that member to another person, and they decide who is a full member.
- Groups name accounts, as the sources of membership do, and may name a realm, taking in every
  account it vouches a full member. A person is in a group through any account linked to them,
  and is cleared for the join of those groups' grants.
- Every change a vouching makes is reported with what writing through the account was cleared
  for before and after.
- A direct message to grit is a room whose one member is the person writing it, spelled by
  their account, and labelled at their clearance. Only an account whose source never reissues
  its ids can have one (a chat user id, never an email address): a reissued id would give its new
  holder the room and everything said in it. The label is resolved through the account's current
  link: a conversation there is created at the clearance then, and every read of it is bounded
  by the clearance now, so a lower clearance lowers the direct message at once, and a thread
  begun above it takes no more messages. Its asker is always that person, whoever wrote a turn's
  opening. Only its person writes messages to it, and nothing is written to it from outside.
- What is said in a direct message is read only there, whatever a reader elsewhere is cleared
  for: the one rule by a room's identity rather than its label. Without it, a colleague cleared
  as high could retrieve it in a shared room and quote it there.
- A person may ask what they are cleared for. In a direct message the answer is in full: their
  clearance, the groups that give it, and their accounts by kind and evidence, never by name.
  Asked anywhere else, nothing about their clearance is said there: grit asks them, in one
  constant line, to write to it directly.
- Services stay out of principals (ADR 0031): no account is ever a service's.

Consequences:

- Sharing by address can be matched to a person asking anywhere their accounts are linked, and a
  deployment can declare a person once instead of granting each account.
- A wrong vouching moves one account and is undone by withdrawing it: the account returns to the
  person it was, with what was asked through it before the move, and what it wrote was kept by
  account. What was asked through it while it was linked stays with the person it was linked
  to. Only a declaration deletes a person, after moving to the declared person everything that
  named them.
- A source that cannot say whether an address is verified, or whether someone is a full member,
  links nothing by email; its people are linked only by declaration.
- Joining two people who each hold several accounts waits for a declaration, and later for an
  authority's decision.
- A deployment can clear every full member of a workspace with one group and one grant, and a
  guest or a partner from another organisation stays at what it grants no one. A channel's
  label still comes from the deployment's labeller, which cannot tell a public channel from a
  private one; labelling a channel by its own audience is a later decision.
- "What am I cleared for" is answered only where the person alone reads it. An edge that cannot
  message a person directly cannot answer it.
- A clearance that falls leaves a direct message's earlier threads unread, never relabelled;
  the person starts a new thread. A group direct message has no one person's clearance and is
  not heard.
