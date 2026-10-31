# 0032. A principal is a person; edges name accounts, and trusted realms attest who they are

Status: accepted (2026-10-07); revised (2026-10-31)

Context: ADR 0030 clears a person by the grants of their groups. Each edge enrolled the people
it saw under its own name for them, so one person with a chat account and a directory account
was two principals, granted twice. Linking accounts fixes that, and is also how a clearance
could be raised by mistake: a wrong link gives one person another's grants. A deployment
declaring its people in code was tried and dropped (a hand-kept directory). Identity systems that link accounts safely do so only on an email
that is verified, from a trusted provider, in a domain the organisation owns, and attach
accounts rather than merge people.

Decision:

- **grit delegates authentication to the realms a deployment trusts, and keeps
  authorisation**: groups and grants are the deployment's, and clearance is computed in core.
- **An account is how one source names someone**, keyed by its realm and the source's stable
  id; a principal is a person, or grit. Edges name accounts, never principals; an email address
  is never an account; services are never principals (ADR 0031). What a person wrote is kept by
  the account it came through, never rewritten, and who that person is is read whenever needed.
- **Every account is first a person of its own, its home**, holding no other account.
- **A trusted realm is attested by the one attester the deployment names for it**, which need
  not be where people talk (a chat workspace, a directory, an identity provider). It says of
  each account whether it is a full member (`Standing.Full`, not a guest, a member of another
  organisation, a bot, invited, suspended or deactivated) and the email its source verified. The
  attester reports; core decides what it means.
- **Core alone decides when a source is asked**: before each message an account writes, when
  the source reports a change, and once its last answer is a day old. Nothing ends an
  attestation by its age. A source that cannot be asked keeps its last word, with an alarm
  once a day overdue; only an account it has never answered for is refused.
- **An attested email links only in a domain the deployment claims.** Every account whose
  attestation holds one claimed email is that email's person, made the first time any realm
  attests it; accounts in different realms are joined that way, or by a declared link.
- **An administrator may declare a link** from an account no claimed domain can name (a
  personal Google account beside a work one) to one claimed email's person. It attaches that
  account as an attestation would, and lapses when its declaration is removed. It covers one
  person's other accounts and is never a directory: who the people are is still attested.
- **Accounts are attached, and people are never merged.** An attestation moves one account
  between its home and one email's person; no person is deleted, and two people never hold one
  email. Revocation is a lapse: a changed or withdrawn attestation returns the account at once.
  A deployment that stops trusting a realm or domain ends what it attested when it next serves
  its edges. No realm vetoes another.
- **Trusting a realm trusts its administrators**, who decide who is a full member; the claimed
  domains bound which addresses can link by attestation.
- **Groups name accounts**, and may name a realm, taking in every account it attests a full
  member. A person is cleared for the join of their groups' grants. Every change to an
  account's person or membership, by attestation or link, is reported with what writing
  through it was cleared for before and after, holding no address.
- **A direct message is a room whose one member is the person writing it**, labelled at their
  clearance: created at the clearance then, every read bounded by the clearance now. Only an
  account whose source never reissues its ids can have one. Its asker is always that person,
  and nothing is written to it from outside. What is said there is read only there, whatever a
  reader elsewhere is cleared for: the one rule by a room's identity rather than its label. A
  group direct message is not served.
- **A person may ask what they are cleared for**, and is answered in full only where they alone
  read it: in a direct message, or by command, whose answer is shown to them alone (ADR 0033),
  or on an ask's ephemeral card, which only its asker sees (ADR 0017). Asked anywhere else,
  grit says nothing of it and asks them to write to it directly.

Consequences:

- One person with accounts in two trusted realms is one person, cleared once, when both verify
  the same address in a claimed domain; a wrong attestation moves one account and is undone by
  its source's next word.
- Risks accepted, each bounded: a realm's administrators can mint a member with a chosen
  address (within the claimed domains); a reused address gives its new holder that person until
  the source's next answer; one human whose realms verify two
  addresses is two people; a source that withholds emails links nothing (fails closed); a guest
  conversion goes unseen until the realm reports it, the person writes, or a day passes; a
  linked account acts as its person, so its compromise is theirs, and its trust rests on its
  holder and the administrator who linked it, never on who shared with it.
- A deployment can clear every full member of a workspace with one group and one grant.
- A clearance that falls leaves a direct message's earlier threads unread, never relabelled.
- Enforced by `IdentitiesTests` and `IdentitiesLiveTests`.
