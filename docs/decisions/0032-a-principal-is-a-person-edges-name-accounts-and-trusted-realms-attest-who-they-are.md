# 0032. A principal is a person; edges name accounts, and trusted realms attest who they are

Status: accepted (2026-10-07); revised (2026-10-31)

Context: ADR 0030 clears a person by the grants of their groups. Each edge enrolled the people
it saw under its own name for them, so one person with a chat account and a directory account
was two principals, granted twice. Linking accounts fixes that, and is also how a clearance
could be raised by mistake: a wrong link gives one person another's grants. A hand-kept
directory of people in code was tried and dropped. Identity systems that link accounts safely do so only on an email
that is verified, from a trusted provider, in a domain the organisation owns, and attach
accounts rather than merge people.

Decision:

- **grit delegates authentication to the realms a deployment trusts, and keeps
  authorisation**: groups and grants are the deployment's, and clearance is computed in core.
- **An account is how one source names someone**, keyed by its realm and the source's stable
  id; a principal is a person, or grit. Edges name accounts, never principals; an email address
  is never an account; services are never principals (ADR 0031). What a person wrote is kept by
  the account it came through, never rewritten; who that person is is read when needed.
- **Every account is first a person of its own, its home**, holding no other account.
- **A trusted realm is attested by the one attester the deployment names for it** (a chat
  workspace, a directory, an identity provider). It says of
  each account whether it is a full member (`Standing.Full`, not a guest, a member of another
  organisation, a bot, invited, suspended or deactivated) and the email its source verified. The
  attester reports; core decides what it means.
- **Core alone decides when a source is asked**: before each message an account writes, on a
  reported change, and once its last answer is a day old. Age ends no attestation: a source
  that cannot be asked keeps its last word, with an alarm once a day overdue; only an account
  it never answered for is refused.
- **An attested email links only in a domain the deployment claims.** Every account whose
  attestation holds one claimed email is that email's person, made the first time any realm
  attests it; accounts in different realms are joined that way, or by a declared link.
- **An administrator may declare a link** from an account no claimed domain names (a personal
  Google account) to one claimed email's person, attaching it until the declaration goes: for
  one person's other accounts, never a directory.
- **Accounts are attached, and people are never merged.** An attestation moves one account
  between its home and one email's person; no person is deleted, and two people never hold one
  email. Revocation is a lapse: a changed or withdrawn attestation returns the account at once.
  A realm or domain no longer trusted ends what it attested when the deployment next serves
  its edges. No realm vetoes another.
- **Trusting a realm trusts its administrators**, who decide who is a full member; the claimed
  domains bound which addresses can link by attestation.
- **Groups name accounts**, and may name a realm, taking in every account it attests a full
  member. A person is cleared for the join of their groups' grants. Every change to an
  account's person or membership, by attestation or link, is reported with what writing
  through it was cleared for before and after, holding no address.
- **A direct message is a room whose one member is the person writing it**, labelled at
  their clearance then, every read bounded by their clearance now. Only an account whose source
  never reissues its ids has one. Its asker is always that person, nothing is written to it
  from outside, and what is said there is read only there, whatever a reader elsewhere is
  cleared for: the one rule by a room's identity, not its label. Group direct messages are not
  served.
- **A person may ask what they are cleared for**, answered in full only where they alone read
  it: a direct message, a command's answer (ADR 0033), an ask's ephemeral card (ADR 0017).
  Elsewhere grit says nothing of it and asks them to write to it directly.

Consequences:

- One person with accounts in two trusted realms is one person, cleared once, when both verify
  the same address in a claimed domain; a wrong attestation moves one account and is undone by
  its source's next word.
- Risks accepted, each bounded: a realm's administrators can mint a member with a chosen
  address (within the claimed domains); a reused address gives its new holder that person until
  the source's next answer; one human whose realms verify two
  addresses is two people; a source that withholds emails links nothing (fails closed); a guest
  conversion goes unseen until the realm reports it, the person writes, or a day passes; a
  linked account acts as its person, trusted for its holder and its linker, never for who
  shared with it.
- A deployment can clear every full member of a workspace with one group and one grant.
- A clearance that falls leaves a direct message's earlier threads unread, never relabelled.
- Enforced by `IdentitiesTests` and `IdentitiesLiveTests`.
