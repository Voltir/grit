"""Hand-written roots, part B: coding, decision, meeting notes, acknowledgements. Synthetic text.

Labels as in corpus_a. A meeting-notes question takes the kind of what it asks (most are lookups
in notes it gives); an acknowledgement is chat and never hard, whatever the thread before it.
"""
from corpus_a import R, H, E

CODING = [
    R("write a regex that matches a UK postcode", "coding", E),
    R("how do I reverse a list in Python?", "coding", E),
    R("convert this to an arrow function: function add(a, b) { return a + b; }", "coding", E),
    R("what's the SQL to count rows per day in the events table?", "coding", E),
    R("write a bash one-liner to find files over 100MB in this directory", "coding", E),
    R("why does this throw? `const x = null; x.length`", "coding", E),
    R("write a function that checks if a string is a palindrome", "coding", E),
    R("how do I undo my last git commit but keep the changes?", "coding", E),
    R("add type hints to this: def greet(name, times): return [f'hi {name}'] * times", "coding", E),
    R("what's the difference between `let` and `const` in JavaScript?", "coding", E),
    R("write a cron expression for every weekday at 7:30am", "coding", E),
    R("fix this off-by-one: for i in range(1, len(xs)): print(xs[i])  # should print all items", "coding", E),
    R("write a Dockerfile for a small Flask app", "coding", E),
    R("give me a jq filter that pulls the name field out of every object in an array", "coding", E),
    R("rename the variable `tmp` to `retryCount` in this snippet: let tmp = 0; while (tmp < 3) tmp++;", "coding", E),
    R("write a unit test for a function that adds two numbers", "coding", E),
    R("how do I parse an ISO date string in Java?", "coding", E),
    R("explain what this does: `xs.reduce((a, b) => a + b, 0)`", "coding", E),
    R("write a SQL query that finds duplicate emails in the users table", "coding", E),
    R("make this Python loop a list comprehension: out = []; for x in xs: out.append(x * 2)", "coding", E),
    R("write a GitHub Actions workflow that runs npm test on every push", "coding", E),
    R("what's the Big-O of binary search?", "coding", E),
    R("write a Go function that reads a file line by line", "coding", E),
    R("refactor our payment module to support multiple currencies; it touches the models, the API, the invoices and the tests", "coding", H),
    R("migrate the web app from Webpack to Vite, keeping the env handling and the dev proxy working", "coding", H),
    R("find and fix the race condition in the job scheduler; it double-runs jobs under load", "coding", H),
    R("add rate limiting to every public API endpoint, configurable per plan, with tests", "coding", H),
    R("upgrade the backend from Django 3.2 to 5.0 and fix whatever breaks", "coding", H),
    R("implement SSO login with SAML for enterprise customers, end to end", "coding", H),
    R("rewrite the reporting queries so the monthly report runs in under a minute instead of 20", "coding", H),
    R("split the monolith's notifications code into its own service, with its own database and a migration path", "coding", H),
    R("add full-text search to the help centre, indexing the articles and keeping it in sync on edit", "coding", H),
    R("review this 1,200-line PR that changes how we handle sessions and tell me what could break", "coding", H),
    R("port the data pipeline from pandas to Spark so it handles the 2TB daily volume", "coding", H),
    R("add offline support to the mobile app so drafts sync when the connection comes back", "coding", H),
    R("our test suite takes 40 minutes; profile it and make it run in under 10", "coding", H),
    R("implement the retry-with-backoff wrapper the thread describes, in TypeScript", "coding", E,
      [("ana", "we need retries on the webhook sender: 3 attempts, exponential backoff starting at 1s"),
       ("ben", "and don't retry 4xx")]),
    R("can you write the migration for this?", "coding", E,
      [("lee", "we need a `deleted_at` timestamp column on the projects table, nullable")]),
    R("fix it please", "coding", E,
      [("raj", "the date picker shows the wrong month in January: it's using getMonth() without +1")]),
    R("write the endpoint", "coding", E,
      [("mia", "we need GET /health that returns 200 and the git sha")]),
    R("can you implement this across the services?", "coding", H,
      [("tom", "we're adding request IDs: generate at the gateway, pass in a header, log in every service"),
       ("kim", "there are seven services, three in Go, four in Python")]),
    R("go ahead and do the refactor", "coding", H,
      [("jo", "the user model is used in 60 places; we want to split it into account and profile"),
       ("sam", "the API responses must not change")]),
    R("write the script", "coding", E,
      [("ana", "need a script to delete S3 objects older than 90 days in the logs bucket")]),
    R("please add tests for the edge cases mentioned", "coding", E,
      [("ben", "parseAmount fails on negative numbers and on commas as thousands separators")]),
    R("implement the plan above", "coding", H,
      [("lee", "plan: move sessions from in-memory to Redis, add a fallback, migrate live sessions without logging people out"),
       ("raj", "and it has to work across both data centres")]),
    R("can you debug this? stack trace above", "coding", E,
      [("mia", "TypeError: Cannot read properties of undefined (reading 'map') at OrderList.render (OrderList.jsx:42)")]),
    R("write a Python script that converts a CSV into JSON", "coding", E),
    R("build a CLI tool that syncs our translation files with the translation vendor's API, handling conflicts", "coding", H),
    R("what does the `-n` flag do in grep?", "coding", E),
    R("set up end-to-end tests with Playwright for the signup, login and billing flows, running in CI", "coding", H),
]

DECISION = [
    R("pizza or tacos for the team lunch?", "decision", E),
    R("should I use tabs or spaces in this new repo?", "decision", E),
    R("which is better for a quick prototype, Flask or FastAPI?", "decision", E),
    R("should the button say 'Save' or 'Save changes'?", "decision", E),
    R("is Tuesday or Wednesday better for the retro?", "decision", E),
    R("should I name this function getUser or fetchUser?", "decision", E),
    R("blue or green for the new banner?", "decision", E),
    R("should we send the newsletter in the morning or the afternoon?", "decision", E),
    R("JSON or YAML for this small config file?", "decision", E),
    R("should I take the 9am or the 11am flight to the offsite?", "decision", E),
    R("which of these two taglines is better: 'Ship faster' or 'Build with confidence'?", "decision", E),
    R("should I squash my commits before merging this small PR?", "decision", E),
    R("Zoom or Meet for the customer call?", "decision", E),
    R("should I reply to this customer now or wait until I have the fix?", "decision", E),
    R("should the docs use 'sign in' or 'log in'?", "decision", E),
    R("is it worth upgrading my laptop's RAM from 16 to 32GB for Docker work?", "decision", E),
    R("should I write the script in bash or Python? it's about 30 lines", "decision", E),
    R("should we host the offsite in Lisbon or Barcelona?", "decision", E),
    R("one long meeting or two short ones for the planning?", "decision", E),
    R("should we choose Postgres or DynamoDB for the new event store? we expect 50k writes a second and need ad hoc queries", "decision", H),
    R("should we build our own billing system or buy one? weigh cost, time, and our pricing complexity", "decision", H),
    R("which cloud region should we expand into next, given our customer locations, latency and data residency rules?", "decision", H),
    R("should we raise prices on the Pro plan by 20%? consider churn data, competitor pricing and our margins", "decision", H),
    R("should we migrate from Kubernetes to a managed container service? weigh cost, team skills and lock-in", "decision", H),
    R("which of the three candidates should we hire for the staff engineer role? their interview notes are in the hiring folder", "decision", H),
    R("should we sunset the desktop app? look at usage, revenue from it and support load", "decision", H),
    R("monorepo or polyrepo for the new platform team, given our CI setup and the six teams involved?", "decision", H),
    R("which vendor should we pick for observability: Datadog, Grafana Cloud or Honeycomb, given our volume and budget?", "decision", H),
    R("should we open-source the SDK? think about support burden, competitors and community value", "decision", H),
    R("should we enter the healthcare market this year, given HIPAA requirements and our roadmap?", "decision", H),
    R("which of these four project proposals should get funding next quarter? the briefs are linked in the planning doc", "decision", H),
    R("should we switch the mobile app to React Native or keep two native codebases? weigh performance, hiring and the rewrite cost", "decision", H),
    R("do we move the data warehouse from Redshift to Snowflake? consider cost at our query volume, migration effort and tooling", "decision", H),
    R("which pricing model should we adopt: per seat, usage based or tiered? look at how our customers actually use the product", "decision", H),
    R("so which one should we go with?", "decision", E,
      [("ana", "lunch options for Friday: the Thai place or the burrito place"),
       ("ben", "both fine by me")]),
    R("what's your call?", "decision", E,
      [("lee", "the release is ready but QA found one cosmetic bug in the footer"),
       ("raj", "ship now or wait a day for the fix?")]),
    R("pick one for us", "decision", E,
      [("mia", "names for the new Slack channel: #platform-help or #ask-platform")]),
    R("which should we do?", "decision", H,
      [("tom", "option 1: shard the database now, 3 months of work"),
       ("kim", "option 2: buy bigger hardware, buys us a year, 40k more per year"),
       ("jo", "option 3: move the analytics reads to a replica and see")]),
    R("given all this, should we take the deal?", "decision", H,
      [("sam", "Globex wants a 3-year contract at 35% discount"),
       ("ana", "they want 4 custom features, two of which aren't on our roadmap"),
       ("ben", "it would be our biggest customer, about 15% of revenue")]),
    R("recommend one?", "decision", H,
      [("lee", "we need a new auth provider: Auth0, Cognito, or roll our own on Keycloak"),
       ("raj", "must support SAML, SCIM and 200k MAU"),
       ("mia", "budget is tight this year")]),
    R("should we go ahead?", "decision", E,
      [("tom", "the dependency bump is green on CI and only touches dev tooling")]),
    R("what would you choose?", "decision", H,
      [("kim", "we can either delay the launch two weeks to finish accessibility fixes, or launch and fix after"),
       ("jo", "a big customer's procurement requires WCAG AA"),
       ("sam", "marketing has spend booked for the launch date")]),
    R("yes or no on this?", "decision", E,
      [("ana", "can I take Friday off? nothing urgent on my plate")]),
    R("which approach is best for us?", "decision", H,
      [("ben", "for search we can use Postgres full-text, Elasticsearch, or a hosted service like Algolia"),
       ("lee", "we have 5M documents and need typo tolerance")]),
    R("should I accept the meeting or skip it?", "decision", E,
      [("raj", "invited to the vendor's product roadmap webinar, 1 hour")]),
    R("should we renew the contract or switch? the usage report and the quotes are in the thread", "decision", H,
      [("mia", "the analytics vendor quoted 60k for renewal, up from 45k"),
       ("tom", "the competitor quoted 38k but we'd have to rebuild 30 dashboards")]),
    R("which laptop should I order, the 14 or the 16 inch?", "decision", E),
    R("should we keep the free tier? weigh conversion, infrastructure cost and support load from free users", "decision", H),
    R("what should we prioritise next sprint: the export bug, the onboarding redesign, or the API docs?", "decision", E),
    R("should we adopt Rust for the new ingestion service, given nobody on the team has shipped Rust before?", "decision", H),
]

_NOTES_1 = ("Notes from Tuesday's planning:\n- launch moved to Nov 12\n- Mia owns the pricing page\n"
            "- Tom to check with legal about the terms update\n- budget for ads approved at 15k\n"
            "- next sync Thursday 10am")
_NOTES_2 = ("Standup 10/9:\nAna: finished the export API, starting on rate limits\n"
            "Ben: blocked on the staging database, waiting on infra\nLee: reviewing the SSO PR, "
            "out Friday\nRaj: on call, two pages overnight, both the queue alert")
_NOTES_3 = ("Customer call with Northwind, 10/8\n- they want SSO by March\n- unhappy with invoice "
            "format, want PO numbers on invoices\n- renewal in Q1, decision maker is their CFO\n"
            "- asked about a data export for their auditors\n- next call in two weeks")
_NOTES_4 = ("Design review: settings page\n- agreed to move notifications into its own tab\n"
            "- dark mode toggle stays in the header\n- open question: do we keep the legacy "
            "API keys section?\n- Kim to make the final mocks by Friday")
_NOTES_5 = ("Retro, sprint 42\nWent well: release was on time, fewer bugs\nWent badly: too many "
            "meetings, unclear ownership of the flaky tests, the staging env was down two days\n"
            "Actions: Ben to own flaky tests, cut the Wednesday sync, Jo to write a staging runbook")

MEETING = [
    R(_NOTES_1 + "\n\nwhen is the launch now?", "lookup", E),
    R(_NOTES_1 + "\n\nwho's talking to legal?", "lookup", E),
    R(_NOTES_1 + "\n\nhow much did we approve for ads?", "lookup", E),
    R(_NOTES_1 + "\n\nturn these into an email to the wider team", "writing", E),
    R(_NOTES_2 + "\n\nwho's blocked?", "lookup", E),
    R(_NOTES_2 + "\n\nis anyone out this week?", "lookup", E),
    R(_NOTES_2 + "\n\nwhat's Ana working on next?", "lookup", E),
    R(_NOTES_2 + "\n\nthe queue alert paged twice; look at its history and tell me whether it's a real problem or a noisy alert", "investigation", H),
    R(_NOTES_3 + "\n\nwhat does Northwind want on their invoices?", "lookup", E),
    R(_NOTES_3 + "\n\nwhen's their renewal?", "lookup", E),
    R(_NOTES_3 + "\n\nwrite a follow-up email to Northwind covering each point", "writing", E),
    R(_NOTES_3 + "\n\ncheck each of their asks against our roadmap and the billing system, and tell me what we can promise by March", "investigation", H),
    R(_NOTES_4 + "\n\nwhat did we decide about dark mode?", "lookup", E),
    R(_NOTES_4 + "\n\nwho's doing the mocks?", "lookup", E),
    R(_NOTES_4 + "\n\nshould we keep the legacy API keys section? check how many customers still use legacy keys", "decision", H),
    R(_NOTES_5 + "\n\nwhat are the action items?", "lookup", E),
    R(_NOTES_5 + "\n\nsummarise this retro in two sentences", "writing", E),
    R(_NOTES_5 + "\n\ncompare this with the last three retros and tell me which problems keep coming back", "investigation", H),
    R(_NOTES_5 + "\n\nwho owns the flaky tests now?", "lookup", E),
    R("Board meeting notes: revenue up 18% YoY, burn down to 400k/month, two new board members "
      "proposed, Series B timing discussed for Q2.\n\nwhat was the burn rate?", "lookup", E),
    R("Notes: interview debrief for the backend role. Candidate A strong on systems, weak on "
      "communication. Candidate B solid all round. Panel split 2-2.\n\nwho should we make an "
      "offer to?", "decision", E),
    R("1:1 notes with Sam: wants to move toward management, interested in mentoring the new "
      "hires, concerned about on-call load.\n\nwrite a development plan for Sam from these notes", "writing", E),
    R("Ops sync: backups verified, cert renewals scheduled, the disk alert on db-3 is still "
      "flapping, and the new monitoring vendor trial ends Friday.\n\nwhat's still open?", "lookup", E),
    R("All-hands notes: new parental leave policy (16 weeks), office closes Dec 24 to Jan 2, "
      "Q4 OKRs published in the wiki.\n\nhow long is parental leave now?", "lookup", E),
    R("Notes from the security review: rotate the API signing keys, enable MFA for all admin "
      "accounts, review S3 bucket policies, pen test booked for November.\n\nturn these into "
      "tickets with a title and a short description each", "writing", E),
    R("what did we decide about the launch date?", "lookup", E,
      [("ana", "notes from planning: launch on the 12th, freeze on the 5th, Ben owns the release checklist"),
       ("ben", "I'll share the checklist tomorrow")]),
    R("who's owning the release checklist?", "lookup", E,
      [("ana", "notes from planning: launch on the 12th, freeze on the 5th, Ben owns the release checklist")]),
    R("can you summarise the meeting for people who missed it?", "writing", E,
      [("lee", "notes: we reviewed Q3 numbers (on target), agreed to hire two more SREs, "
               "postponed the office move to January, and Raj presented the new on-call rota")]),
    R("did anyone take an action on the on-call rota?", "lookup", E,
      [("lee", "notes: Q3 numbers on target, hire two SREs, office move to January, Raj presented "
               "the new on-call rota; Mia to send it round for comments")]),
    R("what were the action items from yesterday's sync?", "lookup", E,
      [("mia", "sync notes: Tom to update the pricing page, Kim to fix the onboarding emails, "
               "Jo to chase the vendor about the contract")]),
    R("write these up as a short status update for leadership", "writing", E,
      [("tom", "weekly sync: mobile release slipped a week (App Store review), API v2 beta has 12 "
               "customers, hiring: 2 offers out, 1 accepted")]),
    R("when is the next call with them?", "lookup", E,
      [("kim", "Globex call notes: they're evaluating us against two competitors, want a security "
               "review package, next call on the 21st")]),
    R("put together the security review package they asked for, from our policies, the SOC 2 report and the infra docs", "writing", H,
      [("kim", "Globex call notes: they're evaluating us against two competitors, want a security "
               "review package, next call on the 21st")]),
    R("what was decided?", "lookup", E,
      [("jo", "notes from the architecture review: we keep Postgres, add a read replica for "
              "analytics, revisit sharding in 12 months"),
       ("sam", "and Lee writes the ADR")]),
    R("who's writing the ADR?", "lookup", E,
      [("jo", "notes from the architecture review: keep Postgres, add a read replica, revisit "
              "sharding in 12 months; Lee writes the ADR")]),
    R("is the read replica plan going to hold up? check our current replication lag and query load before I commit to it", "investigation", H,
      [("jo", "notes from the architecture review: keep Postgres, add a read replica for analytics, "
              "revisit sharding in 12 months")]),
    R("did we agree on a budget?", "lookup", E,
      [("ana", "offsite planning notes: venue shortlist is Lisbon and Porto, dates in May, "
               "budget 30k, Ben collects dietary needs")]),
    R("which venue should we pick?", "decision", E,
      [("ana", "offsite planning notes: venue shortlist is Lisbon and Porto, dates in May, budget 30k"),
       ("ben", "Porto is cheaper, Lisbon has direct flights for everyone")]),
    R("what did the customer say about pricing?", "lookup", E,
      [("raj", "Acme call: happy with the product, pricing feels high for their 20 seats, "
               "asked about a nonprofit discount, would sign this month if we can do 15% off")]),
    R("draft the reply to Acme offering the discount", "writing", E,
      [("raj", "Acme call: pricing feels high for 20 seats, would sign this month at 15% off"),
       ("mia", "approved, go ahead")]),
    R("can you turn the notes into a project plan with milestones and owners?", "writing", H,
      [("tom", "kickoff notes: migrate billing to the new provider by end of Q1; phases: "
               "data mapping, dual-write, cutover, decommission; Kim on data, Jo on dual-write, "
               "Sam on cutover; legal needs to review the new provider's DPA")]),
    R("which of these actions are already done? check the tickets", "investigation", H,
      [("lee", "last week's actions: rotate keys (Raj), fix the export bug (Ana), update the "
               "status page template (Ben), clean up old feature flags (Mia)")]),
    R("what's the date of the next review?", "lookup", E,
      [("sam", "quarterly review notes: OKRs mostly on track, the activation goal is behind; "
               "next review January 15")]),
    R("which OKR was behind?", "lookup", E,
      [("sam", "quarterly review notes: OKRs mostly on track, the activation goal is behind; "
               "next review January 15")]),
    R("why is activation behind? dig into the funnel and what we shipped this quarter", "investigation", H,
      [("sam", "quarterly review notes: OKRs mostly on track, the activation goal is behind")]),
    R("who's collecting the dietary requirements?", "lookup", E,
      [("ana", "offsite planning notes: Lisbon or Porto, May, budget 30k, Ben collects dietary needs")]),
    R("summarise these notes in three bullets", "writing", E,
      [("kim", "vendor call: they'll fix the SSO bug in their next release (two weeks), offered a "
               "10% credit for the outage, and want a case study from us in return")]),
    R("how much credit did they offer?", "lookup", E,
      [("kim", "vendor call: SSO bug fixed in two weeks, 10% credit for the outage, they want a case study")]),
    R("should we agree to the case study?", "decision", E,
      [("kim", "vendor call: SSO bug fixed in two weeks, 10% credit for the outage, they want a case study in return")]),
    R("what time did they move the demo to?", "lookup", E,
      [("jo", "notes: demo moved to 4pm Thursday, Ana presents, bring the new dashboard")]),
]

_HARD_THREADS = [
    [("ana", "the checkout latency regression is from the new fraud check: it calls the scoring API synchronously"),
     ("ben", "I moved it to an async queue with a 200ms budget and a fallback; p95 is back to 300ms")],
    [("lee", "I compared the three observability vendors against our volume, retention needs and budget"),
     ("raj", "summary: Grafana Cloud is cheapest at our volume, Honeycomb best for tracing, Datadog most expensive"),
     ("lee", "recommendation in the doc: Grafana Cloud, revisit in a year")],
    [("mia", "root cause of the duplicate invoices: the cron ran on two nodes after the failover; fixed with a lock"),
     ("tom", "I also refunded the 31 affected customers")],
    [("kim", "the Q3 investor update draft is in the folder: revenue, churn, hiring, and the outlook"),
     ("jo", "I double-checked the numbers against finance's sheet")],
    [("sam", "migration plan for the monolith split: four phases, eight weeks, rollback at each phase"),
     ("ana", "I've put the risk register at the end")],
]
_EASY_THREADS = [
    [("ben", "standup moved to 9:30 from Monday")],
    [("lee", "I've updated the README with the new setup steps")],
    [("raj", "the deploy is done, all green")],
    [("mia", "I'll take the on-call swap on Saturday")],
    [("tom", "lunch is here, kitchen")],
]
_ACK_TEXTS = [
    "thanks!", "got it, thanks", "ok", "sounds good", "perfect, thank you", "nice work",
    "great, appreciate it", "👍", "will do", "noted", "awesome, thanks for digging into this",
    "makes sense", "cool", "thanks, that helps", "ok great", "amazing, thank you both",
    "looks good to me", "ty", "on it", "brilliant", "thanks for the quick turnaround",
    "ok, thanks for letting me know", "great job everyone", "understood", "lovely, cheers",
]
ACKS = [R(t, "chat", E, (_HARD_THREADS if i % 2 == 0 else _EASY_THREADS)[(i // 2) % 5])
        for i, t in enumerate(_ACK_TEXTS + _ACK_TEXTS[::-1])]
# 25 texts twice (reversed), each paired with a thread; half follow hard work, half easy news.
