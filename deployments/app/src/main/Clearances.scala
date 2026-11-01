package grit.app.main

import grit.app.config.Listed.csv
import grit.core.identity.Account
import grit.core.model.{Ladder, Policy}
import grit.core.place.{Place, Service}
import grit.core.visibility.{
  Cap,
  Cleared,
  Compartments,
  Group,
  GroupName,
  Integrity,
  Label,
  Labelled,
  Level,
  Party,
  RoomLabels,
  Visibility
}
import grit.mcp.github.GitHubItems
import grit.models.{JevClassifier, ModelRole, StubClassifier, StubModels}
import grit.slack.edge.{SlackAccounts, SlackEdge}
import grit.slack.event.{TeamId, UserId}

import ServedEdges.*

/** Who administers a deployment's visibility, what it grants, and which roles a label clears. */
private[main] object Clearances {

  /** The variable naming the Slack users who administer grit: their ids, comma-separated. */
  private[main] val AdminsVar = "GRIT_ADMINS"

  /** The group [[AdminsVar]] declares, the deployment's administrators
    * ([[grit.core.visibility.Visibility.administrators]]).
    */
  private[main] val Administrators: GroupName =
    // A group's name by GroupName.of's rule, so the Left is never taken.
    GroupName.of("administrators").fold(why => throw new IllegalStateException(why), identity)

  /** The Slack users `env` names administrators ([[AdminsVar]]), none when unset; why not,
    * naming the variable, at the first that is not a Slack user's id (`U…` or `W…`).
    */
  private[main] def admins(env: Map[String, String]): Either[String, Vector[UserId]] =
    csv(env, AdminsVar)
      .foldLeft[Either[String, Vector[UserId]]](Right(Vector.empty)) { (acc, raw) =>
        acc.flatMap(ids =>
          Either.cond(
            raw.matches("[UW][A-Z0-9]+"),
            ids :+ UserId(raw),
            s"$AdminsVar: $raw is not a Slack user's id (U… or W…, as their profile's Copy member ID gives it)"
          )
        )
      }

  /** The reference deployment's visibility, clearing `parties`: with Slack served from the
    * workspace `slackIn` and `admins` named, the shipped one but for [[Administrators]],
    * declared as those users' accounts there (`slack:{team}/{id}`), who alone may lower a
    * room's label or label a public channel; otherwise [[Visibility.Shipped]], under which no
    * one may, with `parties` cleared. Either way what is said at its places is capped as `caps`
    * declare ([[caps]]), and each of `shared`, a service's place it shares only with the people
    * it names, is invited and labelled internal, so each room within it is unmapped until a
    * person labels it. A party named twice is refused.
    */
  private[main] def visibility(
      admins: Vector[UserId],
      slackIn: Option[TeamId],
      parties: Vector[Cleared],
      caps: Vector[Cap],
      shared: Vector[Place]
  ): Either[String, Visibility] = {
    val rooms = RoomLabels
      .of(shared.map(_ -> Label.at(Level.Internal)), Labelled.Mapped(Label.Public))
      .left
      .map(place => s"a shared place declared twice: ${place.written}")
    rooms.flatMap(rooms =>
      slackIn.filter(_ => admins.nonEmpty) match {
        case None =>
          Visibility
            .of(
              Compartments.Shipped,
              rooms,
              Vector.empty,
              Vector.empty,
              parties,
              caps = caps,
              invited = shared
            )
            .left
            .map(r => s"the deployment's clearances: $r")
        case Some(team) =>
          admins
            .foldLeft[Either[String, Set[Account]]](Right(Set.empty))((acc, user) =>
              acc.flatMap(as => SlackAccounts.account(team, user).map(as + _))
            )
            .left
            .map(why => s"$AdminsVar: $why")
            .flatMap(accounts =>
              Visibility
                .of(
                  Compartments.Shipped,
                  rooms,
                  Vector(Group(Administrators, accounts)),
                  Vector.empty,
                  parties,
                  administrators = Some(Administrators),
                  caps = caps,
                  invited = shared
                )
                .left
                .map(r => s"$AdminsVar: $r")
            )
      }
    )
  }

  /** The reference deployment's caps (ADR 0036) as `env` declares them: Slack's posting
    * service's results ([[SlackEdge.PostsAt]]) at `Member`, since each is its own edge's account
    * of a post it made; each repository [[GithubMemberReposVar]] names at `Member`; the rest of
    * GitHub at `Untrusted`, since its token may reach repositories anyone may write in. Any other
    * service's are `Untrusted`, undeclared, and no room is capped. Why not, naming the variable,
    * when it names a repository that is not `{owner}/{name}`.
    */
  private[main] def caps(env: Map[String, String]): Either[String, Vector[Cap]] =
    for {
      github <- Service.of("github")
      members <- csv(env, GithubMemberReposVar)
        .foldLeft[Either[String, Vector[Place]]](Right(Vector.empty))((kept, name) =>
          kept.flatMap(ps => GitHubItems.repository(github, name).map(ps :+ _))
        )
        .left
        .map(why => s"$GithubMemberReposVar: $why")
    } yield Vector(
      Cap(SlackEdge.PostsAt.place, Integrity.Member),
      Cap(github.place, Integrity.Untrusted)
    ) ++ members.distinct.map(Cap(_, Integrity.Member))

  /** The reference deployment's clearances (ADR 0031), each at [[Compartments.Shipped]]'s top:
    * every pair `policy` pins that names its upstream ([[Party.of]]; grit's own deployment
    * names one only through an operator's provider variable, and that choice is the trust),
    * the stub's pair, and the services of Jev and the stub classifier. Each party once, in
    * that order. A pair whose upstream OpenRouter picks is cleared for public alone.
    */
  private[main] def cleared(policy: Policy): Vector[Cleared] = {
    val pinned = policy.ladders
      .flatMap(_.rungs)
      .flatMap(r => Party.of(r.assignment.ref))
    val always = Party.of(StubModels.Ref).toVector ++ Vector(
      Party.Service(JevClassifier.Service),
      Party.Service(StubClassifier.Service)
    )
    (pinned ++ always).distinct.map(Cleared(_, Compartments.Shipped.top))
  }

  /** What `grit serve` says at start, served by OpenRouter under `policy`, of each role whose
    * first pin's upstream OpenRouter picks, so whose calls a labelled room cannot make: the
    * variable that would name its one upstream, and the clearance that pair then gets
    * ([[cleared]]); the dream summary's adds that it follows no other role's variables.
    */
  private[main] def uncleared(policy: Policy): Vector[String] = {
    def routed(l: Ladder) = Party.of(l.first.assignment.ref).isEmpty
    def name(role: ModelRole) = role match {
      case ModelRole.Turn => "turn"
      case ModelRole.Summary => "summary"
      case ModelRole.Query => "query"
      case ModelRole.Heard => "heard closing"
      case ModelRole.Dream => "dream summary"
    }
    ModelRole.values.toVector.collect {
      case role if routed(role.in(policy)) =>
        s"${role.upstreamVar(role.in(policy).first.grade)} is unset: the ${name(role)}'s first " +
          "model, whose upstream OpenRouter picks, is cleared for public rooms alone; naming its " +
          "one upstream there clears that pair for every label" +
          (if (role == ModelRole.Dream) s" (the ${name(role)} follows no other role's variables)"
           else "")
    }
  }

  /** What `grit serve` says at start when `admins` names no one; nothing when it names any. */
  private[main] def unadministered(admins: Vector[UserId]): Option[String] =
    Option.when(admins.isEmpty)(
      s"$AdminsVar is unset: no one can lower a room's label, label a public channel or set a " +
        "room's baseline"
    )
}
